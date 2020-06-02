/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#![allow(unused_parens)]

use compile::compile_from_file;
use evm::compile_evm_file;
use link::{link, postlink_compile};
use run::{run_from_file, runtime_env::RuntimeEnvironment};
use std::fs::File;
use std::io;
use std::path::Path;

use clap::{App, Arg, SubCommand};

mod build_builtins;
pub mod compile;
pub mod evm;
pub mod link;
pub mod mavm;
#[cfg(test)]
pub mod minitests;
pub mod pos;
pub mod run;
pub mod stringtable;
pub mod uint256;

fn main() {
    let matches = App::new("Mini compiler")
        .version("0.1")
        .author("Ed Felten <ed@offchainlabs.com")
        .about("compiles the Mini language")
        .subcommand(
            SubCommand::with_name("compile")
                .about("compile a source file")
                .arg(
                    Arg::with_name("INPUT")
                        .help("sets the file name to compile")
                        .required(true)
                        .multiple(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("output")
                        .help("sets the output file name")
                        .short("o")
                        .takes_value(true)
                        .value_name("output"),
                )
                .arg(
                    Arg::with_name("format")
                        .help("sets the output format")
                        .short("f")
                        .takes_value(true)
                        .value_name("format"),
                )
                .arg(
                    Arg::with_name("compileonly")
                        .help("compile only not link")
                        .short("c")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("module")
                        .help("compile as loadable module")
                        .short("m")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("debug")
                        .help("provide debug output")
                        .short("d")
                        .takes_value(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("run")
                .about("run a compiled source file")
                .arg(
                    Arg::with_name("INPUT")
                        .help("sets the file name to run")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("evm")
                .about("compile an EVM/Truffle file")
                .arg(
                    Arg::with_name("INPUT")
                        .help("sets the file name to compile")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("format")
                        .help("sets the output format")
                        .short("f")
                        .takes_value(true)
                        .value_name("format"),
                )
                .arg(
                    Arg::with_name("output")
                        .help("sets the output file name")
                        .short("o")
                        .takes_value(true)
                        .value_name("output"),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("compile") {
        let debug_mode = matches.is_present("debug");
        let mut output = get_output(matches.value_of("output")).unwrap();
        let filenames: Vec<_> = matches.values_of("INPUT").unwrap().collect();
        if matches.is_present("compileonly") {
            let filename = filenames[0];
            let path = Path::new(filename);
            match compile_from_file(path, debug_mode) {
                Ok(compiled_program) => {
                    compiled_program.to_output(&mut *output, matches.value_of("format"));
                }
                Err(e) => {
                    println!("Compilation error: {:?}", e);
                }
            }
        } else {
            let mut compiled_progs = Vec::new();
            for filename in filenames {
                let path = Path::new(filename);
                match compile_from_file(path, debug_mode) {
                    Ok(compiled_program) => {
                        compiled_progs.push(compiled_program);
                    }
                    Err(e) => {
                        println!("Compilation error: {}", e);
                        return;
                    }
                }
            }

            match link(&compiled_progs, matches.is_present("module")) {
                Ok(linked_prog) => match postlink_compile(linked_prog, false, Vec::new(), debug_mode) {
                    Ok(completed_program) => {
                        completed_program.to_output(&mut *output, matches.value_of("format"));
                    }
                    Err(e) => {
                        println!("Linking error: {}", e);
                        return;
                    }
                },
                Err(e) => {
                    println!("Linking error: {}", e);
                }
            }
        }
    }

    if let Some(matches) = matches.subcommand_matches("run") {
        let filename = matches.value_of("INPUT").unwrap();
        let path = Path::new(filename);
        let env = RuntimeEnvironment::new();
        match run_from_file(path, Vec::new(), env) {
            Ok(logs) => {
                println!("Logs: {:?}", logs);
            }
            Err(e) => {
                println!("{:?}", e);
            }
        }
    }

    if let Some(matches) = matches.subcommand_matches("evm") {
        let debug_mode = matches.is_present("debug");  
        let mut output = get_output(matches.value_of("output")).unwrap();
        let filename = matches.value_of("INPUT").unwrap();
        let path = Path::new(filename);
        match compile_evm_file(path, debug_mode) {
           Ok(compiled_contracts) => { 
                for contract in compiled_contracts {
                    contract.to_output(&mut *output, matches.value_of("format")); 
                }
            } 
            Err(e) => {
                panic!("Compilation error: {}", e);
            }
        }
    }
}

fn get_output(output_filename: Option<&str>) -> Result<Box<dyn io::Write>, io::Error> {
    match output_filename {
        Some(ref path) => File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>),
        None => Ok(Box::new(io::stdout())),
    }
}
