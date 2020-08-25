/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

#![allow(unused_parens)]

use compile::{compile_from_file, CompileError};
use contracttemplates::generate_contract_template_file_or_die;
use link::{link, postlink_compile};
use mavm::Value;
use run::{profile_gen_from_file, replay_from_testlog_file, run_from_file, RuntimeEnvironment};
use std::collections::{hash_map::DefaultHasher, HashMap};
use std::fs::File;
use std::hash::Hasher;
use std::io;
use std::path::Path;

use crate::uint256::Uint256;
use clap::{App, Arg, SubCommand};

mod compile;
mod contracttemplates;
mod evm;
mod link;
mod mavm;
#[cfg(test)]
mod minitests;
pub mod pos;
mod run;
mod stringtable;
mod uint256;

fn main() -> Result<(), CompileError> {
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
                )
                .arg(
                    Arg::with_name("typecheck")
                        .help("typechecks imported functions")
                        .short("t")
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
                )
                .arg(
                    Arg::with_name("debug")
                        .help("provide debug output")
                        .short("d")
                        .takes_value(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("evmdebug")
                .about("debug the EVM functionality")
                .arg(
                    Arg::with_name("debug")
                        .help("sets debug mode")
                        .short("d")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("profiler")
                        .help("sets profiler mode")
                        .short("p")
                        .takes_value(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("profiler")
                .about("generates info on where arb gas is being used")
                .arg(
                    Arg::with_name("INPUT")
                        .help("sets the file name to run")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("replay")
                .about("replays an execution from a testlogs file")
                .arg(
                    Arg::with_name("INPUT")
                        .help("sets the file name to replay")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("debug")
                        .help("sets debug mode")
                        .short("d")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("profiler")
                        .help("sets profiler mode")
                        .short("p")
                        .takes_value(false),
                )
                .arg(
                    Arg::with_name("trace")
                        .help("sets the file to write execution trace to")
                        .short("t")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("maketestlogs").about("generates test logs for all ArbOS tests"),
        )
        .subcommand(
            SubCommand::with_name("makebenchmarks")
                .about("generates logs for all ArbOS benchmarks"),
        )
        .subcommand(
            SubCommand::with_name("maketemplates").about("generates code for contract templates"),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("compile") {
        let debug_mode = matches.is_present("debug");
        let typecheck = matches.is_present("typecheck");
        let mut output = get_output(matches.value_of("output")).unwrap();
        let filenames: Vec<_> = matches.values_of("INPUT").unwrap().collect();
        let mut file_name_chart = HashMap::new();
        if matches.is_present("compileonly") {
            let filename = filenames[0];
            let path = Path::new(filename);
            let mut file_hasher = DefaultHasher::new();
            file_hasher.write(filename.as_bytes());
            let file_id = file_hasher.finish();
            file_name_chart.insert(file_id, filename.to_string());
            match compile_from_file(path, file_id, debug_mode) {
                Ok(mut compiled_program) => {
                    compiled_program.file_name_chart.extend(file_name_chart);
                    compiled_program.to_output(&mut *output, matches.value_of("format"));
                }
                Err(e) => {
                    println!("Compilation error: {:?}\nIn file: {}", e, filename);
                    return Err(e);
                }
            }
        } else {
            let mut compiled_progs = Vec::new();
            for filename in &filenames {
                let path = Path::new(filename);
                let mut file_hasher = DefaultHasher::new();
                file_hasher.write(filename.as_bytes());
                let file_id = file_hasher.finish();
                file_name_chart.insert(file_id, (*filename).to_string());
                match compile_from_file(path, file_id, debug_mode) {
                    Ok(compiled_program) => {
                        file_name_chart.extend(compiled_program.file_name_chart.clone());
                        compiled_progs.push(compiled_program);
                    }
                    Err(e) => {
                        println!("Compilation error: {}\nIn file: {}", e, filename);
                        return Err(e);
                    }
                }
            }

            let is_module = matches.is_present("module");
            match link(&compiled_progs, is_module, Some(Value::none()), typecheck) {
                Ok(linked_prog) => {
                    match postlink_compile(
                        linked_prog,
                        is_module,
                        Vec::new(),
                        file_name_chart,
                        debug_mode,
                    ) {
                        Ok(completed_program) => {
                            completed_program.to_output(&mut *output, matches.value_of("format"));
                        }
                        Err(e) => {
                            println!("Linking error: {}", e);
                            return Err(e);
                        }
                    }
                }
                Err(e) => {
                    println!("Linking error: {}", e);
                    return Err(e);
                }
            }
        }
    }

    if let Some(matches) = matches.subcommand_matches("run") {
        let filename = matches.value_of("INPUT").unwrap();
        let debug = matches.is_present("debug");
        let path = Path::new(filename);
        let env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
        match run_from_file(path, Vec::new(), env, debug) {
            Ok(logs) => {
                println!("Logs: {:?}", logs);
            }
            Err(e) => {
                println!("{:?}", e);
            }
        }
    }

    if let Some(matches) = matches.subcommand_matches("evmdebug") {
        let debug = matches.is_present("debug");
        let profile = matches.is_present("profiler");
        let _ = evm::evm_xcontract_call_with_constructors(None, debug, profile);
        //let _ = evm::evm_test_sequencer_support(None, debug);
    }

    if let Some(matches) = matches.subcommand_matches("profiler") {
        let path = matches.value_of("INPUT").unwrap();
        profile_gen_from_file(
            path.as_ref(),
            Vec::new(),
            RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        );
    }

    if let Some(matches) = matches.subcommand_matches("replay") {
        let path = matches.value_of("INPUT").unwrap();
        let debug = matches.is_present("debug");
        let profiler = matches.is_present("profiler");
        let trace_file = matches.value_of("trace");

        if let Err(e) = replay_from_testlog_file(path, true, debug, profiler, trace_file) {
            panic!("Error reading from {}: {}", path, e);
        }
    }

    if matches.subcommand_matches("maketestlogs").is_some() {
        evm::make_logs_for_all_arbos_tests();
    }

    if matches.subcommand_matches("makebenchmarks").is_some() {
        evm::benchmarks::make_benchmarks();
    }

    if matches.subcommand_matches("maketemplates").is_some() {
        let path = Path::new("arb_os/contractTemplates.mini");
        generate_contract_template_file_or_die(path);
    }

    Ok(())
}

fn get_output(output_filename: Option<&str>) -> Result<Box<dyn io::Write>, io::Error> {
    match output_filename {
        Some(ref path) => File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>),
        None => Ok(Box::new(io::stdout())),
    }
}
