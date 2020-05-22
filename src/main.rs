#![allow(unused_parens)]

use std::path::Path;
use std::fs::File;
use std::io;
use crate::compile::{compile_from_file};
use crate::link::{link, postlink_compile};
use crate::evm::compile_evm_file;
use crate::run::{run_from_file};

extern crate bincode;
extern crate clap;
use clap::{Arg,App,SubCommand};

pub mod compile;
pub mod run;
pub mod link;
pub mod ast;
pub mod typecheck;
pub mod symtable;
pub mod stringtable;
pub mod mavm;
pub mod uint256;
pub mod codegen;
pub mod striplabels;
pub mod xformcode;
pub mod optimize;
pub mod emulator;
pub mod builtins;
pub mod source;
pub mod pos;
pub mod evm;
pub mod build_builtins;
#[cfg(test)]
pub mod minitests;

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub mini); 


fn main() {
    let matches = App::new("Mini compiler")
                    .version("0.1")
                    .author("Ed Felten <ed@offchainlabs.com")
                    .about("compiles the Mini language")
                    .subcommand(SubCommand::with_name("compile")
                        .about("compile a source file")
                        .arg(Arg::with_name("INPUT")
                            .help("sets the file name to compile")
                            .required(true)
                            .multiple(true)
                            .index(1))
                        .arg(Arg::with_name("output")
                            .help("sets the output file name")
                            .short("o")
                            .takes_value(true)
                            .value_name("output"))
                        .arg(Arg::with_name("format")
                            .help("sets the output format")
                            .short("f")
                            .takes_value(true)
                            .value_name("format"))
                        .arg(Arg::with_name("compileonly")
                            .help("compile only not link")
                            .short("c")
                            .takes_value(false))
                        .arg(Arg::with_name("debug")
                            .help("provide debug output")
                            .short("d")
                            .takes_value(false)))
                    .subcommand(SubCommand::with_name("run")
                        .about("run a compiled source file")
                        .arg(Arg::with_name("INPUT")
                            .help("sets the file name to run")
                            .required(true)
                            .index(1)))
                    .subcommand(SubCommand::with_name("evm")
                        .about("compile an EVM/Truffle file")
                        .arg(Arg::with_name("INPUT")
                            .help("sets the file name to compile")
                            .required(true)
                            .index(1))
                        .arg(Arg::with_name("format")
                            .help("sets the output format")
                            .short("f")
                            .takes_value(true)
                            .value_name("format"))                       
                        .arg(Arg::with_name("output")
                            .help("sets the output file name")
                            .short("o")
                            .takes_value(true)
                            .value_name("output")))
                    .get_matches();


    if let Some(matches) = matches.subcommand_matches("compile") {
        let debug_mode = matches.is_present("debug");  
        let mut output = get_output(matches.value_of("output")).unwrap();
        let filenames: Vec<_> = matches.values_of("INPUT").unwrap().collect();
        if matches.is_present("compileonly") {
            let filename = filenames[0];
            let path = Path::new(filename); 
            match compile_from_file(path, debug_mode) {
                Ok(compiled_program) => { compiled_program.to_output(&mut *output, matches.value_of("format")); }
                Err(e) => { 
                    panic!("Compilation error: {:?}", e); 
                }
            }
        } else {
            let mut compiled_progs = Vec::new();
            for filename in filenames {
                let path = Path::new(filename); 
                match compile_from_file(path, debug_mode) {
                    Ok(compiled_program) => { compiled_progs.push(compiled_program); }
                    Err(e) => { panic!("Compilation error: {:?}", e); }
                }
            }

            match link(&compiled_progs) {
                Ok(linked_prog) => match postlink_compile(linked_prog, debug_mode) {
                    Ok(completed_program) => { 
                        completed_program.to_output(&mut *output, matches.value_of("format")); 
                    }
                    Err(e) => { panic!("Linking error: {:?}", e); }
                }
                Err(e) => {
                    panic!("Linking error: {:?}", e);
                }
            }
        }
    }

    if let Some(matches) = matches.subcommand_matches("run") {
        let filename = matches.value_of("INPUT").unwrap();
        let path = Path::new(filename);
        match run_from_file(path, Vec::new()) {
            Ok(val) => {
                println!("Result: {}", val);
            }
            Err(e) => {
                println!("{:?}", e);
            }
        }
    }

    if let Some(matches) = matches.subcommand_matches("evm") {
        let mut output = get_output(matches.value_of("output")).unwrap();
        let filename = matches.value_of("INPUT").unwrap();
        let path = Path::new(filename);
        match compile_evm_file(path) {
            Ok(compiled_program) => { compiled_program.to_output(&mut *output, matches.value_of("format")); }
            Err(e) => { panic!("Compilation error: {:?}", e); }
        }
    }
}

fn get_output(output_filename: Option<&str>) -> Result<Box<dyn io::Write>, io::Error> {
    match output_filename {
        Some(ref path) => File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>),
        None => Ok(Box::new(io::stdout())),        
    }
}
