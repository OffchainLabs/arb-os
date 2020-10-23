/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

#![allow(unused_parens)]

use compile::{compile_from_file, CompileError};
use contracttemplates::generate_contract_template_file_or_die;
use link::{link, postlink_compile};
use mavm::Value;
use run::{profile_gen_from_file, replay_from_testlog_file, run_from_file, RuntimeEnvironment};
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::path::Path;

use crate::uint256::Uint256;
use clap::Clap;

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

#[derive(Clap, Debug)]
struct CompileStruct {
    input: Vec<String>,
    #[clap(short, long)]
    debug_mode: bool,
    #[clap(short, long)]
    typecheck: bool,
    #[clap(short, long)]
    output: Option<String>,
    #[clap(short, long)]
    compile_only: bool,
    #[clap(short, long)]
    format: Option<String>,
    #[clap(short, long)]
    module: bool,
}

#[derive(Clap, Debug)]
struct RunStruct {
    input: String,
    #[clap(short, long)]
    debug: bool,
}

#[derive(Clap, Debug)]
struct EvmDebug {
    #[clap(short, long)]
    debug: bool,
    #[clap(short, long)]
    profiler: bool,
}

#[derive(Clap, Debug)]
struct Replay {
    input: String,
    #[clap(short, long)]
    debug: bool,
    #[clap(short, long)]
    profiler: bool,
    #[clap(short, long)]
    trace: Option<String>,
}

#[derive(Clap, Debug)]
struct Input {
    input: String,
}

#[derive(Clap, Debug)]
enum Args {
    Compile(CompileStruct),
    Run(RunStruct),
    EvmDebug(EvmDebug),
    Profiler(Input),
    Replay(Replay),
    MakeTestLogs,
    MakeBenchmarks,
    MakeTemplates,
}

fn main() -> Result<(), CompileError> {
    let matches = Args::parse(); /*App::new("Mini compiler")
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
                                 .get_matches();*/

    println!("ARGS: {:?}", matches);

    match matches {
        Args::Compile(compile) => {
            let debug_mode = compile.debug_mode;
            let typecheck = compile.typecheck;
            let mut output = get_output(compile.output.as_deref()).unwrap();
            let filenames: Vec<_> = compile.input.clone();
            let mut file_name_chart = HashMap::new();
            if compile.compile_only {
                let filename = &filenames[0];
                let path = Path::new(filename);
                match compile_from_file(path, &mut file_name_chart, debug_mode) {
                    Ok(mut compiled_program) => {
                        compiled_program.iter_mut().for_each(|prog| {
                            prog.file_name_chart.extend(file_name_chart.clone());
                            prog.to_output(&mut *output, compile.format.as_deref());
                        });
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
                    match compile_from_file(path, &mut file_name_chart, debug_mode) {
                        Ok(compiled_program) => {
                            compiled_program.into_iter().for_each(|prog| {
                                file_name_chart.extend(prog.file_name_chart.clone());
                                compiled_progs.push(prog)
                            });
                        }
                        Err(e) => {
                            println!(
                                "Compilation error: {}\nIn file: {}",
                                e,
                                e.location
                                    .map(|loc| file_name_chart
                                        .get(&loc.file_id)
                                        .unwrap_or(&loc.file_id.to_string())
                                        .clone())
                                    .unwrap_or("Unknown".to_string())
                            );
                            return Err(e);
                        }
                    }
                }

                let is_module = compile.module;
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
                                completed_program
                                    .to_output(&mut *output, compile.format.as_deref());
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

        Args::Run(run) => {
            let filename = run.input;
            let debug = run.debug;
            let path = Path::new(&filename);
            let env = RuntimeEnvironment::new(Uint256::from_usize(1111));
            match run_from_file(path, Vec::new(), env, debug) {
                Ok(logs) => {
                    println!("Logs: {:?}", logs);
                }
                Err(e) => {
                    println!("{:?}", e);
                }
            }
        }

        Args::EvmDebug(evm_debug) => {
            let debug = evm_debug.debug;
            let profile = evm_debug.profiler;
            //let _ = evm::evm_xcontract_call_with_constructors(None, debug, profile);
            let _ = evm::evm_xcontract_call_using_batch(None, debug, profile);
        }

        Args::Profiler(path) => {
            let path = path.input;
            profile_gen_from_file(
                path.as_ref(),
                Vec::new(),
                RuntimeEnvironment::new(Uint256::from_usize(1111)),
            );
        }

        Args::Replay(replay) => {
            let path = replay.input.as_str();
            let debug = replay.debug;
            let profiler = replay.profiler;
            let trace_file = replay.trace.as_deref();

            if let Err(e) = replay_from_testlog_file(path, true, debug, profiler, trace_file) {
                panic!("Error reading from {}: {}", path, e);
            }
        }

        Args::MakeTestLogs => {
            evm::make_logs_for_all_arbos_tests();
        }

        Args::MakeBenchmarks => {
            evm::benchmarks::make_benchmarks();
        }

        Args::MakeTemplates => {
            let path = Path::new("arb_os/contractTemplates.mini");
            generate_contract_template_file_or_die(path);
        }
    }

    Ok(())
}

fn get_output(output_filename: Option<&str>) -> Result<Box<dyn io::Write>, io::Error> {
    match output_filename {
        Some(ref path) => File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>),
        None => Ok(Box::new(io::stdout())),
    }
}
