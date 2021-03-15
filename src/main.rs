/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

#![allow(unused_parens)]

use crate::link::LinkedProgram;
use crate::run::Machine;
use clap::Clap;
use compile::{compile_from_file, CompileError};
use contracttemplates::generate_contract_template_file_or_die;
use link::{link, postlink_compile};
use mavm::{CodePt, Value};
use pos::try_display_location;
use run::{
    profile_gen_from_file, replay_from_testlog_file, run_from_file, ProfilerMode,
    RuntimeEnvironment,
};
use std::collections::BTreeMap;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;
use std::time::Instant;
use uint256::Uint256;

#[cfg(test)]
mod buffertests;
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

mod wasm;

///Command line options for compile subcommand.
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
    #[clap(short, long)]
    inline: bool,
}

///Command line options for run subcommand.
#[derive(Clap, Debug)]
struct RunStruct {
    input: String,
    #[clap(short, long)]
    debug: bool,
}

///Command line options for EvmDebug subcommand.
#[derive(Clap, Debug)]
struct EvmDebug {
    #[clap(short, long)]
    debug: bool,
    #[clap(short, long)]
    profiler: bool,
}

///Command line options for replay subcommand.
#[derive(Clap, Debug)]
struct Replay {
    input: String,
    #[clap(short, long)]
    debug: bool,
    #[clap(short, long)]
    profiler: ProfilerMode,
    #[clap(short, long)]
    trace: Option<String>,
}

///Command line options for profiler subcommand.
#[derive(Clap, Debug)]
struct Profiler {
    input: String,
    #[clap(short, long)]
    mode: ProfilerMode,
}

///Command line options for evm-tests subcommand.
#[derive(Clap, Debug)]
struct EvmTests {
    input: Vec<String>,
    #[clap(short, long)]
    savelogs: bool,
}

///Command line options for wasm-test subcommand.
#[derive(Clap, Debug)]
struct WasmTest {
    input: Vec<String>,
    #[clap(short, long)]
    param: Option<usize>,
}

///Command line options for wasm-test subcommand.
#[derive(Clap, Debug)]
struct WasmRun {
    input: Vec<String>,
    #[clap(short, long)]
    param: Option<String>,
}

///Main enum for command line arguments.
#[derive(Clap, Debug)]
enum Args {
    Compile(CompileStruct),
    Run(RunStruct),
    EvmDebug(EvmDebug),
    Profiler(Profiler),
    Replay(Replay),
    MakeTestLogs,
    MakeBenchmarks,
    MakeTemplates,
    EvmTests(EvmTests),
    WasmTest(WasmTest),
    WasmRun(WasmRun),
}

fn main() -> Result<(), CompileError> {
    let start_time = Instant::now();
    let matches = Args::parse();

    match matches {
        Args::WasmTest(fname) => {
            let filenames: Vec<_> = fname.input.clone();
            if (filenames.len() != 1) {
                println!("no input");
                return Ok(());
            }
            let param = match fname.param {
                Some(p) => p,
                None => 123,
            };
            let mut file = File::open(&filenames[0]).unwrap();
            let mut buffer = Vec::<u8>::new();
            file.read_to_end(&mut buffer).unwrap();

            // wasm::run_jit(&buffer, param as i64);

            let code = wasm::load(&buffer, param);
            let code_len = code.len();
            let env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
            let program = LinkedProgram {
                code: code,
                static_val: Value::new_tuple(vec![]),
                exported_funcs: vec![],
                imported_funcs: vec![],
                file_name_chart: BTreeMap::new(),
            };
            let mut machine = Machine::new(program, env);
            machine.start_at_zero();
            machine.run(Some(CodePt::new_internal(code_len - 1)));
            machine.debug(Some(CodePt::new_internal(code_len - 1)));
        }
        Args::WasmRun(fname) => {
            let filenames: Vec<_> = fname.input.clone();
            if (filenames.len() != 1) {
                println!("no input");
                return Ok(());
            }
            let param = match fname.param {
                Some(p) => hex::decode(p).unwrap(),
                None => vec![],
            };
            let mut file = File::open(&filenames[0]).unwrap();
            let mut buffer = Vec::<u8>::new();
            file.read_to_end(&mut buffer).unwrap();

            wasm::run_jit(&buffer, &param);
        }
        Args::Compile(compile) => {
            let debug_mode = compile.debug_mode;
            let typecheck = compile.typecheck;
            let mut output = get_output(compile.output.as_deref()).unwrap();
            let filenames: Vec<_> = compile.input.clone();
            let mut file_name_chart = BTreeMap::new();
            if compile.compile_only {
                let filename = &filenames[0];
                let path = Path::new(filename);
                match compile_from_file(path, &mut file_name_chart, debug_mode, compile.inline) {
                    Ok(mut compiled_program) => {
                        compiled_program.iter_mut().for_each(|prog| {
                            prog.file_name_chart.extend(file_name_chart.clone());
                            prog.to_output(&mut *output, compile.format.as_deref());
                        });
                    }
                    Err(e) => {
                        println!(
                            "Compilation error: {}\n{}",
                            e,
                            try_display_location(e.location, &file_name_chart)
                        );
                        return Err(e);
                    }
                }
            } else {
                let mut compiled_progs = Vec::new();
                for filename in &filenames {
                    let path = Path::new(filename);
                    match compile_from_file(path, &mut file_name_chart, debug_mode, compile.inline)
                    {
                        Ok(compiled_program) => {
                            compiled_program.into_iter().for_each(|prog| {
                                file_name_chart.extend(prog.file_name_chart.clone());
                                compiled_progs.push(prog)
                            });
                        }
                        Err(e) => {
                            println!(
                                "Compilation error: {}\n{}",
                                e,
                                try_display_location(e.location, &file_name_chart)
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
                            file_name_chart.clone(),
                            debug_mode,
                        ) {
                            Ok(completed_program) => {
                                completed_program
                                    .to_output(&mut *output, compile.format.as_deref());
                            }
                            Err(e) => {
                                println!(
                                    "Linking error: {}\n{}",
                                    e,
                                    try_display_location(e.location, &file_name_chart)
                                );
                                return Err(e);
                            }
                        }
                    }
                    Err(e) => {
                        println!(
                            "Linking error: {}\n{}",
                            e,
                            try_display_location(e.location, &file_name_chart)
                        );
                        return Err(e);
                    }
                }
            }
        }

        Args::Run(run) => {
            let filename = run.input;
            let debug = run.debug;
            let path = Path::new(&filename);
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

        Args::EvmDebug(evm_debug) => {
            let debug = evm_debug.debug;
            let profile = evm_debug.profiler;
            //let _ = evm::evm_xcontract_call_with_constructors(None, debug, profile);
            let _ = evm::evm_xcontract_call_using_batch(None, debug, profile);
        }

        Args::Profiler(path) => {
            let input = path.input;
            profile_gen_from_file(
                input.as_ref(),
                Vec::new(),
                RuntimeEnvironment::new(Uint256::from_usize(1111), None),
                path.mode,
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

        Args::EvmTests(options) => {
            let mut paths = options.input;
            if paths.len() == 0 {
                paths = [
                    "evm-tests/tests/VMTests/vmArithmeticTest",
                    "evm-tests/tests/VMTests/vmPushDupSwapTest",
                    "evm-tests/tests/VMTests/vmBitwiseLogicOperation",
                    "evm-tests/tests/VMTests/vmIOandFlowOperations",
                    "evm-tests/tests/VMTests/vmSha3Test",
                    "evm-tests/tests/VMTests/vmRandomTest",
                    "evm-tests/tests/VMTests/vmSystemOperations",
                    "evm-tests/tests/VMTests/vmEnvironmentalInfo",
                    "evm-tests/tests/VMTests/vmLogTest",
                ]
                .iter()
                .map(|a| a.to_string())
                .collect()
            }
            let mut num_successes = 0u64;
            let mut num_failures = 0u64;
            for path_name in paths.iter() {
                let path = Path::new(path_name);
                let (ns, nf) = evm::evmtest::run_evm_tests(
                    path,
                    if options.savelogs {
                        Some(Path::new("evm-test-logs/"))
                    } else {
                        None
                    },
                )
                .unwrap();
                num_successes = num_successes + ns;
                num_failures = num_failures + nf;
            }
            println!("{} successes, {} failures", num_successes, num_failures);
        }
    }
    let total_time = Instant::now() - start_time;
    println!(
        "Finished in {}.{:0>3} seconds.",
        total_time.as_secs(),
        total_time.subsec_millis()
    );

    Ok(())
}

///Creates a `dyn Write` from an optional filename, if a filename is specified, creates a file
/// handle, otherwise gives stdout.
fn get_output(output_filename: Option<&str>) -> Result<Box<dyn io::Write>, io::Error> {
    match output_filename {
        Some(ref path) => File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>),
        None => Ok(Box::new(io::stdout())),
    }
}
