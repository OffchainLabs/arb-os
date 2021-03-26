/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

#![allow(unused_parens)]

use crate::compile::CompileStruct;
use crate::link::LinkedProgram;
use clap::Clap;
use compile::CompileError;
use contracttemplates::generate_contract_template_file_or_die;
use gen_code::gen_upgrade_code;
use run::{
    profile_gen_from_file, replay_from_testlog_file, run_from_file, ProfilerMode,
    RuntimeEnvironment,
};
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::Instant;

mod compile;
mod contracttemplates;
mod evm;
mod gen_code;
mod link;
mod mavm;
#[cfg(test)]
mod minitests;
pub mod pos;
mod run;
mod stringtable;
mod uint256;

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

///Command line options for reformat subcommand.
#[derive(Clap, Debug)]
struct Reformat {
    input: String,
    output: Option<String>,
    #[clap(short, long)]
    format: Option<String>,
}

///Command line options for evm-tests subcommand.
#[derive(Clap, Debug)]
struct EvmTests {
    input: Vec<String>,
    #[clap(short, long)]
    savelogs: bool,
}

#[derive(Clap, Debug)]
struct GenUpgrade {
    from: PathBuf,
    to: PathBuf,
    out_file: PathBuf,
    impl_file: String,
    config_file: Option<String>,
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
    Reformat(Reformat),
    EvmTests(EvmTests),
    GenUpgradeCode(GenUpgrade),
}

fn main() -> Result<(), CompileError> {
    let start_time = Instant::now();
    let matches = Args::parse();

    match matches {
        Args::Compile(compile) => {
            let mut output = get_output(compile.output.clone()).unwrap();
            compile
                .invoke()?
                .to_output(&mut *output, compile.format.as_deref());
        }

        Args::Run(run) => {
            let filename = run.input;
            let debug = run.debug;
            let path = Path::new(&filename);
            match run_from_file(path, Vec::new(), debug) {
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
                RuntimeEnvironment::default(),
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

        Args::Reformat(reformat) => {
            let path = Path::new(&reformat.input);
            let mut file = File::open(path).map_err(|_| {
                CompileError::new(
                    format!(
                        "Could not open file: \"{}\"",
                        path.to_str().unwrap_or("non-utf8")
                    ),
                    None,
                )
            })?;
            let mut s = String::new();
            file.read_to_string(&mut s).map_err(|_| {
                CompileError::new(
                    format!("Failed to read input file \"{}\" to string", reformat.input),
                    None,
                )
            })?;
            let result: LinkedProgram = serde_json::from_str(&s).map_err(|_| {
                CompileError::new(
                    format!("Could not parse input file \"{}\" as json", reformat.input),
                    None,
                )
            })?;

            result.to_output(
                &mut get_output(reformat.output).unwrap(),
                reformat.format.as_deref(),
            );
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
        Args::GenUpgradeCode(upgrade) => {
            let result = gen_upgrade_code(upgrade);
            if let Err(e) = result {
                println!("Encountered an error: {}", e);
            } else {
                println!("Successfully generated code");
            }
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
fn get_output(output_filename: Option<String>) -> Result<Box<dyn io::Write>, io::Error> {
    match output_filename {
        Some(ref path) => File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>),
        None => Ok(Box::new(io::stdout())),
    }
}
