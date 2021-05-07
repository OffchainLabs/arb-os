/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

#![allow(unused_parens)]

use crate::link::LinkedProgram;
use crate::run::Machine;
use crate::uint256::Uint256;
use crate::compile::CompileStruct;
use crate::pos::try_display_location;
use crate::upload::CodeUploader;
use clap::Clap;
use compile::{CompileError};
use contracttemplates::generate_contract_template_file_or_die;
use mavm::{Buffer, CodePt, Value, Opcode, Instruction};
use gen_code::gen_upgrade_code;
use run::{
    profile_gen_from_file, replay_from_testlog_file, run_from_file, ProfilerMode,
    RuntimeEnvironment,
};
use std::collections::BTreeMap;
use std::fs::File;
use std::io;
use std::io::{Read,Write};
use std::path::{Path, PathBuf};
use std::time::Instant;

mod compile;
mod contracttemplates;
mod evm;
mod wasm;
mod gen_code;
mod link;
mod mavm;
#[cfg(test)]
mod minitests;
pub mod pos;
mod run;
mod stringtable;
mod uint256;
mod upload;

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

///Command line options for wasm-test subcommand.
#[derive(Clap, Debug)]
struct WasmTest {
    input: Vec<String>,
    #[clap(short, long)]
    param: Option<String>,
}

///Command line options for wasm-test subcommand.
#[derive(Clap, Debug)]
struct WasmSuite {
    input: Vec<String>,
}

///Command line options for wasm-test subcommand.
#[derive(Clap, Debug)]
struct WasmRun {
    input: Vec<String>,
    #[clap(short, long)]
    param: Option<String>,
    #[clap(short, long)]
    param_file: Option<String>,
}

#[derive(Clap, Debug)]
struct GenUpgrade {
    from: PathBuf,
    to: PathBuf,
    out_file: PathBuf,
    impl_file: String,
    config_file: Option<String>,
}

#[derive(Clap, Debug)]
struct SerializeUpgrade {
    input: String,
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
    WasmTest(WasmTest),
    WasmSuite(WasmSuite),
    WasmRun(WasmRun),
    GenUpgradeCode(GenUpgrade),
    SerializeUpgrade(SerializeUpgrade),
}

fn run_test(buffer: &[u8], prev_memory: &Buffer, test_args: &[u64], entry: &String, init: bool, debug: bool) -> (Buffer, u64) {
    let code_0 = wasm::make_test(buffer, prev_memory, test_args, entry, init);
    let mut code = vec![];
    for i in 0..code_0.len() {
        match &code_0[i].opcode {
            Opcode::AVMOpcode(op) => {
                code.push(Instruction::new_with_debug(
                    op.clone(),
                    code_0[i].immediate.clone(),
                    code_0[i].debug_info.clone(), 
                    code_0[i].debug_str.clone()));
            }
            _ => {},
        }
    }
    let code_len = code.len();
    // println!("Code length {}", code_len);
    let env = RuntimeEnvironment::new(Uint256::from_usize(11110000), None);
    let program = LinkedProgram {
        code: code,
        static_val: Value::new_tuple(vec![]),
        arbos_version: 10,
        globals: vec![],
        file_name_chart: BTreeMap::new(),
    };
    let mut machine = Machine::new(program, env);
    machine.start_at_zero();
    if debug {
        machine.debug(Some(CodePt::new_internal(code_len - 1)))
    } else {
        machine.run(Some(CodePt::new_internal(code_len - 1)))
    };
    let buf = machine.stack.nth(0);
    let res = machine.stack.nth(1);
    // println!("buf {:?} res {:?}", buf, res);
    match (buf, res) {
        (Some(Value::Buffer(buf)), Some(Value::Int(gl))) => {
            (buf.clone(), gl.to_u64().unwrap())
        }
        (Some(Value::Buffer(buf)), None) => {
            (buf.clone(), 0)
        }
        _ => {
            println!("Bad output");
            (prev_memory.clone(), 0)
        }
    }

}

fn parse_list(lst: &json::JsonValue) -> Vec<u64> {
    let mut args = vec![];
    for arg in lst.members() {
        args.push(arg["value"].as_str().unwrap().parse::<u64>().unwrap())
    }
    args
}


fn main() -> Result<(), CompileError> {
    let mut print_time = true;
    let start_time = Instant::now();
    let matches = Args::parse();

    match matches {
        Args::WasmSuite(fname) => {
            let filenames: Vec<_> = fname.input.clone();
            if (filenames.len() != 1) {
                println!("no input");
                return Ok(());
            }
            let buffer = std::fs::read_to_string(&filenames[0]).unwrap();
            let json = json::parse(&buffer).unwrap();
            // println!("laoded {}", json::stringify(json))
            // get commands
            let mut module_buffer = Vec::<u8>::new();
            let mut cur_file = "".to_string();
            let mut cur_memory = Buffer::from_bytes(vec![]);
            let mut init = true;
            for cmd in json["commands"].members() {
                if cmd["type"] == "module" {
                    let mut file = File::open(format!("wasm-suite/{}", &cmd["filename"].as_str().unwrap())).unwrap();
                    let mut buffer = Vec::<u8>::new();
                    file.read_to_end(&mut buffer).unwrap();
                    module_buffer = buffer;
                    init = true;
                    cur_file = cmd["filename"].as_str().unwrap().to_string();
                } else if cmd["type"] == "assert_return" {
                    // println!("{:?}", cmd);
                    if cmd["action"]["type"] == "invoke" {
                        let entry = cmd["action"]["field"].as_str().unwrap();
                        let args = parse_list(&cmd["action"]["args"]);
                        // println!("{:?}", args);
                        let (mem, result) = run_test(&module_buffer, &cur_memory, &args, &entry.to_string(), init, false);
                        let expected = parse_list(&cmd["expected"]);
                        // println!("expected {:?}", expected);
                        if expected.len() > 0 {
                            if expected[0] != result {
                                println!("At file {} with {}({:?}): Expected {}, Got {}", cur_file, entry, args, expected[0], result);
                                run_test(&module_buffer, &cur_memory, &args, &entry.to_string(), init, true);
                            }
                        }
                        cur_memory = mem;
                        init = false;
                    }
                }
            }
        }
        Args::WasmTest(fname) => {
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

            let code_0 = wasm::load(&buffer, &param);
            let mut code = vec![];
            for i in 0..code_0.len() {
                match &code_0[i].opcode {
                    Opcode::AVMOpcode(op) => {
                        code.push(Instruction::new_with_debug(
                            op.clone(),
                            code_0[i].immediate.clone(),
                            code_0[i].debug_info.clone(), 
                            code_0[i].debug_str.clone()));
                    }
                    _ => {},
                }
            }
            let code_len = code.len();
            println!("Code length {}", code_len);
            let env = RuntimeEnvironment::new(Uint256::from_usize(11110000), None);
            let program = LinkedProgram {
                code: code,
                static_val: Value::new_tuple(vec![]),
                arbos_version: 10,
                // exported_funcs: vec![],
                // imported_funcs: vec![],
                globals: vec![],
                file_name_chart: BTreeMap::new(),
            };
            let mut machine = Machine::new(program, env);
            /*
            for i in 0..100 {
                machine.start_at_zero();
                machine.run(Some(CodePt::new_internal(code_len - 1)));
            }*/
            machine.start_at_zero();
            let used = machine.run(Some(CodePt::new_internal(code_len - 1)));
            // let used = machine.debug(Some(CodePt::new_internal(code_len - 1)));
            let len = machine.stack.nth(0);
            let buf = machine.stack.nth(1);
            let gas_left = machine.stack.nth(2);
            match (len, buf, gas_left) {
                (Some(Value::Int(a)), Some(Value::Buffer(buf)), Some(Value::Int(gl))) => {
                    let len = a.to_usize().unwrap();
                    let mut res = vec![];
                    for i in 0..len {
                        // println!("{}", buf.read_byte(i));
                        res.push(buf.read_byte(i as u128))
                    }
                    println!("WASM Gas used {}, Gas used {}, Result {}", 1000000 - gl.to_usize().unwrap(), used, hex::encode(res));
                }
                _ => println!("Unexpected output")
            };
        }
        Args::WasmRun(fname) => {
            let filenames: Vec<_> = fname.input.clone();
            if (filenames.len() != 1) {
                println!("no input {:?}", filenames);
                return Ok(());
            }
            let param = match (fname.param, fname.param_file) {
                (Some(p), _) => hex::decode(p).unwrap(),
                (_, Some(file)) => {
                    let mut file = File::open(&file).unwrap();
                    let mut buffer = Vec::<u8>::new();
                    file.read_to_end(&mut buffer).unwrap();
                    buffer
                }
                _ => vec![],
            };
            let mut file = File::open(&filenames[0]).unwrap();
            let mut buffer = Vec::<u8>::new();
            file.read_to_end(&mut buffer).unwrap();

            // wasm::run_jit(&buffer, &param);
            let a = wasm::JitWasm::new(&buffer);
            let buf = Buffer::from_bytes(param.to_vec());
            /*
            for i in 0..1000000 {
                let (buf, len; _) = a.run(buf.clone(), param.len());
            }
            */
            let (buf, extra, len, gas_left) = a.run(buf, param.len());
            let mut res = vec![];
            for i in 0..len {
                res.push(buf.read_byte(i as u128))
            }
            if extra.len() > 1000 {
                println!("Gas used {}, Result {}, Extra len {}", 1000000 - gas_left, hex::encode(res), extra.len());
            } else {
                println!("Gas used {}, Result {}, Extra {}", 1000000 - gas_left, hex::encode(res), hex::encode(&extra));
            }
            let mut file = File::create("/home/sami/extra.bin").unwrap();
            file.write_all(&extra).unwrap();
            println!("Wrote extra.bin");
        }
        Args::Compile(compile) => match do_compile(compile) {
            Ok(_) => {}
            Err((err, file_name_chart)) => println!(
                "{}\n{}",
                err,
                try_display_location(err.location, &file_name_chart, true)
            ),
        },

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
            evm::make_benchmarks().map_err(|e| {
                CompileError::new(
                    match e {
                        ethabi::Error::Other(desc) => desc,
                        other => format!("{}", other),
                    },
                    None,
                )
            })?;
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
                let (ns, nf) = evm::run_evm_tests(
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
        Args::SerializeUpgrade(up) => {
            let the_json = CodeUploader::_new_from_file(Path::new(&up.input))._to_json();
            print!("{}", the_json.unwrap());
            print_time = false;
        }
    }
    let total_time = Instant::now() - start_time;
    if print_time {
        println!(
            "Finished in {}.{:0>3} seconds.",
            total_time.as_secs(),
            total_time.subsec_millis()
        );
    }

    Ok(())
}

fn do_compile(compile: CompileStruct) -> Result<(), (CompileError, BTreeMap<u64, String>)> {
    let mut output = get_output(compile.output.clone()).unwrap();
    compile
        .invoke()?
        .to_output(&mut *output, compile.format.as_deref());
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
