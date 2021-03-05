/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functionality for running mavm executables.

use crate::link::LinkedProgram;
use crate::mavm::{CodePt, Value};
use emulator::{ExecutionError, StackTrace};
use std::{fs::File, io::Read, path::Path};

pub use emulator::{Machine, ProfilerMode};
pub use runtime_env::{
    _bytes_from_bytestack, _bytestack_from_bytes, generic_compress_token_amount,
    replay_from_testlog_file, ArbosReceipt, RuntimeEnvironment,
};

mod blake2b;
mod emulator;
mod ripemd160port;
pub mod rolluptest;
mod runtime_env;
pub mod upload;

///Executes the file located at path, or starts the debugger if debug is set to true.
///
/// The args argument specifies the arguments to the executable.  These will be placed on the stack
/// prior to the start of execution and are neither type checked nor checked for having the correct
/// length.
///
/// The env specifies various aspects about the environment such as the inbox messages.  See
/// `RuntimeEnvironment` for more details.
///
/// This function will panic if the specified path cannot be opened or does not contain a valid
/// mini executable.
pub fn run_from_file(
    path: &Path,
    args: Vec<Value>,
    env: RuntimeEnvironment,
    debug: bool,
) -> Result<Vec<Value>, (ExecutionError, StackTrace)> {
    let mut machine = load_from_file(path, env);
    run(&mut machine, args, debug)
}

///Generates a `Machine` from the given path and `RuntimeEnvironment`. See `RuntimeEnvironment` for
/// more details.
///
/// Will panic if the path cannot be opened or doesn't represent a valid mini executable.
pub fn load_from_file(path: &Path, env: RuntimeEnvironment) -> Machine {
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {:?}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    s = match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {:?}", display, why),
        Ok(_) => s,
    };

    load_from_string(s, env)
}

///Interprets s as a mini executable and generates a `Machine` with the specified
/// `RuntimeEnvironment`.
///
/// Will panic if s cannot be interpreted as a mini executable.
fn load_from_string(s: String, env: RuntimeEnvironment) -> Machine {
    let parse_result: Result<LinkedProgram, serde_json::Error> = serde_json::from_str(&s);
    let program = match parse_result {
        Ok(prog) => prog,
        Err(e) => {
            println!("json parsing error: {:?}", e);
            panic!();
        }
    };
    Machine::new(program, env)
}

///Runs the specified `Machine` from its first codepoint.
pub fn run(
    machine: &mut Machine,
    args: Vec<Value>,
    debug: bool,
) -> Result<Vec<Value>, (ExecutionError, StackTrace)> {
    // We use PC 1 here because PC pushes an unwanted value--designed for a different entry ABI
    match machine.test_call(CodePt::new_internal(1), args, debug) {
        Ok(_stack) => Ok(machine.runtime_env.get_all_raw_logs()),
        Err(e) => Err((e, machine.get_stack_trace())),
    }
}

///Interprets path as a mini executable and starts a profiler session with executable arguments args
/// and `RuntimeEnvironment` env.  See `profiler_session` for more details.
pub fn profile_gen_from_file(
    path: &Path,
    args: Vec<Value>,
    env: RuntimeEnvironment,
    mode: ProfilerMode,
) {
    let mut machine = load_from_file(path, env);
    let profile = machine.profile_gen(args, mode);
    profile.profiler_session();
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn run_with_msgs(
        prog: LinkedProgram,
        in_msgs: Vec<Value>,
        debug: bool,
    ) -> Result<Vec<Value>, ExecutionError> {
        let mut env = RuntimeEnvironment::new();
        env.insert_arb_messages(&in_msgs);
        let mut machine = Machine::new(prog, env);
        match run(&mut machine, vec![], debug) {
            Ok(_) => Ok(machine.runtime_env.get_all_logs()),
            Err((e, _)) => Err(e),
        }
    }

    #[test]
    fn test_inbox_and_log() {
        use crate::mavm::{Instruction, Opcode};
        use crate::uint256::Uint256;
        let val = Value::Int(Uint256::from_usize(3));
        let logs = run_with_msgs(
            LinkedProgram {
                code: vec![
                    Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Inbox), None),
                    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Tget), Value::Int(Uint256::one()), None),
                    Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Tget),
                        Value::Int(Uint256::from_usize(3)),
                        None,
                    ),
                    Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Log), None),
                    Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Inbox), None), // should block, stopping execution
                ],
                static_val: Value::none(),
                imported_funcs: vec![],
                exported_funcs: vec![],
                file_name_chart: HashMap::new(),
            },
            vec![val.clone()],
            false,
        )
        .unwrap();
        assert_eq!(logs.len(), 1);
        assert_eq!(logs[0] == val, true);
    }
}
 */
