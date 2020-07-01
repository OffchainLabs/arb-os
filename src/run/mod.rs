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

use crate::link::LinkedProgram;
use crate::mavm::{CodePt, Value};
use emulator::{ExecutionError, StackTrace};
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub use emulator::Machine;
pub use runtime_env::{bytes_from_bytestack, bytestack_from_bytes, RuntimeEnvironment};

mod emulator;
mod runtime_env;

pub fn run_from_file(
    path: &Path,
    args: Vec<Value>,
    env: RuntimeEnvironment,
    debug: bool,
) -> Result<Vec<Value>, (ExecutionError, StackTrace)> {
    let mut machine = load_from_file(path, env);
    run(&mut machine, args, debug)
}

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

fn load_from_string(s: String, env: RuntimeEnvironment) -> Machine {
    let parse_result: Result<LinkedProgram, serde_json::Error> = serde_json::from_str(&s);
    let program = match parse_result {
        Ok(prog) => prog,
        Err(e) => {
            println!("json parsing error: {:?}", e);
            panic!();
        }
    };
    return Machine::new(program, env);
}

pub fn run(
    machine: &mut Machine,
    args: Vec<Value>,
    debug: bool,
) -> Result<Vec<Value>, (ExecutionError, StackTrace)> {
    match machine.test_call(CodePt::new_internal(0), args, debug) {
        Ok(_stack) => Ok(machine.runtime_env.get_all_logs()),
        Err(e) => Err((e, machine.get_stack_trace())),
    }
}

pub fn profile_gen_from_file(path: &Path, args: Vec<Value>, env: RuntimeEnvironment) {
    let mut machine = load_from_file(path, env);
    let profile = machine.profile_gen(args);
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
                    Instruction::from_opcode(Opcode::Inbox, None),
                    Instruction::from_opcode_imm(Opcode::Tget, Value::Int(Uint256::one()), None),
                    Instruction::from_opcode_imm(
                        Opcode::Tget,
                        Value::Int(Uint256::from_usize(3)),
                        None,
                    ),
                    Instruction::from_opcode(Opcode::Log, None),
                    Instruction::from_opcode(Opcode::Inbox, None), // should block, stopping execution
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
