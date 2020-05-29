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
use crate::mavm::{CodePt, Value, Instruction, Opcode};
use crate::uint256::Uint256;
use emulator::{ExecutionError, Machine, StackTrace};
use runtime_env::RuntimeEnvironment;
use std::fs::File;
use std::io::Read;
use std::path::Path;

mod emulator;
pub mod runtime_env;

pub fn run_from_file(
    path: &Path, 
    args: Vec<Value>,
    env: RuntimeEnvironment,
) -> Result<Value, (ExecutionError, StackTrace)> {
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

    run_from_string(s, args, env)
}

fn run_from_string(
    s: String, 
    args: Vec<Value>, 
    env: RuntimeEnvironment,
) -> Result<Value, (ExecutionError, StackTrace)> {
    let parse_result: Result<LinkedProgram, serde_json::Error> = serde_json::from_str(&s);
    let program = match parse_result {
        Ok(prog) => prog,
        Err(e) => {
            println!("json parsing error: {:?}", e);
            panic!();
        }
    };
    let mut new_machine = Machine::new(program, env);
    run(&mut new_machine, args)
}

fn run(machine: &mut Machine, args: Vec<Value>) -> Result<Value, (ExecutionError, StackTrace)> {
    match machine.test_call(CodePt::new_internal(0), args) {
        Ok(mut stack) => match stack.pop(&machine.get_state()) {
            Ok(res) => Ok(res),
            Err(e) => Err((e, machine.get_stack_trace())),
        },
        Err(e) => Err((e, machine.get_stack_trace())),
    }
}

fn run_with_msgs(
    prog: LinkedProgram,
    in_msgs: Vec<Value>,
) -> Result<Vec<Value>, ExecutionError> {
    let mut env = RuntimeEnvironment::new();
    env.insert_messages(&in_msgs);
    let mut machine = Machine::new(prog, env);
    let _ = run(&mut machine, vec![]);
    Ok(machine.runtime_env.get_all_logs())
}

#[test]
fn test_inbox_and_log() {
    let val = Value::Int(Uint256::from_usize(3));
    let logs = run_with_msgs(
        LinkedProgram {
            code: vec![
                Instruction::from_opcode(Opcode::Inbox, None),
                Instruction::from_opcode_imm(Opcode::Tget, Value::Int(Uint256::one()), None),
                Instruction::from_opcode(Opcode::Log, None),
                Instruction::from_opcode(Opcode::Halt, None),
            ],
            static_val: Value::none(),
            imported_funcs: vec![],
            exported_funcs: vec![],
        },
        vec![val.clone()]
    ).unwrap();
    assert_eq!(logs.len(), 1);
    assert_eq!(logs[0]==val, true);
}