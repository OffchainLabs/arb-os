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

use crate::evm::send_inject_evm_messages_from_file;
use crate::mavm::{CodePt, Value};
use crate::run::emulator::{ExecutionError, Machine, StackTrace};
use crate::run::load_from_file;
use crate::run::runtime_env::RuntimeEnvironment;
use std::path::Path;

pub struct AvmChain {
    machine: Machine,
}

impl AvmChain {
    pub fn new(contract_file: Option<&str>, call_msgs: &[Value]) -> Self {
        AvmChain::new_from_file(
            Path::new("arbruntime/runtime.mexe"),
            contract_file,
            call_msgs,
        )
    }

    pub fn new_from_file(
        pathname: &Path,
        contract_file: Option<&str>,
        call_msgs: &[Value],
    ) -> Self {
        let mut rt_env = RuntimeEnvironment::new();
        if let Some(contract_file_name) = contract_file {
            match send_inject_evm_messages_from_file(contract_file_name, &mut rt_env) {
                Ok(_) => {}
                Err(e) => {
                    panic!("error injecting messages: {:?}", e);
                }
            }
        }
        rt_env.insert_arb_messages(call_msgs);
        let machine = load_from_file(pathname, rt_env);
        AvmChain { machine }
    }

    pub fn run(&mut self, debug: bool) -> Result<Vec<Value>, (ExecutionError, StackTrace)> {
        match self
            .machine
            .test_call(CodePt::new_internal(0), vec![], debug)
        {
            Ok(_stack) => Ok(self.machine.runtime_env.get_all_logs()),
            Err(e) => Err((e, self.machine.get_stack_trace())),
        }
    }
}
