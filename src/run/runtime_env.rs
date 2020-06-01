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

use crate::uint256::Uint256;
use crate::mavm::{Value};


pub struct RuntimeEnvironment {
    pub l1_inbox: Value,
    pub logs: Vec<Value>,
}

impl RuntimeEnvironment {
    pub fn new() -> Self {
        RuntimeEnvironment{ 
            l1_inbox: Value::none(),
            logs: Vec::new(),
        }
    }

    pub fn insert_message(&mut self, msg: Value) {
        self.l1_inbox = Value::Tuple(vec![self.l1_inbox.clone(), msg]);
    }

    pub fn insert_messages(&mut self, msgs: &[Value]) {
        for msg in msgs {
            self.insert_message(msg.clone());
        }
    }

    pub fn get_inbox(&mut self) -> Value {
        let ret = self.l1_inbox.clone();
        self.l1_inbox = Value::none();
        ret
    }

    pub fn push_log(&mut self, log_item: Value) {
        self.logs.push(log_item);
    }

    pub fn get_all_logs(&self) -> Vec<Value> {
        self.logs.clone()
    }
}

pub fn bytestack_from_bytes(b: &[u8]) -> Value {
    Value::Tuple(vec![
        Value::Int(Uint256::from_usize(b.len())),
        bytestack_from_bytes_2(b, Value::none()),
    ])
}

fn bytestack_from_bytes_2(b: &[u8], so_far: Value) -> Value {
    let size = b.len();
    if size > 32 {
        bytestack_from_bytes_2(
            &b[32..],
            Value::Tuple(vec![
                so_far,
                bytestack_build_uint(&b[..32]),
            ]),
        )
    } else {
        Value::Tuple(vec![
            so_far,
            bytestack_build_uint(b),
        ])
    } 
}

fn bytestack_build_uint(b: &[u8]) -> Value {
    let mut ui = Uint256::zero();
    for j in (0..b.len()).rev() {
        ui = ui.mul(&Uint256::from_usize(256)).add(&Uint256::from_usize(b[j] as usize));
    }
    Value::Int(ui)
}
