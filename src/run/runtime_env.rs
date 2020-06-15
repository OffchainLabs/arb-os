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

use crate::mavm::Value;
use crate::uint256::Uint256;
use std::collections::HashMap;

#[derive(Debug)]
pub struct RuntimeEnvironment {
    pub l1_inbox: Value,
    pub current_block_num: Uint256,
    pub current_timestamp: Uint256,
    pub logs: Vec<Value>,
    pub seq_nums: HashMap<Uint256, Uint256>,
    next_id: Uint256, // used to assign unique (but artificial) txids to messages
}

impl RuntimeEnvironment {
    pub fn new() -> Self {
        RuntimeEnvironment {
            l1_inbox: Value::none(),
            current_block_num: Uint256::zero(),
            current_timestamp: Uint256::zero(),
            logs: Vec::new(),
            seq_nums: HashMap::new(),
            next_id: Uint256::zero(),
        }
    }

    pub fn insert_eth_message(&mut self, eth_msg: Value) {
        self.l1_inbox = Value::Tuple(vec![self.l1_inbox.clone(), eth_msg]);
    }

    pub fn insert_arb_message(&mut self, msg: Value) {
        self.insert_eth_message(Value::Tuple(vec![
            Value::Int(self.current_block_num.clone()),
            Value::Int(self.current_timestamp.clone()),
            Value::Int(self.next_id.clone()),
            msg,
        ]));
        self.next_id = self.next_id.add(&Uint256::one());
    }

    pub fn insert_arb_messages(&mut self, msgs: &[Value]) {
        for msg in msgs {
            self.insert_arb_message(msg.clone());
        }
    }

    pub fn insert_txcall_message(&mut self, to_addr: Uint256, value: Uint256, data: &[u8]) {
        let txcall_msg = Value::Tuple(vec![
            Value::Int(to_addr.clone()),
            Value::Int(self.get_and_incr_seq_num(&to_addr)),
            Value::Int(value),
            bytestack_from_bytes(data),
        ]);
        let msg = Value::Tuple(vec![
            Value::Int(Uint256::zero()), // message type 0
            Value::Int(Uint256::zero()), // sent from address 0
            txcall_msg,
        ]);
        self.insert_arb_message(msg);
    }

    pub fn get_and_incr_seq_num(&mut self, addr: &Uint256) -> Uint256 {
        let cur_seq_num = match self.seq_nums.get(&addr) {
            Some(sn) => sn.clone(),
            None => Uint256::one(),
        };
        self.seq_nums
            .insert(addr.clone(), cur_seq_num.add(&Uint256::one()));
        cur_seq_num.clone()
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
            Value::Tuple(vec![so_far, bytestack_build_uint(&b[..32])]),
        )
    } else {
        Value::Tuple(vec![so_far, bytestack_build_uint(b)])
    }
}

fn bytestack_build_uint(b: &[u8]) -> Value {
    let mut ui = Uint256::zero();
    for j in (0..32) {
        if j < b.len() {
            ui = ui
                .mul(&Uint256::from_usize(256))
                .add(&Uint256::from_usize(b[j] as usize));
        } else {
            ui = ui.mul(&Uint256::from_usize(256));
        }
    }
    Value::Int(ui)
}
