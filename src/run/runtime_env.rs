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
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct RuntimeEnvironment {
    pub l1_inbox: Value,
    pub current_block_num: Uint256,
    pub current_timestamp: Uint256,
    pub logs: Vec<Value>,
    pub seq_nums: HashMap<Uint256, Uint256>,
    next_id: Uint256, // used to assign unique (but artificial) txids to messages
    pub recorder: RtEnvRecorder,
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
            recorder: RtEnvRecorder::new(),
        }
    }

    pub fn insert_eth_message(&mut self, sender_addr: Uint256, msg: &[u8]) {
        let l1_msg = Value::new_tuple(vec![
            Value::Int(Uint256::zero()), // mocked-up blockhash
            Value::Int(self.current_timestamp.clone()),
            Value::Int(self.current_block_num.clone()),
            Value::Int(sender_addr), // fake message sender
            bytestack_from_bytes(msg),
        ]);
        self.l1_inbox = Value::new_tuple(vec![self.l1_inbox.clone(), l1_msg.clone()]);
        self.recorder.add_msg(l1_msg);
    }

    pub fn insert_txcall_message(
        &mut self,
        to_addr: Uint256,
        value: Uint256,
        max_gas: Uint256,
        gas_price_bid: Uint256,
        data: &[u8],
    ) {
        let sender_addr = Uint256::from_usize(1025);
        let mut buf = vec![0u8];
        let seq_num = self.get_and_incr_seq_num(&sender_addr);
        buf.extend(seq_num.to_bytes_be());
        buf.extend(to_addr.to_bytes_be());
        buf.extend(value.to_bytes_be());
        buf.extend(max_gas.to_bytes_be());
        buf.extend(gas_price_bid.to_bytes_be());
        buf.extend_from_slice(data);

        self.insert_eth_message(sender_addr, &buf);
    }

    pub fn insert_nonmutating_call_message(
        &mut self,
        to_addr: Uint256,
        max_gas: Uint256,
        data: &[u8],
    ) {
        let sender_addr = Uint256::from_usize(1025);
        let mut buf = vec![5u8];
        buf.extend(to_addr.to_bytes_be());
        buf.extend(max_gas.to_bytes_be());
        buf.extend_from_slice(data);

        self.insert_eth_message(sender_addr, &buf);
    }

    pub fn insert_deploy_contract_message(&mut self, contract_code: &[u8]) {
        let sender_addr = Uint256::from_usize(1025);
        let mut buf = vec![6u8];
        let seq_num = self.get_and_incr_seq_num(&sender_addr);
        buf.extend(seq_num.to_bytes_be());
        buf.extend(contract_code);

        self.insert_eth_message(sender_addr, &buf);
    }

    #[cfg(test)]
    pub fn insert_erc20_deposit_message(
        &mut self,
        token_addr: Uint256,
        payee: Uint256,
        amount: Uint256,
    ) {
        let sender_addr = Uint256::from_usize(1025);
        let mut buf = vec![2u8];
        buf.extend(token_addr.to_bytes_be());
        buf.extend(payee.to_bytes_be());
        buf.extend(amount.to_bytes_be());

        self.insert_eth_message(sender_addr, &buf);
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
        self.logs.push(log_item.clone());
        self.recorder.add_log(log_item);
    }

    pub fn get_all_logs(&self) -> Vec<Value> {
        self.logs.clone()
    }
}

pub fn bytestack_from_bytes(b: &[u8]) -> Value {
    Value::new_tuple(vec![
        Value::Int(Uint256::from_usize(b.len())),
        bytestack_from_bytes_2(b, Value::none()),
    ])
}

fn bytestack_from_bytes_2(b: &[u8], so_far: Value) -> Value {
    let size = b.len();
    if size > 32 {
        bytestack_from_bytes_2(
            &b[32..],
            Value::new_tuple(vec![so_far, bytestack_build_uint(&b[..32])]),
        )
    } else {
        Value::new_tuple(vec![so_far, bytestack_build_uint(b)])
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

pub fn bytes_from_bytestack(bs: Value) -> Option<Vec<u8>> {
    if let Value::Tuple(tup) = bs {
        if let Value::Int(ui) = &tup[0] {
            if let Some(nbytes) = ui.to_usize() {
                return bytes_from_bytestack_2(tup[1].clone(), nbytes);
            }
        }
    }
    None
}

fn bytes_from_bytestack_2(cell: Value, nbytes: usize) -> Option<Vec<u8>> {
    if nbytes == 0 {
        Some(vec![])
    } else {
        if let Value::Tuple(tup) = cell {
            assert_eq!((tup.len(), nbytes), (2, nbytes));
            if let Value::Int(mut int_val) = tup[1].clone() {
                let _256 = Uint256::from_usize(256);
                if (nbytes % 32) == 0 {
                    let mut sub_arr = match bytes_from_bytestack_2(tup[0].clone(), nbytes - 32) {
                        Some(arr) => arr,
                        None => {
                            return None;
                        }
                    };
                    let mut this_arr = vec![0u8; 32];
                    for i in 0..32 {
                        let rem = int_val.modulo(&_256).unwrap().to_usize().unwrap(); // safe because denom != 0 and result fits in usize
                        this_arr[31 - i] = rem as u8;
                        int_val = int_val.div(&_256).unwrap(); // safe because denom != 0
                    }
                    sub_arr.append(&mut this_arr);
                    Some(sub_arr)
                } else {
                    let mut sub_arr =
                        match bytes_from_bytestack_2(tup[0].clone(), 32 * (nbytes / 32)) {
                            Some(arr) => arr,
                            None => {
                                return None;
                            }
                        };
                    let this_size = nbytes % 32;
                    let mut this_arr = vec![0u8; this_size];
                    for _ in 0..(32 - this_size) {
                        int_val = int_val.div(&_256).unwrap(); // safe because denom != 0
                    }
                    for i in 0..this_size {
                        let rem = int_val.modulo(&_256).unwrap().to_usize().unwrap(); // safe because denom != 0 and result fits in usize
                        this_arr[this_size - 1 - i] = rem as u8;
                        int_val = int_val.div(&_256).unwrap(); // safe because denom != 0
                    }
                    sub_arr.append(&mut this_arr);
                    Some(sub_arr)
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtEnvRecorder {
    format_version: u64,
    inbox: Value,
    logs: Vec<Value>,
}

impl RtEnvRecorder {
    fn new() -> Self {
        RtEnvRecorder {
            format_version: 1,
            inbox: Value::none(),
            logs: Vec::new(),
        }
    }

    fn add_msg(&mut self, msg: Value) {
        self.inbox = Value::new_tuple(vec![self.inbox.clone(), msg])
    }

    fn add_log(&mut self, log_item: Value) {
        self.logs.push(log_item);
    }

    pub fn to_json_string(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }

    pub fn to_file(&self, path: &Path) -> Result<(), io::Error> {
        let mut file = File::create(path).map(|f| Box::new(f) as Box<dyn io::Write>)?;
        writeln!(file, "{}", self.to_json_string()?)
    }
}

#[test]
fn test_bytestacks() {
    let before =
        "The quick brown fox jumped over the lazy dog. Lorem ipsum and all that.".as_bytes();
    let bs = bytestack_from_bytes(before);
    let after = bytes_from_bytestack(bs);
    assert_eq!(after, Some(before.to_vec()));
}
