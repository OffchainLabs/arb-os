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
use std::convert::TryInto;
use std::{collections::HashMap, fs::File, io, path::Path};

#[derive(Debug, Clone)]
pub struct RuntimeEnvironment {
    pub chain_address: Uint256,
    pub l1_inbox: Value,
    pub current_block_num: Uint256,
    pub current_timestamp: Uint256,
    pub logs: Vec<Value>,
    pub sends: Vec<Value>,
    pub next_inbox_seq_num: Uint256,
    pub caller_seq_nums: HashMap<Uint256, Uint256>,
    next_id: Uint256, // used to assign unique (but artificial) txids to messages
    pub recorder: RtEnvRecorder,
}

impl RuntimeEnvironment {
    pub fn new(chain_address: Uint256) -> Self {
        let mut ret = RuntimeEnvironment {
            chain_address: chain_address.clone(),
            l1_inbox: Value::none(),
            current_block_num: Uint256::zero(),
            current_timestamp: Uint256::zero(),
            logs: Vec::new(),
            sends: Vec::new(),
            next_inbox_seq_num: Uint256::zero(),
            caller_seq_nums: HashMap::new(),
            next_id: Uint256::zero(),
            recorder: RtEnvRecorder::new(),
        };
        ret.insert_l1_message(4, chain_address, &[0u8]);
        ret
    }

    pub fn insert_l1_message(&mut self, msg_type: u8, sender_addr: Uint256, msg: &[u8]) {
        let l1_msg = Value::new_tuple(vec![
            Value::Int(Uint256::from_usize(msg_type as usize)),
            Value::Int(self.current_block_num.clone()),
            Value::Int(self.current_timestamp.clone()),
            Value::Int(sender_addr),
            Value::Int(self.next_inbox_seq_num.clone()),
            bytestack_from_bytes(msg),
        ]);
        self.next_inbox_seq_num = self.next_inbox_seq_num.add(&Uint256::one());
        self.l1_inbox = Value::new_tuple(vec![self.l1_inbox.clone(), l1_msg.clone()]);
        self.recorder.add_msg(l1_msg);
    }

    pub fn insert_l2_message(&mut self, sender_addr: Uint256, msg: &[u8]) {
        self.insert_l1_message(3, sender_addr, msg);
    }

    pub fn insert_tx_message(
        &mut self,
        sender_addr: Uint256,
        max_gas: Uint256,
        gas_price_bid: Uint256,
        to_addr: Uint256,
        value: Uint256,
        data: &[u8],
    ) {
        let mut buf = vec![0u8];
        let seq_num = self.get_and_incr_seq_num(&sender_addr);
        buf.extend(max_gas.to_bytes_be());
        buf.extend(gas_price_bid.to_bytes_be());
        buf.extend(seq_num.to_bytes_be());
        buf.extend(to_addr.to_bytes_be());
        buf.extend(value.to_bytes_be());
        buf.extend_from_slice(data);

        self.insert_l2_message(sender_addr, &buf);
    }

    pub fn new_batch(&self) -> Vec<u8> {
        vec![3u8]
    }

    pub fn append_tx_message_to_batch(
        &mut self,
        batch: &mut Vec<u8>,
        sender_addr: Uint256,
        max_gas: Uint256,
        gas_price_bid: Uint256,
        to_addr: Uint256,
        value: Uint256,
        calldata: &[u8],
    ) {
        let calldata_size: u64 = calldata.len().try_into().unwrap();
        let seq_num = self.get_and_incr_seq_num(&sender_addr);
        batch.extend(&calldata_size.to_be_bytes());
        batch.extend(vec![0u8]);
        batch.extend(max_gas.to_bytes_be());
        batch.extend(gas_price_bid.to_bytes_be());
        batch.extend(seq_num.to_bytes_be());
        batch.extend(to_addr.to_bytes_be());
        batch.extend(value.to_bytes_be());
        batch.extend_from_slice(calldata);
        batch.extend(vec![0u8; 65]);
    }

    pub fn insert_batch_message(&mut self, sender_addr: Uint256, batch: &[u8]) {
        self.insert_l2_message(sender_addr, batch);
    }

    pub fn _insert_nonmutating_call_message(
        &mut self,
        sender_addr: Uint256,
        to_addr: Uint256,
        max_gas: Uint256,
        data: &[u8],
    ) {
        let mut buf = vec![2u8];
        buf.extend(max_gas.to_bytes_be());
        buf.extend(Uint256::zero().to_bytes_be()); // gas price = 0
        buf.extend(to_addr.to_bytes_be());
        buf.extend_from_slice(data);

        self.insert_l2_message(sender_addr, &buf);
    }

    #[cfg(test)]
    pub fn insert_erc20_deposit_message(
        &mut self,
        sender_addr: Uint256,
        token_addr: Uint256,
        payee: Uint256,
        amount: Uint256,
    ) {
        let mut buf = token_addr.to_bytes_be();
        buf.extend(payee.to_bytes_be());
        buf.extend(amount.to_bytes_be());

        self.insert_l1_message(1, sender_addr, &buf);
    }

    pub fn insert_eth_deposit_message(
        &mut self,
        sender_addr: Uint256,
        payee: Uint256,
        amount: Uint256,
    ) {
        let mut buf = payee.to_bytes_be();
        buf.extend(amount.to_bytes_be());

        self.insert_l1_message(0, sender_addr, &buf);
    }

    pub fn get_and_incr_seq_num(&mut self, addr: &Uint256) -> Uint256 {
        let cur_seq_num = match self.caller_seq_nums.get(&addr) {
            Some(sn) => sn.clone(),
            None => Uint256::zero(),
        };
        self.caller_seq_nums
            .insert(addr.clone(), cur_seq_num.add(&Uint256::one()));
        cur_seq_num
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

    pub fn push_send(&mut self, send_item: Value) {
        self.sends.push(send_item.clone());
        self.recorder.add_send(send_item);
    }

    pub fn get_all_sends(&self) -> Vec<Value> {
        self.sends.clone()
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
            Value::new_tuple(vec![bytestack_build_uint(&b[..32]), so_far]),
        )
    } else {
        Value::new_tuple(vec![bytestack_build_uint(b), so_far])
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
    } else if let Value::Tuple(tup) = cell {
        assert_eq!((tup.len(), nbytes), (2, nbytes));
        if let Value::Int(mut int_val) = tup[0].clone() {
            let _256 = Uint256::from_usize(256);
            if (nbytes % 32) == 0 {
                let mut sub_arr = match bytes_from_bytestack_2(tup[1].clone(), nbytes - 32) {
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
                let mut sub_arr = match bytes_from_bytestack_2(tup[1].clone(), 32 * (nbytes / 32)) {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtEnvRecorder {
    format_version: u64,
    inbox: Value,
    logs: Vec<Value>,
    sends: Vec<Value>,
}

impl RtEnvRecorder {
    fn new() -> Self {
        RtEnvRecorder {
            format_version: 1,
            inbox: Value::none(),
            logs: Vec::new(),
            sends: Vec::new(),
        }
    }

    fn add_msg(&mut self, msg: Value) {
        self.inbox = Value::new_tuple(vec![self.inbox.clone(), msg])
    }

    fn add_log(&mut self, log_item: Value) {
        self.logs.push(log_item);
    }

    fn add_send(&mut self, send_item: Value) {
        self.sends.push(send_item);
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
