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
use crate::run::{ArbosReceipt, Machine};
use crate::uint256::Uint256;
use ethers_signers::Wallet;
use std::{fs::File, io::Read, path::Path};

#[derive(Debug, Clone)]
pub struct AbiForContract {
    code_bytes: Vec<u8>,
    contract: ethabi::Contract,
    pub address: Uint256,
    name: String,
}

impl AbiForContract {
    pub fn new_from_file(filename: &str) -> Result<Self, ethabi::Error> {
        let path = Path::new(filename);
        let mut file = match File::open(path) {
            Ok(f) => f,
            Err(e) => {
                return Err(ethabi::Error::from(e.to_string()));
            }
        };
        let mut s = String::new();
        s = match file.read_to_string(&mut s) {
            Err(why) => {
                return Err(ethabi::Error::from(why.to_string()));
            }
            Ok(_) => s,
        };

        let parse_result: Result<serde_json::Value, serde_json::Error> = serde_json::from_str(&s);
        let json_from_file = match parse_result {
            Ok(v) => v,
            Err(e) => {
                return Err(ethabi::Error::from(e.to_string()));
            }
        };

        if let serde_json::Value::Object(fields) = json_from_file {
            let json_abi = match fields.get("abi") {
                Some(val) => val,
                None => {
                    return Err(ethabi::Error::from("no abi key in json"));
                }
            };

            let contract: ethabi::Contract = match serde_json::from_value(json_abi.clone()) {
                Ok(c) => c,
                Err(e) => {
                    return Err(ethabi::Error::from(e.to_string()));
                }
            };

            let name = match &fields.get("contractName") {
                Some(serde_json::Value::String(s)) => s,
                _ => {
                    return Err(ethabi::Error::from("no name key in json"));
                }
            };

            let decoded_insns = match &fields.get("bytecode") {
                Some(val) => {
                    let code_str = val.as_str().unwrap().to_string();
                    hex::decode(&code_str[2..]).unwrap()
                }
                None => {
                    return Err(ethabi::Error::from("no code key in json"));
                }
            };

            Ok(AbiForContract {
                code_bytes: decoded_insns.to_vec(),
                contract,
                address: Uint256::zero(),
                name: name.to_string(),
            })
        } else {
            Err(ethabi::Error::from("json file not an array"))
        }
    }

    pub fn deploy(
        &mut self,
        args: &[ethabi::Token],
        machine: &mut Machine,
        deploy_as_buddy: bool,
        debug: bool,
    ) -> Option<Uint256> {
        let initial_logs_len = machine.runtime_env.get_all_logs().len();
        let initial_sends_len = machine.runtime_env.get_all_sends().len();
        let augmented_code = if let Some(constructor) = self.contract.constructor() {
            match constructor.encode_input(self.code_bytes.clone(), args) {
                Ok(aug_code) => aug_code,
                Err(e) => {
                    panic!("couldn't encode data for constructor: {:?}", e);
                }
            }
        } else {
            self.code_bytes.clone()
        };

        let sender_addr = Uint256::from_usize(1025);
        let request_id = if deploy_as_buddy {
            machine.runtime_env.insert_buddy_deploy_message(
                sender_addr.clone(),
                Uint256::from_usize(1_000_000_000_000),
                Uint256::zero(),
                Uint256::zero(),
                &augmented_code,
            )
        } else {
            machine.runtime_env.insert_tx_message(
                sender_addr.clone(),
                Uint256::from_usize(1_000_000_000_000),
                Uint256::zero(),
                Uint256::zero(),
                Uint256::zero(),
                &augmented_code,
            )
        };

        let _gas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        }; // handle this deploy message
        let logs = machine.runtime_env.get_all_logs();

        if logs.len() != initial_logs_len + 1 {
            println!(
                "deploy: expected 1 new log item, got {}",
                logs.len() - initial_logs_len
            );
            return None;
        }

        if deploy_as_buddy {
            let sends = machine.runtime_env.get_all_sends();
            if sends.len() != initial_sends_len + 1 {
                println!(
                    "deploy: expected 1 new send, got {}",
                    sends.len() - initial_sends_len
                );
                return None;
            }
            if let Value::Tuple(tup) = &sends[sends.len() - 1] {
                if (tup[0] != Value::Int(Uint256::from_usize(5)))
                    || (tup[1] != Value::Int(sender_addr))
                {
                    println!("deploy: incorrect values in send item");
                    return None;
                }
            } else {
                println!("malformed send item");
                return None;
            }
        }

        let log_item = &logs[logs.len() - 1];
        assert!(log_item.succeeded());
        if let Value::Tuple(tup2) = log_item.get_request() {
            assert_eq!(tup2[4], Value::Int(request_id));
        } else {
            println!("Malformed ArbOS log item");
            return None;
        }
        let buf = log_item.get_return_data();
        self.address = Uint256::from_bytes(&buf);
        Some(self.address.clone())
    }

    pub fn get_function(&self, name: &str) -> Result<&ethabi::Function, ethabi::Error> {
        self.contract.function(name)
    }

    pub fn call_function(
        &self,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        debug: bool,
    ) -> Result<(Vec<ArbosReceipt>, Vec<Value>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        machine.runtime_env.insert_tx_message(
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            &calldata,
        );

        let num_logs_before = machine.runtime_env.get_all_logs().len();
        let num_sends_before = machine.runtime_env.get_all_sends().len();
        let _arbgas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        };
        let logs = machine.runtime_env.get_all_logs();
        let sends = machine.runtime_env.get_all_sends();
        Ok((
            logs[num_logs_before..].to_vec(),
            sends[num_sends_before..].to_vec(),
        ))
    }

    pub fn add_function_call_to_batch(
        &self,
        batch: &mut Vec<u8>,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        wallet: &Wallet,
    ) -> Result<Uint256, ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let tx_id_bytes = machine.runtime_env.append_signed_tx_message_to_batch(
            batch,
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            calldata,
            &wallet,
        );

        Ok((Uint256::from_bytes(&tx_id_bytes)))
    }
}
