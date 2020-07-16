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
use crate::run::{bytes_from_bytestack, Machine};
use crate::uint256::Uint256;
use ethers_signers::Wallet;
use std::{fs::File, io::Read, path::Path};

/*
#[derive(Clone)]
pub struct AbiForDappArbCompiled {
    contracts: Vec<AbiForContractArbCompiled>,
    by_name: HashMap<String, AbiForContractArbCompiled>,
}

#[derive(Debug, Clone)]
pub struct AbiForContractArbCompiled {
    code: Vec<u8>,
    contract: ethabi::Contract,
    pub address: Uint256,
    storage_map: Vec<(Uint256, Uint256)>,
    name: String,
}

impl AbiForDappArbCompiled {
    pub fn new(contracts: Vec<AbiForContractArbCompiled>) -> Self {
        let mut by_name = HashMap::new();
        for contract in &contracts {
            by_name.insert(contract.name.clone(), contract.clone());
        }
        AbiForDappArbCompiled { contracts, by_name }
    }

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

        if let serde_json::Value::Array(vals) = json_from_file {
            let mut contracts = Vec::new();
            for val in vals {
                if let serde_json::Value::Object(fields) = val {
                    let json_abi = match fields.get("abi") {
                        Some(val) => val,
                        None => {
                            return Err(ethabi::Error::from("no abi key in json"));
                        }
                    };
                    let contract: ethabi::Contract = match serde_json::from_value(json_abi.clone())
                    {
                        Ok(c) => c,
                        Err(e) => {
                            return Err(ethabi::Error::from(e.to_string()));
                        }
                    };
                    let code = match &fields.get("code") {
                        Some(val) => {
                            let code_str = val.as_str().unwrap().to_string();
                            hex::decode(&code_str[2..]).unwrap()
                        }
                        None => {
                            return Err(ethabi::Error::from("no code key in json"));
                        }
                    };
                    let address = match &fields.get("address") {
                        Some(val) => {
                            match Uint256::from_string_hex(val.as_str().unwrap()[2..].as_ref()) {
                                Some(addr) => addr,
                                None => {
                                    return Err(ethabi::Error::from("invalid address string"));
                                }
                            }
                        }
                        None => {
                            return Err(ethabi::Error::from("no address key in json"));
                        }
                    };
                    let name = match &fields.get("name") {
                        Some(serde_json::Value::String(s)) => s,
                        _ => {
                            return Err(ethabi::Error::from("no name key in json"));
                        }
                    };
                    let mut storage_map = Vec::new();
                    if let serde_json::Value::Object(m) = &fields["storage"] {
                        for (k, v) in m {
                            if let serde_json::Value::String(s) = v {
                                storage_map.push((
                                    Uint256::from_string_hex(&k[2..]).unwrap(),
                                    Uint256::from_string_hex(&s[2..]).unwrap(),
                                ));
                            } else {
                                return Err(ethabi::Error::from(
                                    "malformed storage structure in json",
                                ));
                            }
                        }
                    }
                    contracts.push(AbiForContractArbCompiled {
                        code: code.to_vec(),
                        contract,
                        address,
                        storage_map,
                        name: name.to_string(),
                    })
                } else {
                    return Err(ethabi::Error::from("json object not a map"));
                }
            }
            Ok(AbiForDappArbCompiled::new(contracts))
        } else {
            Err(ethabi::Error::from("json file not an array"))
        }
    }

    pub fn get_contract(&self, name: &str) -> Option<&AbiForContractArbCompiled> {
        self.by_name.get(name)
    }
}

impl AbiForContractArbCompiled {
    pub fn insert_upload_message(&self, rt_env: &mut RuntimeEnvironment) {
        let decoded_insns = &self.code;

        // strip cbor info at tail end of the code
        let cbor_length = u16::from_be_bytes(
            decoded_insns[decoded_insns.len() - 2..]
                .try_into()
                .expect("unexpected u16 parsing error"),
        );
        let cbor_length = cbor_length as usize;
        let decoded_insns = &decoded_insns[..(decoded_insns.len() - cbor_length - 2)];

        let mut buf = vec![7u8]; // message type 7
        buf.extend(self.address.to_bytes_be());
        for (offset, value) in &self.storage_map {
            buf.push(1u8);
            buf.extend(offset.to_bytes_be());
            buf.extend(value.to_bytes_be());
        }
        buf.push(0u8);
        buf.extend(decoded_insns);

        rt_env.insert_l1_message(Uint256::one(), &buf);
    }

    pub fn get_function(&self, name: &str) -> Result<&ethabi::Function, ethabi::Error> {
        self.contract.function(name)
    }
}
*/

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

            // strip cbor info at tail end of the code
            /*
            let cbor_length = u16::from_be_bytes(
                decoded_insns[decoded_insns.len() - 2..]
                    .try_into()
                    .expect("unexpected u16 parsing error"),
            );
            let cbor_length = cbor_length as usize;
            let decoded_insns = &decoded_insns[..(decoded_insns.len() - cbor_length - 2)];
             */

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
        debug: bool,
    ) -> Option<Uint256> {
        let initial_logs_len = machine.runtime_env.get_all_logs().len();
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
        machine.runtime_env.insert_tx_message(
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
            Uint256::zero(),
            Uint256::zero(),
            Uint256::zero(),
            &augmented_code,
        );
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

        if let Value::Tuple(tup) = &logs[logs.len() - 1] {
            assert_eq!(tup[1], Value::Int(Uint256::zero()));
            let buf = bytes_from_bytestack(tup[2].clone())?;
            self.address = Uint256::from_bytes(&buf);
            Some(self.address.clone())
        } else {
            None
        }
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
    ) -> Result<(Vec<Value>, Vec<Value>), ethabi::Error> {
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
    ) -> Result<(), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        machine.runtime_env.append_signed_tx_message_to_batch(
            batch,
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            calldata,
            &wallet,
        );

        Ok(())
    }
}
