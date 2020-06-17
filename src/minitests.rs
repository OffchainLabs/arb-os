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

use crate::evm::abi::AbiForDapp;
use crate::mavm::Value;
use crate::run::runtime_env::{bytes_from_bytestack, RuntimeEnvironment};
use crate::run::{load_from_file, run, run_from_file};
use crate::uint256::Uint256;
use ethabi::{Token, Uint};
use std::convert::TryFrom;
use std::path::Path;

#[test]
fn test_arraytest() {
    let path = Path::new("builtin/arraytest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{:?}", e);
        }
    }
}

#[test]
fn test_kvstest() {
    let path = Path::new("builtin/kvstest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_queuetest() {
    let path = Path::new("stdlib/queuetest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_globaltest() {
    let path = Path::new("builtin/globaltest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_pqtest() {
    let path = Path::new("stdlib/priorityqtest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_bytearray() {
    let path = Path::new("stdlib/bytearraytest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_map() {
    let path = Path::new("builtin/maptest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

//#[test]
#[allow(dead_code)]
fn test_keccak() {
    let path = Path::new("stdlib/keccaktest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_codeload() {
    let path = Path::new("minitests/codeloadtest.mexe");
    let res = run_from_file(path, vec![], RuntimeEnvironment::new(), false);
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_evm_load_add() {
    let logs = evm_load_add(false);
    assert_eq!(logs.len(), 1);
    if let Value::Tuple(tup) = &logs[0] {
        assert_eq!(tup[1], Value::none());
        // evm_load_add checked tup[2] for correctness
        assert_eq!(tup[3], Value::Int(Uint256::one()));
    } else {
        panic!();
    }
}

pub fn evm_load_add(debug: bool) -> Vec<Value> {
    let dapp_file_name = "contracts/add/compiled.json";
    let dapp_abi = match AbiForDapp::new_from_file(dapp_file_name) {
        Ok(dabi) => dabi,
        Err(_) => {
            panic!("failed to load add ABI from file");
        }
    };
    let add_contract = match dapp_abi.get_contract("Add") {
        Some(contract) => contract,
        None => {
            panic!("couldn't find Add contract");
        }
    };

    let mut rt_env = RuntimeEnvironment::new();
    add_contract.insert_upload_message(&mut rt_env);
    let add_func = match add_contract.get_function("add") {
        Ok(func) => func,
        Err(e) => {
            panic!(
                "couldn't find add function in Add contract: {:?}",
                e.to_string()
            );
        }
    };
    let calldata = add_func
        .encode_input(&[
            ethabi::Token::Uint(ethabi::Uint::one()),
            ethabi::Token::Uint(ethabi::Uint::one()),
        ])
        .unwrap();
    rt_env.insert_txcall_message(add_contract.address.clone(), Uint256::zero(), &calldata);

    let mut machine = load_from_file(Path::new("arbruntime/runtime.mexe"), rt_env);

    let logs = match run(&mut machine, vec![], debug) {
        Ok(logs) => logs,
        Err(e) => {
            panic!("run failed: {:?}", e);
        }
    };

    assert_eq!(logs.len(), 1);
    if let Value::Tuple(tup) = &logs[0] {
        if let Some(result_bytes) = bytes_from_bytestack(tup[2].clone()) {
            match add_func.decode_output(&result_bytes) {
                Ok(tokens) => match tokens[0] {
                    Token::Uint(ui) => {
                        assert_eq!(ui, Uint::try_from(2).unwrap());
                        logs
                    }
                    _ => {
                        panic!("token was not a uint: {:?}", tokens[0]);
                    }
                },
                Err(e) => {
                    panic!("error decoding function output: {:?}", e);
                }
            }
        } else {
            panic!("log element was not a bytestack");
        }
    } else {
        panic!("log item was not a Tuple");
    }
}
