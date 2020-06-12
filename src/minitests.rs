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

use crate::evm::abi::{AbiForContract, AbiForDapp};
use crate::mavm::Value;
use crate::run::chain::AvmChain;
use crate::run::runtime_env::RuntimeEnvironment;
use crate::run::{module_from_file_path, run_from_file, run_from_file_with_msgs, load_from_file};
use crate::uint256::Uint256;
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

// #[test]
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
fn test_loader1() {
    run_using_loader(
        "minitests/loadertest1.mexe",
        vec![Value::Int(Uint256::from_usize(777))],
    );
}

#[test]
fn test_loader2() {
    run_using_loader(
        "minitests/loadertest2.mexe",
        vec![Value::Int(Uint256::from_usize(120))],
    );
}

fn run_using_loader(filename: &str, expected_result: Vec<Value>) {
    let loader_path = Path::new("arbruntime/loader.mexe");
    let module_path = Path::new(filename);
    let maybe_msg = module_from_file_path(module_path);
    if let Some(msg) = maybe_msg {
        let res = run_from_file_with_msgs(loader_path, vec![msg], false);
        match res {
            Ok(res) => {
                assert_eq!(res, expected_result);
            }
            Err(e) => {
                panic!("{}", e);
            }
        }
    } else {
        panic!("failed to load and convert module from {}", filename);
    }
}

#[test]
fn test_runtime1() {
    run_using_runtime(
        "minitests/loadertest1.mexe",
        vec![Value::Int(Uint256::from_usize(777))],
    );
}

// #[test]  disabled because we're using a different EVM upload approach
/*fn test_runtime_add() {
    run_using_runtime("evm-add.mexe", vec![Value::Int(Uint256::from_usize(777))]);
}*/

fn run_using_runtime(filename: &str, expected_result: Vec<Value>) {
    let loader_path = Path::new("arbruntime/runtime.mexe");
    let module_path = Path::new(filename);
    let maybe_msg = module_from_file_path(module_path);
    if let Some(msg) = maybe_msg {
        let res = run_from_file_with_msgs(
            loader_path,
            vec![Value::Tuple(vec![
                Value::Int(Uint256::from_usize(6)),
                Value::Int(Uint256::zero()), // sender address
                Value::Tuple(vec![Value::Int(Uint256::from_usize(1)), msg]),
            ])],
            false,
        );
        match res {
            Ok(res) => {
                assert_eq!(res, expected_result);
            }
            Err(e) => {
                panic!("{}", e);
            }
        }
    } else {
        panic!("failed to load and convert module from {}", filename);
    }
}

#[test]
fn test_evm_load_add() {
    let dapp_file_name = "contracts/add/compiled.json";
    let dapp_abi = match AbiForDapp::new_from_file(dapp_file_name) {
        Ok(dabi) => dabi,
        Err(e) => {
            panic!("failed to load add ABI from file");
        }
    };
    let add_contract = match dapp_abi.get_contract("Add") {
        Some(contract) => contract,
        None => { panic!("couldn't find Add contract"); }
    };
    let mut rt_env = RuntimeEnvironment::new();
    add_contract.insert_upload_message(&mut rt_env);
    let add_func = match add_contract.get_function("add") {
        Ok(func) => func,
        Err(e) => { panic!("couldn't find add function in Add contract: {:?}", e.to_string()); }
    };

    let machine = load_from_file(Path::new("arbruntime/runtime.mexe"), rt_env);
}

fn run_evm_using_runtime(contract_file_name: &str, call_msgs: Vec<Value>) {
    let mut chain = AvmChain::new(Some(contract_file_name), &call_msgs);
    let res = chain.run(false);
    match res {
        Ok(logs) => {
            assert_eq!(logs, vec![]);
        }
        Err(e) => {
            panic!("{:?}", e);
        }
    }
}
