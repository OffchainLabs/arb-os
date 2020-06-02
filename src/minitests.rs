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
use crate::run::{run_from_file, run_from_file_with_msgs, module_from_file_path};
use crate::uint256::Uint256;
use crate::run::runtime_env::RuntimeEnvironment;
use std::path::Path;

#[test]
fn test_arraytest() {
    let path = Path::new("builtin/arraytest.mexe");
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()

    );
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
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new()
    );
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
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
fn test_keccak() {
    let path = Path::new("stdlib/keccaktest.mexe");
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
    let res = run_from_file(
        path, 
        vec![],
        RuntimeEnvironment::new()
    );
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
    run_using_loader("minitests/loadertest1.mexe", vec![Value::Int(Uint256::from_usize(777))]);
}

fn run_using_loader(filename: &str, expected_result: Vec<Value>) {
    let loader_path = Path::new("arbruntime/loader.mexe");
    let module_path = Path::new(filename);
    let maybe_msg = module_from_file_path(module_path);
    if let Some(msg) = maybe_msg {
        let res = run_from_file_with_msgs(loader_path, vec![msg]);
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
    run_using_runtime("minitests/loadertest1.mexe", vec![Value::Int(Uint256::from_usize(777))]);
}

fn run_using_runtime(filename: &str, expected_result: Vec<Value>) {
    let loader_path = Path::new("arbruntime/runtime.mexe");
    let module_path = Path::new(filename);
    let maybe_msg = module_from_file_path(module_path);
    if let Some(msg) = maybe_msg {
        let res = run_from_file_with_msgs(
            loader_path, 
            vec![Value::Tuple(
                vec![
                    Value::Int(Uint256::from_usize(6)),
                    Value::Int(Uint256::zero()),   // sender address
                    Value::Tuple(vec![
                        Value::Int(Uint256::from_usize(111)), 
                        msg                    
                    ])
                ]
            )],
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
