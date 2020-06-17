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
use crate::run::run_from_file;
use crate::run::runtime_env::RuntimeEnvironment;
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
    let logs = crate::evm::evm_load_add(false);
    assert_eq!(logs.len(), 1);
    if let Value::Tuple(tup) = &logs[0] {
        assert_eq!(tup[1], Value::none());
        // evm_load_add checked tup[2] for correctness
        assert_eq!(tup[3], Value::Int(Uint256::one()));
    } else {
        panic!();
    }
}
