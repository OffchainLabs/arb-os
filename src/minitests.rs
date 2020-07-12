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
use crate::run::{bytestack_from_bytes, run_from_file, RuntimeEnvironment};
use crate::uint256::Uint256;
use std::path::Path;
use std::convert::TryInto;
use rlp::RlpStream;

#[test]
fn test_arraytest() {
    let path = Path::new("builtin/arraytest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
fn test_rlp() {
    let mut ui = Uint256::one();
    for _i in 0..100 {
        test_rlp_uint(ui.clone(), ui.rlp_encode());
        let ui2 = ui.div(&Uint256::from_usize(2048)).unwrap(); // a valid address
        test_rlp_uint(ui2.clone(), ui2.rlp_encode());
        ui = ui
            .mul(&Uint256::from_usize(19482103))
            .add(&Uint256::from_usize(91));
    }
    let ui = Uint256::from_usize(4313412);
    test_rlp_uint(ui.clone(), ui.rlp_encode());

    let mut byte_testvecs = vec![
        vec![0u8],
        vec![13u8],
        vec![243u8],
        "Hello".as_bytes().to_vec(),
        "The quick brown fox jumped over the lazy dog.".as_bytes().to_vec(),
        vec![77u8; 692],
    ];
    let mut new_test_vec: Vec<u8> = Vec::new();
    for i in 0u64..2198 {
        new_test_vec.push(((73*i) % 256).try_into().unwrap());
    }
    byte_testvecs.push(new_test_vec.clone());
    for testvec in byte_testvecs {
        let res = rlp::encode(&testvec);
        test_rlp_bytearray(testvec, res);
    }

    let list3_testvecs = vec![
        (Uint256::zero(), vec![243u8], Uint256::one()),
        (Uint256::from_usize(9831498), vec![3u8], Uint256::from_usize(4313412)),
        (Uint256::from_usize(9831498), new_test_vec, Uint256::from_usize(4313412)),
    ];
    for testvec in list3_testvecs {
        let res = encode_list3(testvec.clone());
        test_rlp_list3(testvec, res);
    }
}

#[cfg(test)]
fn encode_list3(testvec: (Uint256, Vec<u8>, Uint256)) -> Vec<u8> {
    let mut stream = RlpStream::new_list(3);
    stream
        .append(&testvec.0.to_bytes_minimal())
        .append(&testvec.1)
        .append(&testvec.2.to_bytes_minimal());
    stream.out()
}

#[cfg(test)]
fn test_rlp_uint(ui: Uint256, correct_result: Vec<u8>) {
    let path = Path::new("stdlib/rlptest.mexe");
    let res = run_from_file(
        path,
        vec![Value::Int(Uint256::zero()), Value::Int(ui)],
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], bytestack_from_bytes(&correct_result));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[cfg(test)]
fn test_rlp_bytearray(input: Vec<u8>, correct_result: Vec<u8>) {
    let path = Path::new("stdlib/rlptest.mexe");
    let res = run_from_file(
        path,
        vec![Value::Int(Uint256::one()), bytestack_from_bytes(&input)],
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], bytestack_from_bytes(&correct_result));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[cfg(test)]
fn test_rlp_list3(testvec: (Uint256, Vec<u8>, Uint256), correct_result: Vec<u8>) {
    let path = Path::new("stdlib/rlptest.mexe");
    let res = run_from_file(
        path,
        vec![
            Value::Int(Uint256::from_usize(2)),
            Value::new_tuple(vec![
                Value::Int(testvec.0),
                bytestack_from_bytes(&testvec.1),
                Value::Int(testvec.2),
            ])
        ],
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], bytestack_from_bytes(&correct_result));
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
        RuntimeEnvironment::new(Uint256::from_usize(1111)),
        false,
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
fn test_direct_deploy_add() {
    crate::evm::evm_direct_deploy_add(None, false);
}

#[test]
fn test_direct_deploy_and_call_add() {
    let _log = crate::evm::evm_direct_deploy_and_call_add(None, false);
}

#[test]
fn test_arbsys() {
    let _log = crate::evm::evm_test_arbsys(None, false);
}

#[test]
pub fn test_crosscontract_call_with_constructors() {
    match crate::evm::evm_xcontract_call_with_constructors(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_create_opcode() {
    match crate::evm::evm_test_create(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_crosscontract_call_using_batch() {
    match crate::evm::evm_xcontract_call_using_batch(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
fn test_erc20() {
    crate::evm::mint_erc20_and_get_balance(false);
}

#[test]
fn test_erc721() {
    crate::evm::mint_erc721_and_get_balance(false);
}
