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

use std::path::Path;
use crate::mavm::Value;
use crate::uint256::Uint256;
use crate::run::run_from_file;


#[test]
fn testarray1() {
    test_arraytest(1, Value::none());
}

#[test]
fn testarray2() {
    test_arraytest(2, Value::Int(Uint256::zero()));
}

#[test]
fn testarray3() {
    test_arraytest(3, Value::Int(Uint256::from_usize(3)));
}

#[test]
fn testarray4() {
    test_arraytest(4, Value::Int(Uint256::from_usize(5)));
}

#[test]
fn testarray5() {
    test_arraytest(5, Value::Int(Uint256::from_usize(4)));
}

#[test]
fn testarray6() {
    test_arraytest(6, Value::Int(Uint256::from_usize(7)));
}

fn test_arraytest(test_num: usize, expected_result: Value) {
    let path = Path::new("builtin/arraytest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{:?}", e); }
    }
}

#[test]
fn testkvs0() {
    test_kvstest(0, Value::none());
}

#[test]
fn testkvs1() {
    test_kvstest(1, Value::Int(Uint256::from_usize(42)));
}

#[test]
fn testkvs2() {
    test_kvstest(2, Value::Int(Uint256::from_usize(99)));
}

#[test]
fn testkvs3() {
    test_kvstest(3, Value::Int(Uint256::from_usize(56)));
}

#[test]
fn testkvs4() {
    test_kvstest(4, Value::Int(Uint256::from_usize(1017)));
}

#[test]
fn testkvs5() {
    test_kvstest(5, Value::none());
}

#[test]
fn testkvs6() {
    test_kvstest(6, Value::Int(Uint256::one()));
}

#[test]
fn testkvs7() {
    test_kvstest(7, Value::Int(Uint256::zero()));
}

#[test]
fn testkvs8() {
    test_kvstest(8, Value::Int(Uint256::from_usize(155)));
}

fn test_kvstest(test_num: usize, expected_result: Value) {
    let path = Path::new("builtin/kvstest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}


#[test]
fn testq0() {
    test_queuetest(0, Value::Int(Uint256::one()));
}

#[test]
fn testq1() {
    test_queuetest(1, Value::Int(Uint256::zero()));
}

#[test]
fn testq2() {
    test_queuetest(2, Value::Int(Uint256::from_usize(42)));
}

#[test]
fn testq3() {
    test_queuetest(3, Value::Int(Uint256::from_usize(21)));
}

fn test_queuetest(test_num: usize, expected_result: Value) {
    let path = Path::new("stdlib/queuetest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}

#[test]
fn testglobal0() {
    test_globaltest(0, Value::Int(Uint256::from_usize(3)));
}


#[test]
fn testglobal1() {
    test_globaltest(1, Value::Int(Uint256::from_usize(4)));
}

#[test]
fn testglobal2() {
    test_globaltest(2, Value::Int(Uint256::from_usize(5)));
}

#[test]
fn testglobal3() {
    test_globaltest(3, Value::Int(Uint256::from_usize(6)));
}

#[test]
fn testglobal4() {
    test_globaltest(4, Value::Int(Uint256::from_usize(7)));
}

#[test]
fn testglobal5() {
    test_globaltest(5, Value::Int(Uint256::from_usize(13)));
}

fn test_globaltest(test_num: usize, expected_result: Value) {
    let path = Path::new("builtin/globaltest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}

#[test]
fn testpq0() {
    test_pqtest(0, Value::Int(Uint256::from_usize(1)));
}

#[test]
fn testpq1() {
    test_pqtest(1, Value::Int(Uint256::from_usize(0)));
}

#[test]
fn testpq2() {
    test_pqtest(2, Value::Int(Uint256::from_usize(1)));
}

#[test]
fn testpq3() {
    test_pqtest(3, Value::Int(Uint256::from_usize(96)));
}

#[test]
fn testpq4() {
    test_pqtest(4, Value::Int(Uint256::from_usize(96)));
}

fn test_pqtest(test_num: usize, expected_result: Value) {
    let path = Path::new("stdlib/priorityqtest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}

#[test]
fn testba0() {
    test_bytearray(0, Value::Int(Uint256::from_usize(33)));
}

#[test]
fn testba1() {
    test_bytearray(1, Value::Int(Uint256::from_usize(42)));
}

#[test]
fn testba2() {
    test_bytearray(2, Value::Int(Uint256::from_usize(42)));
}

#[test]
fn testba3() {
    test_bytearray(3, Value::Int(Uint256::from_usize(42)));
}

#[test]
fn testba4() {
    test_bytearray(4, Value::Int(Uint256::from_usize(7373)));
}

#[test]
fn testba5() {
    test_bytearray(5, Value::Int(Uint256::from_usize(7373/256)));
}

#[test]
fn testba6() {
    test_bytearray(6, Value::Int(Uint256::from_usize(1)));
}

#[test]
fn testba7() {
    test_bytearray(7, Value::Int(Uint256::from_usize(100)));
}

fn test_bytearray(test_num: usize, expected_result: Value) {
    let path = Path::new("stdlib/bytearraytest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}

#[test]
fn testmap0() {
    test_map(0, Value::none());
}

#[test]
fn testmap1() {
    test_map(1, Value::Int(Uint256::from_usize(0)));
}

#[test]
fn testmap2() {
    test_map(2, Value::Int(Uint256::from_usize(1)));
}

#[test]
fn testmap3() {
    test_map(3, Value::Int(Uint256::from_usize(42)));
}

#[test]
fn testmap4() {
    test_map(4, Value::Int(Uint256::from_usize(13)));
}

fn test_map(test_num: usize, expected_result: Value) {
    let path = Path::new("builtin/maptest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}