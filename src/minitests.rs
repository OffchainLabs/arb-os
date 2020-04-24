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

#[allow(dead_code)]
fn test_arraytest(test_num: usize, expected_result: Value) {
    let path = Path::new("arraytest.mexe"); 
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

#[allow(dead_code)]
fn test_kvstest(test_num: usize, expected_result: Value) {
    let path = Path::new("kvstest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize(test_num))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{}\n{}", e.0, e.1); }
    }
}


