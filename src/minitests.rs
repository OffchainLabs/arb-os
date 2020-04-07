use std::path::Path;
use crate::mavm::Value;
use crate::uint256::Uint256;
use crate::run::run_from_file;


#[test]
fn test0() {
    test_arraytest(0, Value::none());
}

#[test]
fn test1() {
    test_arraytest(1, Value::none());
}

#[test]
fn test2() {
    test_arraytest(2, Value::Int(Uint256::zero()));
}

#[test]
fn test3() {
    test_arraytest(3, Value::Int(Uint256::from_usize(3)));
}


fn test_arraytest(test_num: usize, expected_result: Value) {
    let path = Path::new("arraytest.mexe"); 
    let res = run_from_file(path, vec![Value::Int(Uint256::from_usize((test_num)))]);
    match res {
        Ok(res) => { assert_eq!(res, expected_result); }
        Err(e) => { panic!("{:?}", e); }
    }
}

