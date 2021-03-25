use crate::compile::CompileStruct;
use crate::mavm::Value;
use crate::run::{run, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use std::rc::Rc;

#[test]
fn test_basic() {
    let mut compile = CompileStruct::default();
    compile.input = vec!["test-programs/basic.mini".to_string()];
    compile.test_mode = true;
    let mexe = compile.invoke().unwrap();
    let mut machine = Machine::new(
        mexe,
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
    );
    let _result = run(&mut machine, vec![], false).unwrap();
}

#[test]
fn test_xif_else() {
    let mut compile = CompileStruct::default();
    compile.input = vec!["test-programs/xif-else.mini".to_string()];
    compile.test_mode = true;
    let mexe = compile.invoke().unwrap();
    let mut machine = Machine::new(
        mexe,
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
    );
    let _result = run(&mut machine, vec![], false).unwrap();
    assert_eq!(machine.stack_top(), Some(&Value::Int(Uint256::zero())));
}

#[test]
fn test_codeblocks() {
    let mut compile = CompileStruct::default();
    compile.input = vec!["test-programs/codeblocks.mini".to_string()];
    compile.test_mode = true;
    let mexe = compile.invoke().unwrap();
    let mut machine = Machine::new(
        mexe,
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
    );
    let _result = run(&mut machine, vec![], false).unwrap();
    assert_eq!(
        machine.stack_top(),
        Some(&Value::Tuple(Rc::new(vec![
            Value::Int(Uint256::zero()),
            Value::Int(Uint256::from_u64(25))
        ])))
    );
}
