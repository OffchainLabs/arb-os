use crate::compile::CompileStruct;
use crate::mavm::Value;
use crate::run::{run, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use std::rc::Rc;

fn compile_run_cycle(input: String) -> Machine {
    let mut compile = CompileStruct::default();
    compile.input = vec![input];
    compile.test_mode = true;
    compile.consts_file = Some(format!("arb_os/constants.json"));
    let mexe = compile.invoke().unwrap();
    let mut machine = Machine::new(
        mexe,
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
    );
    run(&mut machine, vec![], false).unwrap();
    machine
}

#[test]
fn test_basic() {
    let machine = compile_run_cycle("test-programs/basic.mini".to_string());
    assert_eq!(machine.stack_top(), None);
}

#[test]
fn test_xif_else() {
    let machine = compile_run_cycle("test-programs/xif-else.mini".to_string());
    assert_eq!(machine.stack_top(), Some(&Value::Int(Uint256::zero())));
}

#[test]
fn test_codeblocks() {
    let machine = compile_run_cycle("test-programs/codeblocks.mini".to_string());
    assert_eq!(
        machine.stack_top(),
        Some(&Value::Tuple(Rc::new(vec![
            Value::Int(Uint256::zero()),
            Value::Int(Uint256::from_u64(25))
        ])))
    );
}
