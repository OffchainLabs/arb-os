use crate::compile::CompileStruct;
use crate::run::{run, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;

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
