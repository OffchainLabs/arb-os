use std::path::Path;
use std::fs::File;
use std::io::Read;
use crate::mavm::{Value, CodePt};
use crate::emulator::{Machine, ExecutionError};
use crate::link::LinkedProgram;


pub fn run_from_file(path: &Path, args: Vec<Value>) -> Result<Value, ExecutionError> {
   let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {:?}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    s = match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {:?}", display, why),
        Ok(_) => s,
    };

    run_from_string(s, args)
}

fn run_from_string(s: String, args: Vec<Value>) -> Result<Value, ExecutionError> {
    let parse_result: Result<LinkedProgram, serde_json::Error> = serde_json::from_str(&s);
    let program = match parse_result {
        Ok(prog) => prog,
        Err(e) => {
            println!("json parsing error: {:?}", e);
            panic!();
        }
    };
    let mut new_machine = Machine::new(program);
    run(&mut new_machine, args)
}

fn run(machine: &mut Machine, args: Vec<Value>) -> Result<Value, ExecutionError> {
    match machine.test_call(CodePt::new_internal(0), args) {
        Ok(mut stack) => {
            let machine_state = machine.get_state();
            stack.pop(&machine_state)
        }
        Err(e) => Err(e)
    }
}
