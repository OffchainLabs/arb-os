/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved
 */

use crate::compile::translate;
use crate::compile::{CompileError, CompiledFunc};
use crate::console::Color;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Label, Opcode, Value};
use crate::opcode;
use crate::run::{Machine, MachineState};
use std::collections::HashMap;

pub struct Computer {
    code: Vec<Instruction>,
    labels: HashMap<Label, CodePt>,
    cached: HashMap<u64, Option<Value>>,
}

impl Computer {
    pub fn new(funcs: Vec<CompiledFunc>) -> Result<Self, CompileError> {
        let mut code = vec![];

        for func in funcs {
            code.extend(func.code);
        }

        let code = translate::replace_closures_with_errors(code);
        let (mut code, labels) = translate::labels_to_codepoints(code)?;

        for curr in &code {
            match curr.opcode {
                Opcode::AVMOpcode(_) => {}
                op => {
                    return Err(CompileError::internal(
                        format!(
                            "Found virtual opcode {} in linked code",
                            op.pretty_print(Color::RED)
                        ),
                        curr.debug_info.locs(),
                    ));
                }
            }
        }

        code.extend(vec![
            // protect the accepting state from run-over
            opcode!(Error),
            // This is the accepting state. Since no halt's exist in ArbOS, jumping here can be used
            // to determine if an emulated function has successfully computed a result.
            opcode!(Halt),
            // The C++ and rust emulators differ when it comes to building segments.
            // Since we're using rust, we need the final instruction to be the error codepoint,
            // so that the default value for a codepoint still produces an error when called.
            opcode!(Error),
        ]);

        let code = translate::set_error_codepoints(code);

        Ok(Computer {
            code,
            labels,
            cached: HashMap::new(),
        })
    }

    pub fn calc(&self, label: Label, args: Vec<Value>) -> Option<Value> {
        let codept = match self.labels.get(&label) {
            Some(codept) => *codept,
            None => return None,
        };

        let mut machine = Machine::from(self.code.clone());

        // Load the accept state as the return destination when jumping.
        // If the machine reaches this state (i.e. does not error), we have a result.
        let accept = Value::CodePoint(CodePt::Internal(self.code.len() - 2));

        machine.stack.contents.extend(args);
        machine.stack.push(accept);

        machine.state = MachineState::Running(codept);
        machine.run(None);

        if machine.stack.contents.len() > 1 {
            println!("Call {}", label.pretty_print(Color::RED));
            for item in &machine.stack.contents {
                println!("Also {}", item.pretty_print(Color::PINK));
            }
        }

        if let MachineState::Error(err) = &machine.state {
            for item in &machine.stack.contents {
                println!("Have {}", item.pretty_print(Color::PINK));
            }
            println!("{}", err);
            return None;
        }

        let result = match machine.stack.contents.pop_back() {
            Some(result) => result,
            _ => return None,
        };
        assert!(machine.stack.contents.is_empty());

        //println!("Computed {}", result.pretty_print(Color::PINK));

        Some(result)
    }
}