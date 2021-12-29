/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved
 */

use crate::compile::translate;
use crate::compile::{CompileError, CompiledFunc};
use crate::console::Color;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Label, Opcode, Value};
use crate::opcode;
use crate::run::{Machine, MachineState};
use crate::uint256::Uint256;
use parking_lot::Mutex;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Write;

/// Provides a mechanism for emulating pure funcs in ArbOS
pub struct Computer {
    /// The program's instructions
    code: Vec<Instruction>,
    /// Associates labels to where to jump in order to call them
    labels: HashMap<Label, CodePt>,
    /// Memoizes prior computations since emulation is expensive
    cache: Mutex<HashMap<u64, Option<Value>>>,
    /// Log file for debugging simulation warnings
    file: Mutex<File>,
}

impl Computer {
    pub fn new(funcs: Vec<CompiledFunc>) -> Result<Self, CompileError> {
        let mut code = vec![];

        for func in funcs {
            code.extend(func.code);
        }

        let (mut code, labels) = translate::labels_to_codepoints(code)?;

        for curr in &mut code {
            match curr.opcode {
                Opcode::Capture(..) | Opcode::ReserveCapture(..) | Opcode::MakeClosure(..) => {
                    // Simulating closures requires a linking step we don't have,
                    // so instead error if these are hit
                    curr.opcode = Opcode::AVMOpcode(AVMOpcode::Error);
                }
                Opcode::AVMOpcode(AVMOpcode::Halt) => {
                    // These aren't present but would be deadly if introduced in the future.
                    // Making Halt error ensures uniqueness of the accepting state.
                    curr.opcode = Opcode::AVMOpcode(AVMOpcode::Error);
                }
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
        let file = Mutex::new(File::create("logs/sims.txt").expect("failed to open file"));

        Ok(Computer {
            code,
            labels,
            cache: Mutex::new(HashMap::new()),
            file,
        })
    }

    /// Simulate a function call, returning the value should the computation succeed
    ///   label  - the globally unique label of the func
    ///   args   - the values on the stack
    pub fn calc(&self, label: Label, args: Vec<Value>) -> Option<Value> {
        let mut hasher = DefaultHasher::new();
        label.hash(&mut hasher);
        args.hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(cached) = self.cache.lock().get(&hash) {
            return cached.clone();
        }

        macro_rules! cache {
            ($value:expr) => {{
                let value = $value;
                self.cache.lock().insert(hash, value.clone());
                value
            }};
        }

        let codept = match self.labels.get(&label) {
            Some(codept) => *codept,
            None => return cache!(None),
        };

        let mut machine = Machine::from(self.code.clone());
        machine.arb_gas_remaining = Uint256::from_usize(5000000);

        // Load the accept state as the return destination when jumping.
        // If the machine reaches this state (i.e. does not error), we have a result.
        let accept = Value::CodePoint(CodePt::Internal(self.code.len() - 2));

        machine.stack.contents.extend(args);
        machine.stack.push(accept);

        machine.state = MachineState::Running(codept);
        machine.run(None);

        if machine.spec_divergence {
            writeln!(
                &mut self.file.lock(),
                "Skipping {} since execution did something inconsistent with the C++ emulator",
                label.pretty_print(Color::PINK),
            )
            .expect("failed to write file");
            return cache!(None);
        }

        if let MachineState::Error(err) = &machine.state {
            let mut file = self.file.lock();
            writeln!(
                &mut file,
                "{} {}\n\t{}",
                Color::yellow("Failed to precompute:"),
                label.pretty_print(Color::PINK),
                err
            )
            .expect("failed to write file");

            if machine.arb_gas_remaining != Uint256::max_uint() {
                for item in &machine.stack.contents {
                    writeln!(&mut file, "Have {}", item.pretty_print(Color::PINK))
                        .expect("failed to write file");
                }
            }
            return cache!(None);
        }

        let result = match machine.stack.contents.pop_back() {
            Some(result) => result,
            _ => return cache!(None),
        };

        if !machine.stack.is_empty() {
            println!("Call {}", label.pretty_print(Color::RED));
            for item in &machine.stack.contents {
                println!("Also {}", item.pretty_print(Color::PINK));
            }
            panic!("Simulated machine has extra data on the stack");
        }

        cache!(Some(result))
    }
}
