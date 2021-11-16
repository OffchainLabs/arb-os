/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved
 */

use crate::compile::translate;
use crate::compile::{CompileError, CompiledFunc};
use crate::console::Color;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Label, LabelId, Opcode, Value};
use crate::opcode;
use crate::run::{Machine, MachineState};
use crate::uint256::Uint256;
use parking_lot::Mutex;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

#[derive(Default)]
pub struct Computer {
    code: Vec<Instruction>,
    labels: HashMap<Label, CodePt>,
    cache: Mutex<HashMap<u64, Option<Value>>>,
    pub calls: Mutex<HashMap<LabelId, HashSet<LabelId>>>,
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
            cache: Mutex::new(HashMap::new()),
            calls: Mutex::new(HashMap::new()),
        })
    }

    pub fn calc(&self, caller: LabelId, label: Label, args: Vec<Value>) -> Option<Value> {
        // TODO

        // Record that the caller requested a computation from this label.
        // This will help determine which missing funcs in the final output
        // were precomputed away rather than being truly unreachable.
        let func_id = label.get_id();
        self.calls.lock().entry(caller).or_default().insert(func_id);

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

        if let MachineState::Error(err) = &machine.state {
            println!(
                "{} {}\n\t{}",
                Color::yellow("Failed to precompute:"),
                label.pretty_print(Color::PINK),
                err
            );

            if machine.arb_gas_remaining != Uint256::max_uint() {
                for item in &machine.stack.contents {
                    println!("Have {}", item.pretty_print(Color::PINK));
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
