/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!This library target is used for cargo-fuzz

#![allow(unused_parens)]

pub mod compile;
pub mod console;
pub mod evm;
pub mod link;
pub mod mavm;
pub mod pos;
pub mod run;
pub mod stringtable;
pub mod uint256;
pub mod upload;

use crate::console::{ConsoleColors};
use crate::run::{Machine, MachineState};
use crate::mavm::{Instruction, Opcode, AVMOpcode, Value};
use crate::uint256::{Uint256};
use crate::compile::{DebugInfo};
use crate::link::optimize::*;
use crate::link::analyze::*;
use std::collections::BTreeMap;

struct Reader<'a> {
    data: &'a [u8],
    index: usize,
    done: bool,
}
impl Reader<'_> {
    fn get_byte(&mut self) -> u8 {
	if self.index < self.data.len() {
	    let ret = self.data[self.index];
	    self.index += 1;
	    ret
	} else {
	    self.done = true;
	    0
	}
    }
    fn get_usize(&mut self) -> usize {
	self.get_byte() as usize
    }
}


pub fn fuzz_optimizer(data: &[u8], detail: bool) {
    
    let mut entropy = Reader {
	data: data,
	index: 0,
	done: data.len() == 0,
    };
    
    let mut code = vec![];

    let mut index = 0;
    
    while !entropy.done {
        
        let opcode = match AVMOpcode::from_number(entropy.get_usize()) {
            Some(avm) => Opcode::AVMOpcode(avm),
            None => Opcode::AVMOpcode(AVMOpcode::Noop),
        };
        let immediate = match entropy.get_byte() {
            0..=64 => None,
	    65..=192 => Some(Value::Int(Uint256::from_usize(entropy.get_usize() % 16))),
            _ => {
		fn deserialize(entropy: &mut Reader, level: usize) -> Value {
		    let length = entropy.get_byte() % 4;
		    
		    let mut entries = vec![];
		    
		    if length == 0 || level > 3 {
			entries.push(Value::Int(Uint256::from_usize(entropy.get_usize())));
		    } else {
			for i in 0..(length - 1) {
			    entries.push(deserialize(entropy, level + 1));
			}
		    }
		    
		    Value::new_tuple(entries)
		}
		
		Some(deserialize(&mut entropy, 0))
	    }
        };
        
        macro_rules! allow {
            (@internal $opcode:ident) => {
                opcode == Opcode::AVMOpcode(AVMOpcode::$opcode)
            };
            (@internal $opcode:ident, $($remaining:ident),+) => {
                allow!(@internal $opcode) ||
                allow!(@internal $($remaining),+)
            };
            ($($opcodes:ident),* $(,)?) => {
                if ! allow!(@internal $($opcodes),*) {
                    continue;
                }
            };
        }
        
        allow!(
            // primatives
            Noop, Pop, Swap1, Swap2, Dup0, Dup1, Dup2, Tset, Tget,
            
            // those that involve global state
            Rget, Rset,
            
            // those that produce multiple outputs
            EcAdd, EcMul, EcPairing, EcRecover,
            
            // those that use the aux stack
            AuxPush, AuxPop, Xget, Xset,
            
            // simple instructions
            Plus, Mul, Minus, Div, Sdiv, Mod, Smod, AddMod, MulMod, Exp, SignExtend,
            LessThan, GreaterThan, SLessThan, SGreaterThan, Equal, IsZero,
            BitwiseAnd, BitwiseOr, BitwiseXor, BitwiseNeg, Byte, ShiftLeft, ShiftRight, ShiftArith,
            Hash, Type, Keccakf, Sha256f, PushStatic, Tlen, Ripemd160f, PushInsn, PushInsnImm,
            
            // those that involve buffers
            NewBuffer, GetBuffer8, GetBuffer64, GetBuffer256, SetBuffer8, SetBuffer64, SetBuffer256,
            
            // those that induce branching
            // Jump, Cjump,
            
            // those that would create issues to add
            //     Blake2f,           - has a variable cost function
            //     ErrCodePoint,      - the emulator can't check for equality with these
            //     SetGas, GetGas,    - optimization changes these in ways that make correctness impossible to check
            //     Halt, Panic,
            //     Inbox, InboxPeek,  
            
            // those that are out of the scope of the current optimizer
            //   Send, Log, Breakpoint, DebugPrint,
            //   SideLoad, OpenInsn, PushInsnImm, PushInsn, StackEmpty, AuxStackEmpty, ErrCodePoint
        );
	
        let curr = Instruction {
            opcode: opcode,
            immediate: immediate,
            debug_info: DebugInfo::default().propagate(true),
        };
        
        code.push(curr);
    }

    macro_rules! create {
        ($opcode:tt) => {
            Instruction {
		opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		immediate: None,
		debug_info: DebugInfo::default().propagate(true),
	    }
        };
        ($opcode:tt, $value:expr) => {
            Instruction {
		opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		immediate: $value,
		debug_info: DebugInfo::default().propagate(true),
	    }
        };
        (@virtual $opcode:tt($order:expr)) => {
            Instruction {
		opcode: Opcode::$opcode($order),
		immediate: None,
		debug_info: DebugInfo::default().propagate(true),
	    }
        };
    }
    macro_rules! int {
        ($value:expr) => {
            Some(Value::Int(Uint256::from_usize($value)))
        };
    }
    
    let block = code;
    if detail {
        print_code(&block, "unoptimized", &BTreeMap::new());
    }
    
    /*let (opt, _) = devirtualize(&block, detail);
    if detail {
        print_cfg(&create_cfg(&block), &create_cfg(&opt), "de-virtualize");
    }
    return;*/
    
    let opt = match graph_reduce(&create_cfg(&block), detail) {
        Ok((opt, _)) => flatten_cfg(opt),
        Err(_) => vec![create!(Panic)],
    };
    if detail {
        print_cfg(&create_cfg(&block), &create_cfg(&opt), "graph reduce");
    }
    
    let (opt, _) = devirtualize(&opt, detail);
    if detail {
        print_cfg(&create_cfg(&block), &create_cfg(&opt), "de-virtualize");
    }
    
    // check that the stacks have been preserved
    /*let (prior_stack, prior_aux, prior_confused) = eval_stacks(&block);
    let (after_stack, after_aux, after_confused) = eval_stacks(&opt);
    
    if after_confused || after_stack != prior_stack || after_aux != prior_aux {
        print_code(&block, "deleterious", &BTreeMap::new());
        panic!("Optimization didn't preserve the stacks!");
    }*/
    
    // check that execution is equivalent
    let mut prior_machine = Machine::from(block.clone());
    let mut after_machine = Machine::from(opt.clone());
    
    for mut machine in &mut [&mut prior_machine, &mut after_machine] {
        for i in 0..4 {
            machine.stack.push(Value::new_tuple(vec![
                Value::Int(Uint256::from_usize(i)),
                Value::Int(Uint256::from_usize(4 - i)),
                Value::Int(Uint256::from_usize(i)),
                Value::Int(Uint256::from_usize(4 - i)),
            ]));
            machine.stack.push(Value::Int(Uint256::from_usize(4 - i)));
            machine.aux_stack.push(Value::Int(Uint256::from_usize(i)));
        }
        machine.register = Value::new_tuple(vec![
            Value::Int(Uint256::from_usize(0)),
            Value::Int(Uint256::from_usize(1)),
            Value::new_tuple(vec![Value::new_tuple(vec![]), Value::new_tuple(vec![])]),
            Value::new_tuple(vec![Value::Int(Uint256::from_usize(0)), Value::Int(Uint256::from_usize(0))]),
        ]);
        machine.start_at_zero();
        machine.run(None);
    }

    if let MachineState::Error(_) = &prior_machine.state {
        return;
    }
    
    if let MachineState::Error(_) = &after_machine.state {
        print_code(&block, "deleterious", &BTreeMap::new());
        print_cfg(&create_cfg(&block), &create_cfg(&opt), "crash");
        panic!("Optimization causes machine to crash");
    }
    
    if (&prior_machine.stack.contents != &after_machine.stack.contents ||
        &prior_machine.aux_stack.contents != &after_machine.aux_stack.contents ||
        &prior_machine.register != &after_machine.register
    ) {
        print_code(&block, "deleterious", &BTreeMap::new());
        print_cfg(&create_cfg(&block), &create_cfg(&opt), "crash");

        let grey = ConsoleColors::GREY;
        let reset = ConsoleColors::RESET;
        
        for (prior, after) in prior_machine.stack.contents.iter().zip(after_machine.stack.contents.iter()) {
            println!("{:>24}    {}{:<24}{}", prior, grey, after, reset);
        }
        println!("Optimization is incorrect");
        panic!("Optimization is incorrect");
    }
    
    if &after_machine.arb_gas_remaining < &prior_machine.arb_gas_remaining {
        println!("Optimization lowers performance");
        panic!("Optimization lowers performance");
    }
}
