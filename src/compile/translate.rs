/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Provides routines for substituting instructions

use crate::mavm::{AVMOpcode, Instruction, LabelGenerator, Opcode, Value};

/// De-virtualizes FuncCall opcodes into the AVM function call ABI
pub fn expand_calls(code: Vec<Instruction>, label_gen: &mut LabelGenerator) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for curr in code {
        let debug = curr.debug_info;

        macro_rules! opcode {
            ($opcode:ident) => {
                Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
            };
            ($opcode:ident, $immediate:expr) => {
                Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::$opcode),
                    $immediate,
                    debug,
                )
            };
        }

        match &curr.opcode {
            Opcode::FuncCall(prop) => {
                let return_label = label_gen.next();
                let codepoint_call = label_gen.next();

                if prop.returns {
                    out.push(opcode!(Swap1, Value::Label(return_label))); // bury the return label
                }

                // check whether we're calling on a codepoint or closure tuple
                out.push(opcode!(Dup0)); // return ? ?
                out.push(opcode!(Type)); // return ? type
                out.push(opcode!(Equal, Value::from(1))); // return ? (1 for codepoint)
                out.push(opcode!(Cjump, Value::Label(codepoint_call))); // return ?

                // not a codepoint, let's unpack
                out.push(opcode!(Dup0)); // return (closure, frame) (closure, frame)
                out.push(opcode!(Tget, Value::from(1))); // return (closure, frame) frame
                out.push(opcode!(Swap2)); // frame (closure, frame) return
                out.push(opcode!(Swap1)); // frame return (closure, frame)
                out.push(opcode!(Tget, Value::from(0))); // frame return closure

                out.push(Instruction::from_opcode(
                    Opcode::Label(codepoint_call),
                    debug,
                ));
                out.push(opcode!(Jump));
                out.push(Instruction::from_opcode(Opcode::Label(return_label), debug));
            }
            _ => out.push(curr),
        }
    }
    out
}

/// De-virtualizes CjumpTo opcodes into simple Cjumps
pub fn untag_jumps(code: Vec<Instruction>) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for curr in code {
        match curr.opcode {
            Opcode::CjumpTo(..) => {
                let mut jump = curr.clone();
                jump.opcode = Opcode::AVMOpcode(AVMOpcode::Cjump);
                out.push(jump);
            }
            _ => out.push(curr),
        }
    }
    out
}

/// Translates MoveLocal phi-joins into equivalent gets and sets
pub fn replace_phi_nodes(code: Vec<Instruction>) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for curr in code {
        match curr.opcode {
            Opcode::MoveLocal(dest, source) => {
                let mut get_local = curr.clone();
                let mut set_local = curr.clone();
                get_local.opcode = Opcode::GetLocal(source);
                set_local.opcode = Opcode::SetLocal(dest);
                out.extend(vec![get_local, set_local]);
            }
            _ => out.push(curr),
        }
    }
    out
}
