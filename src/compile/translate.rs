/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Provides routines for substituting instructions.

use crate::compile::{ClosureAssignments, FrameSize};
use crate::link::TupleTree;
use crate::mavm::{AVMOpcode, Instruction, LabelGenerator, LabelId, Opcode, Value};
use std::collections::HashMap;

/// De-virtualizes CjumpTo opcodes into simple Cjumps
pub fn untag_jumps(code: Vec<Instruction>) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for curr in code {
        match curr.opcode {
            Opcode::JumpTo(..) => {
                let mut jump = curr.clone();
                jump.opcode = Opcode::AVMOpcode(AVMOpcode::Jump);
                out.push(jump);
            }
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
                if dest != source {
                    let mut get_local = curr.clone();
                    let mut set_local = curr.clone();
                    get_local.opcode = Opcode::GetLocal(source);
                    set_local.opcode = Opcode::SetLocal(dest);
                    out.extend(vec![get_local, set_local]);
                }
            }
            _ => out.push(curr),
        }
    }
    out
}

/// Removes higher-order Pop(n) instructions
pub fn expand_pops(code: Vec<Instruction>) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for curr in code {
        macro_rules! opcode {
            ($opcode:ident) => {
                Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), curr.debug_info)
            };
        }

        match curr.opcode {
            Opcode::Pop(depth) => out.extend(match depth {
                0 => vec![opcode!(Pop)],
                1 => vec![opcode!(Swap1), opcode!(Pop)],
                2 => vec![opcode!(Swap2), opcode!(Pop), opcode!(Swap1)],
                x => {
                    let mut code = vec![];
                    for _ in 0..(x - 1) {
                        code.push(opcode!(AuxPush));
                    }
                    code.push(opcode!(Pop));
                    for _ in 0..(x - 1) {
                        code.push(opcode!(AuxPop));
                    }
                    code
                }
            }),
            _ => out.push(curr),
        }
    }

    out
}

/// De-virtualizes FuncCall opcodes into the AVM function call ABI
pub fn expand_calls(code: Vec<Instruction>, label_gen: &mut LabelGenerator) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for index in 0..code.len() {
        let curr = &code[index];
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

                let is_func = matches!(
                    code.get(index - 1),
                    Some(Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                        immediate: Some(Value::Label(_)),
                        debug_info: _,
                    })
                );

                if !is_func {
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
                }

                out.push(opcode!(Jump));
                out.push(Instruction::from_opcode(Opcode::Label(return_label), debug));
            }
            _ => out.push(curr.clone()),
        }
    }
    out
}

/// Remove and save capture annotations
pub fn read_capture_data(code: Vec<Instruction>) -> (Vec<Instruction>, ClosureAssignments) {
    let mut out = Vec::with_capacity(code.len());
    let mut captures = HashMap::new();

    for curr in code {
        match curr.opcode {
            Opcode::ReserveCapture(slot, id) => {
                // Now we know the slot the optimizer set for using this capture
                captures.insert(id, slot);
            }
            _ => {}
        }
        out.push(curr);
    }
    (out, captures)
}

/// Use capture annotations to bridge the gap between packing a closure and calling it.
pub fn pack_closures(
    code: &Vec<Instruction>,
    capture_map: &HashMap<LabelId, ClosureAssignments>,
    frame_sizes: &HashMap<LabelId, FrameSize>,
) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(code.len());

    for curr in code {
        macro_rules! opcode {
            ($opcode:ident) => {
                Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), curr.debug_info)
            };
            ($opcode:ident, $immediate:expr) => {
                Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::$opcode),
                    $immediate,
                    curr.debug_info,
                )
            };
            (@$($opcode:tt)+) => {
                Instruction::from_opcode(Opcode::$($opcode)+, curr.debug_info)
            };
        }

        match curr.opcode {
            Opcode::MakeClosure(closure) => {
                let size = *frame_sizes.get(&closure).expect("no frame size") as usize;
                let tuple = TupleTree::new(size, true).make_empty();
                out.push(opcode!(Noop, tuple));
            }
            Opcode::Capture(closure, id) => {
                let size = *frame_sizes.get(&closure).expect("no frame size") as usize;
                let captures = capture_map.get(&closure).expect("no captures");
                let place = *captures.get(&id).expect("no slot") as usize;

                // make a tuple tree for a frame, but write to it on the stack
                let tuple = TupleTree::new(size, true);
                tuple
                    .write_code(false, place, &mut out, curr.debug_info)
                    .unwrap();
            }
            Opcode::ReserveCapture(_, _) => {
                // The closure receiving the capture doesn't need to do anything,
                // so we discard this instruction
            }
            _ => out.push(curr.clone()),
        }
    }

    out
}
