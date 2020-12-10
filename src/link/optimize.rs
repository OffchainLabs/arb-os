/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functions for modifying a sequence of Instructions to improve performance and lower gas
//! costs.

use crate::mavm::{AVMOpcode, Instruction, Opcode};

///Removes instructions that have no effect on the output of the program.
fn useless_opcodes_layer<'a, I>(iter: I) -> impl Iterator<Item = &'a Instruction>
where
    I: Iterator<Item = &'a Instruction>,
{
    iter.filter(|&insn| {
        !(insn.opcode == Opcode::AVMOpcode(AVMOpcode::Noop) && insn.immediate.is_none()
            || insn.opcode == Opcode::AVMOpcode(AVMOpcode::Pop) && insn.immediate.is_some())
    })
}

///Takes a slice of `Instruction`s and returns a vector of instructions with certain combinations of
/// instructions recursively either removed or replaced by simpler instructions.
///
/// These combinations are:
/// * Dup0 and Pop with no immediates, removed
/// * AuxPop with an immediate followed by AuxPush with no immediate, replaced by Noop with the same
/// immediate
/// * Noop with an immediate followed by AuxPush with no immediate, replaced by AuxPush with the
/// same immediate
/// * An AuxPush with no immediate followed by an AuxPop with an immediate, replaced by Swap1 with
/// the same immediate
/// * An AuxPush with an immediate followed by an AuxPop with no immediate, replaced by Noop with
/// the same immediate
///  * A Noop with an immediate followed by an AuxPop with no immediate, replaced by AuxPop with the
/// same immediate
/// * IsZero with an immediate followed by IsZero without an immediate, replaced by Noop with the
/// same immediate
/// * A Noop with an immediate followed by any instruction without an immediate, replaced by the
/// second instruction with the immediate from the first.
pub fn peephole(code_in: &[Instruction]) -> Vec<Instruction> {
    let mut code_out = Vec::new();

    for insn in useless_opcodes_layer(code_in.iter()) {
        code_out.push(insn.clone());
        let mut done = false;
        while (!done) && (code_out.len() > 1) {
            match *code_out.last().unwrap() {
                Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                    immediate: None,
                    debug_info: _,
                } => {
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Dup0),
                        immediate: None,
                        debug_info: _,
                    } = code_out[code_out.len() - 2]
                    {
                        code_out.pop();
                        code_out.pop();
                    } else {
                        done = true;
                    }
                }
                Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                    immediate: None,
                    debug_info: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                        immediate: imm,
                        debug_info: loc2,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        if imm.clone().is_some() {
                            code_out.push(Instruction::new(
                                Opcode::AVMOpcode(AVMOpcode::Noop),
                                imm.clone(),
                                loc2,
                            ));
                        }
                    } else if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                        immediate: Some(val),
                        debug_info: _,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        code_out.push(Instruction::from_opcode_imm(
                            Opcode::AVMOpcode(AVMOpcode::AuxPush),
                            val.clone(),
                            loc1,
                        ));
                    } else {
                        done = true;
                    }
                }
                Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                    immediate: Some(_),
                    debug_info: loc1,
                } => {
                    let insn1 = code_out[code_out.len() - 1].clone();
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                        immediate: None,
                        debug_info: _,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        code_out.push(Instruction::new(
                            Opcode::AVMOpcode(AVMOpcode::Swap1),
                            insn1.immediate,
                            loc1,
                        ));
                    } else {
                        done = true;
                    }
                }
                Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                    immediate: None,
                    debug_info: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                        immediate: imm,
                        debug_info: loc2,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        if let Some(val) = imm.clone() {
                            code_out.push(Instruction::from_opcode_imm(
                                Opcode::AVMOpcode(AVMOpcode::Noop),
                                val,
                                loc2,
                            ));
                        }
                    } else {
                        let insn2 = code_out[code_out.len() - 2].clone();
                        if let Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                            immediate: Some(val),
                            debug_info: _,
                        } = insn2
                        {
                            code_out.pop();
                            code_out.pop();
                            code_out.push(Instruction::from_opcode_imm(
                                Opcode::AVMOpcode(AVMOpcode::AuxPop),
                                val.clone(),
                                loc1,
                            ));
                        } else {
                            done = true;
                        }
                    }
                }
                Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::IsZero),
                    immediate: None,
                    debug_info: _,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    match insn2 {
                        Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::IsZero),
                            immediate: imm,
                            debug_info: loc2,
                        } => {
                            code_out.pop();
                            code_out.pop();
                            if let Some(val) = imm {
                                code_out.push(Instruction::new(
                                    Opcode::AVMOpcode(AVMOpcode::Noop),
                                    Some(val),
                                    loc2,
                                ));
                            }
                        }
                        _ => {
                            done = true;
                        }
                    }
                }
                Instruction {
                    opcode: Opcode::AVMOpcode(avm_opcode),
                    immediate: None,
                    debug_info: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                        immediate: Some(val),
                        debug_info: _,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        code_out.push(Instruction::from_opcode_imm(
                            Opcode::AVMOpcode(avm_opcode),
                            val.clone(),
                            loc1,
                        ));
                    } else {
                        done = true;
                    }
                }
                _ => {
                    done = true;
                }
            }
        }
    }
    code_out
}
