/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use crate::mavm::{AVMOpcode, Instruction, Opcode};

fn useless_opcodes_layer<'a, I>(iter: I) -> impl Iterator<Item = &'a Instruction>
where
    I: Iterator<Item = &'a Instruction>,
{
    iter.filter(|&insn| {
        !(insn.opcode == Opcode::AVMOpcode(AVMOpcode::Noop) && insn.immediate.is_none()
            || insn.opcode == Opcode::AVMOpcode(AVMOpcode::Pop) && insn.immediate.is_some())
    })
}

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
                    location: _,
                } => {
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Dup0),
                        immediate: None,
                        location: _,
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
                    location: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                        immediate: imm,
                        location: loc2,
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
                        location: _,
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
                    location: loc1,
                } => {
                    let insn1 = code_out[code_out.len() - 1].clone();
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                        immediate: None,
                        location: _,
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
                    location: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                        immediate: imm,
                        location: loc2,
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
                            location: _,
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
                    location: _,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    match insn2 {
                        Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::IsZero),
                            immediate: imm,
                            location: loc2,
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
                    location: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                        immediate: Some(val),
                        location: _,
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
