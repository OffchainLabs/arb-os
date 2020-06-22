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

use crate::mavm::{Instruction, Opcode};

fn useless_opcodes_layer<'a, I>(iter: I) -> impl Iterator<Item = &'a Instruction>
where
    I: Iterator<Item = &'a Instruction>,
{
    iter.filter(|&insn| {
        !(insn.opcode == Opcode::Noop && insn.immediate.is_none())
            && !(insn.opcode == Opcode::Pop && insn.immediate.is_some())
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
                    opcode: Opcode::Pop,
                    immediate: None,
                    location: _,
                } => {
                    if let Instruction {
                        opcode: Opcode::Dup0,
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
                    opcode: Opcode::AuxPush,
                    immediate: None,
                    location: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AuxPop,
                        immediate: imm,
                        location: loc2,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        if imm.clone().is_some() {
                            code_out.push(Instruction::new(Opcode::Noop, imm.clone(), loc2));
                        }
                    } else if let Instruction {
                        opcode: Opcode::Noop,
                        immediate: Some(val),
                        location: _,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        code_out.push(Instruction::from_opcode_imm(
                            Opcode::AuxPush,
                            val.clone(),
                            loc1,
                        ));
                    } else {
                        done = true;
                    }
                }
                Instruction {
                    opcode: Opcode::AuxPop,
                    immediate: Some(_),
                    location: loc1,
                } => {
                    let insn1 = code_out[code_out.len() - 1].clone();
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AuxPush,
                        immediate: None,
                        location: _,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        code_out.push(Instruction::new(Opcode::Swap1, insn1.immediate, loc1));
                    } else {
                        done = true;
                    }
                }
                Instruction {
                    opcode: Opcode::AuxPop,
                    immediate: None,
                    location: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::AuxPush,
                        immediate: imm,
                        location: loc2,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        if let Some(val) = imm.clone() {
                            code_out.push(Instruction::from_opcode_imm(Opcode::Noop, val, loc2));
                        }
                    } else {
                        let insn2 = code_out[code_out.len() - 2].clone();
                        if let Instruction {
                            opcode: Opcode::Noop,
                            immediate: Some(val),
                            location: _,
                        } = insn2
                        {
                            code_out.pop();
                            code_out.pop();
                            code_out.push(Instruction::from_opcode_imm(
                                Opcode::AuxPop,
                                val.clone(),
                                loc1,
                            ));
                        } else {
                            done = true;
                        }
                    }
                }
                Instruction {
                    opcode: Opcode::IsZero,
                    immediate: None,
                    location: _,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    match insn2 {
                        Instruction {
                            opcode: Opcode::IsZero,
                            immediate: imm,
                            location: loc2,
                        } => {
                            code_out.pop();
                            code_out.pop();
                            if let Some(val) = imm {
                                code_out.push(Instruction::new(Opcode::Noop, Some(val), loc2));
                            }
                        }
                        Instruction {
                            opcode: Opcode::Equal,
                            immediate: imm,
                            location: loc2,
                        } => {
                            code_out.pop();
                            code_out.pop();
                            code_out.push(Instruction::new(Opcode::NotEqual, imm, loc2));
                        }
                        Instruction {
                            opcode: Opcode::NotEqual,
                            immediate: imm,
                            location: loc2,
                        } => {
                            code_out.pop();
                            code_out.pop();
                            code_out.push(Instruction::new(Opcode::Equal, imm, loc2));
                        }
                        _ => {
                            done = true;
                        }
                    }
                }
                Instruction {
                    opcode,
                    immediate: None,
                    location: loc1,
                } => {
                    let insn2 = code_out[code_out.len() - 2].clone();
                    if let Instruction {
                        opcode: Opcode::Noop,
                        immediate: Some(val),
                        location: _,
                    } = insn2
                    {
                        code_out.pop();
                        code_out.pop();
                        code_out.push(Instruction::from_opcode_imm(opcode, val.clone(), loc1));
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
