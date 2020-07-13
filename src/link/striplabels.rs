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

//!Provides utilities used in the `postlink_compile` function

use super::{ExportedFunc, ExportedFuncPoint, ImportedFunc};
use crate::evm::num_runtime_funcs;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Label, Opcode, Value};
use crate::uint256::Uint256;
use std::collections::{HashMap, HashSet};

///Replaces labels with code points in code_in, and in copies of jump_table, and exported_funcs. A
/// tuple of these modified values is returned if the function is successful, and the label causing
/// the error is returned otherwise.
///
/// The maybe_evm_pcs argument appends a list of PCs to the immediate of the first instruction, if
/// set to Some, this should only be done for modules.
pub fn strip_labels(
    mut code_in: Vec<Instruction>,
    jump_table: &[Label],
    exported_funcs: &[ExportedFunc],
    imported_funcs: &[ImportedFunc],
    maybe_evm_pcs: Option<Vec<usize>>, // will be Some iff this is a module
) -> Result<(Vec<Instruction>, Vec<CodePt>, Vec<ExportedFuncPoint>), Label> {
    if let Some(evm_pcs) = maybe_evm_pcs {
        let mut list_val = Value::none();
        for evm_pc in evm_pcs {
            list_val = Value::new_tuple(vec![
                list_val,
                Value::Int(Uint256::from_usize(evm_pc)),
                Value::Label(Label::Evm(evm_pc)),
            ]);
        }
        // re-do the first instruction in the code, which got a dummy value in link
        code_in[0] = Instruction::from_opcode_imm(
            Opcode::AVMOpcode(AVMOpcode::Swap1),
            Value::new_tuple(vec![
                list_val,
                code_in[0]
                    .immediate
                    .as_ref()
                    .expect("module did not have storage immediate")
                    .clone(),
            ]),
            None,
        );
    }
    let mut label_map = HashMap::new();

    for i in 0..num_runtime_funcs() {
        label_map.insert(Label::Runtime(i), CodePt::new_runtime(i));
    }

    for imp_func in imported_funcs {
        let new_codept = CodePt::new_external(imp_func.slot_num);
        label_map.insert(Label::External(imp_func.slot_num), new_codept);
        label_map.insert(Label::Func(imp_func.name_id), new_codept);
    }

    let mut after_count = 0;
    for insn in &code_in {
        match insn.get_label() {
            Some(label) => {
                label_map.insert(*label, CodePt::new_internal(after_count));
            }
            None => {
                after_count += 1;
            }
        }
    }

    let mut code_out = Vec::new();
    for insn in code_in {
        match insn.get_label() {
            Some(_) => {}
            None => {
                let insn_in = insn.clone();
                code_out.push(insn_in.replace_labels(&label_map)?);
            }
        }
    }

    let mut jump_table_out = Vec::new();
    for jt_item in jump_table {
        match label_map.get(&jt_item) {
            Some(index) => {
                jump_table_out.push(*index);
            }
            None => {
                panic!("strip_labels: lookup failed for jump table item");
            }
        }
    }

    let mut exported_funcs_out = Vec::new();
    for exp_func in exported_funcs {
        match label_map.get(&exp_func.label) {
            Some(index) => {
                exported_funcs_out.push(exp_func.resolve(*index));
            }
            None => {
                panic!("strip_labels: lookup failed for exported func");
            }
        }
    }

    Ok((code_out, jump_table_out, exported_funcs_out))
}

///Replaces jumps to labels not moving the PC forward with a series of instructions emulating a
/// direct jump.
///
/// Returns the modified set of instructions and a vector of labels in order of appearance in the
/// code.
pub fn fix_nonforward_labels(
    code_in: &[Instruction],
    imported_funcs: &[ImportedFunc],
) -> (Vec<Instruction>, Vec<Label>) {
    let mut jump_table = Vec::new();
    let mut jump_table_index = HashMap::new();
    let mut imm_labels_seen = HashSet::new();
    let mut code_out = Vec::new();

    let mut imported_func_set = HashSet::new();
    for imp_func in imported_funcs {
        let new_label = Label::External(imp_func.slot_num);
        imported_func_set.insert(new_label);
        /*
        jump_table_index.insert(new_label, jump_table.len());
        jump_table.push(new_label);
        */
    }

    for insn in code_in {
        let insn_in = insn.clone();
        match insn_in.immediate {
            Some(val) => match val {
                Value::Label(label) => {
                    if imm_labels_seen.contains(&label) || imported_func_set.contains(&label) {
                        let idx = match jump_table_index.get(&label) {
                            Some(index) => *index,
                            None => {
                                let index = jump_table.len();
                                jump_table_index.insert(label.clone(), index);
                                jump_table.push(label);
                                index
                            }
                        };
                        code_out.push(Instruction::from_opcode(
                            Opcode::AVMOpcode(AVMOpcode::PushStatic),
                            insn_in.location,
                        ));
                        code_out.push(Instruction::from_opcode(
                            Opcode::PushExternal(idx),
                            insn_in.location,
                        ));
                        code_out.push(Instruction::from_opcode(insn_in.opcode, insn_in.location));
                    } else {
                        code_out.push(Instruction::from_opcode_imm(
                            insn_in.opcode,
                            val,
                            insn_in.location,
                        ));
                    }
                }
                _ => {
                    code_out.push(Instruction::from_opcode_imm(
                        insn_in.opcode,
                        val,
                        insn_in.location,
                    ));
                }
            },
            None => {
                code_out.push(Instruction::from_opcode(insn_in.opcode, insn_in.location));
            }
        }
        if let Opcode::Label(label) = insn_in.opcode {
            imm_labels_seen.insert(label);
        }
    }

    let mut code_xformed = Vec::new();
    for insn in code_out.iter() {
        match insn.opcode {
            Opcode::PushExternal(idx) => {
                if let Some(val) = &insn.immediate {
                    code_xformed.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Noop),
                        val.clone(),
                        insn.location,
                    ));
                }
                code_xformed.push(Instruction::from_opcode_imm(
                    Opcode::TupleGet(jump_table.len()),
                    Value::Int(Uint256::from_usize(idx)),
                    insn.location,
                ));
            }
            _ => {
                code_xformed.push(insn.clone());
            }
        }
    }

    (code_xformed, jump_table)
}
