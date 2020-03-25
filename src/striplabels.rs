use std::collections::{HashMap, HashSet};
use crate::mavm::{Instruction, Opcode, Value, Label, CodePt};
use crate::uint256::Uint256;
use crate::linker::{ExportedFunc, ExportedFuncPoint, ImportedFunc};


pub fn strip_labels(
	code_in: &Vec<Instruction>, 
	jump_table: &Vec<Label>,
	exported_funcs: &Vec<ExportedFunc>,
	imported_funcs: &Vec<ImportedFunc>,
) -> (Vec<Instruction>, Vec<CodePt>, Vec<ExportedFuncPoint>) {
	let mut label_map = HashMap::new();

	for imp_func in imported_funcs {
		label_map.insert(Label::Func(imp_func.name_id), CodePt::new_external(imp_func.name_id));
	}

	let mut after_count = 0;
	for pc in 0..code_in.len() {
		match code_in[pc].get_label() {
			Some(label) => { label_map.insert(*label, CodePt::new_internal(after_count)); }
			None => { after_count = 1+after_count; }
		}
	}

	let mut code_out = Vec::new();
	for pc in 0..code_in.len() {
		match code_in[pc].get_label() {
			Some(_) => {}
			None => {
				let insn_in = code_in[pc].clone();
				code_out.push(insn_in.replace_labels(&label_map));
			}
		}
	}

	let mut jump_table_out = Vec::new();
	for i in 0..jump_table.len() {
		match label_map.get(&jump_table[i]) {
			Some(index) => { jump_table_out.push(*index); }
			None => { panic!("strip_labels: lookup failed for jump table item"); }
		}
	}

	let mut exported_funcs_out = Vec::new();
	for exp_func in exported_funcs {
		match label_map.get(&exp_func.label) {
			Some(index) => { exported_funcs_out.push(exp_func.resolve(*index)); }
			None => { panic!("strip_labels: lookup failed for exported func"); }
		}
	}

	(code_out, jump_table_out, exported_funcs_out)
}

pub fn fix_nonforward_labels(
	code_in: &Vec<Instruction>,
	imported_funcs: &Vec<ImportedFunc>,
) -> (Vec<Instruction>, Vec<Label>) {
	let mut jump_table = Vec::new();
	let mut jump_table_index = HashMap::new();
	let mut imm_labels_seen = HashSet::new();
	let mut code_out = Vec::new();

	let mut imported_func_set = HashSet::new();
	for imp_func in imported_funcs {
		let new_label = Label::Func(imp_func.name_id);
		imported_func_set.insert(new_label); 
		jump_table_index.insert(new_label, jump_table.len());
		jump_table.push(new_label);
	}

	for pc in 0..code_in.len() {
		let insn_in = code_in[pc].clone();
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
						code_out.push(Instruction::from_opcode(Opcode::PushStatic));
						code_out.push(Instruction::from_opcode_imm(Opcode::StaticGet, Value::Int(Uint256::from_usize(idx))));
						code_out.push(Instruction::from_opcode(insn_in.opcode));
					} else {
						code_out.push(Instruction{ opcode: insn_in.opcode, immediate: Some(val) });
					}
				}
				_ => {
					code_out.push(Instruction{ opcode: insn_in.opcode, immediate: Some(val) });
				}
			}
			None => {
				code_out.push(Instruction{ opcode: insn_in.opcode, immediate: None });
			}
		}
		if let Opcode::Label(label) = insn_in.opcode {
			imm_labels_seen.insert(label);
		}
	}

	// now replace JumpTableGet instructions with regular TupleGets
	let mut code_xformed = Vec::new();
	for insn in code_out.iter() {
		match insn.opcode {
			Opcode::StaticGet => {
				code_xformed.push(Instruction{
					opcode: Opcode::TupleGet(jump_table.len()),
					immediate: match &insn.immediate {
						Some(val) => Some(val.clone()),
						None => None,
					}
				});
			}
			_ => { code_xformed.push(insn.clone()); }
		}
	}

	(code_xformed, jump_table)
}