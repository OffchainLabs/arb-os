use std::collections::HashMap;
use crate::mavm::Instruction;

pub fn strip_labels(code_in: &Vec<Instruction>) -> Vec<Instruction> {
	let mut label_map = HashMap::new();

	let mut after_count = 0;
	for pc in 0..code_in.len() {
		match code_in[pc].get_label() {
			Some(label) => { label_map.insert(label, after_count); }
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
	code_out
}