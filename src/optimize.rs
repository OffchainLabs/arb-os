use crate::mavm::{Instruction, Opcode};


pub fn peephole(code_in: &[Instruction]) -> Vec<Instruction> {
	let mut code_out = Vec::new();

	for insn in code_in {
		code_out.push(insn.clone());
		let mut done = false;
		while (!done) && (code_out.len() > 1) {
			match code_out[code_out.len()-1] {
				Instruction{ opcode: Opcode::Pop, immediate: Some(_), location: _ } => {
					code_out.pop();
				}
				Instruction{ opcode: Opcode::Pop, immediate: None, location: _ } => {
					if let Instruction{ opcode: Opcode::Dup0, immediate: None, location: _ } = code_out[code_out.len()-2] {
						code_out.pop();
						code_out.pop();
					} else {
						done = true;
					}
				}
				Instruction{ opcode: Opcode::Noop, immediate: None, location: _ } => {
					code_out.pop();
				}
				Instruction{ opcode: Opcode::AuxPush, immediate: None, location: _ } => {
					let loc1 = code_out[code_out.len()-1].location;
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::AuxPop, immediate: imm, location: loc2 } = insn2 {
						code_out.pop();
						code_out.pop();
						if imm.clone().is_some() {
							code_out.push(Instruction::new(Opcode::Noop, imm.clone(), loc2));
						}
					} else if let Instruction{ opcode: Opcode::Noop, immediate: Some(val), location: _ } = insn2 {
						code_out.pop();
						code_out.pop();
						code_out.push(Instruction::from_opcode_imm(Opcode::AuxPush, val.clone(), loc1));
					} else {
						done = true;
					}
				}
				Instruction{ opcode: Opcode::AuxPop, immediate: Some(_), location: _ } => {
					let loc1 = code_out[code_out.len()-1].location;
					let insn1 = code_out[code_out.len()-1].clone();
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::AuxPush, immediate: None, location: _ } = insn2 {
						code_out.pop();
						code_out.pop();
						code_out.push(Instruction::new(Opcode::Swap1, insn1.immediate, loc1));
					} else {
						done = true;
					}
				}
				Instruction{ opcode: Opcode::AuxPop, immediate: None, location: _ } => {
					let loc1 = code_out[code_out.len()-1].location;
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::AuxPush, immediate: imm, location: loc2 } = insn2 {
						code_out.pop();
						code_out.pop();
						if let Some(val) = imm.clone() {
							code_out.push(Instruction::from_opcode_imm(Opcode::Noop, val, loc2));
						}
					} else {
						let insn2 = code_out[code_out.len()-2].clone();
						if let Instruction{ opcode: Opcode::Noop, immediate: Some(val), location: _ } = insn2 {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::from_opcode_imm(Opcode::AuxPop, val.clone(), loc1));
						} else {
							done = true;
						}
					}
				}
				Instruction{ opcode: Opcode::Not, immediate: None, location: _ } => {
					let insn2 = code_out[code_out.len()-2].clone();
					match insn2 {
						Instruction{ opcode: Opcode::Not, immediate: imm, location: loc2 } => {
							code_out.pop();
							code_out.pop();
							if let Some(val) = imm {
								code_out.push(Instruction::new(Opcode::Noop, Some(val), loc2));
							}
						}
						Instruction{ opcode: Opcode::Equal, immediate: imm , location: loc2 } => {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::new(Opcode::NotEqual, imm, loc2));
						}
						Instruction{ opcode: Opcode::NotEqual, immediate: imm, location: loc2 } => {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::new(Opcode::Equal, imm, loc2));
						}
						_ => { done = true; }
					}
				}
				Instruction{ opcode, immediate: None, location: _ } => {
					let loc1 = code_out[code_out.len()-1].location;
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::Noop, immediate: Some(val), location: _ } = insn2 {
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