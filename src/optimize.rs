use crate::mavm::{Instruction, Opcode};


pub fn peephole(code_in: &[Instruction]) -> Vec<Instruction> {
	let mut code_out = Vec::new();

	for insn in code_in {
		code_out.push(insn.clone());
		let mut done = false;
		while (!done) && (code_out.len() > 1) {
			match code_out[code_out.len()-1] {
				Instruction{ opcode: Opcode::Pop, immediate: Some(_) } => {
					code_out.pop();
				}
				Instruction { opcode: Opcode::Pop, immediate: None } => {
					if let Instruction{ opcode: Opcode::Dup0, immediate: None } = code_out[code_out.len()-2] {
						code_out.pop();
						code_out.pop();
					} else {
						done = true;
					}
				}
				Instruction{ opcode: Opcode::Noop, immediate: None } => {
					code_out.pop();
				}
				Instruction{ opcode: Opcode::AuxPush, immediate: None } => {
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::AuxPop, immediate: imm } = insn2 {
						code_out.pop();
						code_out.pop();
						if imm.clone().is_some() {
							code_out.push(Instruction::new(Opcode::Noop, imm.clone()));
						}
					} else {
						if let Instruction{ opcode: Opcode::Noop, immediate: Some(val) } = insn2 {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::from_opcode_imm(Opcode::AuxPush, val.clone()));
						} else {
							done = true;
						}
					}
				}
				Instruction{ opcode: Opcode::AuxPop, immediate: Some(_) } => {
					let insn1 = code_out[code_out.len()-1].clone();
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::AuxPush, immediate: None } = insn2 {
						code_out.pop();
						code_out.pop();
						code_out.push(Instruction::new(Opcode::Swap1, insn1.immediate));
					} else {
						done = true;
					}
				}
				Instruction{ opcode: Opcode::AuxPop, immediate: None } => {
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::AuxPush, immediate: imm } = insn2 {
						code_out.pop();
						code_out.pop();
						if let Some(val) = imm.clone() {
							code_out.push(Instruction::from_opcode_imm(Opcode::Noop, val));
						}
					} else {
						let insn2 = code_out[code_out.len()-2].clone();
						if let Instruction{ opcode: Opcode::Noop, immediate: Some(val) } = insn2 {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::from_opcode_imm(Opcode::AuxPop, val.clone()));
						} else {
							done = true;
						}
					}
				}
				Instruction{ opcode: Opcode::Not, immediate: None } => {
					let insn2 = code_out[code_out.len()-2].clone();
					match insn2 {
						Instruction{ opcode: Opcode::Not, immediate: imm } => {
							code_out.pop();
							code_out.pop();
							if let Some(val) = imm {
								code_out.push(Instruction::new(Opcode::Noop, Some(val)));
							}
						}
						Instruction{ opcode: Opcode::Equal, immediate: imm } => {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::new(Opcode::NotEqual, imm));
						}
						Instruction{ opcode: Opcode::NotEqual, immediate: imm } => {
							code_out.pop();
							code_out.pop();
							code_out.push(Instruction::new(Opcode::Equal, imm));
						}
						_ => { done = true; }
					}
				}
				Instruction{ opcode, immediate: None } => {
					let insn2 = code_out[code_out.len()-2].clone();
					if let Instruction{ opcode: Opcode::Noop, immediate: Some(val) } = insn2 {
						code_out.pop();
						code_out.pop();
						code_out.push(Instruction::from_opcode_imm(opcode, val.clone()));
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