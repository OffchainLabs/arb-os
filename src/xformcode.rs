use crate::mavm::{Opcode, Instruction, Value, CodePt};
use crate::uint256::Uint256;


pub const TUPLE_SIZE: usize = 8;

pub fn fix_tuple_size(code_in: &Vec<Instruction>) -> Vec<Instruction> {
	let mut code_out = Vec::new();
	let mut locals_tree = TupleTree::new(1);

	for insn in code_in.iter() {
		match insn.opcode {
			Opcode::MakeFrame(nargs, ntotal) => {
				code_out.push(Instruction::from_opcode(Opcode::AuxPush));  // move return address to aux stack
				locals_tree = TupleTree::new(ntotal);
				if let Some(imm) = &insn.immediate {
					code_out.push(Instruction::from_opcode_imm(Opcode::Noop, imm.clone()));
				}
				code_out.push(Instruction::from_opcode_imm(Opcode::Noop, TupleTree::make_empty(&locals_tree)));
				for lnum in 0..nargs {
					code_out = locals_tree.write_code(lnum, &mut code_out);
				}
				code_out.push(Instruction::from_opcode(Opcode::AuxPush));
			}
			Opcode::TupleGet(size) => {
				let ttree = TupleTree::new(size);
				if let Some(index) = &insn.immediate {
					match index.to_usize() {
						Some(iu) => {
							code_out = ttree.read_code(iu, &mut code_out);
						}
						None => { panic!("fix_tuple_size: index too large") }
					}
				} else {
					panic!("fix_tuple_size: TupleGet without immediate arg")
				}
			}
			Opcode::TupleSet(size) => {
				let ttree = TupleTree::new(size);
				if let Some(index) = &insn.immediate {
					match index.to_usize() {
						Some(iu) => {
							code_out = ttree.write_code(iu, &mut code_out);
						}
						None => { panic!("fix_tuple_size: index too large") }
					}
				} else {
					panic!("fix_tuple_size: TupleSet without immediate arg")
				}
			}				
			Opcode::SetLocal => {
				code_out.push(Instruction::from_opcode(Opcode::AuxPop));
				if let Some(index) = &insn.immediate {
					match index.to_usize() {
						Some(iu) => {
							code_out = locals_tree.write_code(iu, &mut code_out);
						}
						None => { panic!("fix_tuple_size: index too large") }
					}
				} else {
					panic!("fix_tuple_size: SetLocal without immediate arg")
				}
				code_out.push(Instruction::from_opcode(Opcode::AuxPush));
			}
			Opcode::GetLocal => {
				code_out.push(Instruction::from_opcode(Opcode::AuxPop));
				code_out.push(Instruction::from_opcode(Opcode::Dup0));
				code_out.push(Instruction::from_opcode(Opcode::AuxPush));
				if let Some(index) = &insn.immediate {
					match index.to_usize() {
						Some(iu) => {
							code_out = locals_tree.read_code(iu, &mut code_out);
						}
						None => { panic!("fix_tuple_size: index too large") }
					}
				} else {
					panic!("fix_tuple_size: GetLocal without immediate arg")
				}
			}
			Opcode::Return => {
				code_out.push(Instruction::new(Opcode::AuxPop, match &insn.immediate {
					Some(v) => Some(v.clone()),
					None => None
				}));
				code_out.push(Instruction::from_opcode(Opcode::Pop));
				code_out.push(Instruction::from_opcode(Opcode::AuxPop));
				code_out.push(Instruction::from_opcode(Opcode::Jump));
			}
			Opcode::UncheckedFixedArrayGet(sz) => {
				let tup_size_val = Value::Int(Uint256::from_usize((sz)));
				let mut remaining_size = sz;
				while remaining_size > TUPLE_SIZE {
					//TODO: can probably make this more efficient
					// stack: idx arr
					code_out.push(Instruction::from_opcode_imm(Opcode::Dup1, tup_size_val.clone()));
					code_out.push(Instruction::from_opcode(Opcode::Mod));
					code_out.push(Instruction::from_opcode(Opcode::Swap1));
					// stack: idx slot arr
					code_out.push(Instruction::from_opcode_imm(Opcode::Swap1, tup_size_val.clone()));
					code_out.push(Instruction::from_opcode(Opcode::Div));
					// stack: subindex slot arr
					code_out.push(Instruction::from_opcode(Opcode::Swap2));
					code_out.push(Instruction::from_opcode(Opcode::Swap1));
					// stack: slot arr subindex
					code_out.push(Instruction::from_opcode(Opcode::Tget));
					code_out.push(Instruction::from_opcode(Opcode::Swap1));
					// stack: subindex subarr
					remaining_size = (remaining_size+(TUPLE_SIZE-1)) / TUPLE_SIZE;
				}
				code_out.push(Instruction::from_opcode(Opcode::Tget));
			}
			_ => { 
				code_out.push(insn.clone()); 
			}
		}
	}
	code_out
}

pub fn jump_table_to_value(jump_table: Vec<CodePt>) -> Value {
	let mut jump_table_codepoints = Vec::new();
	for pc in &jump_table {
		jump_table_codepoints.push(Value::CodePoint(*pc));
	}
	let shape = TupleTree::new(jump_table.len());
	shape.make_value(jump_table_codepoints)
}

enum TupleTree {
	Single,
	Tree(usize, Vec<Box<TupleTree>>),
}

impl TupleTree {
	fn new(size: usize) -> TupleTree {
		if size == 1 {
			return TupleTree::Single;
		}
		let mut current_size: usize = 1;
		while current_size*(TUPLE_SIZE*TUPLE_SIZE) <= size {
			current_size = current_size*TUPLE_SIZE;
		}

		let mut v = Vec::new();
		let mut remaining_size = size;
		let mut remaining_slots = TUPLE_SIZE;

		while remaining_size > 0 {
			if current_size >= remaining_size {
				v.push(Box::new(TupleTree::new(remaining_size)));
				remaining_size = 0;
			} else if current_size*(1+(remaining_slots-1)*TUPLE_SIZE) >= remaining_size {
				v.push(Box::new(TupleTree::new(current_size)));
				remaining_size = remaining_size-current_size;
				remaining_slots = remaining_slots-1;
			} else {
				current_size = current_size*TUPLE_SIZE;
			}
		}
		TupleTree::Tree(size, v)
	}

	fn make_empty(&self) -> Value {
		match self {
			TupleTree::Single => Value::Tuple(Vec::new()),
			TupleTree::Tree(_, v) => {
				let mut tvec: Vec<Value> = Vec::new();
				for tt in v.iter() {
					tvec.push(tt.make_empty());
				}
				Value::Tuple(tvec)
			}
		}
	}

	fn make_value(&self, vals: Vec<Value>) -> Value {
		let (val, _) = self.make_value_2(vals);
		val
	}

	fn make_value_2(&self, vals: Vec<Value>) -> (Value, Vec<Value>) {
		match self {
			TupleTree::Single => (vals[0].clone(), vals[1..].to_vec()),
			TupleTree::Tree(_, subtrees) => {
				let mut ret = Vec::new();
				let mut vals = vals;
				for subtree in subtrees {
					let (subval, new_vals) = subtree.make_value_2(vals);
					ret.push(subval);
					vals = new_vals;
				}
				(Value::Tuple(ret), vals)
			}
		}
	}

	fn tsize(&self) -> usize {
		match self {
			TupleTree::Single => 1,
			TupleTree::Tree(sz, _) => *sz
		}
	}

	fn read_code(&self, index: usize, code: &mut Vec<Instruction>) -> Vec<Instruction> {
		match self {
			TupleTree::Single => { code.to_vec() },
			TupleTree::Tree(_, v) => {
				let mut index = index;
				for (slot, subtree) in v.iter().enumerate() {
					if index < subtree.tsize() {
						code.push(Instruction::from_opcode_imm(Opcode::Tget, Value::Int(Uint256::from_usize(slot))));
						return subtree.read_code(index, code);
					} else {
						index = index-subtree.tsize();
					}
				}
				panic!("TupleTree::read_code: out-of-bounds read");
			}
		}
	}

	fn write_code(&self, index: usize, code: &mut Vec<Instruction>) -> Vec<Instruction> {
		if let TupleTree::Tree(_, v) = self {
			let mut index = index;
			for (slot, subtree) in v.iter().enumerate() {
				if index < subtree.tsize() {
					match **subtree {
						TupleTree::Single => {
							code.push(Instruction::from_opcode_imm(Opcode::Tset, Value::Int(Uint256::from_usize(slot))));
							return code.to_vec();
						}
						TupleTree::Tree(_, _) => {
							code.push(Instruction::from_opcode(Opcode::Swap1));
							code.push(Instruction::from_opcode(Opcode::Dup1));
							code.push(Instruction::from_opcode_imm(Opcode::Tget, Value::Int(Uint256::from_usize(slot))));
							let mut new_code = subtree.write_code(index, code);
							new_code.push(Instruction::from_opcode(Opcode::Swap1));
							new_code.push(Instruction::from_opcode_imm(Opcode::Tset, Value::Int(Uint256::from_usize(slot))));
							return new_code;
						}
					}
				} else {
					index = index-subtree.tsize();
				}
			}
			panic!("TupleTree::write_code: out-of-bounds write");
		} else {
			code.push(Instruction::from_opcode(Opcode::Pop));
			code.to_vec()
		}
	}
}
