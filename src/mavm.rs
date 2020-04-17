use std::fmt;
use std::collections::HashMap;
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use serde::{Serialize, Deserialize};


#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Label {
	Func(StringId),
	Anon(usize),
	External(usize),  // slot in imported funcs list
}

impl Label {
	pub fn relocate(self, int_offset: usize, ext_offset: usize, func_offset: usize) -> (Self, usize) {
		match self {
			Label::Func(sid) => (Label::Func(sid+func_offset), sid),
			Label::Anon(pc) => (Label::Anon(pc+int_offset), 0),
			Label::External(slot) => (Label::External(slot+ext_offset), 0),
		}
	}
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		Label::Func(sid) => write!(f, "function_{}", sid),
    		Label::Anon(n) => write!(f, "label_{}", n),
    		Label::External(slot) => write!(f, "external_{}", slot),
    	}
    }
}

#[derive(Default)]
pub struct LabelGenerator {
	next: usize
}

impl LabelGenerator {
	pub fn new() -> Self {
		LabelGenerator{ next: 0 }
	}

	pub fn next(self) -> (Label, Self) {
		(Label::Anon(self.next), LabelGenerator{next:self.next+1})
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instruction {
	pub opcode: Opcode,
	pub immediate: Option<Value>,
}

impl Instruction {
	pub fn new(opcode: Opcode, immediate: Option<Value>) -> Self {
		Instruction{ opcode, immediate }
	}

	pub fn from_opcode(opcode: Opcode) -> Self {
		Instruction::new(opcode, None)
	}

	pub fn from_opcode_imm(opcode: Opcode, immediate: Value) -> Self {
		Instruction::new(opcode, Some(immediate))
	}

	pub fn get_label(&self) -> Option<&Label> {
		match &self.opcode {
			Opcode::Label(label) => Some(label),
			_ => None
		}
	}

	pub fn replace_labels(self, label_map: &HashMap<Label, CodePt>) -> Self {
		match self.immediate {
			Some(val) => Instruction::from_opcode_imm(self.opcode, val.replace_labels(label_map)),
			None => self
		}
	}

	pub fn relocate(self, int_offset: usize, ext_offset: usize, func_offset: usize) -> (Self, usize) {
		let mut max_func_offset = 0;
		let opcode = match self.opcode {
			Opcode::PushExternal(off) => Opcode::PushExternal(off+ext_offset),
			Opcode::Label(label) => {
				let (new_label, new_func_offset) = label.relocate(int_offset, ext_offset, func_offset);
				if max_func_offset < new_func_offset {
					max_func_offset = new_func_offset;
				}
				Opcode::Label(new_label)
			}
			_ => self.opcode,
		};
		let imm = match self.immediate {
			Some(imm) => {
				let (new_imm, new_func_offset) = imm.relocate(int_offset, ext_offset, func_offset);
				if max_func_offset < new_func_offset {
					max_func_offset = new_func_offset;
				}
				Some(new_imm)
			}
			None => None,
		};
		(Instruction::new(opcode, imm), max_func_offset)
	}

	pub fn xlate_labels(self, xlate_map: &HashMap<Label, &Label>) -> Self {
		match self.immediate {
			Some(val) => Instruction::from_opcode_imm(self.opcode, val.xlate_labels(xlate_map)),
			None => self,
		}
	}
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match &self.immediate {
    		Some(v) => write!(f, "[{}] {}", v, self.opcode),
    		None => write!(f, "{}", self.opcode),
    	}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CodePt {
	Internal(usize),
	External(usize),  // slot in imported funcs list
}

impl CodePt {
	pub fn new_internal(pc: usize) -> Self {
		CodePt::Internal(pc)
	}

	pub fn new_external(name: StringId) -> Self {
		CodePt::External(name)
	}

	pub fn incr(&self) -> Option<Self> {
		match self {
			CodePt::Internal(pc) => Some(CodePt::Internal(pc+1)),
			CodePt::External(_) => None,
		}
	}

	pub fn pc_if_internal(&self) -> Option<usize> {
		if let CodePt::Internal(pc) = self {
			Some(*pc)
		} else {
			None
		}
	}

	pub fn relocate(self, int_offset: usize, ext_offset: usize) -> Self {
		match self {
			CodePt::Internal(pc) => CodePt::Internal(pc+int_offset),
			CodePt::External(off) => CodePt::External(off+ext_offset),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Value {
	Int(Uint256),
	Tuple(Vec<Value>),
	CodePoint(CodePt),
	Label(Label),
}

impl Value {
	pub fn none() -> Self {
		Value::Tuple(Vec::new())
	}

	pub fn is_none(&self) -> bool {
		if let Value::Tuple(v) = self {
			v.is_empty()
		} else {
			false
		}
	}

	pub fn replace_labels(self, label_map: &HashMap<Label, CodePt>) -> Self {
		match self {
			Value::Int(_) => self,
			Value::CodePoint(_) => self,
			Value::Label(label) => {
				let maybe_pc = label_map.get(&label);
				match maybe_pc {
					Some(pc) => Value::CodePoint(*pc),
					None => {
						println!("replace_labels failure:\nlabel = {:?}\nmap = {:?}", label, label_map);
						panic!("replace_labels failure");
					}
				}
			},
			Value::Tuple(tup) => {
				let mut new_vec = Vec::new();
				for v in tup.iter() {
					let val = v.clone();
					new_vec.push(val.replace_labels(label_map));
				}
				Value::Tuple(new_vec)
			}
		}
	}

	pub fn relocate(self, int_offset: usize, ext_offset: usize, func_offset: usize) -> (Self, usize) {
		match self {
			Value::Int(_) => (self, 0),
			Value::Tuple(v) => {
				let mut rel_v = Vec::new();
				let mut max_func_offset = 0;
				for val in v {
					let (new_val, new_func_offset) = val.relocate(int_offset, ext_offset, func_offset);
					rel_v.push(new_val);
					if (max_func_offset < new_func_offset) {
						max_func_offset = new_func_offset;
					}
				}
				(Value::Tuple(rel_v), max_func_offset)
			}
			Value::CodePoint(cpt) => (Value::CodePoint(cpt.relocate(int_offset, ext_offset)), 0),
			Value::Label(label) => {
				let (new_label, new_func_offset) = label.relocate(int_offset, ext_offset, func_offset);
				(Value::Label(new_label), new_func_offset)
			}
		}
	}

	pub fn xlate_labels(self, label_map: &HashMap<Label, &Label>) -> Self {
		match self {
			Value::Int(_) |
			Value::CodePoint(_) => self,
			Value::Tuple(v) => {
				let mut newv = Vec::new();
				for val in v {
					newv.push(val.xlate_labels(label_map));
				}
				Value::Tuple(newv)
			}
			Value::Label(label) => match label_map.get(&label) {
				Some(label2) => Value::Label(**label2),
				None => self,
			}
		}
	}

	pub fn to_usize(&self) -> Option<usize> {
		match self {
			Value::Int(i) => i.to_usize(),
			_ => None
		}
	}

	pub fn avm_hash(&self) -> Value {
		Value::Int(Uint256::zero())   // BUGBUG: need to actually hash things
	}

	pub fn avm_hash2(_v1: &Self, _v2: &Self) -> Value {
		Value::Int(Uint256::zero())   // BUGBUG: need to actually hash things
	}
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		Value::Int(i) => i.fmt(f),
    		Value::CodePoint(pc) => write!(f, "CodePoint({:?})", pc),
    		Value::Label(label) => write!(f, "Label({})", label),
    		Value::Tuple(tup) => {
    			if tup.is_empty() {
    				write!(f, "_")
    			} else {
    				let mut s = "Tuple(".to_owned();
					for (i, v) in tup.iter().enumerate() {
						if i == 0 {
							s = format!("{}{}", s, v);
						} else {
							s = format!("{}, {}", s, v);
						}
					}
					write!(f, "{})", s)
    			}
    		}
    	}
	}	
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Opcode {
	Noop,
	Panic,
	GetLocal,
	SetLocal,
	MakeFrame(usize, usize),
	Label(Label),
	Jump,
	Cjump,
	GetPC,
	PushStatic,
	PushExternal(usize),  // push codeptr of external function -- index in imported_funcs
	TupleGet(usize),  // arg is size of anysize_tuple
	TupleSet(usize),  // arg is size of anysize_tuple
	ArrayGet, 
	UncheckedFixedArrayGet(usize),  // arg is size of array     
	Tset,
	Tget,
	Pop,
	AuxPush,
	AuxPop,
	Dup0,
	Dup1,
	Dup2,
	Swap1,
	Swap2,
	Return,
	Not,
	UnaryMinus,
	BitwiseNeg,
	Hash,
	Len,
	Plus,
	Minus,
	Mul,
	Div,
	Mod,
	Sdiv,
	Smod,
	LessThan,
	GreaterThan,
	SLessThan,
	SGreaterThan,
	Equal,
	NotEqual,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	LogicalAnd,
	LogicalOr,
	Hash2,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		Opcode::MakeFrame(s1, s2) => write!(f, "MakeFrame({}, {})", s1, s2),
    		Opcode::Label(label) => label.fmt(f),
    		_ => write!(f, "{:?}", self),
    	}
    }
}
