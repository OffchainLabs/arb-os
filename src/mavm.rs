use std::fmt;
use std::collections::HashMap;
use crate::stringtable::StringId;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum Label {
	Func(StringId),
	Anon(usize)
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		Label::Func(sid) => write!(f, "function_{}", sid),
    		Label::Anon(n) => write!(f, "label_{}", n)
    	}
    }
}

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

#[derive(Debug, Clone)]
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

	pub fn replace_labels(self, label_map: &HashMap<&Label, usize>) -> Self {
		match self.immediate {
			Some(val) => Instruction::from_opcode_imm(self.opcode, val.replace_labels(label_map)),
			None => self
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

#[derive(Debug, Clone)]
pub enum Value {
	Int(Uint256),
	Tuple(Vec<Value>),
	CodePoint(usize),
	Label(Label),
}

impl Value {
	pub fn none() -> Self {
		Value::Tuple(Vec::new())
	}

	pub fn is_none(&self) -> bool {
		if let Value::Tuple(v) = self {
			v.len() == 0
		} else {
			false
		}
	}

	pub fn replace_labels(self, label_map: &HashMap<&Label, usize>) -> Self {
		match self {
			Value::Int(_) => self,
			Value::CodePoint(_) => self,
			Value::Label(label) => {
				let maybe_pc = label_map.get(&label);
				match maybe_pc {
					Some(pc) => Value::CodePoint(*pc),
					None => {
						print!("replace_labels failure:\nlabel = {:?}\nmap = {:?}\n", label, label_map);
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

	pub fn to_usize(&self) -> Option<usize> {
		match self {
			Value::Int(i) => i.to_usize(),
			_ => None
		}
	}
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		Value::Int(i) => i.fmt(f),
    		Value::CodePoint(pc) => write!(f, "CodePoint({:?})", pc),
    		Value::Label(label) => write!(f, "Label({})", label),
    		Value::Tuple(tup) => {
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

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
	Noop,
	GetLocal,
	SetLocal,
	MakeFrame(usize, usize),
	Label(Label),
	Jump,
	Cjump,
	GetPC,
	PushStatic,
	StaticGet,     // get from static slot (used when static size not yet known)
	TupleGet(usize),  // arg is size of anysize_tuple
	TupleSet(usize),  // arg is size of anysize_tuple
	ArrayGet,      
	Tset,
	Tget,
	Pop,
	AuxPush,
	AuxPop,
	Dup0,
	Dup1,
	Swap,
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
	LessThan,
	GreaterThan,
	LessEq,
	GreaterEq,
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

#[derive(Debug, Clone)]
pub struct Uint256 {
	val: [u64; 4]
}

impl Uint256 {
	pub fn zero() -> Self {
		Uint256{ val: [0, 0, 0, 0] }
	}

	pub fn from_u64(x: u64) -> Self {
		Uint256{ val: [x, 0, 0, 0] }
	}

	pub fn from_usize(x: usize) -> Self {
		Uint256::from_u64(x as u64)
	}

	pub fn from_string(s: &str) -> Self {
		//BUGBUG: this panics on values of 2^64 or more
		Uint256{ val: [s.parse().unwrap(), 0, 0, 0] }
	}

	pub fn to_usize(&self) -> Option<usize> {
		if self.val[1]!=0 || self.val[2]!=0 || self.val[3]!=0 {
			None
		} else {
			let v = self.val[0];
			if ((v as usize) as u64) == v {
				Some(v as usize)
			} else {
				None
			}
		}
	}
}

impl fmt::Display for Uint256 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.val[3] == 0 && self.val[2] == 0 && self.val[1] == 0 {
			write!(f, "{:x}", self.val[0])
		} else {
			write!(f, "{:#016x}{:016x}{:016x}{:016x}", self.val[3], self.val[2], self.val[1], self.val[0])
		}
	}
}
