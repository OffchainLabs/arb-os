use std::fmt;
use std::collections::HashMap;
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use crate::pos::Location;
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
			Label::Func(sid) => (Label::Func(sid+func_offset), sid+func_offset),
			Label::Anon(pc) => (Label::Anon(pc+int_offset), func_offset),
			Label::External(slot) => (Label::External(slot+ext_offset), func_offset),
		}
	}

	pub fn avm_hash(&self) -> Value {
		match self {
			Label::Func(sid) => Value::avm_hash2(&Value::Int(Uint256::from_usize(4)), &Value::Int(Uint256::from_usize(*sid))),
			Label::Anon(n) => Value::avm_hash2(&Value::Int(Uint256::from_usize(5)), &Value::Int(Uint256::from_usize(*n))),
			Label::External(n) => Value::avm_hash2(&Value::Int(Uint256::from_usize(6)), &Value::Int(Uint256::from_usize(*n))),
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
	pub location: Option<Location>,
}

impl Instruction {
	pub fn new(opcode: Opcode, immediate: Option<Value>, location: Option<Location>) -> Self {
		Instruction{ opcode, immediate, location }
	}

	pub fn from_opcode(opcode: Opcode, location: Option<Location>) -> Self {
		Instruction::new(opcode, None, location)
	}

	pub fn from_opcode_imm(opcode: Opcode, immediate: Value, location: Option<Location>) -> Self {
		Instruction::new(opcode, Some(immediate), location)
	}

	pub fn get_label(&self) -> Option<&Label> {
		match &self.opcode {
			Opcode::Label(label) => Some(label),
			_ => None
		}
	}

	pub fn replace_labels(self, label_map: &HashMap<Label, CodePt>) -> Result<Self, Label> {
		match self.immediate {
			Some(val) => Ok(Instruction::from_opcode_imm(self.opcode, val.replace_labels(label_map)?, self.location)),
			None => Ok(self),
		}
	}

	pub fn relocate(self, int_offset: usize, ext_offset: usize, func_offset: usize, globals_offset: usize) -> (Self, usize) {
		let mut max_func_offset = func_offset;
		let opcode = match self.opcode {
			Opcode::PushExternal(off) => Opcode::PushExternal(off+ext_offset),
			Opcode::Label(label) => {
				let (new_label, new_func_offset) = label.relocate(int_offset, ext_offset, func_offset);
				if max_func_offset < new_func_offset {
					max_func_offset = new_func_offset;
				}
				Opcode::Label(new_label)
			}
			Opcode::GetGlobalVar(idx) => Opcode::GetGlobalVar(idx + globals_offset),
			Opcode::SetGlobalVar(idx) => Opcode::SetGlobalVar(idx + globals_offset),
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
		(Instruction::new(opcode, imm, self.location), max_func_offset)
	}

	pub fn xlate_labels(self, xlate_map: &HashMap<Label, &Label>) -> Self {
		match self.immediate {
			Some(val) => Instruction::from_opcode_imm(self.opcode, val.xlate_labels(xlate_map), self.location),
			None => self,
		}
	}
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match &self.immediate {
    		Some(v) => match self.location {
				Some(loc) => write!(f, "[{}] {}\t\t{}", v, self.opcode, loc),
				None => write!(f, "[{}] {}\t\t[no location]", v, self.opcode),
			}
    		None => match self.location {
				Some(loc) => write!(f, "{}\t\t{}", self.opcode, loc),
				None => write!(f, "{}", self.opcode),
			}
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

	pub fn avm_hash(&self) -> Value {
		match self {
			CodePt::Internal(sz) => Value::avm_hash2(&Value::Int(Uint256::from_usize(3)), &Value::Int(Uint256::from_usize(*sz))),
			CodePt::External(sz) => Value::avm_hash2(&Value::Int(Uint256::from_usize(4)), &Value::Int(Uint256::from_usize(*sz))),
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

	pub fn replace_labels(self, label_map: &HashMap<Label, CodePt>) -> Result<Self, Label> {
		match self {
			Value::Int(_) => Ok(self),
			Value::CodePoint(_) => Ok(self),
			Value::Label(label) => {
				let maybe_pc = label_map.get(&label);
				match maybe_pc {
					Some(pc) => Ok(Value::CodePoint(*pc)),
					None => Err(label),
				}
			},
			Value::Tuple(tup) => {
				let mut new_vec = Vec::new();
				for v in tup.iter() {
					let val = v.clone();
					new_vec.push(val.replace_labels(label_map)?);
				}
				Ok(Value::Tuple(new_vec))
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

	pub fn avm_hash(&self) -> Value {  //BUGBUG: should do same hash as AVM
		match self {
			Value::Int(ui) => Value::Int(ui.avm_hash()),
			Value::Tuple(v) => {
				let mut acc = Uint256::zero();
				for val in v {
					let vhash = val.avm_hash();
					if let Value::Int(ui) = vhash {
						acc = Uint256::avm_hash2(&acc, &ui);
					} else {
						panic!("avm_hash returned wrong datatype")
					}
				}
				Value::Int(acc)
			}
			Value::CodePoint(cp) => Value::avm_hash2(&Value::Int(Uint256::one()), &cp.avm_hash()),
			Value::Label(label) => Value::avm_hash2(&Value::Int(Uint256::from_usize(2)), &label.avm_hash()),
		}
	}

	pub fn avm_hash2(v1: &Self, v2: &Self) -> Value {
		Value::Tuple(vec![v1.clone(), v2.clone()]).avm_hash()
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
	Rget,
	Rset,
	PushStatic,
	PushExternal(usize),  // push codeptr of external function -- index in imported_funcs
	TupleGet(usize),  // arg is size of anysize_tuple
	TupleSet(usize),  // arg is size of anysize_tuple
	ArrayGet, 
	UncheckedFixedArrayGet(usize),  // arg is size of array  
	GetGlobalVar(usize),
	SetGlobalVar(usize),   
	Tset,
	Tget,
	Pop,
	AuxPush,
	AuxPop,
	Xget,
	Xset,
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
	Hash2,
	Len,
	Plus,
	Minus,
	Mul,
	Div,
	Mod,
	Sdiv,
	Smod,
	Exp,
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
	DebugPrint,
}

impl Opcode {
	pub fn from_name(name: &str) -> Self {
		match name {
			"rget" => Opcode::Rget,
			"rset" => Opcode::Rset,
			"pushstatic" => Opcode::PushStatic,
			"tset" => Opcode::Tset,
			"tget" => Opcode::Tget,
			"pop" => Opcode::Pop,
			"auxpush" => Opcode::AuxPush,
			"auxpop" => Opcode::AuxPop,
			"xget" => Opcode::Xget,
			"xset" => Opcode::Xset,
			"dup0" => Opcode::Dup0,
			"dup1" => Opcode::Dup1,
			"dup2" => Opcode::Dup2,
			"swap1" => Opcode::Swap1,
			"swap2" => Opcode::Swap2,
			"unaryminus" => Opcode::UnaryMinus,
			"bitwiseneg" => Opcode::BitwiseNeg,
			"hash" => Opcode::Hash,
			"hash2" => Opcode::Hash2,
			"len" => Opcode::Len,
			"plus" => Opcode::Plus,
			"minus" => Opcode::Minus,
			"mul" => Opcode::Mul,
			"div" => Opcode::Div,
			"mod" => Opcode::Mod,
			"sdiv" => Opcode::Sdiv,
			"smod" => Opcode::Smod,
			"exp" => Opcode::Exp,
			"lt" => Opcode::LessThan,
			"gt" => Opcode::GreaterThan,
			"slt" => Opcode::SLessThan,
			"sgt" => Opcode::SGreaterThan,
			"eq" => Opcode::Equal,
			"neq" => Opcode::NotEqual,
			"bitwiseand" => Opcode::BitwiseAnd,
			"bitwiseor" => Opcode::BitwiseOr,
			"bitwisexor" => Opcode::BitwiseXor,
			"logicaland" => Opcode::LogicalAnd,
			"logicalor" => Opcode::LogicalOr,
			_ => { panic!("opcode not supported in asm segment: {}", name); }
		}
	}
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
