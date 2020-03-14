use crate::stringtable::StringId;


#[derive(Debug)]
#[derive(Clone)]
pub enum Label {
	Func(StringId),
	Anon(usize)
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

#[derive(Debug)]
pub struct Instruction {
	opcode: Opcode,
	immediate: Option<Value>,
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
}

#[derive(Debug)]
pub enum Value {
	Int(Uint256),
	Tuple(Vec<Value>),
	CodePoint(u64),
	Label(Label),
}

#[derive(Debug)]
pub enum Opcode {
	Noop,
	GetLocal,
	SetLocal,
	MakeFrame(usize, usize),
	Jump(Label),
	Cjump(Label),
	Label(Label),
	TupleGet,
	TupleSet,
	Push(Value),
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

#[derive(Debug)]
pub struct Uint256 {
	val: [u64; 4]
}

impl Uint256 {
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
}
