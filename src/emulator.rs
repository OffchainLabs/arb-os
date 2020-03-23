use std::fmt;
use crate::mavm::{Value, Instruction, Opcode};
use crate::uint256::Uint256;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub struct CompiledProgram {
    pub code: Vec<Instruction>,
    pub static_val: Value,
}

#[derive(Debug)]
pub struct ValueStack {
	contents: Vec<Value>,
}

impl ValueStack {
	pub fn new() -> Self {
		ValueStack{ contents: Vec::new() }
	}

	pub fn make_empty(&mut self) {
		self.contents.clear();
	}

	pub fn push(&mut self, val: Value) {
		self.contents.push(val);
	}

	pub fn push_uint(&mut self, val: Uint256) {
		self.push(Value::Int(val))
	}

	pub fn push_usize(&mut self, val: usize) {
		self.push_uint(Uint256::from_usize(val));
	}

	pub fn push_codepoint(&mut self, val: usize) {
		self.push(Value::CodePoint(val));
	}

	pub fn push_bool(&mut self, val: bool) {
		self.push_uint(if val { Uint256::one() } else { Uint256::zero() })
	}

	pub fn pop(&mut self) -> Result<Value, ExecutionError> {
		match self.contents.pop() {
			Some(v) => Ok(v),
			None => Err(ExecutionError::new("stack underflow"))
		}
	}

	pub fn pop_codepoint(&mut self) -> Result<usize, ExecutionError> {
		if let Value::CodePoint(cp) = self.pop()? {
			Ok(cp)
		} else {
			Err(ExecutionError::new("expected CodePoint on stack"))
		}
	}

	pub fn pop_uint(&mut self) -> Result<Uint256, ExecutionError> {
		if let Value::Int(i) = self.pop()? {
			Ok(i)
		} else {
			Err(ExecutionError::new("expected integer on stack"))
		}
	}

	pub fn pop_usize(&mut self) -> Result<usize, ExecutionError> {
		match self.pop_uint()?.to_usize() {
			Some(u) => Ok(u),
			None => Err(ExecutionError::new("expected small integer on stack"))
		}
	}

	pub fn pop_bool(&mut self) -> Result<bool, ExecutionError> {
		match self.pop_usize() {
			Ok(0) => Ok(false),
			Ok(1) => Ok(true),
			_ => Err(ExecutionError::new("expected bool on stack"))
		}
	}

	pub fn pop_tuple(&mut self) -> Result<Vec<Value>, ExecutionError> {
		if let Value::Tuple(v) = self.pop()? {
			Ok(v)
		} else {
			Err(ExecutionError::new("expected tuple on stack"))
		}
	}
}

impl fmt::Display for ValueStack {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Stack[\n")?;
		for i in 0..self.contents.len() {
			let j = self.contents.len()-1-i;
			write!(f, "{};;\n", self.contents[j])?;
		}
        write!(f, "]")
    }
}

#[derive(Clone, Debug)]
pub struct ExecutionError {
	why: &'static str
}

impl ExecutionError {
	fn new(why: &'static str) -> Self {
		ExecutionError{ why }
	}
}

enum MachineState {
	Stopped,
	Error(ExecutionError),
	Running(usize),  // pc
}

impl MachineState {
	pub fn is_running(&self) -> bool {
		if let MachineState::Running(_) = self {
			true
		} else {
			false
		}
	}
}

pub struct Machine {
	stack: ValueStack,
	aux_stack: ValueStack,
	state: MachineState,
	code: Vec<Instruction>,
	static_val: Value,
}

impl Machine {
	pub fn new(program: CompiledProgram) -> Self {
		Machine{
			stack: ValueStack::new(),
			aux_stack: ValueStack::new(),
			state: MachineState::Stopped,
			code: program.code,
			static_val: program.static_val, 
		}
	}

	pub fn reset(&mut self) {
		self.stack.make_empty();
		self.aux_stack.make_empty();
		self.state = MachineState::Stopped;
	}

	pub fn test_call(&mut self, func_addr: usize, args: Vec<Value>) -> Result<&ValueStack, ExecutionError> {
		let num_args = args.len();
		let stop_pc = self.code.len() + 1;
		for i in 0..num_args {
			self.stack.push(args[num_args-1-i].clone());
		}
		self.stack.push(Value::CodePoint(stop_pc));
		self.state = MachineState::Running(func_addr);
		self.run(Some(stop_pc));
		match &self.state {
			MachineState::Stopped => Err(ExecutionError::new("execution stopped")),
			MachineState::Error(e) => Err(e.clone()),
			MachineState::Running(_) => Ok(&self.stack),
		}
	}

	pub fn get_pc(&self) -> Result<usize, ExecutionError> {
		if let MachineState::Running(pc) = &self.state {
			Ok(*pc)
		} else {
			Err(ExecutionError::new("tried to get PC of non-running machine"))
		}
	}

	pub fn incr_pc(&mut self) {
		if let MachineState::Running(pc) = &self.state {
			self.state = MachineState::Running(1+pc);
		} else {
			panic!("tried to increment PC of non-running machine")
		}
	}

	pub fn run(&mut self, stop_pc: Option<usize>) {
		while self.state.is_running() {
			if let Some(spc) = stop_pc {
				if let MachineState::Running(pc) = self.state {
					if pc == spc {
						return;
					}
				}
			}
			if let Err(e) = self.run_one() {
				self.state = MachineState::Error(e); 
			}
		}
	}

	pub fn run_one(&mut self) -> Result<bool, ExecutionError> {
		if let MachineState::Running(pc) = self.state {
			println!("Running {}", pc);
			if let Some(insn) = self.code.get(pc) {
				if let Some(val) = &insn.immediate {
					self.stack.push(val.clone());
				}
				match insn.opcode {
					Opcode::Noop => {
						self.incr_pc();
						Ok(true)
					}
					Opcode::Panic => Err(ExecutionError::new("panicked")),
					Opcode::Jump => {
						self.state = MachineState::Running(self.stack.pop_codepoint()?);
						Ok(true)
					}
					Opcode::Cjump => {
						let cp = self.stack.pop_codepoint()?;
						let cond = self.stack.pop_bool()?;
						if cond {
							self.state = MachineState::Running(cp);
						} else {
							self.incr_pc();
						}
						Ok(true)
					}
					Opcode::GetPC => {
						self.stack.push_codepoint(self.get_pc()?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::PushStatic => {
						self.stack.push(self.static_val.clone());
						self.incr_pc();
						Ok(true)
					}
					Opcode::Tset => {
						let idx = self.stack.pop_usize()?;
						let tup = self.stack.pop_tuple()?;
						let val = self.stack.pop()?;
						let mut newv = Vec::new();
						for v in tup {
							newv.push(v);
						}
						if idx < newv.len() {
							newv[idx] = val;
							self.stack.push(Value::Tuple(newv));
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("index out of bounds in Tset"))
						}
					}
					Opcode::Tget => {
						let idx = self.stack.pop_usize()?;
						let tup = self.stack.pop_tuple()?;
						if idx < tup.len() {
							self.stack.push(tup[idx].clone());
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("index out of bounds in Tget"))
						}
					}
					Opcode::Pop => {
						let _ = self.stack.pop()?;
						self.incr_pc();
						Ok(true)
					}
					Opcode::AuxPush => {
						self.aux_stack.push(self.stack.pop()?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AuxPop => {
						self.stack.push(self.aux_stack.pop()?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Dup0 => {
						let top = self.stack.pop()?;
						self.stack.push(top.clone());
						self.stack.push(top);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Dup1 => {
						let top = self.stack.pop()?;
						let snd = self.stack.pop()?;
						self.stack.push(snd.clone());
						self.stack.push(top);
						self.stack.push(snd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Swap1 => {
						let top = self.stack.pop()?;
						let snd = self.stack.pop()?;
						self.stack.push(top);
						self.stack.push(snd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Not => {
						let res = if self.stack.pop_bool()? { 0 } else { 1 };
						self.stack.push(Value::Int(Uint256::from_usize(res)));
						self.incr_pc();
						Ok(true)
					}
					Opcode::UnaryMinus => {
						let res = self.stack.pop_uint()?.unary_minus();
						match res {
							Some(x) => {
								self.stack.push_uint(x);
								self.incr_pc();
								Ok(true)
							}
							None => {
								Err(ExecutionError::new("signed integer overflow in unary minus"))
							}
						}
					}
					Opcode::BitwiseNeg => {
						let res = self.stack.pop_uint()?.bitwise_neg();
						self.stack.push_uint(res);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Hash => {
						let res = self.stack.pop()?.avm_hash();
						self.stack.push(res);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Len => {
						let res = self.stack.pop_tuple()?;
						self.stack.push_uint(Uint256::from_usize(res.len()));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Plus => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_uint(r1.add(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Minus => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_uint(r1.sub(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Mul => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_uint(r1.mul(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Div => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						let ores = r1.div(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("divide by zero"))
						}
					}
					Opcode::Mod => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						let ores = r1.modulo(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("modulo by zero"))
						}
					}
					Opcode::Sdiv => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						let ores = r1.sdiv(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("divide by zero"))
						}
					}
					Opcode::Smod => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						let ores = r1.smodulo(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("modulo by zero"))
						}
					}
					Opcode::LessThan => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_usize(if r1 < r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::GreaterThan => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_usize(if r1 > r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::SLessThan => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_usize(if r1.s_less_than(&r2) { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::SGreaterThan => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_usize(if r2.s_less_than(&r1) { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::Equal => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_usize(if r1 == r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::NotEqual => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_usize(if r1 != r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::BitwiseAnd => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_uint(r1.bitwise_and(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::BitwiseOr => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_uint(r1.bitwise_or(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::BitwiseXor => {
						let r1 = self.stack.pop_uint()?;
						let r2 = self.stack.pop_uint()?;
						self.stack.push_uint(r1.bitwise_xor(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::LogicalAnd => {
						let r1 = self.stack.pop_bool()?;
						let r2 = self.stack.pop_bool()?;
						self.stack.push_bool(r1 && r2);
						self.incr_pc();
						Ok(true)
					}
					Opcode::LogicalOr => {
						let r1 = self.stack.pop_bool()?;
						let r2 = self.stack.pop_bool()?;
						self.stack.push_bool(r1 || r2);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Hash2 => {
						let r1 = self.stack.pop()?;
						let r2 = self.stack.pop()?;
						self.stack.push(Value::avm_hash2(&r1, &r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::GetLocal |
					Opcode::SetLocal |
					Opcode::MakeFrame(_, _) |
					Opcode::Label(_) |
					Opcode::StaticGet |
					Opcode::TupleGet(_) |
					Opcode::TupleSet(_) |
					Opcode::ArrayGet |
					Opcode::Return => Err(ExecutionError::new("invalid opcode"))
				}
			} else {
				Err(ExecutionError::new("invalid program counter"))
			}
		} else {
			Err(ExecutionError::new("tried to run machine that is not runnable"))
		}
	}
}