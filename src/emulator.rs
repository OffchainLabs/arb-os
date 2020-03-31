use std::fmt;
use crate::mavm::{Value, Instruction, Opcode, CodePt};
use crate::uint256::Uint256;
use crate::linker::LinkedProgram;


#[derive(Debug, Clone)]
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

	pub fn push_codepoint(&mut self, val: CodePt) {
		self.push(Value::CodePoint(val));
	}

	pub fn push_bool(&mut self, val: bool) {
		self.push_uint(if val { Uint256::one() } else { Uint256::zero() })
	}

	pub fn pop(&mut self, state: &MachineState) -> Result<Value, ExecutionError> {
		match self.contents.pop() {
			Some(v) => Ok(v),
			None => Err(ExecutionError::new("stack underflow", state))
		}
	}

	pub fn pop_codepoint(&mut self, state: &MachineState) -> Result<CodePt, ExecutionError> {
		if let Value::CodePoint(cp) = self.pop(state)? {
			Ok(cp)
		} else {
			Err(ExecutionError::new("expected CodePoint on stack", state))
		}
	}

	pub fn pop_uint(&mut self, state: &MachineState) -> Result<Uint256, ExecutionError> {
		if let Value::Int(i) = self.pop(state)? {
			Ok(i)
		} else {
			Err(ExecutionError::new("expected integer on stack", state))
		}
	}

	pub fn pop_usize(&mut self, state: &MachineState) -> Result<usize, ExecutionError> {
		match self.pop_uint(state)?.to_usize() {
			Some(u) => Ok(u),
			None => Err(ExecutionError::new("expected small integer on stack", state))
		}
	}

	pub fn pop_bool(&mut self, state: &MachineState) -> Result<bool, ExecutionError> {
		match self.pop_usize(state) {
			Ok(0) => Ok(false),
			Ok(1) => Ok(true),
			_ => Err(ExecutionError::new("expected bool on stack", state))
		}
	}

	pub fn pop_tuple(&mut self, state: &MachineState) -> Result<Vec<Value>, ExecutionError> {
		if let Value::Tuple(v) = self.pop(state)? {
			Ok(v)
		} else {
			Err(ExecutionError::new("expected tuple on stack", state))
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
pub enum ExecutionError {
	StoppedErr(&'static str),
	Wrapped(&'static str, Box<ExecutionError>),
	RunningErr(&'static str, CodePt),
}

impl ExecutionError {
	fn new(why: &'static str, state: &MachineState) -> Self {
		match state {
			MachineState::Stopped => ExecutionError::StoppedErr(why),
			MachineState::Error(e) => ExecutionError::Wrapped(why, Box::new(e.clone())),
			MachineState::Running(cp) => ExecutionError::RunningErr(why, *cp),
		}
	}
}

#[derive(Clone)]
pub enum MachineState {
	Stopped,
	Error(ExecutionError),
	Running(CodePt),  // pc
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
	pub fn new(program: LinkedProgram) -> Self {
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

	pub fn get_state(&self) -> MachineState {
		self.state.clone()
	}

	pub fn pop_stack(&mut self) -> Result<Value, ExecutionError> {
		self.stack.pop(&self.state)
	}

	pub fn test_call(&mut self, func_addr: CodePt, args: Vec<Value>) -> Result<ValueStack, ExecutionError> {
		let num_args = args.len();
		let stop_pc = CodePt::new_internal(self.code.len() + 1);
		for i in 0..num_args {
			self.stack.push(args[num_args-1-i].clone());
		}
		self.stack.push(Value::CodePoint(stop_pc));
		self.state = MachineState::Running(func_addr);
		self.run(Some(stop_pc));
		match &self.state {
			MachineState::Stopped => Err(ExecutionError::new("execution stopped", &self.state)),
			MachineState::Error(e) => Err(e.clone()),
			MachineState::Running(_) => Ok(self.stack.clone()),
		}
	}

	pub fn get_pc(&self) -> Result<CodePt, ExecutionError> {
		if let MachineState::Running(pc) = &self.state {
			Ok(*pc)
		} else {
			Err(ExecutionError::new("tried to get PC of non-running machine", &self.state))
		}
	}

	pub fn incr_pc(&mut self) {
		if let MachineState::Running(pc) = &self.state {
			if let Some(new_pc) = pc.incr() {
				self.state = MachineState::Running(new_pc);
			} else {
				panic!("machine PC was set of external CodePt")
			}
		} else {
			panic!("tried to increment PC of non-running machine")
		}
	}

	pub fn run(&mut self, stop_pc: Option<CodePt>) {
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
			if let Some(insn) = self.code.get(pc.pc_if_internal().unwrap()) {
				if let Some(val) = &insn.immediate {
					self.stack.push(val.clone());
				}
				match insn.opcode {
					Opcode::Noop => {
						self.incr_pc();
						Ok(true)
					}
					Opcode::Panic => Err(ExecutionError::new("panicked", &self.state)),
					Opcode::Jump => {
						self.state = MachineState::Running(self.stack.pop_codepoint(&self.state)?);
						Ok(true)
					}
					Opcode::Cjump => {
						let cp = self.stack.pop_codepoint(&self.state)?;
						let cond = self.stack.pop_bool(&self.state)?;
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
						let idx = self.stack.pop_usize(&self.state)?;
						let tup = self.stack.pop_tuple(&self.state)?;
						let val = self.stack.pop(&self.state)?;
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
							Err(ExecutionError::new("index out of bounds in Tset", &self.state))
						}
					}
					Opcode::Tget => {
						let idx = self.stack.pop_usize(&self.state)?;
						let tup = self.stack.pop_tuple(&self.state)?;
						if idx < tup.len() {
							self.stack.push(tup[idx].clone());
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("index out of bounds in Tget", &self.state))
						}
					}
					Opcode::Pop => {
						let _ = self.stack.pop(&self.state)?;
						self.incr_pc();
						Ok(true)
					}
					Opcode::AuxPush => {
						self.aux_stack.push(self.stack.pop(&self.state)?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AuxPop => {
						self.stack.push(self.aux_stack.pop(&self.state)?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Dup0 => {
						let top = self.stack.pop(&self.state)?;
						self.stack.push(top.clone());
						self.stack.push(top);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Dup1 => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						self.stack.push(snd.clone());
						self.stack.push(top);
						self.stack.push(snd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Swap1 => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						self.stack.push(top);
						self.stack.push(snd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Not => {
						let res = if self.stack.pop_bool(&self.state)? { 0 } else { 1 };
						self.stack.push(Value::Int(Uint256::from_usize(res)));
						self.incr_pc();
						Ok(true)
					}
					Opcode::UnaryMinus => {
						let res = self.stack.pop_uint(&self.state)?.unary_minus();
						match res {
							Some(x) => {
								self.stack.push_uint(x);
								self.incr_pc();
								Ok(true)
							}
							None => {
								Err(ExecutionError::new("signed integer overflow in unary minus", &self.state))
							}
						}
					}
					Opcode::BitwiseNeg => {
						let res = self.stack.pop_uint(&self.state)?.bitwise_neg();
						self.stack.push_uint(res);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Hash => {
						let res = self.stack.pop(&self.state)?.avm_hash();
						self.stack.push(res);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Len => {
						let res = self.stack.pop_tuple(&self.state)?;
						self.stack.push_uint(Uint256::from_usize(res.len()));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Plus => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.add(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Minus => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.sub(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Mul => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.mul(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Div => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						let ores = r1.div(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("divide by zero", &self.state))
						}
					}
					Opcode::Mod => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						let ores = r1.modulo(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("modulo by zero", &self.state))
						}
					}
					Opcode::Sdiv => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						let ores = r1.sdiv(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("divide by zero", &self.state))
						}
					}
					Opcode::Smod => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						let ores = r1.smodulo(&r2);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("modulo by zero", &self.state))
						}
					}
					Opcode::LessThan => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1 < r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::GreaterThan => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1 > r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::SLessThan => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1.s_less_than(&r2) { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::SGreaterThan => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r2.s_less_than(&r1) { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::Equal => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1 == r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::NotEqual => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1 != r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::BitwiseAnd => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.bitwise_and(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::BitwiseOr => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.bitwise_or(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::BitwiseXor => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.bitwise_xor(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::LogicalAnd => {
						let r1 = self.stack.pop_bool(&self.state)?;
						let r2 = self.stack.pop_bool(&self.state)?;
						self.stack.push_bool(r1 && r2);
						self.incr_pc();
						Ok(true)
					}
					Opcode::LogicalOr => {
						let r1 = self.stack.pop_bool(&self.state)?;
						let r2 = self.stack.pop_bool(&self.state)?;
						self.stack.push_bool(r1 || r2);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Hash2 => {
						let r1 = self.stack.pop(&self.state)?;
						let r2 = self.stack.pop(&self.state)?;
						self.stack.push(Value::avm_hash2(&r1, &r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::GetLocal |  // these opcodes are for intermediate use in compilation only
					Opcode::SetLocal |  // they should never appear in fully compiled code
					Opcode::MakeFrame(_, _) |
					Opcode::Label(_) |
					Opcode::PushExternal(_) |
					Opcode::TupleGet(_) |
					Opcode::TupleSet(_) |
					Opcode::ArrayGet |
					Opcode::Return => Err(ExecutionError::new("invalid opcode", &self.state))
				}
			} else {
				Err(ExecutionError::new("invalid program counter", &self.state))
			}
		} else {
			Err(ExecutionError::new("tried to run machine that is not runnable", &self.state))
		}
	}
}