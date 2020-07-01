/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use super::runtime_env::RuntimeEnvironment;
use crate::link::LinkedProgram;
use crate::mavm::{CodePt, Instruction, Opcode, Value};
use crate::pos::Location;
use crate::uint256::Uint256;
use std::cmp::max;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::io::stdin;

#[derive(Debug, Default, Clone)]
pub struct ValueStack {
    contents: Vec<Value>,
}

impl ValueStack {
    pub fn new() -> Self {
        ValueStack {
            contents: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.contents.len() == 0
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

    pub fn top(&self) -> Option<Value> {
        if self.is_empty() {
            None
        } else {
            Some(self.contents[self.contents.len() - 1].clone())
        }
    }

    pub fn pop(&mut self, state: &MachineState) -> Result<Value, ExecutionError> {
        match self.contents.pop() {
            Some(v) => Ok(v),
            None => Err(ExecutionError::new("stack underflow", state, None)),
        }
    }

    pub fn pop_codepoint(&mut self, state: &MachineState) -> Result<CodePt, ExecutionError> {
        let val = self.pop(state)?;
        if let Value::CodePoint(cp) = val {
            Ok(cp)
        } else {
            Err(ExecutionError::new(
                "expected CodePoint on stack",
                state,
                Some(val),
            ))
        }
    }

    pub fn pop_uint(&mut self, state: &MachineState) -> Result<Uint256, ExecutionError> {
        let val = self.pop(state)?;
        if let Value::Int(i) = val {
            Ok(i)
        } else {
            Err(ExecutionError::new(
                "expected integer on stack",
                state,
                Some(val),
            ))
        }
    }

    pub fn pop_usize(&mut self, state: &MachineState) -> Result<usize, ExecutionError> {
        let val = self.pop_uint(state)?;
        match val.to_usize() {
            Some(u) => Ok(u),
            None => Err(ExecutionError::new(
                "expected small integer on stack",
                state,
                Some(Value::Int(val)),
            )),
        }
    }

    pub fn pop_bool(&mut self, state: &MachineState) -> Result<bool, ExecutionError> {
        let val = self.pop_usize(state);
        match val {
            Ok(0) => Ok(false),
            Ok(1) => Ok(true),
            Ok(v) => Err(ExecutionError::new(
                "expected bool on stack",
                state,
                Some(Value::Int(Uint256::from_usize(v))),
            )),
            _ => Err(ExecutionError::new("expected bool on stack", state, None)),
        }
    }

    pub fn pop_tuple(&mut self, state: &MachineState) -> Result<Vec<Value>, ExecutionError> {
        let val = self.pop(state)?;
        if let Value::Tuple(v) = val {
            Ok(v)
        } else {
            Err(ExecutionError::new(
                "expected tuple on stack",
                state,
                Some(val),
            ))
        }
    }

    pub fn all_codepts(&self) -> Vec<CodePt> {
        self.contents
            .iter()
            .filter_map(|item| {
                if let Value::CodePoint(cp) = item {
                    Some(*cp)
                } else {
                    None
                }
            })
            .collect()
    }
}

impl fmt::Display for ValueStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Stack[")?;
        for i in self.contents.iter().rev() {
            writeln!(f, "{};;", i)?;
        }
        write!(f, "]")
    }
}

#[derive(Clone, Debug)]
pub enum ExecutionError {
    StoppedErr(&'static str),
    Wrapped(&'static str, Box<ExecutionError>),
    RunningErr(&'static str, CodePt, Option<Value>),
}

impl ExecutionError {
    fn new(why: &'static str, state: &MachineState, val: Option<Value>) -> Self {
        match state {
            MachineState::Stopped => ExecutionError::StoppedErr(why),
            MachineState::Error(e) => ExecutionError::Wrapped(why, Box::new(e.clone())),
            MachineState::Running(cp) => ExecutionError::RunningErr(why, *cp, val),
        }
    }
}

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecutionError::StoppedErr(s) => writeln!(f, "error with machine stopped: {}", s),
            ExecutionError::Wrapped(s, bee) => writeln!(f, "{} ({})", s, *bee),
            ExecutionError::RunningErr(s, cp, ov) => match ov {
                Some(val) => writeln!(f, "{} ({:?}) with value {}", s, cp, val),
                None => writeln!(f, "{} ({:?})", s, cp),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum MachineState {
    Stopped,
    Error(ExecutionError),
    Running(CodePt), // pc
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

#[derive(Debug)]
struct CodeStore {
    segments: Vec<Vec<Instruction>>,
}

impl CodeStore {
    fn new(runtime: Vec<Instruction>) -> Self {
        CodeStore {
            segments: vec![runtime],
        }
    }

    #[allow(dead_code)]
    fn segment_size(&self, seg_num: usize) -> Option<usize> {
        match self.segments.get(seg_num) {
            Some(seg) => Some(seg.len()),
            None => None,
        }
    }

    fn runtime_segment_size(&self) -> usize {
        self.segments[0].len()
    }

    fn get_insn(&self, codept: CodePt) -> Option<&Instruction> {
        match codept {
            CodePt::Internal(pc) => self.segments[0].get(pc),
            CodePt::InSegment(seg_num, pc) => {
                if seg_num < self.segments.len() {
                    self.segments[seg_num].get(pc)
                } else {
                    None
                }
            }
            _ => {
                panic!("unlinked codepoint reference in running code: {:?}", codept);
            }
        }
    }

    fn create_segment(&mut self) -> CodePt {
        self.segments
            .push(vec![Instruction::from_opcode(Opcode::Panic, None)]);
        CodePt::new_in_segment(self.segments.len() - 1, 0)
    }

    fn push_insn(&mut self, op: usize, imm: Option<Value>, codept: CodePt) -> Option<CodePt> {
        if let CodePt::InSegment(seg_num, old_offset) = codept {
            if seg_num >= self.segments.len() {
                panic!("bad segment number in push_insn");
            //None
            } else {
                let segment = &mut self.segments[seg_num];
                if old_offset == segment.len() - 1 {
                    if let Some(opcode) = Opcode::from_number(op) {
                        segment.push(Instruction::new(opcode, imm, None));
                        Some(CodePt::new_in_segment(seg_num, old_offset + 1))
                    } else {
                        panic!(
                            "bad opcode number {} in push_insn at length {}",
                            op,
                            segment.len()
                        );
                        //None
                    }
                } else {
                    panic!("branching segments not yet implemented");
                }
            }
        } else {
            panic!("invalid codepoint in push_insn: {:?}", codept);
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ProfilerData {
    data: HashMap<String, BTreeMap<(usize, usize), u64>>,
    unknown_gas: u64,
}

impl ProfilerData {
    fn get_mut(
        &mut self,
        loc: &Option<Location>,
        chart: &HashMap<u64, String>,
    ) -> Option<&mut u64> {
        if let Some(loc) = loc {
            let filename = chart.get(&loc.file_id)?;
            self.data
                .get_mut(filename)?
                .get_mut(&(loc.line.to_usize(), loc.column.to_usize()))
        } else {
            Some(&mut self.unknown_gas)
        }
    }
    fn insert(
        &mut self,
        loc: &Option<Location>,
        gas: u64,
        chart: &HashMap<u64, String>,
    ) -> Option<u64> {
        if let Some(loc) = loc {
            let filename = match chart.get(&loc.file_id) {
                Some(name) => name,
                None => {
                    let old_unknown_gas = self.unknown_gas;
                    self.unknown_gas = gas;
                    return Some(old_unknown_gas);
                }
            };
            let btree = match self.data.get_mut(filename) {
                Some(tree) => tree,
                None => {
                    self.data.insert(filename.clone(), BTreeMap::new());
                    self.data.get_mut(filename)?
                }
            };
            btree.insert((loc.line.to_usize(), loc.column.to_usize()), gas)
        } else {
            let old_unknown_gas = self.unknown_gas;
            self.unknown_gas = gas;
            Some(old_unknown_gas)
        }
    }
    pub fn profiler_session(&self) {
        let file_gas_costs: Vec<(String, u64)> = self
            .data
            .iter()
            .map(|(name, tree)| (name.clone(), tree.values().sum()))
            .collect();
        println!("Per file gas cost usage:");
        for (filename, gas_cost) in &file_gas_costs {
            println!("{}: {};", filename, gas_cost);
        }
        println!("unknown_file: {}", self.unknown_gas);
        loop {
            println!("Enter file to examine");
            let mut command = String::new();
            if let Ok(_) = stdin().read_line(&mut command) {
                let trimmed_command = command.trim_end();
                match trimmed_command {
                    "exit" => return,
                    _ => match self.data.get(trimmed_command) {
                        Some(tree) => loop {
                            command.clear();
                            let res = stdin().read_line(&mut command);
                            if res.is_err() {
                                println!("Error reading line");
                                continue;
                            }
                            if command.trim_end() == "change" {
                                break;
                            }
                            let start_row = match command.trim_end().parse::<usize>() {
                                Ok(u) => u,
                                Err(_) => {
                                    println!("Invalid line number");
                                    continue;
                                }
                            };
                            command.clear();
                            let res = stdin().read_line(&mut command);
                            if res.is_err() {
                                println!("Error reading line");
                                continue;
                            }
                            let end_row = match command.trim_end().parse::<usize>() {
                                Ok(u) => u,
                                Err(_) => {
                                    println!("Invalid line number");
                                    continue;
                                }
                            };
                            if start_row > end_row {
                                println!("Invalid range");
                                continue;
                            }
                            let area_cost: u64 = tree
                                .range((max(start_row, 1) - 1, 0)..(end_row, 0))
                                .map(|(_, val)| *val)
                                .sum();
                            println!("ArbGas cost of region: {}", area_cost);
                        },
                        None => {
                            println!("Could not find file");
                        }
                    },
                }
            } else {
                println!("Error reading line, aborting");
                return;
            }
        }
    }
}

#[derive(Debug)]
pub struct Machine {
    stack: ValueStack,
    aux_stack: ValueStack,
    state: MachineState,
    code: CodeStore,
    static_val: Value,
    register: Value,
    err_codepoint: CodePt,
    arb_gas_remaining: Uint256,
    pub runtime_env: RuntimeEnvironment,
    file_name_chart: HashMap<u64, String>,
}

impl Machine {
    pub fn new(program: LinkedProgram, env: RuntimeEnvironment) -> Self {
        Machine {
            stack: ValueStack::new(),
            aux_stack: ValueStack::new(),
            state: MachineState::Stopped,
            code: CodeStore::new(program.code),
            static_val: program.static_val,
            register: Value::none(),
            err_codepoint: CodePt::Null,
            arb_gas_remaining: Uint256::zero().bitwise_neg(),
            runtime_env: env,
            file_name_chart: program.file_name_chart,
        }
    }

    pub fn get_stack_trace(&self) -> StackTrace {
        StackTrace::Known(self.aux_stack.all_codepts())
    }

    pub fn call_state(&mut self, func_addr: CodePt, args: Vec<Value>) -> CodePt {
        let stop_pc = CodePt::new_internal(self.code.runtime_segment_size());
        for i in args.into_iter().rev() {
            self.stack.push(i);
        }
        self.stack.push(Value::CodePoint(stop_pc));
        self.state = MachineState::Running(func_addr);
        stop_pc
    }

    pub fn test_call(
        &mut self,
        func_addr: CodePt,
        args: Vec<Value>,
        debug: bool,
    ) -> Result<ValueStack, ExecutionError> {
        let stop_pc = self.call_state(func_addr, args);
        let cost = if debug {
            self.debug(Some(stop_pc))
        } else {
            self.run(Some(stop_pc))
        };
        println!("ArbGas cost of call: {}", cost);
        match &self.state {
            MachineState::Stopped => {
                Err(ExecutionError::new("execution stopped", &self.state, None))
            }
            MachineState::Error(e) => Err(e.clone()),
            MachineState::Running(_) => Ok(self.stack.clone()),
        }
    }

    pub fn get_pc(&self) -> Result<CodePt, ExecutionError> {
        if let MachineState::Running(pc) = &self.state {
            Ok(*pc)
        } else {
            Err(ExecutionError::new(
                "tried to get PC of non-running machine",
                &self.state,
                None,
            ))
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

    pub fn next_opcode(&self) -> Option<Instruction> {
        if let MachineState::Running(pc) = self.state {
            if let Some(insn) = self.code.get_insn(pc) {
                Some(insn.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn debug(&mut self, stop_pc: Option<CodePt>) -> u64 {
        println!("Blank line or \"step\" to run one opcode, \"set break\" followed by a \
         line number to resume program until that line, \"show static\" to show the static contents.");
        let mut breakpoint = true;
        let mut break_line = 0;
        let mut gas_cost = 0;
        while self.state.is_running() {
            if let Some(gas) = self.next_op_gas() {
                gas_cost += gas;
            } else {
                println!("Warning: next opcode does not have a gas cost")
            }
            if !breakpoint {
                if let Some(insn) = self.next_opcode() {
                    if let Some(location) = insn.location {
                        if location.line() == break_line {
                            breakpoint = true;
                        }
                    }
                    if insn.opcode == Opcode::DebugPrint {
                        breakpoint = true;
                    }
                }
            }
            if breakpoint {
                if let Ok(pc) = self.get_pc() {
                    println!("PC: {:?}", pc);
                }
                println!("Stack contents: {}", self.stack);
                println!("Aux-stack contents: {}", self.aux_stack);
                println!("Register contents: {}", self.register);
                if !self.stack.is_empty() {
                    println!("Stack top: {}", self.stack.top().unwrap());
                }
                if let Some(code) = self.next_opcode() {
                    println!("Next Opcode: {}", code.opcode);
                    if let Some(imm) = code.immediate {
                        println!("Immediate: {}", imm);
                    }
                    if let Some(location) = code.location {
                        let line = location.line.to_usize();
                        let column = location.column.to_usize();
                        if let Some(filename) = self.file_name_chart.get(&location.file_id) {
                            println!(
                                "Origin: (Line: {}, Column: {}, File: {})",
                                line, column, filename
                            );
                        } else {
                            println!(
                                "Origin: (Line: {}, Column: {}, Unknown File ID: {})",
                                line, column, location.file_id
                            );
                        }
                    }
                }
                println!();
                let mut exit = false;
                loop {
                    let mut debugger_state = String::new();
                    std::io::stdin().read_line(&mut debugger_state).unwrap();
                    match debugger_state.as_str() {
                        "\n" | "step\n" => exit = true,
                        "set break\n" => {
                            breakpoint = false;
                            loop {
                                let mut debugger_state = String::new();
                                std::io::stdin().read_line(&mut debugger_state).unwrap();
                                if let Ok(val) = str::parse(&debugger_state.trim()) {
                                    break_line = val;
                                    exit = true;
                                    break;
                                } else {
                                    println!("Could not parse input as number");
                                }
                            }
                        }
                        "show static\n" => println!("Static contents: {}", self.static_val),
                        "r\n" | "run\n" => {
                            breakpoint = false;
                            exit = true;
                        }
                        _ => println!("invalid input"),
                    }
                    if exit {
                        break;
                    }
                }
            }
            if let Some(spc) = stop_pc {
                if let MachineState::Running(pc) = self.state {
                    if pc == spc {
                        return gas_cost;
                    }
                }
            }
            match self.run_one(false) {
                Ok(false) => {
                    return gas_cost;
                }
                Err(e) => {
                    self.state = MachineState::Error(e);
                    return gas_cost;
                }
                _ => {}
            }
        }
        gas_cost
    }

    pub fn run(&mut self, stop_pc: Option<CodePt>) -> u64 {
        let mut gas_used = 0;
        while self.state.is_running() {
            if let Some(spc) = stop_pc {
                if let MachineState::Running(pc) = self.state {
                    if pc == spc {
                        return gas_used;
                    }
                }
            }
            if let Some(gas) = self.next_op_gas() {
                gas_used += gas;
            } else {
                println!("Warning: next opcode does not have a gas cost");
            }
            match self.run_one(false) {
                Ok(still_runnable) => {
                    if !still_runnable {
                        return gas_used;
                    }
                }
                Err(e) => {
                    self.state = MachineState::Error(e);
                    return gas_used;
                }
            }
        }
        gas_used
    }

    pub fn profile_gen(&mut self, args: Vec<Value>) -> ProfilerData {
        self.call_state(CodePt::new_internal(0), args);
        let mut loc_map = ProfilerData::default();
        while let Some(insn) = self.next_opcode() {
            let loc = insn.location;
            if let Some(gas_cost) = loc_map.get_mut(&loc, &self.file_name_chart) {
                *gas_cost += self.next_op_gas().unwrap_or(0);
            } else {
                loc_map.insert(&loc, self.next_op_gas().unwrap_or(0), &self.file_name_chart);
            }
            match self.run_one(false) {
                Ok(false) => {
                    return loc_map;
                }
                Err(e) => {
                    self.state = MachineState::Error(e);
                    return loc_map;
                }
                _ => {}
            }
        }
        loc_map
    }

    pub(crate) fn next_op_gas(&self) -> Option<u64> {
        if let MachineState::Running(pc) = self.state {
            Some(match self.code.get_insn(pc)?.opcode {
                Opcode::Plus => 3,
                Opcode::Mul => 3,
                Opcode::Minus => 3,
                Opcode::Div => 4,
                Opcode::Sdiv => 7,
                Opcode::Mod => 4,
                Opcode::Smod => 7,
                Opcode::AddMod => 4,
                Opcode::MulMod => 4,
                Opcode::Exp => 25,
                Opcode::LessThan => 2,
                Opcode::GreaterThan => 2,
                Opcode::SLessThan => 2,
                Opcode::SGreaterThan => 2,
                Opcode::Equal => 2,
                Opcode::IsZero => 1,
                Opcode::BitwiseAnd => 2,
                Opcode::BitwiseOr => 2,
                Opcode::BitwiseXor => 2,
                Opcode::BitwiseNeg => 1,
                Opcode::Byte => 4,
                Opcode::SignExtend => 7,
                Opcode::NotEqual => 3, // This opcode should be phased out
                Opcode::Hash => 7,
                Opcode::Type => 3,
                Opcode::Hash2 => 8,
                Opcode::Pop => 1,
                Opcode::PushStatic => 1,
                Opcode::Rget => 1,
                Opcode::Rset => 2,
                Opcode::Jump => 4,
                Opcode::Cjump => 4,
                Opcode::StackEmpty => 2,
                Opcode::GetPC => 1,
                Opcode::AuxPush => 1,
                Opcode::AuxPop => 1,
                Opcode::AuxStackEmpty => 2,
                Opcode::Noop => 1,
                Opcode::ErrPush => 1,
                Opcode::ErrSet => 1,
                Opcode::Dup0 => 1,
                Opcode::Dup1 => 1,
                Opcode::Dup2 => 1,
                Opcode::Swap1 => 1,
                Opcode::Swap2 => 1,
                Opcode::Tget => 2,
                Opcode::Tset => 40,
                Opcode::Tlen => 2,
                Opcode::Xget => 3,
                Opcode::Xset => 41,
                Opcode::Breakpoint => 100,
                Opcode::Log => 100,
                Opcode::Send => 100,
                Opcode::GetTime => 40,
                Opcode::Inbox => 40,
                Opcode::Panic => 5,
                Opcode::Halt => 10,
                Opcode::ErrCodePoint => 25,
                Opcode::PushInsn => 25,
                Opcode::PushInsnImm => 25,
                Opcode::OpenInsn => 25,
                Opcode::DebugPrint => 25,
                Opcode::GetGas => 0,
                Opcode::SetGas => 0,
                _ => return None,
            })
        } else {
            None
        }
    }

    pub fn run_one(&mut self, debug: bool) -> Result<bool, ExecutionError> {
        if let MachineState::Running(pc) = self.state {
            if let Some(insn) = self.code.get_insn(pc) {
                if let Some(val) = &insn.immediate {
                    self.stack.push(val.clone());
                }
                if let Some(gas) = self.next_op_gas() {
                    if let Some(remaining) = self.arb_gas_remaining.sub(&Uint256::from_u64(gas)) {
                        self.arb_gas_remaining = remaining;
                    } else {
                        return Err(ExecutionError::new("Out of ArbGas", &self.state, None));
                    }
                }
                match insn.opcode {
					Opcode::Noop => {
						self.incr_pc();
						Ok(true)
					}
					Opcode::Panic => Err(ExecutionError::new("panicked", &self.state, None)),
					Opcode::Jump => {
						self.state = MachineState::Running(self.stack.pop_codepoint(&self.state)?);
						Ok(true)
					}
					Opcode::Cjump => {
						let cp = self.stack.pop_codepoint(&self.state)?;
						let cond = self.stack.pop_uint(&self.state)?;
						if cond != Uint256::zero() {
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
					Opcode::Rget => {
						self.stack.push(self.register.clone());
						self.incr_pc();
						Ok(true)
					}
					Opcode::Rset => {
						let val = self.stack.pop(&self.state)?;
						self.register = val;
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
							Err(ExecutionError::new("index out of bounds in Tset", &self.state, None))
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
							Err(ExecutionError::new("index out of bounds in Tget", &self.state, None))
						}
					}
					Opcode::Tlen => {
						let tup = self.stack.pop_tuple(&self.state)?;
						self.stack.push_usize(tup.len());
						self.incr_pc();
						Ok(true)
					}
					Opcode::Pop => {
						let _ = self.stack.pop(&self.state)?;
						self.incr_pc();
						Ok(true)
					}
					Opcode::StackEmpty => {
						self.stack.push_bool(self.stack.is_empty());
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
					Opcode::AuxStackEmpty => {
						self.stack.push_bool(self.aux_stack.is_empty());
						self.incr_pc();
						Ok(true)
					}
					Opcode::Xget => {
						let slot_num = self.stack.pop_usize(&self.state)?;
						let aux_top = match self.aux_stack.top() {
							Some(top) => top,
							None => { return Err(ExecutionError::new("aux stack underflow", &self.state, None)); }
						};
						if let Value::Tuple(v) = aux_top {
							match v.get(slot_num) {
								Some(val) => {
									self.stack.push(val.clone());
									self.incr_pc();
									Ok(true)
								}
								None => Err(ExecutionError::new("tuple access out of bounds", &self.state, None))
							}
						} else {
							Err(ExecutionError::new("expected tuple on aux stack", &self.state, Some(aux_top)))
						}
					}
					Opcode::Xset => {
						let slot_num = self.stack.pop_usize(&self.state)?;
						let tup = self.aux_stack.pop_tuple(&self.state)?;
						if slot_num < tup.len() {
							let mut new_tup = tup;
							new_tup[slot_num] = self.stack.pop(&self.state)?;
							self.aux_stack.push(Value::Tuple(new_tup));
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("tuple access out of bounds", &self.state, None))
						}
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
					Opcode::Dup2 => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						let trd = self.stack.pop(&self.state)?;
						self.stack.push(trd.clone());
						self.stack.push(snd);
						self.stack.push(top);
						self.stack.push(trd);
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
					Opcode::Swap2 => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						let trd = self.stack.pop(&self.state)?;
						self.stack.push(top);
						self.stack.push(snd);
						self.stack.push(trd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::IsZero => {
						let res = if (self.stack.pop_uint(&self.state)? == Uint256::zero()) { 1 } else { 0 };
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
								Err(ExecutionError::new("signed integer overflow in unary minus", &self.state, None))
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
						self.stack.push_uint(r1.sub(&r2)
							.ok_or(ExecutionError::new("signed integer underflow in subtraction", &self.state, None))?);
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
							None => Err(ExecutionError::new("divide by zero", &self.state, None))
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
							None => Err(ExecutionError::new("modulo by zero", &self.state, None))
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
							None => Err(ExecutionError::new("divide by zero", &self.state, None))
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
							None => Err(ExecutionError::new("modulo by zero", &self.state, None))
						}
					}
					Opcode::AddMod => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						let r3 = self.stack.pop_uint(&self.state)?;
						let ores = r1.add_mod(&r2, &r3);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("modulo by zero", &self.state, None))
						}
					}
					Opcode::MulMod => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						let r3 = self.stack.pop_uint(&self.state)?;
						let ores = r1.mul_mod(&r2, &r3);
						match ores {
							Some(res) => {
								self.stack.push_uint(res);
								self.incr_pc();
								Ok(true)
							}
							None => Err(ExecutionError::new("modulo by zero", &self.state, None))
						}
					}
					Opcode::Exp => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.exp(&r2));
						self.incr_pc();
						Ok(true)
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
						let r1 = self.stack.pop(&self.state)?;
						let r2 = self.stack.pop(&self.state)?;
						self.stack.push_usize(if r1 == r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::NotEqual => {
						let r1 = self.stack.pop(&self.state)?;
						let r2 = self.stack.pop(&self.state)?;
						self.stack.push_usize(if r1 == r2 { 0 } else { 1 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::Type => {
						let val = self.stack.pop(&self.state)?;
						self.stack.push_usize(val.type_insn_result());
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
					Opcode::Byte => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(
							if r1 < Uint256::from_usize(32) {
								let shift_factor = Uint256::one().exp(&Uint256::from_usize(8*(31-r1.to_usize().unwrap())));
								r2.div(&shift_factor).unwrap().bitwise_and(&Uint256::from_usize(255))
							} else {
								Uint256::zero()
							}
						);
						self.incr_pc();
						Ok(true)
					}
					Opcode::SignExtend => {
						let bnum = self.stack.pop_uint(&self.state)?;
						let x = self.stack.pop_uint(&self.state)?;
						let out = match bnum.to_usize() {
							Some(ub) => {
								if ub > 31 {
									x
								} else {
									let t = 248-ub;
									let shifted_bit = Uint256::from_usize(2).exp(&Uint256::from_usize(t));
									let sign_bit = x.bitwise_and(&shifted_bit) != Uint256::zero();
									let mask = shifted_bit.sub(&Uint256::one()).ok_or(ExecutionError::new("underflow in signextend", &self.state, None))?;
									if sign_bit {
										x.bitwise_and(&mask)
									} else {
										x.bitwise_or(&mask.bitwise_neg())
									}
								}
							}
							None => x,
						};
						self.stack.push_uint(out);
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
					Opcode::GetTime => {
						panic!("GetTime instruction not yet implemented");
					}
					Opcode::Inbox => {
						let msgs = self.runtime_env.get_inbox();
						if msgs.is_none() {
							// machine is blocked, waiting for nonempty inbox
							Ok(false)
						} else {
							self.stack.push(msgs);
							self.incr_pc();
							Ok(true)
						}
					}
					Opcode::ErrCodePoint => {
						self.stack.push(Value::CodePoint(
							self.code.create_segment()
						));
						self.incr_pc();
						Ok(true)
					}
					Opcode::Send => {
						panic!("Send instruction not yet implemented");
					}
					Opcode::Log => {
						let val = self.stack.pop(&self.state)?;
						self.runtime_env.push_log(val);
						self.incr_pc();
						Ok(true)
					}
					Opcode::ErrSet => {
						let cp = self.stack.pop_codepoint(&self.state)?;
						self.err_codepoint = cp;
						self.incr_pc();
						Ok(true)
					}
					Opcode::ErrPush => {
						self.stack.push_codepoint(self.err_codepoint);
						self.incr_pc();
						Ok(true)
					}
					Opcode::PushInsn => {
						let opcode = self.stack.pop_usize(&self.state)?;
						let cp = self.stack.pop_codepoint(&self.state)?;
						let new_cp = self.code.push_insn(opcode, None, cp);
						if let Some(cp) = new_cp {
							self.stack.push_codepoint(cp);
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("invalid args to PushInsn", &self.state, None))
						}
					}
					Opcode::PushInsnImm => {
						let opcode = self.stack.pop_usize(&self.state)?;
						let imm = self.stack.pop(&self.state)?;
						let cp = self.stack.pop_codepoint(&self.state)?;
						let new_cp = self.code.push_insn(opcode, Some(imm), cp);
						if let Some(cp) = new_cp {
							self.stack.push_codepoint(cp);
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("invalid args to PushInsnImm", &self.state, None))
						}
					}
					Opcode::OpenInsn => {
						let insn = self.code.get_insn(self.stack.pop_codepoint(&self.state)?).unwrap();
						if let Some(val) = &insn.immediate {
							self.stack.push(Value::Tuple(vec![val.clone()]));
						} else {
							self.stack.push(Value::none());
						}
						self.stack.push_usize(insn.opcode.to_number().unwrap() as usize);
						self.incr_pc();
						Ok(true)
					}
					Opcode::Breakpoint => {
						self.incr_pc();
						Ok(false)
					}
					Opcode::Halt => {
						self.state = MachineState::Stopped;
						Ok(false)
					}
					Opcode::DebugPrint => {
						let r1 = self.stack.pop(&self.state)?;
						if debug {
                            println!("debugprint: {}", r1);
                        }
						self.incr_pc();
						Ok(true)
					}
                    Opcode::GetGas => {
                        self.stack.push(Value::Int(self.arb_gas_remaining.clone()));
                        self.incr_pc();
                        Ok(true)
                    },
                    Opcode::SetGas => {
                        let gas = self.stack.pop_uint(&self.state)?;
                        self.arb_gas_remaining = gas;
                        self.incr_pc();
                        Ok(true)
                    },
					Opcode::GetLocal |  // these opcodes are for intermediate use in compilation only
					Opcode::SetLocal |  // they should never appear in fully compiled code
					Opcode::MakeFrame(_, _) |
					Opcode::Label(_) |
					Opcode::PushExternal(_) |
					Opcode::TupleGet(_) |
					Opcode::TupleSet(_) |
					Opcode::ArrayGet |
					Opcode::UncheckedFixedArrayGet(_) |
					Opcode::GetGlobalVar(_) |
					Opcode::SetGlobalVar(_) |
					Opcode::Return => Err(ExecutionError::new("invalid opcode", &self.state, None))
				}
            } else {
                Err(ExecutionError::new(
                    "invalid program counter",
                    &self.state,
                    None,
                ))
            }
        } else {
            Err(ExecutionError::new(
                "tried to run machine that is not runnable",
                &self.state,
                None,
            ))
        }
    }
}

#[derive(Debug)]
pub enum StackTrace {
    _Unknown,
    Known(Vec<CodePt>),
}

impl fmt::Display for StackTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StackTrace::_Unknown => writeln!(f, "[stack trace unknown]"),
            StackTrace::Known(v) => writeln!(f, "{:?}", v),
        }
    }
}
