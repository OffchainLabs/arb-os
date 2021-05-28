/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides utilities for emulation of AVM bytecode.

use crate::compile::{CompileError, DebugInfo};
use crate::link::LinkedProgram;
use crate::mavm::{AVMOpcode, Buffer, CodePt, Instruction, Value, WInstruction};
use crate::uint256::Uint256;
use std::fmt;


///Represents a stack of `Value`s
#[derive(Debug, Default, Clone)]
pub struct ValueStack {
    contents: Vec<u64>,
    ptr: usize,
}

impl ValueStack {
    pub fn new() -> Self {
        ValueStack {
            contents: vec![0; 10000],
            ptr: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ptr == 0
    }

    pub fn num_items(&self) -> usize {
        self.ptr
    }

    ///Pushes val to the top of self.
    pub fn push(&mut self, val: u64) {
        self.contents[self.ptr] = val;
        self.ptr = self.ptr + 1;
    }

    pub fn push_bool(&mut self, val: bool) {
        self.push(if val { 1 } else { 0 });
    }

    pub fn push_usize(&mut self, val: usize) {
        self.contents[self.ptr] = val as u64;
        self.ptr = self.ptr + 1;
    }

    ///Returns the `Value` on the top of self, or None if self is empty.
    pub fn top(&self) -> Option<u64> {
        if self.is_empty() {
            None
        } else {
            Some(self.contents[self.ptr - 1])
        }
    }

    pub fn nth(&self, n: usize) -> Option<u64> {
        if self.num_items() > n {
            Some(self.contents[self.ptr - 1 - n])
        } else {
            None
        }
    }

    ///Pops the top value off the stack and returns it, or if the stack is empty returns an
    /// `ExecutionError`.
    pub fn pop(&mut self, state: &MachineState) -> Result<u64, ExecutionError> {
        if self.ptr > 0 {
            self.ptr = self.ptr - 1;
            Ok(self.contents[self.ptr])
        } else {
            Err(ExecutionError::new("stack underflow", state, None))
        }
    }
    pub fn pop_usize(&mut self, state: &MachineState) -> Result<usize, ExecutionError> {
        if self.ptr > 0 {
            self.ptr = self.ptr - 1;
            Ok(self.contents[self.ptr] as usize)
        } else {
            Err(ExecutionError::new("stack underflow", state, None))
        }
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

///Represents an error encountered during runtime.
///
/// StoppedErr is for errors encountered when the `Machine` is not running, RunningErr is for when
/// the machine is running, and Wrapped adds additional context to its contained error.
#[derive(Clone, Debug)]
pub enum ExecutionError {
    StoppedErr(&'static str),
    Wrapped(&'static str, Box<ExecutionError>),
    RunningErr(&'static str, CodePt, Option<u64>),
}

impl ExecutionError {
    fn new(why: &'static str, state: &MachineState, val: Option<u64>) -> Self {
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
                Some(val) => writeln!(f, "{} ({}) with value {}", s, cp, val),
                None => writeln!(f, "{} ({})", s, cp),
            },
        }
    }
}

///Represents the state of the containing `Machine`.
///
/// Running is used during execution, Stopped occurs when the program exits normally, and Error
/// occurs when the `Machine` encounters a runtime error.
#[derive(Clone, Debug)]
pub enum MachineState {
    Stopped,
    Error(ExecutionError),
    Running(CodePt), // pc
}

impl MachineState {
    ///Returns true if self is the Running variant.
    pub fn is_running(&self) -> bool {
        if let MachineState::Running(_) = self {
            true
        } else {
            false
        }
    }
}

///Holds AVM bytecode in a list of segments, the runtime is held on segment 0.
#[derive(Debug)]
struct CodeStore {
    segments: Vec<Vec<WInstruction>>,
}

impl CodeStore {
    fn new(runtime: Vec<WInstruction>) -> Self {
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

    ///Gets the size of the first code segment.
    fn runtime_segment_size(&self) -> usize {
        self.segments[0].len()
    }

    ///Returns the `Instruction` that codept points to, or None if codept points to an invalid
    /// location.
    ///
    /// Panics if codept is not an Internal or InSegment reference.
    fn get_insn(&self, codept: CodePt) -> Option<&WInstruction> {
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
                panic!("unlinked codepoint reference in running code: {}", codept);
            }
        }
    }

    ///Creates a new code segment containing a single panic instruction, returns a `CodePt` pointing
    /// to the start of that segment.
    fn create_segment(&mut self) -> CodePt {
        self.segments
            .push(vec![WInstruction::from_opcode(AVMOpcode::Zero)]);
        CodePt::new_in_segment(self.segments.len() - 1, 0)
    }

    ///Appends an instruction with opcode derived from op, and immediate from imm, to the end of the
    /// code segment pointed to by codept.
    ///
    /// The codept argument must point to a code segment, and must point to the end of that segment,
    /// also op must be the numeric representation of some AVM opcode, if these conditions aren't
    /// met the function will panic.
    fn push_insn(&mut self, op: usize, imm: Option<u64>, codept: CodePt) -> Option<CodePt> {
        if let CodePt::InSegment(seg_num, old_offset) = codept {
            if seg_num >= self.segments.len() {
                panic!("bad segment number in push_insn");
            //None
            } else {
                let segment = &mut self.segments[seg_num];
                if old_offset == segment.len() - 1 {
                    if let Some(opcode) = AVMOpcode::from_number(op) {
                        segment.push(WInstruction::new(opcode, imm, None));
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
                    unimplemented!("branching segments");
                }
            }
        } else {
            panic!("invalid codepoint in push_insn: {}", codept);
        }
    }
}

///Represents the state of execution of a AVM program including the code it is compiled from.
#[derive(Debug)]
pub struct Machine {
    pub stack: ValueStack,
    aux_stack: ValueStack,
    pub state: MachineState,
    code: CodeStore,
    memory: u64,
    call_table: Vec<CodePt>,
    buffers: Vec<Vec<u8>>,
    frames: Vec<u64>,
    err_codepoint: CodePt,
    counter: usize,
}

impl Machine {
    pub fn new(program: LinkedProgram, call_table: Vec<CodePt>) -> Self {
        Machine {
            stack: ValueStack::new(),
            aux_stack: ValueStack::new(),
            state: MachineState::Stopped,
            code: CodeStore::new(program.code.into_iter().map(|insn| insn.into()).collect()),
            err_codepoint: CodePt::Null,
            counter: 0,
            memory: 0,
            call_table,
            buffers: vec![],
            frames: vec![],
        }
    }

    #[cfg(test)]
    pub fn stack_top(&self) -> Option<&u64> {
        self.stack.contents.last()
    }

    ///Pushes 0 to the stack and sets the program counter to the first instruction. Used by the EVM
    /// compiler.
    pub fn start_at_zero(&mut self) {
        self.state = MachineState::Running(CodePt::Internal(0));
    }

    ///If the machine is running returns the `CodePt` that represents the current program counter,
    /// otherwise produces an `ExecutionError`.
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

    ///Increments self's program counter.
    ///
    /// Panics if self is not running or the current program counter is an external reference.
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

    ///Returns the `Instruction` pointed to by self's program counter if it exists, and None
    /// otherwise.
    pub fn next_opcode(&self) -> Option<WInstruction> {
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

    ///Starts the debugger, execution will end when the program counter of self reaches stop_pc, or
    /// an error state is reached.
    ///
    /// Returns the total gas used by the machine.
    pub fn debug(&mut self, stop_pc: Option<CodePt>) -> u64 {
        println!("Blank line or \"step\" to run one opcode, \"set break\" followed by a \
         line number to resume program until that line, \"show static\" to show the static contents.");
        let mut breakpoint = true;
        let mut break_line = 0;
        let mut break_gas_amount = 0u64;
        let mut gas_cost = 0;
        let mut show_aux = true;
        let mut show_reg = true;
        while self.state.is_running() {
            if breakpoint {
                if let Ok(pc) = self.get_pc() {
                    println!("PC: {}", pc);
                }
                println!("Stack contents: {}", self.stack);
                // println!("Aux-stack contents: {}", self.aux_stack);
                if !self.stack.is_empty() {
                    println!("Stack size: {}", self.stack.num_items());
                }
                if !self.stack.is_empty() {
                    println!("Stack top: {}", self.stack.top().unwrap());
                }
                if let Some(code) = self.next_opcode() {
                    println!("Next Opcode: {}", code.opcode);
                    if let Some(imm) = code.immediate {
                        println!("Immediate: {}", imm);
                    }
                    if let Some(str) = code.debug_str {
                        println!("*************************** Debug: {}", str);
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
                        "break at gas\n" => {
                            breakpoint = false;
                            loop {
                                let mut debugger_state = String::new();
                                std::io::stdin().read_line(&mut debugger_state).unwrap();
                                if let Ok(val) = str::parse(&debugger_state.trim()) {
                                    break_gas_amount = val;
                                    exit = true;
                                    break;
                                } else {
                                    println!("Could not parse input as number");
                                }
                            }
                        }
                        "r\n" | "run\n" => {
                            breakpoint = false;
                            exit = true;
                        }
                        "toggle aux\n" => {
                            show_aux = !show_aux;
                        }
                        "toggle reg\n" => {
                            show_reg = !show_reg;
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

    ///Runs self until the program counter reaches stop_pc, an error state is encountered or the
    /// machine reaches a stopped state for any other reason.  Returns the total gas used by self.
    pub fn run(&mut self, stop_pc: Option<CodePt>) -> u64 {
        while self.state.is_running() {
            match self.run_one(false) {
                Ok(still_runnable) => {
                    if !still_runnable {
                        return 0;
                    }
                }
                Err(e) => {
                    self.state = MachineState::Error(e);
                    return 0;
                }
            }
        }
        0
    }

    ///If the opcode has a specified gas cost returns the gas cost, otherwise returns None.
    pub(crate) fn next_op_gas(&self) -> Option<u64> {
        if let MachineState::Running(pc) = self.state {
            Some(match self.code.get_insn(pc)?.opcode {
                AVMOpcode::Zero => 5,
                AVMOpcode::Plus => 3,
                AVMOpcode::Mul => 3,
                AVMOpcode::Minus => 3,
                AVMOpcode::Div => 4,
                AVMOpcode::Sdiv => 7,
                AVMOpcode::Mod => 4,
                AVMOpcode::Smod => 7,
                AVMOpcode::AddMod => 4,
                AVMOpcode::MulMod => 4,
                AVMOpcode::Exp => 25,
                AVMOpcode::SignExtend => 7,
                AVMOpcode::LessThan => 2,
                AVMOpcode::GreaterThan => 2,
                AVMOpcode::SLessThan => 2,
                AVMOpcode::SGreaterThan => 2,
                AVMOpcode::Equal => 2,
                AVMOpcode::IsZero => 1,
                AVMOpcode::BitwiseAnd => 2,
                AVMOpcode::BitwiseOr => 2,
                AVMOpcode::BitwiseXor => 2,
                AVMOpcode::BitwiseNeg => 1,
                AVMOpcode::Byte => 4,
                AVMOpcode::ShiftLeft => 4,
                AVMOpcode::ShiftRight => 4,
                AVMOpcode::ShiftArith => 4,
                AVMOpcode::Hash => 7,
                AVMOpcode::Type => 3,
                AVMOpcode::Hash2 => 8,
                AVMOpcode::Keccakf => 600,
                AVMOpcode::Sha256f => 250,
                AVMOpcode::Ripemd160f => 250, //TODO: measure and update this
                AVMOpcode::Pop => 1,
                AVMOpcode::PushStatic => 1,
                AVMOpcode::Rget => 1,
                AVMOpcode::Rset => 2,
                AVMOpcode::Jump => 4,
                AVMOpcode::Cjump => 4,
                AVMOpcode::StackEmpty => 2,
                AVMOpcode::GetPC => 1,
                AVMOpcode::AuxPush => 1,
                AVMOpcode::AuxPop => 1,
                AVMOpcode::AuxStackEmpty => 2,
                AVMOpcode::Noop => 1,
                AVMOpcode::ErrPush => 1,
                AVMOpcode::ErrSet => 1,
                AVMOpcode::Dup0 => 1,
                AVMOpcode::Dup1 => 1,
                AVMOpcode::Dup2 => 1,
                AVMOpcode::Swap1 => 1,
                AVMOpcode::Swap2 => 1,
                AVMOpcode::Tget => 2,
                AVMOpcode::Tset => 40,
                AVMOpcode::Tlen => 2,
                AVMOpcode::Xget => 3,
                AVMOpcode::Xset => 41,
                AVMOpcode::Breakpoint => 100,
                AVMOpcode::Log => 100,
                AVMOpcode::Send => 100,
                AVMOpcode::InboxPeek => 40,
                AVMOpcode::Inbox => 40,
                AVMOpcode::Panic => 5,
                AVMOpcode::Halt => 10,
                AVMOpcode::ErrCodePoint => 25,
                AVMOpcode::PushInsn => 25,
                AVMOpcode::PushInsnImm => 25,
                AVMOpcode::OpenInsn => 25,
                AVMOpcode::DebugPrint => 1,
                AVMOpcode::GetGas => 1,
                AVMOpcode::SetGas => 1,
                AVMOpcode::EcRecover => 20_000,
                AVMOpcode::EcAdd => 3500,
                AVMOpcode::EcMul => 82_000,
                AVMOpcode::Sideload => 10,
                AVMOpcode::NewBuffer => 1,
                AVMOpcode::GetBuffer8 => 10,
                AVMOpcode::GetBuffer64 => 10,
                AVMOpcode::GetBuffer256 => 10,
                AVMOpcode::SetBuffer8 => 100,
                AVMOpcode::SetBuffer64 => 100,
                AVMOpcode::SetBuffer256 => 100,
                AVMOpcode::GetBuffer16 => 10,
                AVMOpcode::GetBuffer32 => 10,
                AVMOpcode::SetBuffer16 => 100,
                AVMOpcode::SetBuffer32 => 100,
                AVMOpcode::RunWasm => 1000000,
                AVMOpcode::CompileWasm => 100,
                AVMOpcode::MakeWasm => 100,
                AVMOpcode::JumpTable => 10,
                AVMOpcode::CjumpTable => 10,
                _ => 0,
            })
        } else {
            None
        }
    }

    ///Runs the instruction pointed to by the program counter, returns either a bool indicating
    /// whether the instruction was blocked if execution does not hit an error state, or an
    /// `ExecutionError` if an error was encountered.
    pub fn run_one(&mut self, _debug: bool) -> Result<bool, ExecutionError> {
        match self.run_one_dont_catch_errors(_debug) {
            Ok(b) => Ok(b),
            Err(e) => {
                if self.err_codepoint == CodePt::Null {
                    Err(e)
                } else {
                    self.state = MachineState::Running(self.err_codepoint);
                    Ok(true)
                }
            }
        }
    }

    fn find_jump(&self, num: usize) -> Result<CodePt, ExecutionError> {
        Ok(self.call_table[num])
        /*
        if let Value::Tuple(val) = &self.register {
            let elem = get_from_table(&val[1], num);
            if let Value::CodePoint(pt) = elem {
                Ok(pt)
            } else {
                Err(ExecutionError::new("Cannot resolve jump", &self.state, None))
            }
        } else {
            Err(ExecutionError::new("Cannot resolve jump", &self.state, None))
        }*/
    }

    fn run_one_dont_catch_errors(&mut self, _debug: bool) -> Result<bool, ExecutionError> {
        if let MachineState::Running(pc) = self.state {
            if let Some(insn) = self.code.get_insn(pc) {
                let _stack_len = self.stack.num_items();
                if let Some(val) = &insn.immediate {
                    self.stack.push(val.clone());
                }
                if let Some(_str) = &insn.debug_str {
                    self.counter = self.counter + 1;
                    // println!("{}, stack sz {}", str, stack_len);
                    // println!("{}", str);
                    if self.counter % 1000000 == 0 {
                        println!("Wasm instruction {}", self.counter);
                    }
                };
                match insn.opcode {
                    AVMOpcode::Noop => {
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Zero | AVMOpcode::Panic => {
                        Err(ExecutionError::new("panicked", &self.state, None))
                    }
                    AVMOpcode::JumpTable => {
                        let num = self.stack.pop_usize(&self.state)?;
                        let cp = self.find_jump(num)?;
                        self.state = MachineState::Running(cp);
                        Ok(true)
                    }
                    AVMOpcode::CjumpTable => {
                        let num = self.stack.pop_usize(&self.state)?;
                        let cond = self.stack.pop(&self.state)?;
                        if cond != 0 {
                            let cp = self.find_jump(num)?;
                            self.state = MachineState::Running(cp);
                        } else {
                            self.incr_pc();
                        }
                        Ok(true)
                    }
                    AVMOpcode::Pop => {
                        let _ = self.stack.pop(&self.state)?;
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::StackEmpty => {
                        self.stack.push_bool(self.stack.is_empty());
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::AuxPush => {
                        self.aux_stack.push(self.stack.pop(&self.state)?);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::AuxPop => {
                        self.stack.push(self.aux_stack.pop(&self.state)?);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::AuxStackEmpty => {
                        self.stack.push_bool(self.aux_stack.is_empty());
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Dup0 => {
                        let top = self.stack.pop(&self.state)?;
                        self.stack.push(top.clone());
                        self.stack.push(top);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Dup1 => {
                        let top = self.stack.pop(&self.state)?;
                        let snd = self.stack.pop(&self.state)?;
                        self.stack.push(snd.clone());
                        self.stack.push(top);
                        self.stack.push(snd);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Dup2 => {
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
                    AVMOpcode::Swap1 => {
                        let top = self.stack.pop(&self.state)?;
                        let snd = self.stack.pop(&self.state)?;
                        self.stack.push(top);
                        self.stack.push(snd);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Swap2 => {
                        let top = self.stack.pop(&self.state)?;
                        let snd = self.stack.pop(&self.state)?;
                        let trd = self.stack.pop(&self.state)?;
                        self.stack.push(top);
                        self.stack.push(snd);
                        self.stack.push(trd);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::IsZero => {
                        let res = self.stack.pop(&self.state)? == 0;
                        self.stack.push_bool(res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::BitwiseNeg => {
                        let res = self.stack.pop(&self.state)?;
                        self.stack.push(!res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Plus => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r1 + r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Minus => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r1 - r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Mul => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r1 * r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Div => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        let res = r1 / r2;
                        self.stack.push(res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Sdiv => {
                        let r1 = self.stack.pop(&self.state)? as i64;
                        let r2 = self.stack.pop(&self.state)? as i64;
                        let res = r1 / r2;
                        self.stack.push(res as u64);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Mod => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        let res = r1 % r2;
                        self.stack.push(res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Smod => {
                        let r1 = self.stack.pop(&self.state)? as i64;
                        let r2 = self.stack.pop(&self.state)? as i64;
                        let res = r1 % r2;
                        self.stack.push(res as u64);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SignExtend => {
                        let ub = self.stack.pop(&self.state)?;
                        let x = self.stack.pop(&self.state)?;
                        let out = if ub >= 31 {
                            x
                        } else {
                            let shifted_bit = 1 << (8 * ub + 7);
                            let sign_bit = x & shifted_bit != 0;
                            let mask = (shifted_bit - 1) * 2 + 1;
                            if sign_bit {
                                x | !mask
                            } else {
                                x & mask
                            }
                        };
                        self.stack.push(out);
                        self.incr_pc();
                        Ok(true)
                    }

                    AVMOpcode::LessThan => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push_bool(r1 < r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::GreaterThan => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push_bool(r1 > r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SLessThan => {
                        let r1 = self.stack.pop(&self.state)? as i64;
                        let r2 = self.stack.pop(&self.state)? as i64;
                        self.stack.push_bool(r1 < r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SGreaterThan => {
                        let r1 = self.stack.pop(&self.state)? as i64;
                        let r2 = self.stack.pop(&self.state)? as i64;
                        self.stack.push_bool(r1 > r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Equal => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push_bool(r1 == r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::BitwiseAnd => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r1 & r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::BitwiseOr => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r1 | r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::BitwiseXor => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r1 ^ r2);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::ShiftLeft => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r2 << r1);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::ShiftRight => {
                        let r1 = self.stack.pop(&self.state)?;
                        let r2 = self.stack.pop(&self.state)?;
                        self.stack.push(r2 >> r1);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::ShiftArith => {
                        let r1 = self.stack.pop(&self.state)? as i64;
                        let r2 = self.stack.pop(&self.state)? as i64;
                        self.stack.push((r2 >> r1) as u64);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::Halt => {
                        self.state = MachineState::Stopped;
                        Ok(false)
                    }
                    AVMOpcode::DebugPrint => {
                        let r1 = self.stack.pop(&self.state)?;
                        println!("debugprint: {}", r1);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::NewBuffer => {
                        self.buffers.push(vec![]);
                        self.stack.push_usize(self.buffers.len() - 1);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SetMemory => {
                        let buf = self.stack.pop(&self.state)?;
                        self.memory = buf;
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::GetMemory => {
                        self.stack.push(self.memory);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::GetBuffer8 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        self.stack.push_usize(self.buffers[buf][offset].into());
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::GetBuffer16 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let mut vec = &mut self.buffers[buf];
                        let res = if vec.len() <= offset {
                            0u64
                        } else {
                            let mut res = [0u8; 2];
                            for i in 0..2 {
                                res[i] = vec[offset + i];
                            }
                            u16::from_le_bytes(res) as u64
                        };
                        // println!("getting buffer offset {} value {:?}", offset, res);
                        self.stack.push(res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::GetBuffer32 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let mut vec = &mut self.buffers[buf];
                        let res = if vec.len() <= offset {
                            0u64
                        } else {
                            let mut res = [0u8; 4];
                            for i in 0..4 {
                                res[i] = vec[offset + i];
                            }
                            u32::from_le_bytes(res) as u64
                        };
                        // println!("getting buffer offset {} value {:?}", offset, res);
                        self.stack.push(res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::GetBuffer64 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let mut vec = &mut self.buffers[buf];
                        let res = if vec.len() <= offset {
                            0u64
                        } else {
                            let mut res = [0u8; 8];
                            for i in 0..8 {
                                res[i] = vec[offset + i];
                            }
                            u64::from_le_bytes(res)
                        };
                        // println!("getting buffer offset {} value {:?}", offset, res);
                        self.stack.push(res);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SetBuffer8 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let val = self.stack.pop(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let bytes = val.to_le_bytes();
                        let mut vec = &mut self.buffers[buf];
                        if offset >= vec.len() {
                            if offset > 1000 {
                                vec.resize(10000000, 0u8)
                            } else {
                                vec.resize(1001, 0u8)
                            }
                        }
                        vec[offset] = bytes[0];
                        self.stack.push_usize(buf);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SetBuffer16 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let val = self.stack.pop(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let bytes = val.to_le_bytes();
                        let mut vec = &mut self.buffers[buf];
                        if offset >= vec.len() {
                            if offset > 1000 {
                                vec.resize(10000000, 0u8)
                            } else {
                                vec.resize(1001, 0u8)
                            }
                        }
                        for i in 0..2 {
                            vec[offset + i] = bytes[i];
                        }
                        self.stack.push_usize(buf);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SetBuffer32 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let val = self.stack.pop(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let bytes = val.to_le_bytes();
                        let mut vec = &mut self.buffers[buf];
                        if offset >= vec.len() {
                            if offset > 1000 {
                                vec.resize(10000000, 0u8)
                            } else {
                                vec.resize(1001, 0u8)
                            }
                        }
                        for i in 0..4 {
                            vec[offset + i] = bytes[i];
                        }
                        self.stack.push_usize(buf);
                        self.incr_pc();
                        Ok(true)
                    }
                    AVMOpcode::SetBuffer64 => {
                        let offset = self.stack.pop_usize(&self.state)?;
                        let val = self.stack.pop(&self.state)?;
                        let buf = self.stack.pop_usize(&self.state)?;
                        let bytes = val.to_le_bytes();
                        let mut vec = &mut self.buffers[buf];
                        if offset >= vec.len() {
                            if offset > 1000 {
                                vec.resize(10000000, 0u8)
                            } else {
                                vec.resize(1001, 0u8)
                            }
                        }
                        for i in 0..8 {
                            vec[offset + i] = bytes[i];
                        }
                        self.stack.push_usize(buf);
                        self.incr_pc();
                        Ok(true)
                    }
                    _ => Ok(true),
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
