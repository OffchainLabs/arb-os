/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides utilities for emulation of AVM bytecode.

use super::runtime_env::RuntimeEnvironment;
use crate::link::LinkedProgram;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Opcode, Value};
use crate::pos::Location;
use crate::uint256::Uint256;
use ethers_core::types::{Signature, H256};
use std::cmp::max;
use std::collections::{BTreeMap, HashMap};
use std::convert::TryInto;
use std::fmt;
use std::fs::File;
use std::io::{stdin, BufWriter, Write};
use std::path::Path;

///Represents a stack of `Value`s
#[derive(Debug, Default, Clone)]
pub struct ValueStack {
    contents: im::Vector<Value>,
}

impl ValueStack {
    pub fn new() -> Self {
        ValueStack {
            contents: im::Vector::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.contents.len() == 0
    }

    pub fn num_items(&self) -> usize {
        self.contents.len()
    }

    ///Pushes val to the top of self.
    pub fn push(&mut self, val: Value) {
        self.contents.push_back(val);
    }

    ///Pushes a `Value` created from val to the top of self.
    pub fn push_uint(&mut self, val: Uint256) {
        self.push(Value::Int(val))
    }

    ///Pushes a `Value` created from val to the top of self.
    pub fn push_usize(&mut self, val: usize) {
        self.push_uint(Uint256::from_usize(val));
    }

    ///Pushes a `Value` created from val to the top of self.
    pub fn push_codepoint(&mut self, val: CodePt) {
        self.push(Value::CodePoint(val));
    }

    ///Pushes a `Value` created from val to the top of self.
    pub fn push_bool(&mut self, val: bool) {
        self.push_uint(if val { Uint256::one() } else { Uint256::zero() })
    }

    ///Returns the `Value` on the top of self, or None if self is empty.
    pub fn top(&self) -> Option<Value> {
        if self.is_empty() {
            None
        } else {
            Some(self.contents[self.contents.len() - 1].clone())
        }
    }

    pub fn nth(&self, n: usize) -> Option<Value> {
        if self.num_items() > n {
            Some(self.contents[self.contents.len() - 1 - n].clone())
        } else {
            None
        }
    }

    ///Pops the top value off the stack and returns it, or if the stack is empty returns an
    /// `ExecutionError`.
    pub fn pop(&mut self, state: &MachineState) -> Result<Value, ExecutionError> {
        match self.contents.pop_back() {
            Some(v) => Ok(v),
            None => Err(ExecutionError::new("stack underflow", state, None)),
        }
    }

    ///If the top `Value` on the stack is a code point, pops the value and returns it as a `CodePt`,
    /// otherwise returns an `ExecutionError`.
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

    ///If the top `Value` on the stack is an integer, pops the value and returns it as a `Uint256`,
    /// otherwise returns an `ExecutionError`.
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

    ///If the top `Value` on the stack is not greater than the max usize, pops the value and returns
    /// it as a `usize`, otherwise returns an `ExecutionError`
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

    ///If the top `Value` on the stack is 0 or 1, returns false or true respectively, otherwise
    /// returns an `ExecutionError`.
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

    ///If the top `Value` on self is a tuple, pops the `Value` and returns the contained values of
    /// self as a vector. Otherwise returns an `ExecutionError`.
    pub fn pop_tuple(&mut self, state: &MachineState) -> Result<Vec<Value>, ExecutionError> {
        let val = self.pop(state)?;
        if let Value::Tuple(v) = val {
            let vs = &*v;
            Ok(vs.to_vec())
        } else {
            Err(ExecutionError::new(
                "expected tuple on stack",
                state,
                Some(val),
            ))
        }
    }

    ///Returns a list of all CodePoint `Value`s as `CodePt`s, this is used for generating stack
    /// traces, as all code points on the aux stack represent the start of a call frame.
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

///Represents an error encountered during runtime.
///
/// StoppedErr is for errors encountered when the `Machine` is not running, RunningErr is for when
/// the machine is running, and Wrapped adds additional context to its contained error.
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

    ///Gets the size of the first code segment.
    fn runtime_segment_size(&self) -> usize {
        self.segments[0].len()
    }

    ///Returns the `Instruction` that codept points to, or None if codept points to an invalid
    /// location.
    ///
    /// Panics if codept is not an Internal or InSegment reference.
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

    ///Creates a new code segment containing a single panic instruction, returns a `CodePt` pointing
    /// to the start of that segment.
    fn create_segment(&mut self) -> CodePt {
        self.segments.push(vec![Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Panic),
            None,
        )]);
        CodePt::new_in_segment(self.segments.len() - 1, 0)
    }

    ///Appends an instruction with opcode derived from op, and immediate from imm, to the end of the
    /// code segment pointed to by codept.
    ///
    /// The codept argument must point to a code segment, and must point to the end of that segment,
    /// also op must be the numeric representation of some AVM opcode, if these conditions aren't
    /// met the function will panic.
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
                    unimplemented!("branching segments");
                }
            }
        } else {
            panic!("invalid codepoint in push_insn: {:?}", codept);
        }
    }
}

///Records how much gas was used at each source location in a run of a `Machine`. Gas used by any
/// instructions without an associated location are added to the unknown_gas field.
#[derive(Debug, Clone, Default)]
pub struct ProfilerData {
    data: HashMap<String, BTreeMap<(usize, usize), u64>>,
    unknown_gas: u64,
}

impl ProfilerData {
    ///Gets a reference to the gas cost at a given location if it has been recorded, or None
    /// otherwise.
    ///
    /// The chart argument is used to convert the file ID in the location to a filename.
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
    ///Inserts gas into the entry corresponding to loc. Returns the previous value at that location
    /// if it exists.
    ///
    /// The chart argument is used to convert the file ID in the location to a filename.
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

    ///Starts a profiler session from self.  Allows the user to view the gas cost per file and view
    /// the gas used over a range of lines.
    ///
    /// Use exit to exit the profiler.
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
            if stdin().read_line(&mut command).is_ok() {
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

///Represents the state of execution of a AVM program including the code it is compiled from.
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
    total_gas_usage: Uint256,
    trace_writer: Option<BufWriter<File>>,
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
            total_gas_usage: Uint256::zero(),
            trace_writer: None,
        }
    }

    ///Pushes 0 to the stack and sets the program counter to the first instruction. Used by the EVM
    /// compiler.
    pub fn start_at_zero(&mut self) {
        self.state = MachineState::Running(CodePt::Internal(0));
    }

    ///Returns a stack trace of the current state of the machine.
    pub fn get_stack_trace(&self) -> StackTrace {
        StackTrace::Known(self.aux_stack.all_codepts())
    }

    ///Adds a trace writer to the machine
    pub fn add_trace_writer(&mut self, filename: &str) {
        self.trace_writer = Some(BufWriter::new(File::create(Path::new(filename)).unwrap()));
    }

    ///Returns the value of the ArbGasRemaining register
    pub fn get_total_gas_usage(&self) -> Uint256 {
        self.total_gas_usage.clone()
    }

    ///Sets the state of the machine to call the function at func_addr with args.
    ///
    /// Returns the stop location of the program counter, calculated by `runtime_segment_size`.
    pub fn call_state(&mut self, func_addr: CodePt, args: Vec<Value>) -> CodePt {
        let stop_pc = CodePt::new_internal(self.code.runtime_segment_size());
        for i in args.into_iter().rev() {
            self.stack.push(i);
        }
        self.stack.push(Value::CodePoint(stop_pc));
        self.state = MachineState::Running(func_addr);
        stop_pc
    }

    ///Calls the function at address func_addr and runs until the program counter advances by the
    /// result of `runtime_segment_size`, or an error is encountered.
    ///
    /// If the machine stops normally, then returns the stack contents, otherwise returns an
    /// `ExecutionError`
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
                    if insn.opcode == Opcode::AVMOpcode(AVMOpcode::DebugPrint) {
                        breakpoint = true;
                    }
                }
                if self.total_gas_usage > Uint256::from_u64(break_gas_amount) {
                    breakpoint = true;
                }
            }
            if breakpoint {
                if let Ok(pc) = self.get_pc() {
                    println!("PC: {:?}", pc);
                }
                println!("Stack contents: {}", self.stack);
                //println!("Aux-stack contents: {}", self.aux_stack);
                //println!("Register contents: {}", self.register);
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

    ///Runs self until the program counter reaches stop_pc, an error state is encountered or the
    /// machine reaches a stopped state for any other reason.  Returns the total gas used by self.
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

            let cp = self.get_pc();

            if let Ok(codept) = cp {
                if let Some(trace_writer) = &mut self.trace_writer {
                    let res = match codept {
                        CodePt::Internal(pc) => Some((
                            0,
                            pc as u64,
                            self.code
                                .get_insn(codept)
                                .unwrap()
                                .opcode
                                .to_number()
                                .unwrap(),
                        )),
                        CodePt::InSegment(seg_num, rev_pc) => Some((
                            seg_num as u64,
                            (self.code.segment_size(seg_num).unwrap() as u64) - 1 - (rev_pc as u64),
                            self.code
                                .get_insn(codept)
                                .unwrap()
                                .opcode
                                .to_number()
                                .unwrap(),
                        )),
                        _ => None,
                    };
                    if let Some((seg_num, pc, opcode)) = res {
                        write!(trace_writer, "{} {} {}", seg_num, pc, opcode)
                            .expect("failed to write PC trace file");
                        if !self.stack.is_empty() {
                            let val = self.stack.top().unwrap();
                            if let Value::Int(ui) = val {
                                write!(trace_writer, " {}", ui.avm_hash()).unwrap();
                            }
                            if self.stack.num_items() > 1 {
                                let val = self.stack.nth(1).unwrap();
                                if let Value::Int(ui) = val {
                                    write!(trace_writer, " {}", ui.avm_hash()).unwrap();
                                }
                            }
                        }
                        write!(trace_writer, "\n").unwrap();
                    }
                }
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

    ///Generates a `ProfilerData` from a run of self with args from address 0.
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

    ///If the opcode has a specified gas cost returns the gas cost, otherwise returns None.
    pub(crate) fn next_op_gas(&self) -> Option<u64> {
        if let MachineState::Running(pc) = self.state {
            Some(match self.code.get_insn(pc)?.opcode {
                Opcode::AVMOpcode(AVMOpcode::Plus) => 3,
                Opcode::AVMOpcode(AVMOpcode::Mul) => 3,
                Opcode::AVMOpcode(AVMOpcode::Minus) => 3,
                Opcode::AVMOpcode(AVMOpcode::Div) => 4,
                Opcode::AVMOpcode(AVMOpcode::Sdiv) => 7,
                Opcode::AVMOpcode(AVMOpcode::Mod) => 4,
                Opcode::AVMOpcode(AVMOpcode::Smod) => 7,
                Opcode::AVMOpcode(AVMOpcode::AddMod) => 4,
                Opcode::AVMOpcode(AVMOpcode::MulMod) => 4,
                Opcode::AVMOpcode(AVMOpcode::Exp) => 25,
                Opcode::AVMOpcode(AVMOpcode::SignExtend) => 7,
                Opcode::AVMOpcode(AVMOpcode::LessThan) => 2,
                Opcode::AVMOpcode(AVMOpcode::GreaterThan) => 2,
                Opcode::AVMOpcode(AVMOpcode::SLessThan) => 2,
                Opcode::AVMOpcode(AVMOpcode::SGreaterThan) => 2,
                Opcode::AVMOpcode(AVMOpcode::Equal) => 2,
                Opcode::AVMOpcode(AVMOpcode::IsZero) => 1,
                Opcode::AVMOpcode(AVMOpcode::BitwiseAnd) => 2,
                Opcode::AVMOpcode(AVMOpcode::BitwiseOr) => 2,
                Opcode::AVMOpcode(AVMOpcode::BitwiseXor) => 2,
                Opcode::AVMOpcode(AVMOpcode::BitwiseNeg) => 1,
                Opcode::AVMOpcode(AVMOpcode::Byte) => 4,
                Opcode::AVMOpcode(AVMOpcode::ShiftLeft) => 4,
                Opcode::AVMOpcode(AVMOpcode::ShiftRight) => 4,
                Opcode::AVMOpcode(AVMOpcode::ShiftArith) => 4,
                Opcode::AVMOpcode(AVMOpcode::Hash) => 7,
                Opcode::AVMOpcode(AVMOpcode::Type) => 3,
                Opcode::AVMOpcode(AVMOpcode::Hash2) => 8,
                Opcode::AVMOpcode(AVMOpcode::Keccakf) => 600,
                Opcode::AVMOpcode(AVMOpcode::Pop) => 1,
                Opcode::AVMOpcode(AVMOpcode::PushStatic) => 1,
                Opcode::AVMOpcode(AVMOpcode::Rget) => 1,
                Opcode::AVMOpcode(AVMOpcode::Rset) => 2,
                Opcode::AVMOpcode(AVMOpcode::Jump) => 4,
                Opcode::AVMOpcode(AVMOpcode::Cjump) => 4,
                Opcode::AVMOpcode(AVMOpcode::StackEmpty) => 2,
                Opcode::AVMOpcode(AVMOpcode::GetPC) => 1,
                Opcode::AVMOpcode(AVMOpcode::AuxPush) => 1,
                Opcode::AVMOpcode(AVMOpcode::AuxPop) => 1,
                Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty) => 2,
                Opcode::AVMOpcode(AVMOpcode::Noop) => 1,
                Opcode::AVMOpcode(AVMOpcode::ErrPush) => 1,
                Opcode::AVMOpcode(AVMOpcode::ErrSet) => 1,
                Opcode::AVMOpcode(AVMOpcode::Dup0) => 1,
                Opcode::AVMOpcode(AVMOpcode::Dup1) => 1,
                Opcode::AVMOpcode(AVMOpcode::Dup2) => 1,
                Opcode::AVMOpcode(AVMOpcode::Swap1) => 1,
                Opcode::AVMOpcode(AVMOpcode::Swap2) => 1,
                Opcode::AVMOpcode(AVMOpcode::Tget) => 2,
                Opcode::AVMOpcode(AVMOpcode::Tset) => 40,
                Opcode::AVMOpcode(AVMOpcode::Tlen) => 2,
                Opcode::AVMOpcode(AVMOpcode::Xget) => 3,
                Opcode::AVMOpcode(AVMOpcode::Xset) => 41,
                Opcode::AVMOpcode(AVMOpcode::Breakpoint) => 100,
                Opcode::AVMOpcode(AVMOpcode::Log) => 100,
                Opcode::AVMOpcode(AVMOpcode::Send) => 100,
                Opcode::AVMOpcode(AVMOpcode::InboxPeek) => 40,
                Opcode::AVMOpcode(AVMOpcode::Inbox) => 40,
                Opcode::AVMOpcode(AVMOpcode::Panic) => 5,
                Opcode::AVMOpcode(AVMOpcode::Halt) => 10,
                Opcode::AVMOpcode(AVMOpcode::ErrCodePoint) => 25,
                Opcode::AVMOpcode(AVMOpcode::PushInsn) => 25,
                Opcode::AVMOpcode(AVMOpcode::PushInsnImm) => 25,
                Opcode::AVMOpcode(AVMOpcode::OpenInsn) => 25,
                Opcode::AVMOpcode(AVMOpcode::DebugPrint) => 1,
                Opcode::AVMOpcode(AVMOpcode::GetGas) => 1,
                Opcode::AVMOpcode(AVMOpcode::SetGas) => 0,
                Opcode::AVMOpcode(AVMOpcode::EcRecover) => 20_000,
                Opcode::AVMOpcode(AVMOpcode::Sideload) => 10,
                _ => return None,
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

    fn run_one_dont_catch_errors(&mut self, _debug: bool) -> Result<bool, ExecutionError> {
        if let MachineState::Running(pc) = self.state {
            if let Some(insn) = self.code.get_insn(pc) {
                if let Some(val) = &insn.immediate {
                    self.stack.push(val.clone());
                }
                if let Some(gas) = self.next_op_gas() {
                    let gas256 = Uint256::from_u64(gas);
                    if let Some(remaining) = self.arb_gas_remaining.sub(&gas256) {
                        self.arb_gas_remaining = remaining;
                        self.total_gas_usage = self.total_gas_usage.add(&gas256);
                    } else {
                        self.arb_gas_remaining = Uint256::max_int();
                        return Err(ExecutionError::new("Out of ArbGas", &self.state, None));
                    }
                }
                match insn.opcode {
					Opcode::AVMOpcode(AVMOpcode::Noop) => {
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Panic) => Err(ExecutionError::new("panicked", &self.state, None)),
					Opcode::AVMOpcode(AVMOpcode::Jump) => {
						self.state = MachineState::Running(self.stack.pop_codepoint(&self.state)?);
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Cjump) => {
						let cp = self.stack.pop_codepoint(&self.state)?;
						let cond = self.stack.pop_uint(&self.state)?;
						if cond != Uint256::zero() {
							self.state = MachineState::Running(cp);
						} else {
							self.incr_pc();
						}
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::GetPC) => {
						self.stack.push_codepoint(self.get_pc()?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Rget) => {
						self.stack.push(self.register.clone());
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Rset) => {
						let val = self.stack.pop(&self.state)?;
						self.register = val;
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::PushStatic) => {
						self.stack.push(self.static_val.clone());
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Tset) => {
						let idx = self.stack.pop_usize(&self.state)?;
						let tup = self.stack.pop_tuple(&self.state)?;
						let val = self.stack.pop(&self.state)?;
						let mut newv = Vec::new();
						for v in tup {
							newv.push(v);
						}
						if idx < newv.len() {
							newv[idx] = val;
							self.stack.push(Value::new_tuple(newv));
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("index out of bounds in Tset", &self.state, None))
						}
					}
					Opcode::AVMOpcode(AVMOpcode::Tget) => {
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
					Opcode::AVMOpcode(AVMOpcode::Tlen) => {
						let tup = self.stack.pop_tuple(&self.state)?;
						self.stack.push_usize(tup.len());
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Pop) => {
						let _ = self.stack.pop(&self.state)?;
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::StackEmpty) => {
						self.stack.push_bool(self.stack.is_empty());
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::AuxPush) => {
						self.aux_stack.push(self.stack.pop(&self.state)?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::AuxPop) => {
						self.stack.push(self.aux_stack.pop(&self.state)?);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty) => {
						self.stack.push_bool(self.aux_stack.is_empty());
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Xget) => {
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
					Opcode::AVMOpcode(AVMOpcode::Xset) => {
						let slot_num = self.stack.pop_usize(&self.state)?;
						let tup = self.aux_stack.pop_tuple(&self.state)?;
						if slot_num < tup.len() {
							let mut new_tup = tup;
							new_tup[slot_num] = self.stack.pop(&self.state)?;
							self.aux_stack.push(Value::new_tuple(new_tup));
							self.incr_pc();
							Ok(true)
						} else {
							Err(ExecutionError::new("tuple access out of bounds", &self.state, None))
						}
					}
					Opcode::AVMOpcode(AVMOpcode::Dup0) => {
						let top = self.stack.pop(&self.state)?;
						self.stack.push(top.clone());
						self.stack.push(top);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Dup1) => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						self.stack.push(snd.clone());
						self.stack.push(top);
						self.stack.push(snd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Dup2) => {
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
					Opcode::AVMOpcode(AVMOpcode::Swap1) => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						self.stack.push(top);
						self.stack.push(snd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Swap2) => {
						let top = self.stack.pop(&self.state)?;
						let snd = self.stack.pop(&self.state)?;
						let trd = self.stack.pop(&self.state)?;
						self.stack.push(top);
						self.stack.push(snd);
						self.stack.push(trd);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::IsZero) => {
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
					Opcode::AVMOpcode(AVMOpcode::BitwiseNeg) => {
						let res = self.stack.pop_uint(&self.state)?.bitwise_neg();
						self.stack.push_uint(res);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Hash) => {
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
					Opcode::AVMOpcode(AVMOpcode::Plus) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.add(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Minus) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.unchecked_sub(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Mul) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.mul(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Div) => {
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
					Opcode::AVMOpcode(AVMOpcode::Mod) => {
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
					Opcode::AVMOpcode(AVMOpcode::Sdiv) => {
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
					Opcode::AVMOpcode(AVMOpcode::Smod) => {
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
					Opcode::AVMOpcode(AVMOpcode::AddMod) => {
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
					Opcode::AVMOpcode(AVMOpcode::MulMod) => {
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
					Opcode::AVMOpcode(AVMOpcode::Exp) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.exp(&r2));
						self.incr_pc();
						Ok(true)
					}
                    Opcode::AVMOpcode(AVMOpcode::SignExtend) => {
                        let bnum = self.stack.pop_uint(&self.state)?;
                        let x = self.stack.pop_uint(&self.state)?;
                        let out = match bnum.to_usize() {
                            Some(ub) => {
                                if ub >= 31 {
                                    x
                                } else {
                                    let shifted_bit = Uint256::from_usize(2).exp(&Uint256::from_usize(8*ub+7));
                                    let sign_bit = x.bitwise_and(&shifted_bit) != Uint256::zero();
                                    let mask = shifted_bit.mul(&Uint256::from_u64(2)).sub(&Uint256::one()).ok_or_else(|| ExecutionError::new("underflow in signextend", &self.state, None))?;
                                    if sign_bit {
                                        x.bitwise_or(&mask.bitwise_neg())
                                    } else {
                                        x.bitwise_and(&mask)
                                    }
                                }
                            }
                            None => x,
                        };
                        self.stack.push_uint(out);
                        self.incr_pc();
                        Ok(true)
                    }
                    Opcode::AVMOpcode(AVMOpcode::LessThan) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1 < r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::GreaterThan) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1 > r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::SLessThan) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r1.s_less_than(&r2) { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::SGreaterThan) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_usize(if r2.s_less_than(&r1) { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Equal) => {
						let r1 = self.stack.pop(&self.state)?;
						let r2 = self.stack.pop(&self.state)?;
						self.stack.push_usize(if r1 == r2 { 1 } else { 0 });
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Type) => {
						let val = self.stack.pop(&self.state)?;
						self.stack.push_usize(val.type_insn_result());
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::BitwiseAnd) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.bitwise_and(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::BitwiseOr) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.bitwise_or(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::BitwiseXor) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(r1.bitwise_xor(&r2));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Byte) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(
							if r1 < Uint256::from_usize(32) {
								let shift_factor = Uint256::from_u64(256).exp(&Uint256::from_usize(31-r1.to_usize().unwrap()));
								r2.div(&shift_factor).unwrap().bitwise_and(&Uint256::from_usize(255))
							} else {
								Uint256::zero()
							}
						);
						self.incr_pc();
						Ok(true)
					}
                    Opcode::AVMOpcode(AVMOpcode::ShiftLeft) => {
                        let shift_big = self.stack.pop_uint(&self.state)?;
                        let value = self.stack.pop_uint(&self.state)?;
                        let result = if let Some(shift) = shift_big.to_usize() {
                            value.shift_left(shift)
                        } else {
                            Uint256::zero()
                        };
                        self.stack.push_uint(result);
                        self.incr_pc();
                        Ok(true)
                    }
                    Opcode::AVMOpcode(AVMOpcode::ShiftRight) => {
                        let shift_big = self.stack.pop_uint(&self.state)?;
                        let value = self.stack.pop_uint(&self.state)?;
                        let result = if let Some(shift) = shift_big.to_usize() {
                            value.shift_right(shift)
                        } else {
                            Uint256::zero()
                        };
                        self.stack.push_uint(result);
                        self.incr_pc();
                        Ok(true)
                    }
                    Opcode::AVMOpcode(AVMOpcode::ShiftArith) => {
                        let shift_big = self.stack.pop_uint(&self.state)?;
                        let value = self.stack.pop_uint(&self.state)?;
                        let result = if let Some(shift) = shift_big.to_usize() {
                            value.shift_arith(shift)
                        } else {
                            Uint256::zero()
                        };
                        self.stack.push_uint(result);
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
					Opcode::AVMOpcode(AVMOpcode::Hash2) => {
						let r1 = self.stack.pop_uint(&self.state)?;
						let r2 = self.stack.pop_uint(&self.state)?;
						self.stack.push_uint(Uint256::avm_hash2(&r1, &r2));
						self.incr_pc();
						Ok(true)
					}
                    Opcode::AVMOpcode(AVMOpcode::Keccakf) => {
                        let t1 = self.stack.pop_tuple(&self.state)?;
                        let t2 = tuple_keccak(t1, &self.state)?;
                        self.stack.push(Value::new_tuple(t2));
                        self.incr_pc();
                        Ok(true)
                    }
					Opcode::AVMOpcode(AVMOpcode::Inbox) => {
						match self.runtime_env.get_from_inbox() {
                            Some(msg) => {
                                self.stack.push(msg);
                                self.incr_pc();
                                Ok(true)
                            }
                            None => Ok(false)   // machine is blocked, waiting for message
                        }
					}
                    Opcode::AVMOpcode(AVMOpcode::InboxPeek) => {
                        let bn = self.stack.pop_uint(&self.state)?;
                        match self.runtime_env.peek_at_inbox_head() {
                            Some(msg) => {
                                if let Value::Tuple(tup) = msg {
                                    if let Value::Int(msg_bn) = &tup[1] {
                                        self.stack.push_bool(bn == msg_bn.clone());
                                        self.incr_pc();
                                        Ok(true)
                                    } else {
                                        Err(ExecutionError::new("inbox contents not a tuple", &self.state, None))
                                    }
                                } else {
                                    Err(ExecutionError::new("blocknum not an integer", &self.state, None))
                                }
                            }
                            None => {
                                // machine is blocked, waiting for nonempty inbox
                                self.stack.push_uint(bn);   // put stack back the way it was
                                Ok(false)
                            }
                        }
                    }
					Opcode::AVMOpcode(AVMOpcode::ErrCodePoint) => {
						self.stack.push(Value::CodePoint(
							self.code.create_segment()
						));
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Send) => {
                        let val = self.stack.pop(&self.state)?;
                        self.runtime_env.push_send(val);
                        self.incr_pc();
                        Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Log) => {
						let val = self.stack.pop(&self.state)?;
						self.runtime_env.push_log(val);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::ErrSet) => {
						let cp = self.stack.pop_codepoint(&self.state)?;
						self.err_codepoint = cp;
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::ErrPush) => {
						self.stack.push_codepoint(self.err_codepoint);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::PushInsn)=> {
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
					Opcode::AVMOpcode(AVMOpcode::PushInsnImm) => {
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
					Opcode::AVMOpcode(AVMOpcode::OpenInsn) => {
						let insn = self.code.get_insn(self.stack.pop_codepoint(&self.state)?).unwrap();
						if let Some(val) = &insn.immediate {
							self.stack.push(Value::new_tuple(vec![val.clone()]));
						} else {
							self.stack.push(Value::none());
						}
						self.stack.push_usize(insn.opcode.to_number().unwrap() as usize);
						self.incr_pc();
						Ok(true)
					}
					Opcode::AVMOpcode(AVMOpcode::Breakpoint) => {
						self.incr_pc();
						Ok(false)
					}
					Opcode::AVMOpcode(AVMOpcode::Halt) => {
						self.state = MachineState::Stopped;
						Ok(false)
					}
					Opcode::AVMOpcode(AVMOpcode::DebugPrint) => {
						let r1 = self.stack.pop(&self.state)?;
                        println!("debugprint: {}", r1);
						self.incr_pc();
						Ok(true)
					}
                    Opcode::AVMOpcode(AVMOpcode::GetGas) => {
                        self.stack.push(Value::Int(self.arb_gas_remaining.clone()));
                        self.incr_pc();
                        Ok(true)
                    },
                    Opcode::AVMOpcode(AVMOpcode::SetGas) => {
                        let gas = self.stack.pop_uint(&self.state)?;
                        self.arb_gas_remaining = gas;
                        self.incr_pc();
                        Ok(true)
                    },
                    Opcode::AVMOpcode(AVMOpcode::Sideload) => {
                        self.stack.push(Value::none());
                        self.incr_pc();
                        Ok(true)
                    }
                    Opcode::AVMOpcode(AVMOpcode::EcRecover) => {
                        let _first_half = self.stack.pop_uint(&self.state)?;
                        let _second_half = self.stack.pop_uint(&self.state)?;
                        let _recover_id = self.stack.pop_uint(&self.state)?;
                        let _msg_hash = self.stack.pop_uint(&self.state)?;
                        let result = do_ecrecover(_first_half, _second_half, _recover_id, _msg_hash);
                        self.stack.push_uint(result);
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

fn tuple_keccak(intup: Vec<Value>, state: &MachineState) -> Result<Vec<Value>, ExecutionError> {
    let mask64 = Uint256::from_u64(((1 << 32) + 1) * ((1 << 32) - 1));
    let two_to_64 = mask64.add(&Uint256::one());
    if intup.len() != 7 {
        println!("keccakf operand length: {}", intup.len());
        return Err(ExecutionError::new(
            "invalid tuple length for keccakf: ",
            state,
            None,
        ));
    }
    let mut inuis = vec![];
    for inelt in intup {
        inuis.push(if let Value::Int(ui) = inelt {
            ui
        } else {
            return Err(ExecutionError::new(
                "invalid operand for keccakf",
                state,
                None,
            ));
        });
    }
    let mut outtup = [0u64; 25];
    for i in 0..25 {
        outtup[i] = inuis[i / 4].bitwise_and(&mask64).trim_to_u64();
        inuis[i / 4] = inuis[i / 4].div(&two_to_64).unwrap(); // safe because denom not zero
    }
    keccak::f1600(&mut outtup);

    for i in 0..6 {
        inuis[i] = Uint256::from_u64(outtup[4 * i + 3]);
        for j in (0..3).rev() {
            inuis[i] = inuis[i]
                .mul(&two_to_64)
                .add(&Uint256::from_u64(outtup[4 * i + j]));
        }
    }
    inuis[6] = Uint256::from_u64(outtup[24]);
    let ret = inuis.iter().map(|ui| Value::Int(ui.clone())).collect();
    Ok(ret)
}

fn do_ecrecover(
    first_half: Uint256,
    second_half: Uint256,
    recover_id: Uint256,
    msg_hash: Uint256,
) -> Uint256 {
    let sig = Signature {
        r: H256(first_half.to_bytes_be()[..32].try_into().unwrap()),
        s: H256(second_half.to_bytes_be()[..32].try_into().unwrap()),
        v: if recover_id == Uint256::zero() {
            27u64
        } else {
            28u64
        },
    };
    let hash_to_check = H256(msg_hash.to_bytes_be()[..32].try_into().unwrap());
    match sig.recover(hash_to_check) {
        Ok(addr) => Uint256::from_bytes(&addr.0),
        Err(_) => Uint256::zero(),
    }
}

///Represents a stack trace, with each CodePt indicating a stack frame, Unknown variant is unused.
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
