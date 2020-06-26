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

use crate::compile::MiniProperties;
use crate::evm::runtime_func_name;
use crate::pos::Location;
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Label {
    Func(StringId),
    Anon(usize),
    External(usize), // slot in imported funcs list
    Runtime(usize),  // function exported by the trusted runtime
    Evm(usize),      // program counter in EVM contract
}

impl Label {
    pub fn relocate(
        self,
        int_offset: usize,
        ext_offset: usize,
        func_offset: usize,
    ) -> (Self, usize) {
        match self {
            Label::Func(sid) => (Label::Func(sid + func_offset), sid + func_offset),
            Label::Anon(pc) => (Label::Anon(pc + int_offset), func_offset),
            Label::External(slot) => (Label::External(slot + ext_offset), func_offset),
            Label::Runtime(_) => (self, func_offset),
            Label::Evm(_) => (self, func_offset),
        }
    }

    pub fn avm_hash(&self) -> Value {
        match self {
            Label::Func(sid) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(4)),
                &Value::Int(Uint256::from_usize(*sid)),
            ),
            Label::Anon(n) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(5)),
                &Value::Int(Uint256::from_usize(*n)),
            ),
            Label::External(n) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(6)),
                &Value::Int(Uint256::from_usize(*n)),
            ),
            Label::Runtime(_) => {
                panic!("tried to avm_hash a runtime call index");
            }
            Label::Evm(_) => {
                panic!("tried to avm_hash an EVM label");
            }
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Label::Func(sid) => write!(f, "function_{}", sid),
            Label::Anon(n) => write!(f, "label_{}", n),
            Label::External(slot) => write!(f, "external_{}", slot),
            Label::Runtime(slot) => write!(f, "{}", runtime_func_name(*slot)),
            Label::Evm(pc) => write!(f, "EvmPC({})", pc),
        }
    }
}

#[derive(Default)]
pub struct LabelGenerator {
    next: usize,
}

impl LabelGenerator {
    pub fn new() -> Self {
        LabelGenerator { next: 0 }
    }

    pub fn next(self) -> (Label, Self) {
        (
            Label::Anon(self.next),
            LabelGenerator {
                next: self.next + 1,
            },
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instruction {
    pub opcode: Opcode,
    pub immediate: Option<Value>,
    pub location: Option<Location>,
}

impl MiniProperties for Instruction {
    fn is_pure(&self) -> bool {
        self.opcode.is_pure()
    }
}

impl Instruction {
    pub fn new(opcode: Opcode, immediate: Option<Value>, location: Option<Location>) -> Self {
        Instruction {
            opcode,
            immediate,
            location,
        }
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
            _ => None,
        }
    }

    pub fn replace_labels(self, label_map: &HashMap<Label, CodePt>) -> Result<Self, Label> {
        match self.immediate {
            Some(val) => Ok(Instruction::from_opcode_imm(
                self.opcode,
                val.replace_labels(label_map)?,
                self.location,
            )),
            None => Ok(self),
        }
    }

    pub fn relocate(
        self,
        int_offset: usize,
        ext_offset: usize,
        func_offset: usize,
        globals_offset: usize,
    ) -> (Self, usize) {
        let mut max_func_offset = func_offset;
        let opcode = match self.opcode {
            Opcode::PushExternal(off) => Opcode::PushExternal(off + ext_offset),
            Opcode::Label(label) => {
                let (new_label, new_func_offset) =
                    label.relocate(int_offset, ext_offset, func_offset);
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
        (
            Instruction::new(opcode, imm, self.location),
            max_func_offset,
        )
    }

    pub fn xlate_labels(self, xlate_map: &HashMap<Label, &Label>) -> Self {
        match self.immediate {
            Some(val) => Instruction::from_opcode_imm(
                self.opcode,
                val.xlate_labels(xlate_map),
                self.location,
            ),
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
            },
            None => match self.location {
                Some(loc) => write!(f, "{}\t\t{}", self.opcode, loc),
                None => write!(f, "{}", self.opcode),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CodePt {
    Internal(usize),
    External(usize),         // slot in imported funcs list
    Runtime(usize),          // slot in runtime funcs list
    InSegment(usize, usize), // in code segment, at offset
    Null,                    // initial value of the Error Codepoint register
}

impl CodePt {
    pub fn new_internal(pc: usize) -> Self {
        CodePt::Internal(pc)
    }

    pub fn new_external(name: StringId) -> Self {
        CodePt::External(name)
    }

    pub fn new_runtime(slot: usize) -> Self {
        CodePt::Runtime(slot)
    }

    pub fn new_in_segment(seg_num: usize, offset: usize) -> Self {
        CodePt::InSegment(seg_num, offset)
    }

    pub fn incr(&self) -> Option<Self> {
        match self {
            CodePt::Internal(pc) => Some(CodePt::Internal(pc + 1)),
            CodePt::InSegment(seg, offset) => {
                if *offset == 0 {
                    None
                } else {
                    Some(CodePt::InSegment(*seg, offset - 1))
                }
            }
            CodePt::External(_) => None,
            CodePt::Runtime(_) => None,
            CodePt::Null => None,
        }
    }

    /*
    pub fn pc_if_internal(&self) -> Option<usize> {
        if let CodePt::Internal(pc) = self {
            Some(*pc)
        } else {
            None
        }
    }
    */

    pub fn relocate(self, int_offset: usize, ext_offset: usize) -> Self {
        match self {
            CodePt::Internal(pc) => CodePt::Internal(pc + int_offset),
            CodePt::External(off) => CodePt::External(off + ext_offset),
            CodePt::Runtime(_) => self,
            CodePt::InSegment(_, _) => {
                panic!("tried to relocate/link code at runtime");
            }
            CodePt::Null => {
                panic!("tried to relocate/link null codepoint");
            }
        }
    }

    pub fn avm_hash(&self) -> Value {
        match self {
            CodePt::Internal(sz) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(3)),
                &Value::Int(Uint256::from_usize(*sz)),
            ),
            CodePt::External(_) => {
                panic!("tried to avm_hash unlinked codepoint");
            }
            CodePt::Runtime(sz) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(5)),
                &Value::Int(Uint256::from_usize(*sz)),
            ),
            CodePt::InSegment(_, _) => {
                panic!("avm_hash not yet implemented for in-module codepoints");
            }
            CodePt::Null => Value::Int(Uint256::zero()),
        }
    }
}

impl fmt::Display for CodePt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CodePt::Internal(pc) => write!(f, "Internal({})", pc),
            CodePt::External(idx) => write!(f, "External({})", idx),
            CodePt::Runtime(slot) => write!(f, "{}", runtime_func_name(*slot)),
            CodePt::InSegment(seg, offset) => write!(f, "(segment {}, offset {})", seg, offset),
            CodePt::Null => write!(f, "Null"),
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

    pub fn type_insn_result(&self) -> usize {
        match self {
            Value::Int(_) => 0,
            Value::CodePoint(_) => 1,
            Value::Tuple(_) => 3,
            Value::Label(_) => {
                panic!("tried to run type instruction on a label");
            }
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
            }
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

    pub fn relocate(
        self,
        int_offset: usize,
        ext_offset: usize,
        func_offset: usize,
    ) -> (Self, usize) {
        match self {
            Value::Int(_) => (self, 0),
            Value::Tuple(v) => {
                let mut rel_v = Vec::new();
                let mut max_func_offset = 0;
                for val in v {
                    let (new_val, new_func_offset) =
                        val.relocate(int_offset, ext_offset, func_offset);
                    rel_v.push(new_val);
                    if (max_func_offset < new_func_offset) {
                        max_func_offset = new_func_offset;
                    }
                }
                (Value::Tuple(rel_v), max_func_offset)
            }
            Value::CodePoint(cpt) => (Value::CodePoint(cpt.relocate(int_offset, ext_offset)), 0),
            Value::Label(label) => {
                let (new_label, new_func_offset) =
                    label.relocate(int_offset, ext_offset, func_offset);
                (Value::Label(new_label), new_func_offset)
            }
        }
    }

    pub fn xlate_labels(self, label_map: &HashMap<Label, &Label>) -> Self {
        match self {
            Value::Int(_) | Value::CodePoint(_) => self,
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
            },
        }
    }

    pub fn to_usize(&self) -> Option<usize> {
        match self {
            Value::Int(i) => i.to_usize(),
            _ => None,
        }
    }

    pub fn avm_hash(&self) -> Value {
        //BUGBUG: should do same hash as AVM
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
            Value::Label(label) => {
                Value::avm_hash2(&Value::Int(Uint256::from_usize(2)), &label.avm_hash())
            }
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
            Value::CodePoint(pc) => write!(f, "CodePoint({})", pc),
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
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
    PushExternal(usize), // push codeptr of external function -- index in imported_funcs
    TupleGet(usize),     // arg is size of anysize_tuple
    TupleSet(usize),     // arg is size of anysize_tuple
    ArrayGet,
    UncheckedFixedArrayGet(usize), // arg is size of array
    GetGlobalVar(usize),
    SetGlobalVar(usize),
    Tset,
    Tget,
    Tlen,
    Pop,
    StackEmpty,
    AuxPush,
    AuxPop,
    AuxStackEmpty,
    Xget,
    Xset,
    Dup0,
    Dup1,
    Dup2,
    Swap1,
    Swap2,
    Return,
    IsZero,
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
    AddMod,
    MulMod,
    Exp,
    LessThan,
    GreaterThan,
    SLessThan,
    SGreaterThan,
    Equal,
    NotEqual,
    Byte,
    SignExtend,
    Type,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    GetTime,
    Inbox,
    ErrCodePoint,
    PushInsn,
    PushInsnImm,
    OpenInsn,
    Halt,
    Send,
    Log,
    ErrSet,
    ErrPush,
    Breakpoint,
    DebugPrint,
    SetGas,
    GetGas,
}

impl MiniProperties for Opcode {
    fn is_pure(&self) -> bool {
        match self {
            Opcode::Log
            | Opcode::Inbox
            | Opcode::Send
            | Opcode::GetTime
            | Opcode::Rset
            | Opcode::Rget
            | Opcode::PushInsn
            | Opcode::PushInsnImm
            | Opcode::ErrCodePoint
            | Opcode::ErrSet
            | Opcode::ErrPush
            | Opcode::SetGas
            | Opcode::GetGas
            | Opcode::Jump
            | Opcode::Cjump => false,
            _ => true,
        }
    }
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
            "stackempty" => Opcode::StackEmpty,
            "auxpush" => Opcode::AuxPush,
            "auxpop" => Opcode::AuxPop,
            "auxstackempty" => Opcode::AuxStackEmpty,
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
            "length" => Opcode::Len,
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
            "iszero" => Opcode::IsZero,
            "byte" => Opcode::Byte,
            "bitwiseand" => Opcode::BitwiseAnd,
            "bitwiseor" => Opcode::BitwiseOr,
            "bitwisexor" => Opcode::BitwiseXor,
            "logicaland" => Opcode::LogicalAnd,
            "logicalor" => Opcode::LogicalOr,
            "gettime" => Opcode::GetTime,
            "inbox" => Opcode::Inbox,
            "jump" => Opcode::Jump,
            "log" => Opcode::Log,
            "errcodept" => Opcode::ErrCodePoint,
            "pushinsn" => Opcode::PushInsn,
            "pushinsnimm" => Opcode::PushInsnImm,
            "openinsn" => Opcode::OpenInsn,
            "debugprint" => Opcode::DebugPrint,
            "setgas" => Opcode::SetGas,
            "getgas" => Opcode::GetGas,
            _ => {
                panic!("opcode not supported in asm segment: {}", name);
            }
        }
    }

    pub fn from_number(num: usize) -> Option<Self> {
        match num {
            0x01 => Some(Opcode::Plus),
            0x02 => Some(Opcode::Mul),
            0x03 => Some(Opcode::Minus),
            0x04 => Some(Opcode::Div),
            0x05 => Some(Opcode::Sdiv),
            0x06 => Some(Opcode::Mod),
            0x07 => Some(Opcode::Smod),
            0x08 => Some(Opcode::AddMod),
            0x09 => Some(Opcode::MulMod),
            0x0a => Some(Opcode::Exp),
            0x10 => Some(Opcode::LessThan),
            0x11 => Some(Opcode::GreaterThan),
            0x12 => Some(Opcode::SLessThan),
            0x13 => Some(Opcode::SGreaterThan),
            0x14 => Some(Opcode::Equal),
            0x15 => Some(Opcode::IsZero),
            0x16 => Some(Opcode::BitwiseAnd),
            0x17 => Some(Opcode::BitwiseOr),
            0x18 => Some(Opcode::BitwiseXor),
            0x19 => Some(Opcode::BitwiseNeg),
            0x1a => Some(Opcode::Byte),
            0x1b => Some(Opcode::SignExtend),
            0x1c => Some(Opcode::NotEqual), //BUGBUG: this should be eliminated, doesn't exist in AVM
            0x20 => Some(Opcode::Hash),
            0x21 => Some(Opcode::Type),
            0x22 => Some(Opcode::Hash2),
            0x30 => Some(Opcode::Pop),
            0x31 => Some(Opcode::PushStatic),
            0x32 => Some(Opcode::Rget),
            0x33 => Some(Opcode::Rset),
            0x34 => Some(Opcode::Jump),
            0x35 => Some(Opcode::Cjump),
            0x36 => Some(Opcode::StackEmpty),
            0x37 => Some(Opcode::GetPC),
            0x38 => Some(Opcode::AuxPush),
            0x39 => Some(Opcode::AuxPop),
            0x3a => Some(Opcode::AuxStackEmpty),
            0x3b => Some(Opcode::Noop),
            0x3c => Some(Opcode::ErrPush),
            0x3d => Some(Opcode::ErrSet),
            0x40 => Some(Opcode::Dup0),
            0x41 => Some(Opcode::Dup1),
            0x42 => Some(Opcode::Dup2),
            0x43 => Some(Opcode::Swap1),
            0x44 => Some(Opcode::Swap2),
            0x50 => Some(Opcode::Tget),
            0x51 => Some(Opcode::Tset),
            0x52 => Some(Opcode::Tlen),
            0x53 => Some(Opcode::Xget),
            0x54 => Some(Opcode::Xset),
            0x60 => Some(Opcode::Breakpoint),
            0x61 => Some(Opcode::Log),
            0x70 => Some(Opcode::Send),
            0x71 => Some(Opcode::GetTime),
            0x72 => Some(Opcode::Inbox),
            0x73 => Some(Opcode::Panic),
            0x74 => Some(Opcode::Halt),
            0x75 => Some(Opcode::SetGas),
            0x76 => Some(Opcode::GetGas),
            0x77 => Some(Opcode::ErrCodePoint),
            0x78 => Some(Opcode::PushInsn),
            0x79 => Some(Opcode::PushInsnImm),
            0x7a => Some(Opcode::OpenInsn),
            0x90 => Some(Opcode::DebugPrint),
            _ => None,
        }
    }

    pub fn to_number(&self) -> Option<u8> {
        match self {
            Opcode::Plus => Some(0x01),
            Opcode::Mul => Some(0x02),
            Opcode::Minus => Some(0x03),
            Opcode::Div => Some(0x04),
            Opcode::Sdiv => Some(0x05),
            Opcode::Mod => Some(0x06),
            Opcode::Smod => Some(0x07),
            Opcode::AddMod => Some(0x08),
            Opcode::MulMod => Some(0x09),
            Opcode::Exp => Some(0x0a),
            Opcode::LessThan => Some(0x10),
            Opcode::GreaterThan => Some(0x11),
            Opcode::SLessThan => Some(0x012),
            Opcode::SGreaterThan => Some(0x13),
            Opcode::Equal => Some(0x14),
            Opcode::IsZero => Some(0x15),
            Opcode::BitwiseAnd => Some(0x16),
            Opcode::BitwiseOr => Some(0x17),
            Opcode::BitwiseXor => Some(0x18),
            Opcode::BitwiseNeg => Some(0x19),
            Opcode::Byte => Some(0x1a),
            Opcode::SignExtend => Some(0x1b),
            Opcode::NotEqual => Some(0x1c),
            Opcode::Hash => Some(0x20),
            Opcode::Type => Some(0x21),
            Opcode::Hash2 => Some(0x22),
            Opcode::Pop => Some(0x30),
            Opcode::PushStatic => Some(0x31),
            Opcode::Rget => Some(0x32),
            Opcode::Rset => Some(0x33),
            Opcode::Jump => Some(0x34),
            Opcode::Cjump => Some(0x35),
            Opcode::StackEmpty => Some(0x36),
            Opcode::GetPC => Some(0x37),
            Opcode::AuxPush => Some(0x38),
            Opcode::AuxPop => Some(0x39),
            Opcode::AuxStackEmpty => Some(0x3a),
            Opcode::Noop => Some(0x3b),
            Opcode::ErrPush => Some(0x3c),
            Opcode::ErrSet => Some(0x3d),
            Opcode::Dup0 => Some(0x40),
            Opcode::Dup1 => Some(0x41),
            Opcode::Dup2 => Some(0x42),
            Opcode::Swap1 => Some(0x43),
            Opcode::Swap2 => Some(0x44),
            Opcode::Tget => Some(0x50),
            Opcode::Tset => Some(0x51),
            Opcode::Tlen => Some(0x52),
            Opcode::Xget => Some(0x53),
            Opcode::Xset => Some(0x54),
            Opcode::Breakpoint => Some(0x60),
            Opcode::Log => Some(0x61),
            Opcode::Send => Some(0x70),
            Opcode::GetTime => Some(0x71),
            Opcode::Inbox => Some(0x72),
            Opcode::Panic => Some(0x73),
            Opcode::Halt => Some(0x74),
            Opcode::SetGas => Some(0x75),
            Opcode::GetGas => Some(0x76),
            Opcode::ErrCodePoint => Some(0x77),
            Opcode::PushInsn => Some(0x78),
            Opcode::PushInsnImm => Some(0x79),
            Opcode::OpenInsn => Some(0x7a),
            Opcode::DebugPrint => Some(0x90),
            _ => None,
        }
    }
}

#[test]
fn test_consistent_opcode_numbers() {
    for i in 0..256 {
        if let Some(op) = Opcode::from_number(i) {
            assert_eq!(i as u8, op.to_number().unwrap());
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
