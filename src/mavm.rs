/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, MiniProperties};
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::{collections::HashMap, fmt, rc::Rc};

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Label {
    Func(StringId),
    Anon(usize),
    External(usize), // slot in imported funcs list
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
    pub debug_info: DebugInfo,
}

impl MiniProperties for Instruction {
    fn is_pure(&self) -> bool {
        self.opcode.is_pure()
    }
}

impl Instruction {
    pub fn new(opcode: Opcode, immediate: Option<Value>, debug_info: DebugInfo) -> Self {
        Instruction {
            opcode,
            immediate,
            debug_info,
        }
    }

    pub fn from_opcode(opcode: Opcode, debug_info: DebugInfo) -> Self {
        Instruction::new(opcode, None, debug_info)
    }

    pub fn from_opcode_imm(opcode: Opcode, immediate: Value, debug_info: DebugInfo) -> Self {
        Instruction::new(opcode, Some(immediate), debug_info)
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
                self.debug_info,
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
            Instruction::new(opcode, imm, self.debug_info),
            max_func_offset,
        )
    }

    pub fn xlate_labels(self, xlate_map: &HashMap<Label, &Label>) -> Self {
        match self.immediate {
            Some(val) => Instruction::from_opcode_imm(
                self.opcode,
                val.xlate_labels(xlate_map),
                self.debug_info,
            ),
            None => self,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.immediate {
            Some(v) => match self.debug_info.location {
                Some(loc) => write!(f, "[{}] {}\t\t{}", v, self.opcode, loc),
                None => write!(f, "[{}] {}\t\t[no location]", v, self.opcode),
            },
            None => match self.debug_info.location {
                Some(loc) => write!(f, "{}\t\t{}", self.opcode, loc),
                None => write!(f, "{}", self.opcode),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum CodePt {
    Internal(usize),
    External(usize),         // slot in imported funcs list
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
            CodePt::Null => None,
        }
    }

    pub fn relocate(self, int_offset: usize, ext_offset: usize) -> Self {
        match self {
            CodePt::Internal(pc) => CodePt::Internal(pc + int_offset),
            CodePt::External(off) => CodePt::External(off + ext_offset),
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
            CodePt::InSegment(_, _) => {
                unimplemented!("avm_hash for in-module codepoints");
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
            CodePt::InSegment(seg, offset) => write!(f, "(segment {}, offset {})", seg, offset),
            CodePt::Null => write!(f, "Null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Value {
    Int(Uint256),
    Tuple(Rc<Vec<Value>>),
    CodePoint(CodePt),
    Label(Label),
}

impl Value {
    pub fn none() -> Self {
        Value::Tuple(Rc::new(vec![]))
    }

    pub fn _is_none(&self) -> bool {
        if let Value::Tuple(v) = self {
            v.is_empty()
        } else {
            false
        }
    }

    pub fn new_tuple(v: Vec<Value>) -> Self {
        Value::Tuple(Rc::new(v))
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
                Ok(Value::new_tuple(new_vec))
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
                for val in &*v {
                    let (new_val, new_func_offset) =
                        val.clone().relocate(int_offset, ext_offset, func_offset);
                    rel_v.push(new_val);
                    if (max_func_offset < new_func_offset) {
                        max_func_offset = new_func_offset;
                    }
                }
                (Value::new_tuple(rel_v), max_func_offset)
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
                for val in &*v {
                    newv.push(val.clone().xlate_labels(label_map));
                }
                Value::new_tuple(newv)
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
                for val in &*v.clone() {
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
        Value::new_tuple(vec![v1.clone(), v2.clone()]).avm_hash()
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
    GetLocal,
    SetLocal,
    MakeFrame(usize, usize),
    Label(Label),
    PushExternal(usize), // push codeptr of external function -- index in imported_funcs
    TupleGet(usize),     // arg is size of anysize_tuple
    TupleSet(usize),     // arg is size of anysize_tuple
    ArrayGet,
    UncheckedFixedArrayGet(usize), // arg is size of array
    GetGlobalVar(usize),
    SetGlobalVar(usize),
    Return,
    UnaryMinus,
    Len,
    LogicalAnd,
    LogicalOr,
    AVMOpcode(AVMOpcode),
}

#[derive(Debug, Clone, Copy, Serialize_repr, Deserialize_repr, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum AVMOpcode {
    Plus = 0x01,
    Mul,
    Minus,
    Div,
    Sdiv,
    Mod,
    Smod,
    AddMod,
    MulMod,
    Exp,
    SignExtend,
    LessThan = 0x10,
    GreaterThan,
    SLessThan,
    SGreaterThan,
    Equal,
    IsZero,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNeg,
    Byte,
    ShiftLeft,
    ShiftRight,
    ShiftArith,
    Hash = 0x20,
    Type,
    Hash2,
    Keccakf,
    Sha256f,
    Pop = 0x30,
    PushStatic,
    Rget,
    Rset,
    Jump,
    Cjump,
    StackEmpty,
    GetPC,
    AuxPush,
    AuxPop,
    AuxStackEmpty,
    Noop,
    ErrPush,
    ErrSet,
    Dup0 = 0x40,
    Dup1,
    Dup2,
    Swap1,
    Swap2,
    Tget = 0x50,
    Tset,
    Tlen,
    Xget,
    Xset,
    Breakpoint = 0x60,
    Log,
    Send = 0x70,
    InboxPeek,
    Inbox,
    Panic,
    Halt,
    SetGas,
    GetGas,
    ErrCodePoint,
    PushInsn,
    PushInsnImm,
    OpenInsn,
    Sideload,
    EcRecover = 0x80,
    EcAdd,
    EcMul,
    EcPairing,
    DebugPrint = 0x90,
}

impl MiniProperties for Opcode {
    fn is_pure(&self) -> bool {
        match self {
            Opcode::AVMOpcode(AVMOpcode::Log)
            | Opcode::AVMOpcode(AVMOpcode::Inbox)
            | Opcode::AVMOpcode(AVMOpcode::InboxPeek)
            | Opcode::AVMOpcode(AVMOpcode::Send)
            | Opcode::AVMOpcode(AVMOpcode::Rset)
            | Opcode::AVMOpcode(AVMOpcode::Rget)
            | Opcode::AVMOpcode(AVMOpcode::PushInsn)
            | Opcode::AVMOpcode(AVMOpcode::PushInsnImm)
            | Opcode::AVMOpcode(AVMOpcode::ErrCodePoint)
            | Opcode::AVMOpcode(AVMOpcode::ErrSet)
            | Opcode::AVMOpcode(AVMOpcode::ErrPush)
            | Opcode::AVMOpcode(AVMOpcode::SetGas)
            | Opcode::AVMOpcode(AVMOpcode::GetGas)
            | Opcode::AVMOpcode(AVMOpcode::Jump)
            | Opcode::AVMOpcode(AVMOpcode::Cjump)
            | Opcode::AVMOpcode(AVMOpcode::AuxPop)
            | Opcode::AVMOpcode(AVMOpcode::AuxPush)
            | Opcode::AVMOpcode(AVMOpcode::Sideload) => false,
            _ => true,
        }
    }
}

impl Opcode {
    pub fn from_name(name: &str) -> Self {
        match name {
            "rget" => Opcode::AVMOpcode(AVMOpcode::Rget),
            "rset" => Opcode::AVMOpcode(AVMOpcode::Rset),
            "pushstatic" => Opcode::AVMOpcode(AVMOpcode::PushStatic),
            "tset" => Opcode::AVMOpcode(AVMOpcode::Tset),
            "tget" => Opcode::AVMOpcode(AVMOpcode::Tget),
            "pop" => Opcode::AVMOpcode(AVMOpcode::Pop),
            "stackempty" => Opcode::AVMOpcode(AVMOpcode::StackEmpty),
            "auxpush" => Opcode::AVMOpcode(AVMOpcode::AuxPush),
            "auxpop" => Opcode::AVMOpcode(AVMOpcode::AuxPop),
            "auxstackempty" => Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty),
            "xget" => Opcode::AVMOpcode(AVMOpcode::Xget),
            "xset" => Opcode::AVMOpcode(AVMOpcode::Xset),
            "dup0" => Opcode::AVMOpcode(AVMOpcode::Dup0),
            "dup1" => Opcode::AVMOpcode(AVMOpcode::Dup1),
            "dup2" => Opcode::AVMOpcode(AVMOpcode::Dup2),
            "swap1" => Opcode::AVMOpcode(AVMOpcode::Swap1),
            "swap2" => Opcode::AVMOpcode(AVMOpcode::Swap2),
            "unaryminus" => Opcode::UnaryMinus,
            "bitwiseneg" => Opcode::AVMOpcode(AVMOpcode::BitwiseNeg),
            "hash" => Opcode::AVMOpcode(AVMOpcode::Hash),
            "hash2" => Opcode::AVMOpcode(AVMOpcode::Hash2),
            "keccakf" => Opcode::AVMOpcode(AVMOpcode::Keccakf),
            "sha256f" => Opcode::AVMOpcode(AVMOpcode::Sha256f),
            "length" => Opcode::AVMOpcode(AVMOpcode::Tlen),
            "plus" => Opcode::AVMOpcode(AVMOpcode::Plus),
            "minus" => Opcode::AVMOpcode(AVMOpcode::Minus),
            "mul" => Opcode::AVMOpcode(AVMOpcode::Mul),
            "div" => Opcode::AVMOpcode(AVMOpcode::Div),
            "mod" => Opcode::AVMOpcode(AVMOpcode::Mod),
            "sdiv" => Opcode::AVMOpcode(AVMOpcode::Sdiv),
            "smod" => Opcode::AVMOpcode(AVMOpcode::Smod),
            "exp" => Opcode::AVMOpcode(AVMOpcode::Exp),
            "lt" => Opcode::AVMOpcode(AVMOpcode::LessThan),
            "gt" => Opcode::AVMOpcode(AVMOpcode::GreaterThan),
            "slt" => Opcode::AVMOpcode(AVMOpcode::SLessThan),
            "sgt" => Opcode::AVMOpcode(AVMOpcode::SGreaterThan),
            "eq" => Opcode::AVMOpcode(AVMOpcode::Equal),
            "iszero" => Opcode::AVMOpcode(AVMOpcode::IsZero),
            "byte" => Opcode::AVMOpcode(AVMOpcode::Byte),
            "signextend" => Opcode::AVMOpcode(AVMOpcode::SignExtend),
            "shl" => Opcode::AVMOpcode(AVMOpcode::ShiftLeft),
            "shr" => Opcode::AVMOpcode(AVMOpcode::ShiftRight),
            "sar" => Opcode::AVMOpcode(AVMOpcode::ShiftArith),
            "bitwiseand" => Opcode::AVMOpcode(AVMOpcode::BitwiseAnd),
            "bitwiseor" => Opcode::AVMOpcode(AVMOpcode::BitwiseOr),
            "bitwisexor" => Opcode::AVMOpcode(AVMOpcode::BitwiseXor),
            "logicaland" => Opcode::LogicalAnd,
            "logicalor" => Opcode::LogicalOr,
            "inbox" => Opcode::AVMOpcode(AVMOpcode::Inbox),
            "inboxpeek" => Opcode::AVMOpcode(AVMOpcode::InboxPeek),
            "jump" => Opcode::AVMOpcode(AVMOpcode::Jump),
            "log" => Opcode::AVMOpcode(AVMOpcode::Log),
            "send" => Opcode::AVMOpcode(AVMOpcode::Send),
            "errcodept" => Opcode::AVMOpcode(AVMOpcode::ErrCodePoint),
            "pushinsn" => Opcode::AVMOpcode(AVMOpcode::PushInsn),
            "pushinsnimm" => Opcode::AVMOpcode(AVMOpcode::PushInsnImm),
            "openinsn" => Opcode::AVMOpcode(AVMOpcode::OpenInsn),
            "debugprint" => Opcode::AVMOpcode(AVMOpcode::DebugPrint),
            "setgas" => Opcode::AVMOpcode(AVMOpcode::SetGas),
            "getgas" => Opcode::AVMOpcode(AVMOpcode::GetGas),
            "errset" => Opcode::AVMOpcode(AVMOpcode::ErrSet),
            "sideload" => Opcode::AVMOpcode(AVMOpcode::Sideload),
            "ecrecover" => Opcode::AVMOpcode(AVMOpcode::EcRecover),
            "ecadd" => Opcode::AVMOpcode(AVMOpcode::EcAdd),
            "ecmul" => Opcode::AVMOpcode(AVMOpcode::EcMul),
            "ecpairing" => Opcode::AVMOpcode(AVMOpcode::EcPairing),
            _ => {
                panic!("opcode not supported in asm segment: {}", name);
            }
        }
    }

    pub fn from_number(num: usize) -> Option<Self> {
        match num {
            0x01 => Some(Opcode::AVMOpcode(AVMOpcode::Plus)),
            0x02 => Some(Opcode::AVMOpcode(AVMOpcode::Mul)),
            0x03 => Some(Opcode::AVMOpcode(AVMOpcode::Minus)),
            0x04 => Some(Opcode::AVMOpcode(AVMOpcode::Div)),
            0x05 => Some(Opcode::AVMOpcode(AVMOpcode::Sdiv)),
            0x06 => Some(Opcode::AVMOpcode(AVMOpcode::Mod)),
            0x07 => Some(Opcode::AVMOpcode(AVMOpcode::Smod)),
            0x08 => Some(Opcode::AVMOpcode(AVMOpcode::AddMod)),
            0x09 => Some(Opcode::AVMOpcode(AVMOpcode::MulMod)),
            0x0a => Some(Opcode::AVMOpcode(AVMOpcode::Exp)),
            0x0b => Some(Opcode::AVMOpcode(AVMOpcode::SignExtend)),
            0x10 => Some(Opcode::AVMOpcode(AVMOpcode::LessThan)),
            0x11 => Some(Opcode::AVMOpcode(AVMOpcode::GreaterThan)),
            0x12 => Some(Opcode::AVMOpcode(AVMOpcode::SLessThan)),
            0x13 => Some(Opcode::AVMOpcode(AVMOpcode::SGreaterThan)),
            0x14 => Some(Opcode::AVMOpcode(AVMOpcode::Equal)),
            0x15 => Some(Opcode::AVMOpcode(AVMOpcode::IsZero)),
            0x16 => Some(Opcode::AVMOpcode(AVMOpcode::BitwiseAnd)),
            0x17 => Some(Opcode::AVMOpcode(AVMOpcode::BitwiseOr)),
            0x18 => Some(Opcode::AVMOpcode(AVMOpcode::BitwiseXor)),
            0x19 => Some(Opcode::AVMOpcode(AVMOpcode::BitwiseNeg)),
            0x1a => Some(Opcode::AVMOpcode(AVMOpcode::Byte)),
            0x20 => Some(Opcode::AVMOpcode(AVMOpcode::Hash)),
            0x21 => Some(Opcode::AVMOpcode(AVMOpcode::Type)),
            0x22 => Some(Opcode::AVMOpcode(AVMOpcode::Hash2)),
            0x23 => Some(Opcode::AVMOpcode(AVMOpcode::Keccakf)),
            0x30 => Some(Opcode::AVMOpcode(AVMOpcode::Pop)),
            0x31 => Some(Opcode::AVMOpcode(AVMOpcode::PushStatic)),
            0x32 => Some(Opcode::AVMOpcode(AVMOpcode::Rget)),
            0x33 => Some(Opcode::AVMOpcode(AVMOpcode::Rset)),
            0x34 => Some(Opcode::AVMOpcode(AVMOpcode::Jump)),
            0x35 => Some(Opcode::AVMOpcode(AVMOpcode::Cjump)),
            0x36 => Some(Opcode::AVMOpcode(AVMOpcode::StackEmpty)),
            0x37 => Some(Opcode::AVMOpcode(AVMOpcode::GetPC)),
            0x38 => Some(Opcode::AVMOpcode(AVMOpcode::AuxPush)),
            0x39 => Some(Opcode::AVMOpcode(AVMOpcode::AuxPop)),
            0x3a => Some(Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty)),
            0x3b => Some(Opcode::AVMOpcode(AVMOpcode::Noop)),
            0x3c => Some(Opcode::AVMOpcode(AVMOpcode::ErrPush)),
            0x3d => Some(Opcode::AVMOpcode(AVMOpcode::ErrSet)),
            0x40 => Some(Opcode::AVMOpcode(AVMOpcode::Dup0)),
            0x41 => Some(Opcode::AVMOpcode(AVMOpcode::Dup1)),
            0x42 => Some(Opcode::AVMOpcode(AVMOpcode::Dup2)),
            0x43 => Some(Opcode::AVMOpcode(AVMOpcode::Swap1)),
            0x44 => Some(Opcode::AVMOpcode(AVMOpcode::Swap2)),
            0x50 => Some(Opcode::AVMOpcode(AVMOpcode::Tget)),
            0x51 => Some(Opcode::AVMOpcode(AVMOpcode::Tset)),
            0x52 => Some(Opcode::AVMOpcode(AVMOpcode::Tlen)),
            0x53 => Some(Opcode::AVMOpcode(AVMOpcode::Xget)),
            0x54 => Some(Opcode::AVMOpcode(AVMOpcode::Xset)),
            0x60 => Some(Opcode::AVMOpcode(AVMOpcode::Breakpoint)),
            0x61 => Some(Opcode::AVMOpcode(AVMOpcode::Log)),
            0x70 => Some(Opcode::AVMOpcode(AVMOpcode::Send)),
            0x71 => Some(Opcode::AVMOpcode(AVMOpcode::InboxPeek)),
            0x72 => Some(Opcode::AVMOpcode(AVMOpcode::Inbox)),
            0x73 => Some(Opcode::AVMOpcode(AVMOpcode::Panic)),
            0x74 => Some(Opcode::AVMOpcode(AVMOpcode::Halt)),
            0x75 => Some(Opcode::AVMOpcode(AVMOpcode::SetGas)),
            0x76 => Some(Opcode::AVMOpcode(AVMOpcode::GetGas)),
            0x77 => Some(Opcode::AVMOpcode(AVMOpcode::ErrCodePoint)),
            0x78 => Some(Opcode::AVMOpcode(AVMOpcode::PushInsn)),
            0x79 => Some(Opcode::AVMOpcode(AVMOpcode::PushInsnImm)),
            0x7a => Some(Opcode::AVMOpcode(AVMOpcode::OpenInsn)),
            0x7b => Some(Opcode::AVMOpcode(AVMOpcode::Sideload)),
            0x80 => Some(Opcode::AVMOpcode(AVMOpcode::EcRecover)),
            0x81 => Some(Opcode::AVMOpcode(AVMOpcode::EcAdd)),
            0x82 => Some(Opcode::AVMOpcode(AVMOpcode::EcMul)),
            0x83 => Some(Opcode::AVMOpcode(AVMOpcode::EcPairing)),
            0x90 => Some(Opcode::AVMOpcode(AVMOpcode::DebugPrint)),
            _ => None,
        }
    }

    pub fn to_number(&self) -> Option<u8> {
        match self {
            Opcode::AVMOpcode(AVMOpcode::Plus) => Some(0x01),
            Opcode::AVMOpcode(AVMOpcode::Mul) => Some(0x02),
            Opcode::AVMOpcode(AVMOpcode::Minus) => Some(0x03),
            Opcode::AVMOpcode(AVMOpcode::Div) => Some(0x04),
            Opcode::AVMOpcode(AVMOpcode::Sdiv) => Some(0x05),
            Opcode::AVMOpcode(AVMOpcode::Mod) => Some(0x06),
            Opcode::AVMOpcode(AVMOpcode::Smod) => Some(0x07),
            Opcode::AVMOpcode(AVMOpcode::AddMod) => Some(0x08),
            Opcode::AVMOpcode(AVMOpcode::MulMod) => Some(0x09),
            Opcode::AVMOpcode(AVMOpcode::Exp) => Some(0x0a),
            Opcode::AVMOpcode(AVMOpcode::SignExtend) => Some(0x0b),
            Opcode::AVMOpcode(AVMOpcode::LessThan) => Some(0x10),
            Opcode::AVMOpcode(AVMOpcode::GreaterThan) => Some(0x11),
            Opcode::AVMOpcode(AVMOpcode::SLessThan) => Some(0x012),
            Opcode::AVMOpcode(AVMOpcode::SGreaterThan) => Some(0x13),
            Opcode::AVMOpcode(AVMOpcode::Equal) => Some(0x14),
            Opcode::AVMOpcode(AVMOpcode::IsZero) => Some(0x15),
            Opcode::AVMOpcode(AVMOpcode::BitwiseAnd) => Some(0x16),
            Opcode::AVMOpcode(AVMOpcode::BitwiseOr) => Some(0x17),
            Opcode::AVMOpcode(AVMOpcode::BitwiseXor) => Some(0x18),
            Opcode::AVMOpcode(AVMOpcode::BitwiseNeg) => Some(0x19),
            Opcode::AVMOpcode(AVMOpcode::Byte) => Some(0x1a),
            Opcode::AVMOpcode(AVMOpcode::ShiftLeft) => Some(0x1b),
            Opcode::AVMOpcode(AVMOpcode::ShiftRight) => Some(0x1c),
            Opcode::AVMOpcode(AVMOpcode::ShiftArith) => Some(0x1d),
            Opcode::AVMOpcode(AVMOpcode::Hash) => Some(0x20),
            Opcode::AVMOpcode(AVMOpcode::Type) => Some(0x21),
            Opcode::AVMOpcode(AVMOpcode::Hash2) => Some(0x22),
            Opcode::AVMOpcode(AVMOpcode::Keccakf) => Some(0x23),
            Opcode::AVMOpcode(AVMOpcode::Pop) => Some(0x30),
            Opcode::AVMOpcode(AVMOpcode::PushStatic) => Some(0x31),
            Opcode::AVMOpcode(AVMOpcode::Rget) => Some(0x32),
            Opcode::AVMOpcode(AVMOpcode::Rset) => Some(0x33),
            Opcode::AVMOpcode(AVMOpcode::Jump) => Some(0x34),
            Opcode::AVMOpcode(AVMOpcode::Cjump) => Some(0x35),
            Opcode::AVMOpcode(AVMOpcode::StackEmpty) => Some(0x36),
            Opcode::AVMOpcode(AVMOpcode::GetPC) => Some(0x37),
            Opcode::AVMOpcode(AVMOpcode::AuxPush) => Some(0x38),
            Opcode::AVMOpcode(AVMOpcode::AuxPop) => Some(0x39),
            Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty) => Some(0x3a),
            Opcode::AVMOpcode(AVMOpcode::Noop) => Some(0x3b),
            Opcode::AVMOpcode(AVMOpcode::ErrPush) => Some(0x3c),
            Opcode::AVMOpcode(AVMOpcode::ErrSet) => Some(0x3d),
            Opcode::AVMOpcode(AVMOpcode::Dup0) => Some(0x40),
            Opcode::AVMOpcode(AVMOpcode::Dup1) => Some(0x41),
            Opcode::AVMOpcode(AVMOpcode::Dup2) => Some(0x42),
            Opcode::AVMOpcode(AVMOpcode::Swap1) => Some(0x43),
            Opcode::AVMOpcode(AVMOpcode::Swap2) => Some(0x44),
            Opcode::AVMOpcode(AVMOpcode::Tget) => Some(0x50),
            Opcode::AVMOpcode(AVMOpcode::Tset) => Some(0x51),
            Opcode::AVMOpcode(AVMOpcode::Tlen) => Some(0x52),
            Opcode::AVMOpcode(AVMOpcode::Xget) => Some(0x53),
            Opcode::AVMOpcode(AVMOpcode::Xset) => Some(0x54),
            Opcode::AVMOpcode(AVMOpcode::Breakpoint) => Some(0x60),
            Opcode::AVMOpcode(AVMOpcode::Log) => Some(0x61),
            Opcode::AVMOpcode(AVMOpcode::Send) => Some(0x70),
            Opcode::AVMOpcode(AVMOpcode::InboxPeek) => Some(0x71),
            Opcode::AVMOpcode(AVMOpcode::Inbox) => Some(0x72),
            Opcode::AVMOpcode(AVMOpcode::Panic) => Some(0x73),
            Opcode::AVMOpcode(AVMOpcode::Halt) => Some(0x74),
            Opcode::AVMOpcode(AVMOpcode::SetGas) => Some(0x75),
            Opcode::AVMOpcode(AVMOpcode::GetGas) => Some(0x76),
            Opcode::AVMOpcode(AVMOpcode::ErrCodePoint) => Some(0x77),
            Opcode::AVMOpcode(AVMOpcode::PushInsn) => Some(0x78),
            Opcode::AVMOpcode(AVMOpcode::PushInsnImm) => Some(0x79),
            Opcode::AVMOpcode(AVMOpcode::OpenInsn) => Some(0x7a),
            Opcode::AVMOpcode(AVMOpcode::Sideload) => Some(0x7b),
            Opcode::AVMOpcode(AVMOpcode::EcRecover) => Some(0x80),
            Opcode::AVMOpcode(AVMOpcode::EcAdd) => Some(0x81),
            Opcode::AVMOpcode(AVMOpcode::EcMul) => Some(0x82),
            Opcode::AVMOpcode(AVMOpcode::EcPairing) => Some(0x83),
            Opcode::AVMOpcode(AVMOpcode::DebugPrint) => Some(0x90),
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
