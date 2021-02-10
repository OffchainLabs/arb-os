/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, MiniProperties};
use crate::run::upload::CodeUploader;
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use ethers_core::utils::keccak256;
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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

    pub fn _upload(&self, u: &mut CodeUploader) {
        u._push_byte(self.opcode.to_number().unwrap());
        if let Some(val) = &self.immediate {
            u._push_byte(1u8);
            val._upload(u);
        } else {
            u._push_byte(0u8);
        }
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

    pub fn _upload(&self, u: &mut CodeUploader) {
        match self {
            CodePt::Internal(pc) => {
                u._push_byte(1);
                u._push_bytes(&Uint256::from_usize(u._translate_pc(*pc)).rlp_encode());
            }
            _ => {
                println!("Can't upload codepoint: {}", self);
                panic!();
            }
        }
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
                Value::Int(Uint256::zero()) // never gets called when it matters
            }
            CodePt::InSegment(_, _) => {
                Value::Int(Uint256::zero()) // never gets called when it matters
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BufferElem {
    Leaf(Rc<Vec<u8>>),
    Node(Rc<Vec<Buffer>>, u8),
    Sparse(Rc<Vec<usize>>, Rc<Vec<u8>>, u8),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Buffer {
    elem: BufferElem,
    hash: RefCell<Option<Packed>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Packed {
    pub hash: Uint256,
    size: u8,      // total size
    packed: usize, // packed levels
}

fn calc_len(h: u8) -> usize {
    if h == 0 {
        1024
    } else {
        128 * calc_len(h - 1)
    }
}

fn calc_height(h: u8) -> u8 {
    if h == 0 {
        10
    } else {
        7 + calc_height(h - 1)
    }
}

pub fn needed_height(offset: usize) -> u8 {
    if offset <= 1 {
        1
    } else {
        1 + needed_height(offset / 2)
    }
}

fn hash_buf(buf: &[u8]) -> Packed {
    if buf.len() == 0 {
        return zero_packed(10);
    }
    if buf.len() == 32 {
        return normal(Uint256::from_bytes(buf).avm_hash(), 5);
    }
    let len = buf.len();
    let h1 = hash_buf(&buf[0..len / 2]);
    let h2 = hash_buf(&buf[len / 2..len]);
    if is_zero_hash(&h2) {
        return pack(&h1);
    }
    normal(Uint256::avm_hash2(&unpack(&h1), &unpack(&h2)), h1.size + 1)
}

#[allow(dead_code)]
pub fn hash_buffer(buf: &[u8]) -> Uint256 {
    unpack(&hash_buf(buf))
}

fn hash_node(buf: &mut [Buffer], sz: u8) -> Packed {
    if buf.len() == 1 {
        return buf[0].hash();
    }
    let len = buf.len();
    let h1 = hash_node(&mut buf[0..len / 2], sz - 1);
    let h2 = hash_node(&mut buf[len / 2..len], sz - 1);
    if is_zero_hash(&h2) {
        pack(&h1)
    } else {
        normal(Uint256::avm_hash2(&unpack(&h1), &unpack(&h2)), sz)
    }
}

pub fn zero_hash(sz: u8) -> Uint256 {
    if sz == 5 {
        return Uint256::zero().avm_hash();
    }
    let h1 = zero_hash(sz - 1);
    Uint256::avm_hash2(&h1, &h1)
}

fn normal(hash: Uint256, sz: u8) -> Packed {
    Packed {
        size: sz,
        packed: 0,
        hash: hash,
    }
}

fn pack(packed: &Packed) -> Packed {
    Packed {
        size: packed.size,
        packed: packed.packed + 1,
        hash: packed.hash.clone(),
    }
}

fn is_zero_hash(packed: &Packed) -> bool {
    packed.hash == Uint256::zero().avm_hash()
}

fn unpack(packed: &Packed) -> Uint256 {
    let mut res = packed.hash.clone();
    let mut sz = packed.size;
    for _i in 0..packed.packed {
        res = Uint256::avm_hash2(&res, &zero_hash(sz));
        sz = sz + 1;
    }
    res
}

fn zero_packed(sz: u8) -> Packed {
    if sz == 5 {
        normal(zero_hash(5), 5)
    } else {
        pack(&zero_packed(sz - 1))
    }
}

fn hash_sparse(idx: &[usize], buf: &[u8], sz: u8) -> Packed {
    if sz == 5 {
        let mut res = [0u8; 32];
        for (i, &el) in idx.iter().enumerate() {
            res[el] = buf[i];
        }
        return normal(Uint256::from_bytes(&res).avm_hash(), 5);
    }
    if idx.len() == 0 {
        return zero_packed(sz);
    }
    let pivot = 1 << (sz - 1);
    let mut idx1 = Vec::new();
    let mut buf1 = Vec::new();
    let mut idx2 = Vec::new();
    let mut buf2 = Vec::new();
    for (i, &el) in idx.iter().enumerate() {
        if idx[i] < pivot {
            idx1.push(el);
            buf1.push(buf[i]);
        } else {
            idx2.push(el - pivot);
            buf2.push(buf[i]);
        }
    }
    let h1 = hash_sparse(&idx1, &buf1, sz - 1);
    let h2 = hash_sparse(&idx2, &buf2, sz - 1);
    if is_zero_hash(&h2) {
        pack(&h1)
    } else {
        normal(Uint256::avm_hash2(&unpack(&h1), &unpack(&h2)), sz)
    }
}

impl Buffer {
    fn node(vec: Rc<Vec<Buffer>>, h: u8) -> Buffer {
        Buffer {
            elem: BufferElem::Node(vec, h),
            hash: RefCell::new(None),
        }
    }

    fn leaf(vec: Rc<Vec<u8>>) -> Buffer {
        Buffer {
            elem: BufferElem::Leaf(vec),
            hash: RefCell::new(None),
        }
    }

    fn sparse(vec: Rc<Vec<usize>>, vec2: Rc<Vec<u8>>, h: u8) -> Buffer {
        Buffer {
            elem: BufferElem::Sparse(vec, vec2, h),
            hash: RefCell::new(None),
        }
    }

    pub fn as_bytes(&self, nbytes: usize) -> Vec<u8> {
        let mut ret = vec![];
        for i in 0..nbytes {
            ret.push(self.read_byte(i));
        }
        ret
    }

    pub fn avm_hash(&self) -> Uint256 {
        Uint256::avm_hash2(&Uint256::from_u64(123), &self.hash().hash)
    }

    pub fn hash_no_caching(&self) -> Packed {
        match self.hash.borrow().clone() {
            None => {
                let res = match &self.elem {
                    BufferElem::Leaf(cell) => hash_buf(&cell.to_vec()),
                    BufferElem::Node(cell, h) => hash_node(&mut cell.to_vec(), calc_height(*h)),
                    BufferElem::Sparse(idx_cell, buf_cell, h) => {
                        hash_sparse(&idx_cell.to_vec(), &buf_cell.to_vec(), calc_height(*h))
                    }
                };
                res
            }
            Some(x) => x.clone(),
        }
    }

    pub fn hash(&self) -> Packed {
        let res = self.hash_no_caching();
        self.hash.replace(Some(res.clone()));
        res
    }

    pub fn read_byte(&self, offset: usize) -> u8 {
        match &self.elem {
            BufferElem::Leaf(buf) => {
                if offset >= buf.len() {
                    0
                } else {
                    buf[offset]
                }
            }
            BufferElem::Sparse(idx, buf, _) => {
                for (i, &el) in idx.iter().enumerate() {
                    if el == offset {
                        return buf[i];
                    }
                }
                0
            }
            BufferElem::Node(buf, h) => {
                let len = calc_height(*h);
                let cell_len = calc_len(*h - 1);
                if needed_height(offset) > len {
                    0
                } else {
                    buf[offset / cell_len].read_byte(offset % cell_len)
                }
            }
        }
    }

    #[allow(dead_code)]
    pub fn empty0() -> Buffer {
        Buffer::leaf(Rc::new(Vec::new()))
    }

    pub fn empty1() -> Rc<Vec<Buffer>> {
        let mut vec = Vec::new();
        let empty = Rc::new(Vec::new());
        for _i in 0..128 {
            vec.push(Buffer::leaf(Rc::clone(&empty)));
        }
        Rc::new(vec)
    }

    fn make_empty(h: u8) -> Rc<Vec<Buffer>> {
        if h == 1 {
            return Buffer::empty1();
        }
        let mut vec = Vec::new();
        let empty = Buffer::make_empty(h - 1);
        for _i in 0..128 {
            vec.push(Buffer::node(Rc::clone(&empty), h - 1));
        }
        Rc::new(vec)
    }

    // Make for level, lower levels are sparse
    fn make_sparse(h: u8) -> Buffer {
        if h == 0 {
            return Buffer::leaf(Rc::new(Vec::new()));
        }
        let mut vec = Vec::new();
        let empty = Buffer::sparse(Rc::new(Vec::new()), Rc::new(Vec::new()), h - 1);
        for _i in 0..128 {
            vec.push(empty.clone());
        }
        Buffer::node(Rc::new(vec), h)
    }

    pub fn set_byte(&self, offset: usize, v: u8) -> Self {
        match &self.elem {
            BufferElem::Leaf(cell) => {
                if offset >= 1024 {
                    let mut vec = Vec::new();
                    vec.push(Buffer::leaf(cell.clone()));
                    let empty = Rc::new(Vec::new());
                    for _i in 1..128 {
                        vec.push(Buffer::leaf(Rc::clone(&empty)));
                    }
                    let buf = Buffer::node(Rc::new(vec), 1);
                    return buf.set_byte(offset, v);
                }
                let mut buf = cell.to_vec().clone();
                if buf.len() < 1024 {
                    buf.resize(1024, 0);
                }
                buf[offset] = v;
                Buffer::leaf(Rc::new(buf))
            }
            BufferElem::Sparse(idx, buf, h) => {
                if idx.len() > 16 {
                    let mut nbuf = Buffer::make_sparse(*h);
                    for (i, &el) in idx.iter().enumerate() {
                        nbuf = nbuf.set_byte(el, buf[i]);
                    }
                    return nbuf.set_byte(offset, v);
                }
                let mut nidx = idx.to_vec().clone();
                let mut nbuf = buf.to_vec().clone();
                nidx.push(offset);
                nbuf.push(v);
                Buffer::sparse(Rc::new(nidx), Rc::new(nbuf), *h)
            }
            BufferElem::Node(cell, h) => {
                if needed_height(offset) > calc_height(*h) {
                    let mut vec = Vec::new();
                    vec.push(Buffer::node(cell.clone(), *h));
                    #[cfg(feature = "sparse")]
                    for _i in 1..128 {
                        vec.push(Buffer::sparse(Rc::new(Vec::new()), Rc::new(Vec::new()), *h));
                    }
                    #[cfg(not(feature = "sparse"))]
                    {
                        let empty = Buffer::make_empty(*h);
                        for _i in 1..128 {
                            vec.push(Buffer::node(Rc::clone(&empty), *h));
                        }
                    }
                    let buf = Buffer::node(Rc::new(vec), *h + 1);
                    return buf.set_byte(offset, v);
                }
                let mut vec = cell.to_vec().clone();
                let cell_len = calc_len(*h - 1);
                vec[offset / cell_len] = vec[offset / cell_len].set_byte(offset % cell_len, v);
                Buffer::node(Rc::new(vec), *h)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    Int(Uint256),
    Tuple(Rc<Vec<Value>>),
    CodePoint(CodePt),
    Label(Label),
    Buffer(Buffer),
}

impl Value {
    ///Returns a value containing no data, a zero sized tuple.
    pub fn none() -> Self {
        Value::Tuple(Rc::new(vec![]))
    }

    ///Creates a single tuple `Value` from a `Vec<Value>`
    pub fn new_tuple(v: Vec<Value>) -> Self {
        Value::Tuple(Rc::new(v))
    }

    pub fn new_buffer(v: Vec<u8>) -> Self {
        Value::Buffer(Buffer::leaf(Rc::new(v)))
    }

    pub fn copy_buffer(v: Buffer) -> Self {
        Value::Buffer(v)
    }

    pub fn _upload(&self, u: &mut CodeUploader) {
        match self {
            Value::Int(ui) => {
                u._push_byte(0u8); // type code for uint
                u._push_bytes(&ui.rlp_encode());
            }
            Value::Tuple(tup) => {
                u._push_byte((10 + tup.len()) as u8);
                for subval in &**tup {
                    subval._upload(u);
                }
            }
            Value::CodePoint(cp) => {
                cp._upload(u);
            }
            Value::Buffer(buf) => {
                if let BufferElem::Leaf(b) = &buf.elem {
                    if b.len() == 0 {
                        u._push_byte(2u8);
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
            _ => {
                println!("unable to upload value: {}", self);
                panic!();
            } // other types should never be encountered here
        }
    }

    pub fn is_none(&self) -> bool {
        self == &Value::none()
    }

    pub fn type_insn_result(&self) -> usize {
        match self {
            Value::Int(_) => 0,
            Value::CodePoint(_) => 1,
            Value::Tuple(_) => 3,
            Value::Buffer(_) => 4,
            Value::Label(_) => {
                panic!("tried to run type instruction on a label");
            }
        }
    }

    pub fn replace_labels(self, label_map: &HashMap<Label, CodePt>) -> Result<Self, Label> {
        match self {
            Value::Int(_) => Ok(self),
            Value::CodePoint(_) => Ok(self),
            Value::Buffer(_) => Ok(self),
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

    pub fn replace_last_none(&self, val: &Value) -> Self {
        if self.is_none() {
            return val.clone();
        }
        if let Value::Tuple(tup) = self {
            let tlen = tup.len();
            let mut mut_tup = tup.clone();
            let new_tup = Rc::<Vec<Value>>::make_mut(&mut mut_tup);
            new_tup[tlen - 1] = new_tup[tlen - 1].replace_last_none(val);
            Value::new_tuple(new_tup.to_vec())
        } else {
            panic!();
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
            Value::Buffer(_) => (self, 0),
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
            Value::Int(_) | Value::CodePoint(_) | Value::Buffer(_) => self,
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

    ///Converts `Value` to usize if possible, otherwise returns `None`.
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
            Value::Buffer(buf) => Value::Int(buf.avm_hash()),
            Value::Tuple(v) => {
                let mut acc = Uint256::zero();
                for val in v.to_vec() {
                    if let Value::Int(ui) = val.avm_hash() {
                        acc = Uint256::avm_hash2(&acc, &ui);
                    } else {
                        panic!("Invalid value type from hash");
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
        if let Value::Int(ui) = v1 {
            if let Value::Int(ui2) = v2 {
                let mut buf = ui.to_bytes_be();
                buf.extend(ui2.to_bytes_be());
                Value::Int(Uint256::from_bytes(&keccak256(&buf)))
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => i.fmt(f),
            Value::Buffer(buf) => match &buf.elem {
                BufferElem::Leaf(vec) => write!(f, "Buffer(Leaf({}))", vec.len()),
                BufferElem::Node(vec, h) => write!(f, "Buffer(Node({}, {}))", vec.len(), h),
                BufferElem::Sparse(vec1, _, _) => write!(f, "Buffer(Sparse({}))", vec1.len()),
            },
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
    Ripemd160f,
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
    NewBuffer = 0xa0,
    GetBuffer8,
    GetBuffer64,
    GetBuffer256,
    SetBuffer8,
    SetBuffer64,
    SetBuffer256,
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
            "ripemd160f" => Opcode::AVMOpcode(AVMOpcode::Ripemd160f),
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
            "addmod" => Opcode::AVMOpcode(AVMOpcode::AddMod),
            "mulmod" => Opcode::AVMOpcode(AVMOpcode::MulMod),
            _ => {
                panic!("opcode not supported in asm segment: {}", name);
            }
        }
    }

    pub fn to_name(&self) -> &str {
        match self {
            Opcode::AVMOpcode(AVMOpcode::Rget) => "rget",
            Opcode::AVMOpcode(AVMOpcode::Rset) => "rset",
            Opcode::AVMOpcode(AVMOpcode::PushStatic) => "pushstatic",
            Opcode::AVMOpcode(AVMOpcode::Tset) => "tset",
            Opcode::AVMOpcode(AVMOpcode::Tget) => "tget",
            Opcode::AVMOpcode(AVMOpcode::Pop) => "pop",
            Opcode::AVMOpcode(AVMOpcode::StackEmpty) => "stackempty",
            Opcode::AVMOpcode(AVMOpcode::AuxPush) => "auxpush",
            Opcode::AVMOpcode(AVMOpcode::AuxPop) => "auxpop",
            Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty) => "auxstackempty",
            Opcode::AVMOpcode(AVMOpcode::Xget) => "xget",
            Opcode::AVMOpcode(AVMOpcode::Xset) => "xset",
            Opcode::AVMOpcode(AVMOpcode::Dup0) => "dup0",
            Opcode::AVMOpcode(AVMOpcode::Dup1) => "dup1",
            Opcode::AVMOpcode(AVMOpcode::Dup2) => "dup2",
            Opcode::AVMOpcode(AVMOpcode::Swap1) => "swap1",
            Opcode::AVMOpcode(AVMOpcode::Swap2) => "swap2",
            Opcode::UnaryMinus => "unaryminus",
            Opcode::AVMOpcode(AVMOpcode::BitwiseNeg) => "bitwiseneg",
            Opcode::AVMOpcode(AVMOpcode::Hash) => "hash",
            Opcode::AVMOpcode(AVMOpcode::Hash2) => "hash2",
            Opcode::AVMOpcode(AVMOpcode::Keccakf) => "keccakf",
            Opcode::AVMOpcode(AVMOpcode::Tlen) => "length",
            Opcode::AVMOpcode(AVMOpcode::Plus) => "plus",
            Opcode::AVMOpcode(AVMOpcode::Minus) => "minus",
            Opcode::AVMOpcode(AVMOpcode::Mul) => "mul",
            Opcode::AVMOpcode(AVMOpcode::Div) => "div",
            Opcode::AVMOpcode(AVMOpcode::Mod) => "mod",
            Opcode::AVMOpcode(AVMOpcode::Sdiv) => "sdiv",
            Opcode::AVMOpcode(AVMOpcode::Smod) => "smod",
            Opcode::AVMOpcode(AVMOpcode::Exp) => "exp",
            Opcode::AVMOpcode(AVMOpcode::LessThan) => "lt",
            Opcode::AVMOpcode(AVMOpcode::GreaterThan) => "gt",
            Opcode::AVMOpcode(AVMOpcode::SLessThan) => "slt",
            Opcode::AVMOpcode(AVMOpcode::SGreaterThan) => "sgt",
            Opcode::AVMOpcode(AVMOpcode::Equal) => "eq",
            Opcode::AVMOpcode(AVMOpcode::IsZero) => "iszero",
            Opcode::AVMOpcode(AVMOpcode::Byte) => "byte",
            Opcode::AVMOpcode(AVMOpcode::SignExtend) => "signextend",
            Opcode::AVMOpcode(AVMOpcode::ShiftLeft) => "shl",
            Opcode::AVMOpcode(AVMOpcode::ShiftRight) => "shr",
            Opcode::AVMOpcode(AVMOpcode::ShiftArith) => "sar",
            Opcode::AVMOpcode(AVMOpcode::BitwiseAnd) => "bitwiseand",
            Opcode::AVMOpcode(AVMOpcode::BitwiseOr) => "bitwiseor",
            Opcode::AVMOpcode(AVMOpcode::BitwiseXor) => "bitwisexor",
            Opcode::LogicalAnd => "logicaland",
            Opcode::LogicalOr => "logicalor",
            Opcode::AVMOpcode(AVMOpcode::Noop) => "noop",
            Opcode::AVMOpcode(AVMOpcode::Inbox) => "inbox",
            Opcode::AVMOpcode(AVMOpcode::InboxPeek) => "inboxpeek",
            Opcode::AVMOpcode(AVMOpcode::Jump) => "jump",
            Opcode::AVMOpcode(AVMOpcode::Cjump) => "cjump",
            Opcode::AVMOpcode(AVMOpcode::Log) => "log",
            Opcode::AVMOpcode(AVMOpcode::Send) => "send",
            Opcode::AVMOpcode(AVMOpcode::ErrCodePoint) => "errcodept",
            Opcode::AVMOpcode(AVMOpcode::PushInsn) => "pushinsn",
            Opcode::AVMOpcode(AVMOpcode::PushInsnImm) => "pushinsnimm",
            Opcode::AVMOpcode(AVMOpcode::OpenInsn) => "openinsn",
            Opcode::AVMOpcode(AVMOpcode::DebugPrint) => "debugprint",
            Opcode::AVMOpcode(AVMOpcode::SetGas) => "setgas",
            Opcode::AVMOpcode(AVMOpcode::GetGas) => "getgas",
            Opcode::AVMOpcode(AVMOpcode::ErrSet) => "errset",
            Opcode::AVMOpcode(AVMOpcode::Sideload) => "sideload",
            Opcode::AVMOpcode(AVMOpcode::EcRecover) => "ecrecover",
            Opcode::AVMOpcode(AVMOpcode::NewBuffer) => "newbuffer",
            Opcode::AVMOpcode(AVMOpcode::GetBuffer8) => "getbuffer8",
            Opcode::AVMOpcode(AVMOpcode::GetBuffer64) => "getbuffer64",
            Opcode::AVMOpcode(AVMOpcode::GetBuffer256) => "getbuffer256",
            Opcode::AVMOpcode(AVMOpcode::SetBuffer8) => "setbuffer8",
            Opcode::AVMOpcode(AVMOpcode::SetBuffer64) => "setbuffer64",
            Opcode::AVMOpcode(AVMOpcode::SetBuffer256) => "setbuffer256",
            _ => "Unknown",
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
            0x1b => Some(Opcode::AVMOpcode(AVMOpcode::ShiftLeft)),
            0x1c => Some(Opcode::AVMOpcode(AVMOpcode::ShiftRight)),
            0x1d => Some(Opcode::AVMOpcode(AVMOpcode::ShiftArith)),
            0x20 => Some(Opcode::AVMOpcode(AVMOpcode::Hash)),
            0x21 => Some(Opcode::AVMOpcode(AVMOpcode::Type)),
            0x22 => Some(Opcode::AVMOpcode(AVMOpcode::Hash2)),
            0x23 => Some(Opcode::AVMOpcode(AVMOpcode::Keccakf)),
            0x24 => Some(Opcode::AVMOpcode(AVMOpcode::Sha256f)),
            0x25 => Some(Opcode::AVMOpcode(AVMOpcode::Ripemd160f)),
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
            0xa0 => Some(Opcode::AVMOpcode(AVMOpcode::NewBuffer)),
            0xa1 => Some(Opcode::AVMOpcode(AVMOpcode::GetBuffer8)),
            0xa2 => Some(Opcode::AVMOpcode(AVMOpcode::GetBuffer64)),
            0xa3 => Some(Opcode::AVMOpcode(AVMOpcode::GetBuffer256)),
            0xa4 => Some(Opcode::AVMOpcode(AVMOpcode::SetBuffer8)),
            0xa5 => Some(Opcode::AVMOpcode(AVMOpcode::SetBuffer64)),
            0xa6 => Some(Opcode::AVMOpcode(AVMOpcode::SetBuffer256)),
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
            Opcode::AVMOpcode(AVMOpcode::Sha256f) => Some(0x24),
            Opcode::AVMOpcode(AVMOpcode::Ripemd160f) => Some(0x25),
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
            Opcode::AVMOpcode(AVMOpcode::NewBuffer) => Some(0xa0),
            Opcode::AVMOpcode(AVMOpcode::GetBuffer8) => Some(0xa1),
            Opcode::AVMOpcode(AVMOpcode::GetBuffer64) => Some(0xa2),
            Opcode::AVMOpcode(AVMOpcode::GetBuffer256) => Some(0xa3),
            Opcode::AVMOpcode(AVMOpcode::SetBuffer8) => Some(0xa4),
            Opcode::AVMOpcode(AVMOpcode::SetBuffer64) => Some(0xa5),
            Opcode::AVMOpcode(AVMOpcode::SetBuffer256) => Some(0xa6),

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
            _ => write!(f, "{}", self.to_name()),
        }
    }
}
