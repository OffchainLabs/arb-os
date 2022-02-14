/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, FrameSize, FuncProperties, SlotNum, TypeTree};
use crate::console::Color;
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use crate::upload::CodeUploader;
use ethers_core::utils::keccak256;
use serde::de::Visitor;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::hash::{Hash, Hasher};
use std::{collections::HashMap, fmt, sync::Arc};

/// A label who's value is the same across ArbOS versions
pub type LabelId = u64;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Label {
    Func(LabelId),    // A function uniquely identified by module & name
    Closure(LabelId), // A closure uniquely identified by module & name
    Anon(LabelId),    // An anonymous label identified by func/closure + count
    Evm(usize),       // program counter in EVM contract
}

impl Label {
    pub fn get_id(&self) -> LabelId {
        match self {
            Label::Func(id) | Label::Closure(id) | Label::Anon(id) => *id,
            Label::Evm(n) => panic!("no unique id for evm label {}", n),
        }
    }

    pub fn avm_hash(&self) -> Value {
        match self {
            Label::Func(id) | Label::Closure(id) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(4)),
                &Value::Int(Uint256::from_u64(*id)),
            ),
            Label::Anon(n) => Value::avm_hash2(
                &Value::Int(Uint256::from_usize(5)),
                &Value::Int(Uint256::from_usize(*n as usize)),
            ),
            Label::Evm(_) => {
                panic!("tried to avm_hash an EVM label");
            }
        }
    }

    pub fn pretty_print(&self, highlight: &str) -> String {
        Value::Label(*self).pretty_print(highlight)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Label::Func(sid) => write!(f, "function_{}", sid),
            Label::Closure(sid) => write!(f, "closure_{}", sid),
            Label::Anon(n) => write!(f, "label_{}", n),
            Label::Evm(pc) => write!(f, "EvmPC({})", pc),
        }
    }
}

#[derive(Default)]
pub struct LabelGenerator {
    current: LabelId,
}

impl LabelGenerator {
    /// Creates a new label generator that will hand out labels starting at some value.
    /// In practice, this means giving the generator a unique func id, so that local labels
    /// are always unique regardless of the function they are in.
    pub fn new(current: LabelId) -> Self {
        LabelGenerator { current }
    }

    /// Hands out a new label, advancing the generator
    pub fn next(&mut self) -> Label {
        let next = Label::Anon(self.current);
        self.current += 1;
        next
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Instruction<T = Opcode> {
    pub opcode: T,
    pub immediate: Option<Value>,
    #[serde(default)]
    pub debug_info: DebugInfo,
}

impl From<Instruction<AVMOpcode>> for Instruction {
    fn from(from: Instruction<AVMOpcode>) -> Self {
        let Instruction {
            opcode,
            immediate,
            debug_info,
        } = from;
        Self {
            opcode: opcode.into(),
            immediate,
            debug_info,
        }
    }
}

impl From<AVMOpcode> for Instruction<AVMOpcode> {
    fn from(opcode: AVMOpcode) -> Self {
        Self::from_opcode(opcode, DebugInfo::default())
    }
}

impl From<Opcode> for Instruction {
    fn from(opcode: Opcode) -> Self {
        Self::from_opcode(opcode, DebugInfo::default())
    }
}

impl<T> Instruction<T> {
    pub fn new(opcode: T, immediate: Option<Value>, debug_info: DebugInfo) -> Self {
        Instruction {
            opcode,
            immediate,
            debug_info,
        }
    }

    pub fn from_opcode(opcode: T, debug_info: DebugInfo) -> Self {
        Instruction::new(opcode, None, debug_info)
    }

    pub fn from_opcode_imm(opcode: T, immediate: Value, debug_info: DebugInfo) -> Self {
        Instruction::new(opcode, Some(immediate), debug_info)
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
}

impl Instruction<AVMOpcode> {
    pub fn _upload(&self, u: &mut CodeUploader) {
        u.push_byte(self.opcode.to_number());
        if let Some(val) = &self.immediate {
            u.push_byte(1u8);
            val.upload(u);
        } else {
            u.push_byte(0u8);
        }
    }

    pub fn pretty_print(&self, highlight: &str) -> String {
        let label_color = Color::PINK;
        let op = Opcode::AVMOpcode(self.opcode).pretty_print(label_color);
        match &self.immediate {
            Some(value) => format!("{} {}", op, value.pretty_print(highlight)),
            None => op,
        }
    }
}

impl Instruction {
    pub fn is_view(&self, type_tree: &TypeTree) -> bool {
        self.opcode.is_view(type_tree)
    }
    pub fn is_write(&self, type_tree: &TypeTree) -> bool {
        self.opcode.is_write(type_tree)
    }
    pub fn get_label(&self) -> Option<Label> {
        match &self.opcode {
            Opcode::Label(label) => Some(*label),
            _ => None,
        }
    }

    pub fn get_uniques(&self) -> Vec<LabelId> {
        let mut uniques = vec![];
        if let Opcode::Label(Label::Func(id) | Label::Closure(id)) = self.opcode {
            uniques.push(id);
        }
        if let Some(value) = &self.immediate {
            uniques.extend(value.get_uniques());
        }
        uniques
    }

    pub fn pretty_print(&self, highlight: &str) -> String {
        let label_color = Color::PINK;
        let text = match &self.immediate {
            Some(value) => format!(
                "{} {}",
                self.opcode.pretty_print(label_color),
                value.pretty_print(highlight)
            ),
            None => format!("{}", self.opcode.pretty_print(label_color)),
        };
        match self.debug_info.attributes.color_group {
            1 => Color::orange(text),
            _ => text,
        }
    }
}

impl<T> fmt::Display for Instruction<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.immediate {
            Some(value) => match value {
                Value::Tuple(_) => write!(f, "[{}]\n        {}", value, self.opcode),
                _ => write!(f, "{} {}", self.opcode, value),
            },
            None => write!(f, "{}", self.opcode),
        }
    }
}

#[macro_export]
macro_rules! opcode {
    ($opcode:ident) => {
        Instruction::from(Opcode::AVMOpcode(AVMOpcode::$opcode))
    };
    ($opcode:ident, $immediate:expr) => {
        Instruction::from_opcode_imm(
            Opcode::AVMOpcode(AVMOpcode::$opcode),
            $immediate,
            DebugInfo::default(),
        )
    };
    (@$($opcode:tt)+) => {
        Instruction::from_opcode(Opcode::$($opcode)+, DebugInfo::default())
    };
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

    pub fn new_in_segment(seg_num: usize, offset: usize) -> Self {
        CodePt::InSegment(seg_num, offset)
    }

    pub fn upload(&self, u: &mut CodeUploader) {
        match self {
            CodePt::Internal(pc) => {
                u.push_byte(1);
                u.push_bytes(&Uint256::from_usize(u._translate_pc(*pc)).rlp_encode());
            }
            _ => {
                panic!("Tried to upload bad codepoint");
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

#[derive(Debug, Clone, Eq)]
pub struct Buffer {
    root: Arc<BufferNode>,
    size: u128,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BufferNode {
    Leaf(Vec<u8>),
    Internal(BufferInternal),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BufferInternal {
    height: usize,
    capacity: u128,
    left: Arc<BufferNode>,
    right: Arc<BufferNode>,
    hash_val: Uint256,
}

impl Buffer {
    pub fn new_empty() -> Self {
        Buffer {
            root: Arc::new(BufferNode::new_empty()),
            size: 0,
        }
    }

    pub fn from_bytes(contents: Vec<u8>) -> Self {
        let mut ret = Buffer::new_empty();
        for i in 0..contents.len() {
            ret = ret.set_byte(i as u128, contents[i]);
        }
        ret
    }

    pub fn as_bytes(&self, nbytes: usize) -> Vec<u8> {
        // TODO: make this more efficient
        let mut ret = vec![];
        for i in 0..nbytes {
            ret.push(self.read_byte(i as u128));
        }
        ret
    }

    pub fn max_size(&self) -> u128 {
        self.size
    }

    fn avm_hash(&self) -> Uint256 {
        self.root.hash()
    }

    pub fn hex_encode(&self) -> String {
        self.root.hex_encode(self.size as usize)
    }

    pub fn read_byte(&self, offset: u128) -> u8 {
        if offset >= self.size {
            0u8
        } else {
            self.root.read_byte(offset)
        }
    }

    pub fn set_byte(&self, offset: u128, val: u8) -> Self {
        Buffer {
            root: Arc::new(self.root.set_byte(offset, val)),
            size: if offset >= self.size {
                offset + 1
            } else {
                self.size
            },
        }
    }
}

impl Hash for Buffer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.root.hash().hash(state);
    }
}

impl PartialEq for Buffer {
    fn eq(&self, other: &Buffer) -> bool {
        self.avm_hash() == other.avm_hash()
    }
}

struct BufferVisitor;

impl<'de> Visitor<'de> for BufferVisitor {
    type Value = Buffer;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("Expected hex string")
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Buffer::from_bytes(hex::decode(v).map_err(|_| {
            E::custom("Could not buffer as hex string".to_string())
        })?))
    }
}

impl Serialize for Buffer {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&*format!(
            "{}",
            hex::encode(self.as_bytes(self.size as usize))
        ))
    }
}

impl<'de> Deserialize<'de> for Buffer {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(BufferVisitor)
    }
}

impl BufferNode {
    pub fn new_empty() -> Self {
        BufferNode::Leaf(vec![0u8; 32])
    }

    pub fn _leaf_from_bytes(v: &[u8]) -> Self {
        assert!(v.len() <= 32);
        let mut buf = vec![0u8; 32];
        for i in 0..v.len() {
            buf[i] = v[i]
        }
        BufferNode::Leaf(buf)
    }

    fn _minimal_internal_from_bytes(v: &[u8]) -> Self {
        assert!(v.len() > 32);
        let (height, size) = _levels_needed(v.len() as u128);
        BufferNode::_internal_from_bytes(height, size, v)
    }

    fn _internal_from_bytes(height: usize, capacity: u128, v: &[u8]) -> Self {
        if height == 1 {
            BufferNode::_leaf_from_bytes(v)
        } else if v.len() == 0 {
            BufferNode::new_empty_internal(height, capacity)
        } else if v.len() as u128 <= capacity / 2 {
            let left = Arc::new(BufferNode::_internal_from_bytes(
                height - 1,
                capacity / 2,
                v,
            ));
            let right = Arc::new(BufferNode::new_empty_internal(height - 1, capacity / 2));
            BufferNode::Internal(BufferInternal {
                height,
                capacity,
                left: left.clone(),
                right: right.clone(),
                hash_val: {
                    let mut b = left.hash().to_bytes_be();
                    b.extend(right.hash().to_bytes_be());
                    Uint256::from_bytes(&keccak256(&b))
                },
            })
        } else {
            let mid = (capacity / 2) as usize;
            let left = Arc::new(BufferNode::_internal_from_bytes(
                height - 1,
                capacity / 2,
                &v[0..mid],
            ));
            let right = Arc::new(BufferNode::_internal_from_bytes(
                height - 1,
                capacity / 2,
                &v[mid..],
            ));
            BufferNode::Internal(BufferInternal {
                height,
                capacity,
                left: left.clone(),
                right: right.clone(),
                hash_val: {
                    let mut b = left.hash().to_bytes_be();
                    b.extend(right.hash().to_bytes_be());
                    Uint256::from_bytes(&keccak256(&b))
                },
            })
        }
    }

    fn new_empty_internal(height: usize, capacity: u128) -> Self {
        let child = Arc::new(if height == 1 {
            BufferNode::new_empty()
        } else {
            BufferNode::new_empty_internal(height - 1, capacity / 2)
        });
        BufferNode::Internal(BufferInternal {
            height,
            capacity,
            left: child.clone(),
            right: child.clone(),
            hash_val: {
                let a = child.hash().to_bytes_be();
                let mut b = a.clone();
                b.extend(a);
                Uint256::from_bytes(&keccak256(&b))
            },
        })
    }

    fn hash(&self) -> Uint256 {
        match self {
            BufferNode::Leaf(b) => Uint256::from_bytes(&keccak256(&b[..])),
            BufferNode::Internal(x) => x.hash_val.clone(),
        }
    }

    fn hex_encode(&self, size: usize) -> String {
        match self {
            BufferNode::Leaf(b) => hex::encode(b)[0..(2 * size)].to_string(),
            BufferNode::Internal(node) => node.hex_encode(size),
        }
    }

    fn read_byte(&self, offset: u128) -> u8 {
        match self {
            BufferNode::Leaf(b) => b[offset as usize],
            BufferNode::Internal(node) => node.read_byte(offset),
        }
    }

    fn set_byte(&self, offset: u128, val: u8) -> Self {
        match self {
            BufferNode::Leaf(b) => {
                if (offset < 32) {
                    let mut bb = b.clone();
                    bb[offset as usize] = val;
                    BufferNode::Leaf(bb)
                } else {
                    BufferNode::Internal(
                        BufferInternal::grow_from_leaf(self.clone()).set_byte(offset, val),
                    )
                }
            }
            BufferNode::Internal(node) => BufferNode::Internal(node.set_byte(offset, val)),
        }
    }
}

impl BufferInternal {
    fn new(height: usize, capacity: u128, left: BufferNode, right: BufferNode) -> Self {
        BufferInternal {
            height,
            capacity,
            left: Arc::new(left.clone()),
            right: Arc::new(right.clone()),
            hash_val: {
                let mut b = left.hash().to_bytes_be();
                b.extend(right.hash().to_bytes_be());
                Uint256::from_bytes(&keccak256(&b))
            },
        }
    }

    fn grow(&self) -> Self {
        BufferInternal::new(
            self.height + 1,
            self.capacity * 2,
            BufferNode::Internal(self.clone()),
            BufferNode::new_empty_internal(self.height, self.capacity),
        )
    }

    fn grow_from_leaf(leaf: BufferNode) -> Self {
        BufferInternal::new(2, 2 * 32, leaf, BufferNode::new_empty())
    }

    fn hex_encode(&self, size: usize) -> String {
        let half_capacity = (self.capacity / 2) as usize;
        if size == 0 {
            "".to_string()
        } else if size <= half_capacity {
            self.left.hex_encode(size)
        } else {
            let mut left_str = self.left.hex_encode(half_capacity);
            let right_str = self.right.hex_encode(size - half_capacity);
            left_str.push_str(&right_str);
            left_str
        }
    }

    fn read_byte(&self, offset: u128) -> u8 {
        if offset < self.capacity / 2 {
            self.left.read_byte(offset)
        } else {
            self.right.read_byte(offset - self.capacity / 2)
        }
    }

    fn set_byte(&self, offset: u128, val: u8) -> BufferInternal {
        if offset < self.capacity / 2 {
            BufferInternal::new(
                self.height,
                self.capacity,
                self.left.set_byte(offset, val),
                (*self.right).clone(),
            )
        } else if offset < self.capacity {
            BufferInternal::new(
                self.height,
                self.capacity,
                (*self.left).clone(),
                self.right.set_byte(offset - self.capacity / 2, val),
            )
        } else {
            self.grow().set_byte(offset, val)
        }
    }
}

fn _levels_needed(x: u128) -> (usize, u128) {
    let mut height = 1;
    let mut size = 32u128;
    while (size < x) {
        height = height + 1;
        size = size * 2;
    }
    (height, size)
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    Int(Uint256),
    Tuple(Arc<Vec<Value>>),
    CodePoint(CodePt),
    Label(Label),
    Buffer(Buffer),
}

impl Value {
    /// Returns a value containing no data, a zero sized tuple.
    pub fn none() -> Self {
        Value::Tuple(Arc::new(vec![]))
    }

    /// Creates a single tuple `Value` from a `Vec<Value>`
    pub fn new_tuple(v: Vec<Value>) -> Self {
        Value::Tuple(Arc::new(v))
    }

    pub fn new_buffer(v: Vec<u8>) -> Self {
        Value::Buffer(Buffer::from_bytes(v))
    }

    pub fn copy_buffer(v: Buffer) -> Self {
        Value::Buffer(v)
    }

    pub fn upload(&self, u: &mut CodeUploader) {
        match self {
            Value::Int(ui) => {
                u.push_byte(0u8); // type code for uint
                u.push_bytes(&ui.rlp_encode());
            }
            Value::Tuple(tup) => {
                u.push_byte((10 + tup.len()) as u8);
                for subval in &**tup {
                    subval.upload(u);
                }
            }
            Value::CodePoint(cp) => {
                cp.upload(u);
            }
            Value::Buffer(buf) => {
                if buf.size == 0 {
                    u.push_byte(2u8);
                } else {
                    u.push_byte(3u8);
                    let size = buf.max_size() as usize;
                    u.push_bytes(&Uint256::from_usize(size).rlp_encode());
                    u.push_bytes(&buf.as_bytes(size));
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
            let new_tup = Arc::<Vec<Value>>::make_mut(&mut mut_tup);
            new_tup[tlen - 1] = new_tup[tlen - 1].replace_last_none(val);
            Value::new_tuple(new_tup.to_vec())
        } else {
            panic!();
        }
    }

    pub fn avm_hash(&self) -> Value {
        //BUGBUG: should do same hash as AVM
        match self {
            Value::Int(ui) => Value::Int(ui.avm_hash()),
            Value::Buffer(buf) => Value::Int(buf.avm_hash()),
            Value::Tuple(v) => {
                // According to the C++ emulator, the AVM hash of a tuple is
                //   H(3 || H(uint8(tlen) || A(tuple[0]) || ... || A(tuple[tlen-1])) || uint256(recursiveSize))
                //   where A is an AVM hash & H is keccack

                let total_size = 1 + v.len(); // we assume tuples only contain ints for now
                let outer_size = v.len() as u8;

                let mut all_bytes = vec![3u8];
                let mut content_bytes = vec![outer_size];

                for val in v.to_vec() {
                    if let Value::Int(ui) = val.avm_hash() {
                        let child_hash = Uint256::avm_hash(&ui);
                        content_bytes.extend(child_hash.to_bytes_be());
                    } else {
                        panic!("Invalid value type from hash");
                    }
                }

                let content_hash = keccak256(&content_bytes);
                all_bytes.extend(content_hash);
                all_bytes.extend(Uint256::from_usize(total_size).to_bytes_be());

                let hash = Uint256::from_bytes(&keccak256(&all_bytes));
                Value::Int(hash)
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

    pub fn get_uniques(&self) -> Vec<LabelId> {
        let mut uniques = vec![];
        match self {
            Value::Label(Label::Func(id) | Label::Closure(id)) => uniques.push(*id),
            Value::Tuple(tup) => {
                for child in &**tup {
                    uniques.extend(child.get_uniques());
                }
            }
            _ => {}
        }
        uniques
    }

    /// Surgically replace value potentially nested within a tuple with others.
    /// |with| should return true when a value is to be replaced.
    /// |when| makes the value substitution.
    /// The application order allows a substituted value to itself be replaced.
    pub fn replace<With, When>(self, with: &mut With, when: &mut When) -> Self
    where
        With: FnMut(Value) -> Value,
        When: FnMut(&Value) -> bool,
    {
        let mut current = match when(&self) {
            true => with(self),
            false => self,
        };
        if let Value::Tuple(ref mut contents) = current {
            let items = contents
                .to_vec()
                .into_iter()
                .map(|val| val.replace(with, when))
                .collect();
            *contents = Arc::new(items);
        }
        current
    }

    /// Surgically replace types potentially nested within others.
    /// |via| makes the type substitution.
    pub fn replace2<Via>(&mut self, via: &mut Via)
    where
        Via: FnMut(&mut Self),
    {
        match self {
            Self::Tuple(ref mut contents) => {
                let mut nested = contents.to_vec();
                nested.iter_mut().for_each(|val| val.replace2(via));
                *contents = Arc::new(nested);
            }
            _ => {}
        }
        via(self);
    }

    pub fn pretty_print(&self, highlight: &str) -> String {
        match self {
            Value::Int(i) => Color::color(highlight, i),
            Value::CodePoint(pc) => match pc {
                CodePt::Null => Color::maroon("Err"),
                _ => Color::color(highlight, pc),
            },
            Value::Label(label) => match label {
                Label::Func(id) => Color::color(highlight, format!("func_{}", id % 256)),
                Label::Closure(id) => Color::color(highlight, format!("λ_{}", id % 256)),
                Label::Anon(id) => Color::color(highlight, format!("label_{}", id % 256)),
                _ => Color::color(highlight, label),
            },
            Value::Buffer(buf) => {
                let text = String::from_utf8_lossy(&hex::decode(buf.hex_encode()).unwrap())
                    .chars()
                    .filter(|c| !c.is_ascii_control())
                    .take(100)
                    .collect::<String>();
                Color::lavender(format!("\"{}\"", text))
            }
            Value::Tuple(tup) => match tup.is_empty() {
                true => Color::grey("_"),
                false => {
                    let mut s = Color::color(highlight, "(");
                    for (i, value) in tup.iter().enumerate() {
                        let child = value.pretty_print(highlight);
                        match i == 0 {
                            true => {
                                s = format!("{}{}", s, child);
                            }
                            false => {
                                s = format!("{}{}{}", s, Color::grey(", "), child);
                            }
                        }
                    }
                    format!("{}{}", s, Color::color(highlight, ")"))
                }
            },
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        return Value::none();
    }
}

impl From<usize> for Value {
    fn from(v: usize) -> Self {
        Self::Int(Uint256::from_usize(v))
    }
}

impl From<u8> for Value {
    fn from(v: u8) -> Self {
        Self::Int(Uint256::from_usize(v.into()))
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        match v < 0 {
            true => panic!("tried to make mavm::Value from {}", v),
            false => Self::Int(Uint256::from_usize(v as usize)),
        }
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Self::Int(Uint256::from_u64(v))
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::Buffer(Buffer::from_bytes(v.as_bytes().to_vec()))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => i.fmt(f),
            Value::Buffer(buf) => write!(f, "Buffer({})", buf.hex_encode()),
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
    MakeFrame(FrameSize, bool, bool), // make a func frame: space, returns, prebuilt
    GetLocal(SlotNum),                // get a local variable within a func frame
    SetLocal(SlotNum),                // set a local variable within a func frame
    MoveLocal(SlotNum, SlotNum),      // move into arg1 arg2 within a func frame
    DropLocal(SlotNum),               // annotate a point after which the local is never used again
    ReserveCapture(SlotNum, StringId), // annotate where a capture should be placed within a func frame
    Capture(LabelId, StringId),        // annotate which value to retrieve for closure packing
    MakeClosure(LabelId),              // create a callable closure frame
    FuncCall(FuncProperties),          // make a function call: nargs, nouts, and view/write-props
    TupleGet(usize, usize),            // args are offset and size for the anysize_tuple
    TupleSet(usize, usize),            // args are offset and size for the anysize_tuple
    GetGlobalVar(usize),               // gets a global variable at a global index
    SetGlobalVar(usize),               // sets a global variable at a global index
    BackwardLabelTarget(usize),        // sets up a backward label as indexed by the jump table
    Label(Label),                      // a location in code
    JumpTo(Label),                     // Like a Jump, but with info about where it'll go
    CjumpTo(Label),                    // Like a Cjump, but with info about where it'll go
    Return,                            // return from a func, popping the frame
    Pop(usize),                        // pop a value deep within the stack
    AVMOpcode(AVMOpcode),              // a non-virtual, AVM opcode
}

#[derive(Debug, Clone, Copy, Serialize_repr, Deserialize_repr, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum AVMOpcode {
    Zero = 0x00,
    Add = 0x01,
    Mul,
    Sub,
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
    EthHash2,
    Keccakf,
    Sha256f,
    Ripemd160f,
    Blake2f,
    Pop = 0x30,
    Spush,
    Rpush,
    Rset,
    Jump,
    Cjump,
    StackEmpty,
    PCpush,
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
    Error,
    Halt,
    SetGas,
    PushGas,
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

impl Opcode {
    pub fn is_view(&self, _: &TypeTree) -> bool {
        match self {
            Opcode::AVMOpcode(AVMOpcode::Inbox)
            | Opcode::AVMOpcode(AVMOpcode::InboxPeek)
            | Opcode::AVMOpcode(AVMOpcode::Rpush)
            | Opcode::AVMOpcode(AVMOpcode::PushInsn)
            | Opcode::AVMOpcode(AVMOpcode::PushInsnImm)
            | Opcode::AVMOpcode(AVMOpcode::ErrCodePoint)
            | Opcode::AVMOpcode(AVMOpcode::ErrPush)
            | Opcode::AVMOpcode(AVMOpcode::PushGas)
            | Opcode::AVMOpcode(AVMOpcode::Sideload)
            | Opcode::AVMOpcode(AVMOpcode::Jump)
            | Opcode::AVMOpcode(AVMOpcode::Cjump)
            | Opcode::AVMOpcode(AVMOpcode::AuxPop)
            | Opcode::AVMOpcode(AVMOpcode::AuxPush) => true,
            Opcode::FuncCall(prop) => prop.view,
            _ => false,
        }
    }
    pub fn is_write(&self, _: &TypeTree) -> bool {
        match self {
            Opcode::AVMOpcode(AVMOpcode::Log)
            | Opcode::AVMOpcode(AVMOpcode::DebugPrint)
            | Opcode::AVMOpcode(AVMOpcode::Inbox)
            | Opcode::AVMOpcode(AVMOpcode::InboxPeek)
            | Opcode::AVMOpcode(AVMOpcode::Send)
            | Opcode::AVMOpcode(AVMOpcode::Rset)
            | Opcode::AVMOpcode(AVMOpcode::PushInsn)
            | Opcode::AVMOpcode(AVMOpcode::PushInsnImm)
            | Opcode::AVMOpcode(AVMOpcode::ErrSet)
            | Opcode::AVMOpcode(AVMOpcode::SetGas)
            | Opcode::AVMOpcode(AVMOpcode::Sideload)    // this is special-cased, so we can't reorder these
            | Opcode::AVMOpcode(AVMOpcode::Jump)
            | Opcode::AVMOpcode(AVMOpcode::Cjump)
            | Opcode::AVMOpcode(AVMOpcode::AuxPop)
            | Opcode::AVMOpcode(AVMOpcode::AuxPush) => true,
            Opcode::FuncCall(prop) => prop.write,
            _ => false,
        }
    }

    pub fn base_cost(&self) -> usize {
        macro_rules! avm {
            ($first:ident $(,$opcode:ident)*) => {
                Opcode::AVMOpcode(AVMOpcode::$first $(| AVMOpcode::$opcode)*)
            };
        }

        #[rustfmt::skip]
        let cost = match self {
            avm!(
                Pop, Spush, Rpush, PCpush, AuxPush, AuxPop, Noop, ErrPush, ErrSet,
                Dup0, Dup1, Dup2, Swap1, Swap2, PushGas, SetGas, NewBuffer, DebugPrint,
                IsZero, BitwiseNeg
            ) => 1,
            avm!(
                LessThan, GreaterThan, SLessThan, SGreaterThan, Equal,
                Tget, Tlen, BitwiseAnd, BitwiseOr, BitwiseXor,
                Rset, StackEmpty, AuxStackEmpty
            ) => 2,
            avm!(
                Div, Mod, AddMod, MulMod, Hash, Jump, Cjump,
                Byte, ShiftLeft, ShiftRight, ShiftArith
            ) => 4,
            avm!(Add, Sub, Mul, Type, Xget) => 3,
            avm!(Zero, Error) => 5,
            avm!(Sdiv, Smod, SignExtend) => 7,
            avm!(EthHash2) => 8,
            avm!(Halt, Sideload, GetBuffer8, GetBuffer64, GetBuffer256) => 10,
            avm!(Exp, ErrCodePoint, PushInsn, PushInsnImm, OpenInsn) => 25,
            avm!(Tset, Inbox, InboxPeek) => 40,
            avm!(Xset) => 41,
            avm!(Breakpoint, Log, Send, SetBuffer8, SetBuffer64, SetBuffer256) => 100,
            avm!(Sha256f, Ripemd160f) => 250,
            avm!(Keccakf, Blake2f) => 600,
            avm!(EcPairing) => 1_000,
            avm!(EcAdd) => 3_500,
            avm!(EcRecover) => 20_000,
            avm!(EcMul) => 82_000,
            Self::MakeFrame(..) => 3,
            Self::GetLocal(..) => 3,
            Self::SetLocal(..) => 41,
            Self::DropLocal(..) => 0,
            Self::Capture(..) => 41,
            Self::ReserveCapture(..) => 0,
            Self::MakeClosure(..) => 1,
            Self::FuncCall(..) => 10,
            Self::TupleGet(offset, _) =>   2 +  2 * (offset / 8),
            Self::TupleSet(offset, _) =>  40 + 42 * (offset / 8),
            Self::GetGlobalVar(offset) =>  3 +  2 * (offset / 8),
            Self::SetGlobalVar(offset) => 43 + 42 * (offset / 8),
            Self::Label(..) => 0,
            Self::BackwardLabelTarget(..) => 0,
            Self::JumpTo(..) => 4,
            Self::CjumpTo(..) => 4,
            Self::Return => 7,
            Self::MoveLocal(dest, source) => {
                if dest == source {
                    0
                } else {
                    43
                }
            }
            Self::Pop(depth) => {
                match depth {
                    0 => 1,
                    1 => 2,
                    2 => 3,
                    x => 1 + 2*(x-1),
                }
            }
        };
        cost
    }

    pub fn pretty_print(&self, label_color: &str) -> String {
        match self {
            Opcode::MakeFrame(space, returns, prebuilt) => match prebuilt {
                true => format!(
                    "MakeFrame<{}, {}>",
                    Color::mint(space),
                    Color::color_if(*returns, Color::GREY, Color::MINT)
                ),
                false => format!(
                    "MakeFrame {} {}",
                    Color::mint(space),
                    Color::color_if(*returns, Color::GREY, Color::MINT)
                ),
            },
            Opcode::GetLocal(slot) => format!("GetLocal {}", Color::pink(slot)),
            Opcode::SetLocal(slot) => format!("SetLocal {}", Color::pink(slot)),
            Opcode::DropLocal(slot) => format!("δ({})", Color::pink(slot)),
            Opcode::MoveLocal(dest, source) => {
                format!("φ({}, {})", Color::pink(dest), Color::pink(source))
            }
            Opcode::ReserveCapture(slot, id) => {
                format!("ReserveCapture {} {}", Color::pink(slot), Color::grey(id))
            }
            Opcode::Capture(label, id) => {
                let label = Value::Label(Label::Closure(*label));
                format!(
                    "Capture {} {}",
                    label.pretty_print(label_color),
                    Color::grey(id)
                )
            }
            Opcode::MakeClosure(label) => {
                let label = Value::Label(Label::Closure(*label));
                format!("MakeClosure {}", label.pretty_print(label_color))
            }
            Opcode::SetGlobalVar(id) => format!("SetGlobal {}", Color::pink(id)),
            Opcode::GetGlobalVar(id) => format!("GetGlobal {}", Color::pink(id)),
            Opcode::TupleGet(slot, size) => {
                format!("TupleGet {} {}", Color::pink(slot), Color::grey(size))
            }
            Opcode::TupleSet(slot, size) => {
                format!("TupleSet {} {}", Color::pink(slot), Color::grey(size))
            }
            Opcode::Label(label) => label.pretty_print(label_color),
            Opcode::JumpTo(label) => {
                format!("JumpTo {}", label.pretty_print(label_color))
            }
            Opcode::CjumpTo(label) => {
                format!("CjumpTo {}", label.pretty_print(label_color))
            }
            Opcode::Pop(depth) => format!("Pop {}", Color::pink(depth)),
            Opcode::FuncCall(prop) => format!(
                "FuncCall {}{}{}{}{} {}{}",
                Color::mint(match prop.view {
                    true => "υ ",
                    false => "",
                }),
                Color::mint(match prop.write {
                    true => "ω ",
                    false => "",
                }),
                Color::mint(match prop.throw {
                    true => "τ ",
                    false => "",
                }),
                Color::mint(match prop.sensitive {
                    true => "Σ ",
                    false => "",
                }),
                Color::mint(prop.nargs),
                Color::mint(prop.nouts),
                match prop.returns {
                    true => "".to_string(),
                    false => Color::mint(" noreturn"),
                }
            ),
            _ => format!("{}", self.to_name()),
        }
    }
}

impl Opcode {
    pub fn from_name(name: &str) -> Self {
        match name {
            "rget" => Opcode::AVMOpcode(AVMOpcode::Rpush),
            "rset" => Opcode::AVMOpcode(AVMOpcode::Rset),
            "spush" => Opcode::AVMOpcode(AVMOpcode::Spush),
            "tset" => Opcode::AVMOpcode(AVMOpcode::Tset),
            "tget" => Opcode::AVMOpcode(AVMOpcode::Tget),
            "tlen" => Opcode::AVMOpcode(AVMOpcode::Tlen),
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
            "bitwiseneg" => Opcode::AVMOpcode(AVMOpcode::BitwiseNeg),
            "hash" => Opcode::AVMOpcode(AVMOpcode::Hash),
            "ethhash2" => Opcode::AVMOpcode(AVMOpcode::EthHash2),
            "keccakf" => Opcode::AVMOpcode(AVMOpcode::Keccakf),
            "sha256f" => Opcode::AVMOpcode(AVMOpcode::Sha256f),
            "ripemd160f" => Opcode::AVMOpcode(AVMOpcode::Ripemd160f),
            "blake2f" => Opcode::AVMOpcode(AVMOpcode::Blake2f),
            "length" => Opcode::AVMOpcode(AVMOpcode::Tlen),
            "add" => Opcode::AVMOpcode(AVMOpcode::Add),
            "sub" => Opcode::AVMOpcode(AVMOpcode::Sub),
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
            "inbox" => Opcode::AVMOpcode(AVMOpcode::Inbox),
            "inboxpeek" => Opcode::AVMOpcode(AVMOpcode::InboxPeek),
            "jump" => Opcode::AVMOpcode(AVMOpcode::Jump),
            "log" => Opcode::AVMOpcode(AVMOpcode::Log),
            "send" => Opcode::AVMOpcode(AVMOpcode::Send),
            "pushinsn" => Opcode::AVMOpcode(AVMOpcode::PushInsn),
            "pushinsnimm" => Opcode::AVMOpcode(AVMOpcode::PushInsnImm),
            "openinsn" => Opcode::AVMOpcode(AVMOpcode::OpenInsn),
            "debugprint" => Opcode::AVMOpcode(AVMOpcode::DebugPrint),
            "setgas" => Opcode::AVMOpcode(AVMOpcode::SetGas),
            "pushgas" => Opcode::AVMOpcode(AVMOpcode::PushGas),
            "errset" => Opcode::AVMOpcode(AVMOpcode::ErrSet),
            "errpush" => Opcode::AVMOpcode(AVMOpcode::ErrPush),
            "errcodept" => Opcode::AVMOpcode(AVMOpcode::ErrCodePoint),
            "sideload" => Opcode::AVMOpcode(AVMOpcode::Sideload),
            "ecrecover" => Opcode::AVMOpcode(AVMOpcode::EcRecover),
            "ecadd" => Opcode::AVMOpcode(AVMOpcode::EcAdd),
            "ecmul" => Opcode::AVMOpcode(AVMOpcode::EcMul),
            "ecpairing" => Opcode::AVMOpcode(AVMOpcode::EcPairing),
            "addmod" => Opcode::AVMOpcode(AVMOpcode::AddMod),
            "mulmod" => Opcode::AVMOpcode(AVMOpcode::MulMod),
            "noop" => Opcode::AVMOpcode(AVMOpcode::Noop),
            _ => {
                panic!("opcode not supported in asm segment: {}", name);
            }
        }
    }

    pub fn to_name(&self) -> &str {
        match self {
            Opcode::AVMOpcode(avm) => avm.to_name(),
            Opcode::GetLocal(_) => "GetLocal",
            Opcode::SetLocal(_) => "SetLocal",
            Opcode::MoveLocal(_, _) => "MoveLocal",
            Opcode::DropLocal(_) => "DropLocal",
            Opcode::ReserveCapture(_, _) => "ReserveCapture",
            Opcode::Capture(_, _) => "Capture",
            Opcode::MakeClosure(_) => "MakeClosure",
            Opcode::GetGlobalVar(_) => "GetGlobal",
            Opcode::SetGlobalVar(_) => "SetGlobal",
            Opcode::Label(_) => "Label",
            Opcode::JumpTo(_) => "JumpTo",
            Opcode::CjumpTo(_) => "CjumpTo",
            Opcode::Pop(_) => "PopN",
            Opcode::TupleGet(_, _) => "TupleGet",
            Opcode::TupleSet(_, _) => "TupleSet",
            Opcode::Return => "return",
            Opcode::FuncCall(_) => "FuncCall",
            _ => "to_name() not implemented",
        }
    }
}

impl From<AVMOpcode> for Opcode {
    fn from(from: AVMOpcode) -> Self {
        Opcode::AVMOpcode(from)
    }
}

impl AVMOpcode {
    fn to_name(&self) -> &str {
        match self {
            AVMOpcode::Rpush => "rpush",
            AVMOpcode::Rset => "rset",
            AVMOpcode::Spush => "spush",
            AVMOpcode::Tset => "tset",
            AVMOpcode::Tget => "tget",
            AVMOpcode::Pop => "pop",
            AVMOpcode::StackEmpty => "stackempty",
            AVMOpcode::AuxPush => "auxpush",
            AVMOpcode::AuxPop => "auxpop",
            AVMOpcode::AuxStackEmpty => "auxstackempty",
            AVMOpcode::Xget => "xget",
            AVMOpcode::Xset => "xset",
            AVMOpcode::Dup0 => "dup0",
            AVMOpcode::Dup1 => "dup1",
            AVMOpcode::Dup2 => "dup2",
            AVMOpcode::Swap1 => "swap1",
            AVMOpcode::Swap2 => "swap2",
            AVMOpcode::BitwiseNeg => "bitwiseneg",
            AVMOpcode::Hash => "hash",
            AVMOpcode::EthHash2 => "ethhash2",
            AVMOpcode::Type => "type",
            AVMOpcode::Keccakf => "keccakf",
            AVMOpcode::Sha256f => "sha256f",
            AVMOpcode::Ripemd160f => "ripemd160f",
            AVMOpcode::Blake2f => "blake2f",
            AVMOpcode::Tlen => "length",
            AVMOpcode::Add => "add",
            AVMOpcode::Sub => "sub",
            AVMOpcode::Mul => "mul",
            AVMOpcode::Div => "div",
            AVMOpcode::Mod => "mod",
            AVMOpcode::Sdiv => "sdiv",
            AVMOpcode::Smod => "smod",
            AVMOpcode::AddMod => "addmod",
            AVMOpcode::MulMod => "mulmod",
            AVMOpcode::Exp => "exp",
            AVMOpcode::LessThan => "lt",
            AVMOpcode::GreaterThan => "gt",
            AVMOpcode::SLessThan => "slt",
            AVMOpcode::SGreaterThan => "sgt",
            AVMOpcode::Equal => "eq",
            AVMOpcode::IsZero => "iszero",
            AVMOpcode::Byte => "byte",
            AVMOpcode::SignExtend => "signextend",
            AVMOpcode::ShiftLeft => "shl",
            AVMOpcode::ShiftRight => "shr",
            AVMOpcode::ShiftArith => "sar",
            AVMOpcode::BitwiseAnd => "bitwiseand",
            AVMOpcode::BitwiseOr => "bitwiseor",
            AVMOpcode::BitwiseXor => "bitwisexor",
            AVMOpcode::Noop => "noop",
            AVMOpcode::ErrPush => "errpush",
            AVMOpcode::Inbox => "inbox",
            AVMOpcode::Error => "error",
            AVMOpcode::Zero => "zero",
            AVMOpcode::Halt => "halt",
            AVMOpcode::InboxPeek => "inboxpeek",
            AVMOpcode::Jump => "jump",
            AVMOpcode::Cjump => "cjump",
            AVMOpcode::PCpush => "pcpush",
            AVMOpcode::Breakpoint => "breakpoint",
            AVMOpcode::Log => "log",
            AVMOpcode::Send => "send",
            AVMOpcode::ErrCodePoint => "errcodept",
            AVMOpcode::PushInsn => "pushinsn",
            AVMOpcode::PushInsnImm => "pushinsnimm",
            AVMOpcode::OpenInsn => "openinsn",
            AVMOpcode::DebugPrint => "debugprint",
            AVMOpcode::SetGas => "setgas",
            AVMOpcode::PushGas => "pushgas",
            AVMOpcode::ErrSet => "errset",
            AVMOpcode::Sideload => "sideload",
            AVMOpcode::EcRecover => "ecrecover",
            AVMOpcode::EcAdd => "ecadd",
            AVMOpcode::EcMul => "ecmul",
            AVMOpcode::EcPairing => "ecpairing",
            AVMOpcode::NewBuffer => "newbuffer",
            AVMOpcode::GetBuffer8 => "getbuffer8",
            AVMOpcode::GetBuffer64 => "getbuffer64",
            AVMOpcode::GetBuffer256 => "getbuffer256",
            AVMOpcode::SetBuffer8 => "setbuffer8",
            AVMOpcode::SetBuffer64 => "setbuffer64",
            AVMOpcode::SetBuffer256 => "setbuffer256",
        }
    }

    pub fn from_number(num: usize) -> Option<Self> {
        match num {
            0x00 => Some(AVMOpcode::Zero),
            0x01 => Some(AVMOpcode::Add),
            0x02 => Some(AVMOpcode::Mul),
            0x03 => Some(AVMOpcode::Sub),
            0x04 => Some(AVMOpcode::Div),
            0x05 => Some(AVMOpcode::Sdiv),
            0x06 => Some(AVMOpcode::Mod),
            0x07 => Some(AVMOpcode::Smod),
            0x08 => Some(AVMOpcode::AddMod),
            0x09 => Some(AVMOpcode::MulMod),
            0x0a => Some(AVMOpcode::Exp),
            0x0b => Some(AVMOpcode::SignExtend),
            0x10 => Some(AVMOpcode::LessThan),
            0x11 => Some(AVMOpcode::GreaterThan),
            0x12 => Some(AVMOpcode::SLessThan),
            0x13 => Some(AVMOpcode::SGreaterThan),
            0x14 => Some(AVMOpcode::Equal),
            0x15 => Some(AVMOpcode::IsZero),
            0x16 => Some(AVMOpcode::BitwiseAnd),
            0x17 => Some(AVMOpcode::BitwiseOr),
            0x18 => Some(AVMOpcode::BitwiseXor),
            0x19 => Some(AVMOpcode::BitwiseNeg),
            0x1a => Some(AVMOpcode::Byte),
            0x1b => Some(AVMOpcode::ShiftLeft),
            0x1c => Some(AVMOpcode::ShiftRight),
            0x1d => Some(AVMOpcode::ShiftArith),
            0x20 => Some(AVMOpcode::Hash),
            0x21 => Some(AVMOpcode::Type),
            0x22 => Some(AVMOpcode::EthHash2),
            0x23 => Some(AVMOpcode::Keccakf),
            0x24 => Some(AVMOpcode::Sha256f),
            0x25 => Some(AVMOpcode::Ripemd160f),
            0x26 => Some(AVMOpcode::Blake2f),
            0x30 => Some(AVMOpcode::Pop),
            0x31 => Some(AVMOpcode::Spush),
            0x32 => Some(AVMOpcode::Rpush),
            0x33 => Some(AVMOpcode::Rset),
            0x34 => Some(AVMOpcode::Jump),
            0x35 => Some(AVMOpcode::Cjump),
            0x36 => Some(AVMOpcode::StackEmpty),
            0x37 => Some(AVMOpcode::PCpush),
            0x38 => Some(AVMOpcode::AuxPush),
            0x39 => Some(AVMOpcode::AuxPop),
            0x3a => Some(AVMOpcode::AuxStackEmpty),
            0x3b => Some(AVMOpcode::Noop),
            0x3c => Some(AVMOpcode::ErrPush),
            0x3d => Some(AVMOpcode::ErrSet),
            0x40 => Some(AVMOpcode::Dup0),
            0x41 => Some(AVMOpcode::Dup1),
            0x42 => Some(AVMOpcode::Dup2),
            0x43 => Some(AVMOpcode::Swap1),
            0x44 => Some(AVMOpcode::Swap2),
            0x50 => Some(AVMOpcode::Tget),
            0x51 => Some(AVMOpcode::Tset),
            0x52 => Some(AVMOpcode::Tlen),
            0x53 => Some(AVMOpcode::Xget),
            0x54 => Some(AVMOpcode::Xset),
            0x60 => Some(AVMOpcode::Breakpoint),
            0x61 => Some(AVMOpcode::Log),
            0x70 => Some(AVMOpcode::Send),
            0x71 => Some(AVMOpcode::InboxPeek),
            0x72 => Some(AVMOpcode::Inbox),
            0x73 => Some(AVMOpcode::Error),
            0x74 => Some(AVMOpcode::Halt),
            0x75 => Some(AVMOpcode::SetGas),
            0x76 => Some(AVMOpcode::PushGas),
            0x77 => Some(AVMOpcode::ErrCodePoint),
            0x78 => Some(AVMOpcode::PushInsn),
            0x79 => Some(AVMOpcode::PushInsnImm),
            0x7a => Some(AVMOpcode::OpenInsn),
            0x7b => Some(AVMOpcode::Sideload),
            0x80 => Some(AVMOpcode::EcRecover),
            0x81 => Some(AVMOpcode::EcAdd),
            0x82 => Some(AVMOpcode::EcMul),
            0x83 => Some(AVMOpcode::EcPairing),
            0x90 => Some(AVMOpcode::DebugPrint),
            0xa0 => Some(AVMOpcode::NewBuffer),
            0xa1 => Some(AVMOpcode::GetBuffer8),
            0xa2 => Some(AVMOpcode::GetBuffer64),
            0xa3 => Some(AVMOpcode::GetBuffer256),
            0xa4 => Some(AVMOpcode::SetBuffer8),
            0xa5 => Some(AVMOpcode::SetBuffer64),
            0xa6 => Some(AVMOpcode::SetBuffer256),
            _ => None,
        }
    }

    pub fn to_number(&self) -> u8 {
        match self {
            AVMOpcode::Zero => 0,
            AVMOpcode::Add => 0x01,
            AVMOpcode::Mul => 0x02,
            AVMOpcode::Sub => 0x03,
            AVMOpcode::Div => 0x04,
            AVMOpcode::Sdiv => 0x05,
            AVMOpcode::Mod => 0x06,
            AVMOpcode::Smod => 0x07,
            AVMOpcode::AddMod => 0x08,
            AVMOpcode::MulMod => 0x09,
            AVMOpcode::Exp => 0x0a,
            AVMOpcode::SignExtend => 0x0b,
            AVMOpcode::LessThan => 0x10,
            AVMOpcode::GreaterThan => 0x11,
            AVMOpcode::SLessThan => 0x12,
            AVMOpcode::SGreaterThan => 0x13,
            AVMOpcode::Equal => 0x14,
            AVMOpcode::IsZero => 0x15,
            AVMOpcode::BitwiseAnd => 0x16,
            AVMOpcode::BitwiseOr => 0x17,
            AVMOpcode::BitwiseXor => 0x18,
            AVMOpcode::BitwiseNeg => 0x19,
            AVMOpcode::Byte => 0x1a,
            AVMOpcode::ShiftLeft => 0x1b,
            AVMOpcode::ShiftRight => 0x1c,
            AVMOpcode::ShiftArith => 0x1d,
            AVMOpcode::Hash => 0x20,
            AVMOpcode::Type => 0x21,
            AVMOpcode::EthHash2 => 0x22,
            AVMOpcode::Keccakf => 0x23,
            AVMOpcode::Sha256f => 0x24,
            AVMOpcode::Ripemd160f => 0x25,
            AVMOpcode::Blake2f => 0x26,
            AVMOpcode::Pop => 0x30,
            AVMOpcode::Spush => 0x31,
            AVMOpcode::Rpush => 0x32,
            AVMOpcode::Rset => 0x33,
            AVMOpcode::Jump => 0x34,
            AVMOpcode::Cjump => 0x35,
            AVMOpcode::StackEmpty => 0x36,
            AVMOpcode::PCpush => 0x37,
            AVMOpcode::AuxPush => 0x38,
            AVMOpcode::AuxPop => 0x39,
            AVMOpcode::AuxStackEmpty => 0x3a,
            AVMOpcode::Noop => 0x3b,
            AVMOpcode::ErrPush => 0x3c,
            AVMOpcode::ErrSet => 0x3d,
            AVMOpcode::Dup0 => 0x40,
            AVMOpcode::Dup1 => 0x41,
            AVMOpcode::Dup2 => 0x42,
            AVMOpcode::Swap1 => 0x43,
            AVMOpcode::Swap2 => 0x44,
            AVMOpcode::Tget => 0x50,
            AVMOpcode::Tset => 0x51,
            AVMOpcode::Tlen => 0x52,
            AVMOpcode::Xget => 0x53,
            AVMOpcode::Xset => 0x54,
            AVMOpcode::Breakpoint => 0x60,
            AVMOpcode::Log => 0x61,
            AVMOpcode::Send => 0x70,
            AVMOpcode::InboxPeek => 0x71,
            AVMOpcode::Inbox => 0x72,
            AVMOpcode::Error => 0x73,
            AVMOpcode::Halt => 0x74,
            AVMOpcode::SetGas => 0x75,
            AVMOpcode::PushGas => 0x76,
            AVMOpcode::ErrCodePoint => 0x77,
            AVMOpcode::PushInsn => 0x78,
            AVMOpcode::PushInsnImm => 0x79,
            AVMOpcode::OpenInsn => 0x7a,
            AVMOpcode::Sideload => 0x7b,
            AVMOpcode::EcRecover => 0x80,
            AVMOpcode::EcAdd => 0x81,
            AVMOpcode::EcMul => 0x82,
            AVMOpcode::EcPairing => 0x83,
            AVMOpcode::DebugPrint => 0x90,
            AVMOpcode::NewBuffer => 0xa0,
            AVMOpcode::GetBuffer8 => 0xa1,
            AVMOpcode::GetBuffer64 => 0xa2,
            AVMOpcode::GetBuffer256 => 0xa3,
            AVMOpcode::SetBuffer8 => 0xa4,
            AVMOpcode::SetBuffer64 => 0xa5,
            AVMOpcode::SetBuffer256 => 0xa6,
        }
    }
}

#[test]
fn test_consistent_opcode_numbers() {
    for i in 0..256 {
        if let Some(op) = AVMOpcode::from_number(i) {
            assert_eq!(i as u8, op.to_number());
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::MakeFrame(space, returns, prebuilt) => {
                write!(f, "MakeFrame({}, {}, {})", space, returns, prebuilt)
            }
            Opcode::Label(label) => label.fmt(f),
            _ => write!(f, "{}", self.to_name()),
        }
    }
}

impl fmt::Display for AVMOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_name())
    }
}
