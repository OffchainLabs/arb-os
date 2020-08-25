/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides utilities for dealing with nested tuples and conversion from large flat tuples to
//! nested tuples

use crate::mavm::{AVMOpcode, CodePt, Instruction, Opcode, Value};
use crate::pos::Location;
use crate::uint256::Uint256;

///The maximum size of an AVM tuple
pub const TUPLE_SIZE: usize = 8;

///Takes a slice of instructions from a single function scope, and changes tuples of size greater
/// than TUPLE_SIZE to nested tuples with each subtuple at most TUPLE_SIZE
pub fn fix_tuple_size(code_in: &[Instruction], num_globals: usize) -> Vec<Instruction> {
    let mut code_out = Vec::new();
    let mut locals_tree = TupleTree::new(1, true);
    let global_tree = TupleTree::new(num_globals, false);

    for insn in code_in.iter() {
        let location = insn.location;
        match insn.opcode {
            Opcode::MakeFrame(nargs, ntotal) => {
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::AuxPush),
                    location,
                )); // move return address to aux stack
                locals_tree = TupleTree::new(ntotal, true);
                if let Some(imm) = &insn.immediate {
                    code_out.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Noop),
                        imm.clone(),
                        location,
                    ));
                }
                code_out.push(Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::AuxPush),
                    TupleTree::make_empty(&locals_tree),
                    location,
                ));
                for lnum in 0..nargs {
                    code_out = locals_tree.write_code(true, lnum, &mut code_out, location);
                }
            }
            Opcode::TupleGet(size) => {
                let ttree = TupleTree::new(size, false);
                if let Some(index) = &insn.immediate {
                    match index.to_usize() {
                        Some(iu) => {
                            code_out = ttree.read_code(false, iu, &mut code_out, location);
                        }
                        None => panic!("fix_tuple_size: index too large"),
                    }
                } else {
                    panic!("fix_tuple_size: TupleGet without immediate arg")
                }
            }
            Opcode::TupleSet(size) => {
                let ttree = TupleTree::new(size, false);
                if let Some(index) = &insn.immediate {
                    match index.to_usize() {
                        Some(iu) => {
                            code_out = ttree.write_code(false, iu, &mut code_out, location);
                        }
                        None => panic!("fix_tuple_size: TupleSet index too large"),
                    }
                } else {
                    panic!("fix_tuple_size: TupleSet without immediate arg")
                }
            }
            Opcode::SetLocal => {
                if let Some(index) = &insn.immediate {
                    match index.to_usize() {
                        Some(iu) => {
                            code_out = locals_tree.write_code(true, iu, &mut code_out, location);
                        }
                        None => panic!("fix_tuple_size: index too large"),
                    }
                } else {
                    panic!("fix_tuple_size: SetLocal without immediate arg")
                }
            }
            Opcode::GetLocal => {
                if let Some(index) = &insn.immediate {
                    match index.to_usize() {
                        Some(iu) => {
                            code_out = locals_tree.read_code(true, iu, &mut code_out, location);
                        }
                        None => panic!("fix_tuple_size: index too large"),
                    }
                } else {
                    panic!("fix_tuple_size: GetLocal without immediate arg")
                }
            }
            Opcode::SetGlobalVar(idx) => {
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Rget),
                    location,
                ));
                code_out = global_tree.write_code(false, idx, &mut code_out, location);
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Rset),
                    location,
                ));
            }
            Opcode::GetGlobalVar(idx) => {
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Rget),
                    location,
                ));
                code_out = global_tree.read_code(false, idx, &mut code_out, location);
            }
            Opcode::Return => {
                code_out.push(Instruction::new(
                    Opcode::AVMOpcode(AVMOpcode::AuxPop),
                    match &insn.immediate {
                        Some(v) => Some(v.clone()),
                        None => None,
                    },
                    location,
                ));
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Pop),
                    location,
                ));
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::AuxPop),
                    location,
                ));
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Jump),
                    location,
                ));
            }
            Opcode::UncheckedFixedArrayGet(sz) => {
                let tup_size_val = Value::Int(Uint256::from_usize(TUPLE_SIZE));
                let mut remaining_size = sz;
                while remaining_size > TUPLE_SIZE {
                    //TODO: can probably make this more efficient
                    // stack: idx arr
                    code_out.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Dup1),
                        tup_size_val.clone(),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Mod),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Swap1),
                        location,
                    ));
                    // stack: idx slot arr
                    code_out.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Swap1),
                        tup_size_val.clone(),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Div),
                        location,
                    ));
                    // stack: subindex slot arr
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Swap2),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Swap1),
                        location,
                    ));
                    // stack: slot arr subindex
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Tget),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Swap1),
                        location,
                    ));
                    // stack: subindex subarr
                    remaining_size = (remaining_size + (TUPLE_SIZE - 1)) / TUPLE_SIZE;
                }
                code_out.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Tget),
                    location,
                ));
            }
            _ => {
                code_out.push(insn.clone());
            }
        }
    }
    code_out
}

///Used for generating the static_val for a `LinkedProgram`.
///
/// Takes a vector of codepoints, and places them in order into a nested tuple `Value`
pub fn jump_table_to_value(jump_table: Vec<CodePt>) -> Value {
    let mut jump_table_codepoints = Vec::new();
    for pc in &jump_table {
        jump_table_codepoints.push(Value::CodePoint(*pc));
    }
    let shape = TupleTree::new(jump_table.len(), false);
    shape.make_value(jump_table_codepoints)
}

///Generates a `Value` that is a nested tuple with size total leaf values, all leaf values are null.
pub fn make_uninitialized_tuple(size: usize) -> Value {
    TupleTree::new(size, false).make_empty()
}

///
#[derive(Debug)]
pub enum TupleTree {
    Single,
    Tree(usize, Vec<TupleTree>),
}

impl TupleTree {
    ///Constructs new `TupleTree` with capacity of size.
    ///
    /// The is_local argument indicates whether the `TupleTree` is intended to be used for locals,
    /// if set to false and size is 1 will return the Single variant, and will return the Tree
    /// variant otherwise.
    pub fn new(size: usize, is_local: bool) -> TupleTree {
        if (size == 1) && !is_local {
            return TupleTree::Single;
        }
        let mut current_size: usize = 1;
        while current_size * (TUPLE_SIZE * TUPLE_SIZE) <= size {
            current_size *= TUPLE_SIZE;
        }

        let mut v = Vec::new();
        let mut remaining_size = size;
        let mut remaining_slots = TUPLE_SIZE;

        while remaining_size > 0 {
            if current_size >= remaining_size {
                v.push(TupleTree::new(remaining_size, false));
                remaining_size = 0;
            } else if current_size * (1 + (remaining_slots - 1) * TUPLE_SIZE) >= remaining_size {
                v.push(TupleTree::new(current_size, false));
                remaining_size -= current_size;
                remaining_slots -= 1;
            } else {
                current_size *= TUPLE_SIZE;
            }
        }
        TupleTree::Tree(size, v)
    }

    ///Creates a `Value` with the same tree structure as self.
    pub fn make_empty(&self) -> Value {
        match self {
            TupleTree::Single => Value::new_tuple(Vec::new()),
            TupleTree::Tree(_, v) => {
                let mut tvec: Vec<Value> = Vec::new();
                for tt in v.iter() {
                    tvec.push(tt.make_empty());
                }
                Value::new_tuple(tvec)
            }
        }
    }

    ///Create a nested tuple `Value` with structure determined by self, and leaf nodes determined
    /// left to right by vals.
    fn make_value(&self, vals: Vec<Value>) -> Value {
        let (val, _) = self.make_value_2(vals);
        val
    }

    ///Internal call used by `make_value`.
    ///
    /// The returned `Value` is the value constructed from vals, and the returned `Vec<Values>` are
    /// the unconsumed `Value`s in vals.
    fn make_value_2(&self, vals: Vec<Value>) -> (Value, Vec<Value>) {
        match self {
            TupleTree::Single => (vals[0].clone(), vals[1..].to_vec()),
            TupleTree::Tree(_, subtrees) => {
                let mut ret = Vec::new();
                let mut vals = vals;
                for subtree in subtrees {
                    let (subval, new_vals) = subtree.make_value_2(vals);
                    ret.push(subval);
                    vals = new_vals;
                }
                (Value::new_tuple(ret), vals)
            }
        }
    }

    ///Gets the total number of nodes in the `TupleTree`
    fn tsize(&self) -> usize {
        match self {
            TupleTree::Single => 1,
            TupleTree::Tree(sz, _) => *sz,
        }
    }

    ///Generates code for pushing a copy the index-th element of self to the top of the stack.
    ///
    /// Argument is_local defines whether locals or globals are being accessed, generated code is
    /// appended to the code argument and also returned as an owned value, location is used to
    /// assign a code location for the generated instructions.
    fn read_code(
        &self,
        is_local: bool,
        index: usize,
        code: &mut Vec<Instruction>,
        location: Option<Location>,
    ) -> Vec<Instruction> {
        match self {
            TupleTree::Single => {
                if is_local {
                    code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::AuxPop),
                        location,
                    ));
                    code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Dup0),
                        location,
                    ));
                    code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::AuxPush),
                        location,
                    ));
                    code.to_vec()
                } else {
                    code.to_vec()
                }
            }
            TupleTree::Tree(_, v) => {
                let mut index = index;
                for (slot, subtree) in v.iter().enumerate() {
                    if index < subtree.tsize() {
                        code.push(Instruction::from_opcode_imm(
                            if is_local {
                                Opcode::AVMOpcode(AVMOpcode::Xget)
                            } else {
                                Opcode::AVMOpcode(AVMOpcode::Tget)
                            },
                            Value::Int(Uint256::from_usize(slot)),
                            location,
                        ));
                        return subtree.read_code(false, index, code, location);
                    } else {
                        index -= subtree.tsize();
                    }
                }
                panic!("TupleTree::read_code: out-of-bounds read");
            }
        }
    }

    ///Generates code for writing to the index_in-th element of self.
    ///
    /// Argument is_local defines whether locals or globals are being accessed, generated code is
    /// appended to the code argument and also returned as an owned value, location is used to
    /// assign a code location for the generated instructions.
    fn write_code(
        &self,
        is_local: bool,
        index_in: usize,
        code: &mut Vec<Instruction>,
        location: Option<Location>,
    ) -> Vec<Instruction> {
        if let TupleTree::Tree(_, v) = self {
            let mut index = index_in;
            for (slot, subtree) in v.iter().enumerate() {
                if index < subtree.tsize() {
                    match *subtree {
                        TupleTree::Single => {
                            code.push(Instruction::from_opcode_imm(
                                if is_local {
                                    Opcode::AVMOpcode(AVMOpcode::Xset)
                                } else {
                                    Opcode::AVMOpcode(AVMOpcode::Tset)
                                },
                                Value::Int(Uint256::from_usize(slot)),
                                location,
                            ));
                            return code.to_vec();
                        }
                        TupleTree::Tree(_, _) => {
                            if is_local {
                                code.push(Instruction::from_opcode_imm(
                                    Opcode::AVMOpcode(AVMOpcode::Xget),
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            } else {
                                code.push(Instruction::from_opcode(
                                    Opcode::AVMOpcode(AVMOpcode::Swap1),
                                    location,
                                ));
                                code.push(Instruction::from_opcode(
                                    Opcode::AVMOpcode(AVMOpcode::Dup1),
                                    location,
                                ));
                                code.push(Instruction::from_opcode_imm(
                                    Opcode::AVMOpcode(AVMOpcode::Tget),
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            }
                            let mut new_code = subtree.write_code(false, index, code, location);
                            if is_local {
                                new_code.push(Instruction::from_opcode_imm(
                                    Opcode::AVMOpcode(AVMOpcode::Xset),
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            } else {
                                new_code.push(Instruction::from_opcode(
                                    Opcode::AVMOpcode(AVMOpcode::Swap1),
                                    location,
                                ));
                                new_code.push(Instruction::from_opcode_imm(
                                    Opcode::AVMOpcode(AVMOpcode::Tset),
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            }
                            return new_code;
                        }
                    }
                } else {
                    index -= subtree.tsize();
                }
            }
            panic!("TupleTree::write_code: out-of-bounds write");
        } else {
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Pop),
                location,
            ));
            code.to_vec()
        }
    }
}

///Generates a `TupleTree` of size equal to the length of lis, and fills its leaf nodes with the
/// `Value`s in lis.
pub fn value_from_field_list(lis: Vec<Value>) -> Value {
    TupleTree::new(lis.len(), false).make_value(lis)
}
