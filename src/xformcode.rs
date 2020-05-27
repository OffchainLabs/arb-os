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

use crate::mavm::{CodePt, Instruction, Opcode, Value};
use crate::pos::Location;
use crate::uint256::Uint256;

pub const TUPLE_SIZE: usize = 8;

pub fn fix_tuple_size(code_in: &[Instruction], num_globals: usize) -> Vec<Instruction> {
    let mut code_out = Vec::new();
    let mut locals_tree = TupleTree::new(1, true);
    let global_tree = TupleTree::new(num_globals, false);

    for insn in code_in.iter() {
        let location = insn.location;
        match insn.opcode {
            Opcode::MakeFrame(nargs, ntotal) => {
                code_out.push(Instruction::from_opcode(Opcode::AuxPush, location)); // move return address to aux stack
                locals_tree = TupleTree::new(ntotal, true);
                if let Some(imm) = &insn.immediate {
                    code_out.push(Instruction::from_opcode_imm(
                        Opcode::Noop,
                        imm.clone(),
                        location,
                    ));
                }
                code_out.push(Instruction::from_opcode_imm(
                    Opcode::AuxPush,
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
                code_out.push(Instruction::from_opcode(Opcode::Rget, location));
                code_out = global_tree.write_code(false, idx, &mut code_out, location);
                code_out.push(Instruction::from_opcode(Opcode::Rset, location));
            }
            Opcode::GetGlobalVar(idx) => {
                code_out.push(Instruction::from_opcode(Opcode::Rget, location));
                code_out = global_tree.read_code(false, idx, &mut code_out, location);
            }
            Opcode::Return => {
                code_out.push(Instruction::new(
                    Opcode::AuxPop,
                    match &insn.immediate {
                        Some(v) => Some(v.clone()),
                        None => None,
                    },
                    location,
                ));
                code_out.push(Instruction::from_opcode(Opcode::Pop, location));
                code_out.push(Instruction::from_opcode(Opcode::AuxPop, location));
                code_out.push(Instruction::from_opcode(Opcode::Jump, location));
            }
            Opcode::UncheckedFixedArrayGet(sz) => {
                let tup_size_val = Value::Int(Uint256::from_usize(TUPLE_SIZE));
                let mut remaining_size = sz;
                while remaining_size > TUPLE_SIZE {
                    //TODO: can probably make this more efficient
                    // stack: idx arr
                    code_out.push(Instruction::from_opcode_imm(
                        Opcode::Dup1,
                        tup_size_val.clone(),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(Opcode::Mod, location));
                    code_out.push(Instruction::from_opcode(Opcode::Swap1, location));
                    // stack: idx slot arr
                    code_out.push(Instruction::from_opcode_imm(
                        Opcode::Swap1,
                        tup_size_val.clone(),
                        location,
                    ));
                    code_out.push(Instruction::from_opcode(Opcode::Div, location));
                    // stack: subindex slot arr
                    code_out.push(Instruction::from_opcode(Opcode::Swap2, location));
                    code_out.push(Instruction::from_opcode(Opcode::Swap1, location));
                    // stack: slot arr subindex
                    code_out.push(Instruction::from_opcode(Opcode::Tget, location));
                    code_out.push(Instruction::from_opcode(Opcode::Swap1, location));
                    // stack: subindex subarr
                    remaining_size = (remaining_size + (TUPLE_SIZE - 1)) / TUPLE_SIZE;
                }
                code_out.push(Instruction::from_opcode(Opcode::Tget, location));
            }
            _ => {
                code_out.push(insn.clone());
            }
        }
    }
    code_out
}

pub fn jump_table_to_value(jump_table: Vec<CodePt>) -> Value {
    let mut jump_table_codepoints = Vec::new();
    for pc in &jump_table {
        jump_table_codepoints.push(Value::CodePoint(*pc));
    }
    let shape = TupleTree::new(jump_table.len(), false);
    shape.make_value(jump_table_codepoints)
}

pub fn make_uninitialized_tuple(size: usize) -> Value {
    TupleTree::new(size, false).make_empty()
}

#[derive(Debug)]
enum TupleTree {
    Single,
    Tree(usize, Vec<TupleTree>),
}

impl TupleTree {
    fn new(size: usize, is_local: bool) -> TupleTree {
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

    fn make_empty(&self) -> Value {
        match self {
            TupleTree::Single => Value::Tuple(Vec::new()),
            TupleTree::Tree(_, v) => {
                let mut tvec: Vec<Value> = Vec::new();
                for tt in v.iter() {
                    tvec.push(tt.make_empty());
                }
                Value::Tuple(tvec)
            }
        }
    }

    fn make_value(&self, vals: Vec<Value>) -> Value {
        let (val, _) = self.make_value_2(vals);
        val
    }

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
                (Value::Tuple(ret), vals)
            }
        }
    }

    fn tsize(&self) -> usize {
        match self {
            TupleTree::Single => 1,
            TupleTree::Tree(sz, _) => *sz,
        }
    }

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
                    code.push(Instruction::from_opcode(Opcode::AuxPop, location));
                    code.push(Instruction::from_opcode(Opcode::Dup0, location));
                    code.push(Instruction::from_opcode(Opcode::AuxPush, location));
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
                            if is_local { Opcode::Xget } else { Opcode::Tget },
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
                                if is_local { Opcode::Xset } else { Opcode::Tset },
                                Value::Int(Uint256::from_usize(slot)),
                                location,
                            ));
                            return code.to_vec();
                        }
                        TupleTree::Tree(_, _) => {
                            if is_local {
                                code.push(Instruction::from_opcode_imm(
                                    Opcode::Xget,
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            } else {
                                code.push(Instruction::from_opcode(Opcode::Swap1, location));
                                code.push(Instruction::from_opcode(Opcode::Dup1, location));
                                code.push(Instruction::from_opcode_imm(
                                    Opcode::Tget,
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            }
                            let mut new_code = subtree.write_code(false, index, code, location);
                            if is_local {
                                new_code.push(Instruction::from_opcode_imm(
                                    Opcode::Xset,
                                    Value::Int(Uint256::from_usize(slot)),
                                    location,
                                ));
                            } else {
                                new_code.push(Instruction::from_opcode(Opcode::Swap1, location));
                                new_code.push(Instruction::from_opcode_imm(
                                    Opcode::Tset,
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
            code.push(Instruction::from_opcode(Opcode::Pop, location));
            code.to_vec()
        }
    }
}

pub fn value_from_field_list(lis: Vec<Value>) -> Value {
    TupleTree::new(lis.len(), false).make_value(lis)
}
