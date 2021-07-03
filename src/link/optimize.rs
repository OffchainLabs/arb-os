/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functions for modifying a sequence of Instructions to improve performance and lower gas
//! costs.

use crate::console::ConsoleColors;
use crate::mavm::{AVMOpcode, Instruction, Opcode, OpcodeEffect, Value};
use crate::uint256::Uint256;
use crate::link::{FlowGraph};
use crate::link::analyze::{print_code, print_cfg, create_cfg, flatten_cfg, eval_stacks};
use crate::compile::{DebugInfo, CompileError};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::sync::Arc;
use std::convert::TryInto;
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use petgraph::graph::{DiGraph, NodeIndex, EdgeIndex, EdgeReference};
use petgraph::stable_graph::{StableGraph};
use petgraph::dot::{Dot, Config};
use rand::{thread_rng, SeedableRng};
use rand::rngs::{SmallRng};
use rand::seq::SliceRandom;

type Block = Vec<Instruction>;

pub fn corrupt_block(block: &Block, _debug: bool) -> (Block, bool) {
    
    let mut opt = vec![];

    for index in 0..block.len() {
        if index != 8 {
            opt.push(block[index].clone());
        }
    }
    
    (opt, true)
}

///Removes instructions that have no effect on the output of the program.
fn useless_opcodes_layer<'a, I>(iter: I) -> impl Iterator<Item = &'a Instruction>
where
    I: Iterator<Item = &'a Instruction>,
{
    iter.filter(|&insn| {
        !(insn.opcode == Opcode::AVMOpcode(AVMOpcode::Noop) && insn.immediate.is_none()
            || insn.opcode == Opcode::AVMOpcode(AVMOpcode::Pop) && insn.immediate.is_some())
    })
}

///Takes a slice of `Instruction`s and returns a vector of instructions with certain combinations of
/// instructions recursively either removed or replaced by simpler instructions.
pub fn simplify_pairs(code_in: &[Instruction]) -> Vec<Instruction> {
    let mut code_out = Vec::new();
    for insn in useless_opcodes_layer(code_in.iter()) {
        code_out.push(insn.clone());
    }
    code_out
}

///Applies simple optimizations to adjacent instructions.
pub fn peephole(block: &Block, _debug: bool) -> (Block, bool) {
    let mut same = true;

    let better = simplify_pairs(&block);
    same = same && block.len() == better.len();
    let block = better;

    if block.len() < 2 {
        return (block.clone(), same);
    }

    let mut opt = Vec::with_capacity(block.len());
    let mut index = 0;

    while index < block.len() - 1 {
        let curr = &block[index];
        let next = &block[index + 1];
        let later = block.get(index + 2);

        let immediates = (curr.immediate.as_ref(), next.immediate.as_ref());
        
        
        match (curr.opcode, next.opcode) {
            (Opcode::AVMOpcode(AVMOpcode::Dup0), Opcode::AVMOpcode(AVMOpcode::Pop)) => {
                match immediates {
                    (None, None) => {
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::Dup0), Opcode::AVMOpcode(AVMOpcode::Swap1)) => {
                match immediates {
                    (_, None) => {
                        opt.push(curr.clone());
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::AuxPop), Opcode::AVMOpcode(AVMOpcode::AuxPush)) => {
                match immediates {
                    (None, None) => {
                        index += 2;
                        continue;
                    }
                    (Some(value), None) => {
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                            immediate: Some(value.clone()),
                            debug_info: curr.debug_info,
                        });
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::AuxPush), Opcode::AVMOpcode(AVMOpcode::AuxPop)) => {
                match immediates {
                    (None, None) => {
                        index += 2;
                        continue;
                    }
                    (Some(value), None) => {
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                            immediate: Some(value.clone()),
                            debug_info: curr.debug_info,
                        });
                        index += 2;
                        continue;
                    }
                    (None, Some(value)) => {
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Swap1),
                            immediate: Some(value.clone()),
                            debug_info: next.debug_info,
                        });
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::Swap1), Opcode::AVMOpcode(AVMOpcode::Swap1))
            | (Opcode::AVMOpcode(AVMOpcode::Swap2), Opcode::AVMOpcode(AVMOpcode::Swap2)) => {
                match immediates {
                    (None, None) => {
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::Xget), Opcode::AVMOpcode(AVMOpcode::Pop)) => {
                match immediates {
                    (Some(_), None) => {
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::Tget), Opcode::AVMOpcode(AVMOpcode::Pop)) => {
                match immediates {
                    (Some(_), None) => {
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: curr.debug_info,
                        });
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::Noop), Opcode::AVMOpcode(AVMOpcode::Tget)) => {
                match immediates {
                    (Some(Value::Tuple(tup)), Some(Value::Int(spot))) => {
                        let spot = spot.to_usize().unwrap();

                        let value = &tup[spot];

                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                            immediate: Some(value.clone()),
                            debug_info: next.debug_info,
                        });

                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            (Opcode::AVMOpcode(AVMOpcode::Pop), ..) => match immediates {
                (Some(_), None) => {
                    index += 1;
                    continue;
                }
                _ => {}
            },
            (Opcode::AVMOpcode(AVMOpcode::Noop), ..) => match immediates {
                (Some(value), None) => {
                    match value {
                     // Value::Int(_)
                        
                        Value::Int(_) | Value::Tuple(_) | Value::Buffer(_)
                            
                     // | Value::Tuple(_)
                     // | Value::Buffer(_)
                     // | Value::Label(_)
                     // | Value::CodePoint(_)    
                            => {
                            opt.push(Instruction {
                                opcode: next.opcode.clone(),
                                immediate: Some(value.clone()),
                                debug_info: next.debug_info,
                            });
                            index += 2;
                            continue;
                        }
                        _ => {}
                    }
                }
                _ => {}
            },
            (Opcode::AVMOpcode(AVMOpcode::Xset), Opcode::AVMOpcode(AVMOpcode::Xget)) => {
                match immediates {
                    (Some(a), Some(b)) => {
                        if (a == b) {
                            opt.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                                immediate: None,
                                debug_info: next.debug_info,
                            });
                            opt.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                                immediate: None,
                                debug_info: next.debug_info,
                            });
                            opt.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::Dup0),
                                immediate: None,
                                debug_info: next.debug_info,
                            });
                            opt.push(curr.clone());
                            same = false;
                            index += 2;
                            continue;
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        
        if let Some(last) = later {
            let immediates = (
                curr.immediate.as_ref(),
                next.immediate.as_ref(),
                last.immediate.as_ref(),
            );

            match (curr.opcode, next.opcode, last.opcode) {
                (
                    Opcode::AVMOpcode(AVMOpcode::AuxPush),
                    Opcode::AVMOpcode(AVMOpcode::Pop),
                    Opcode::AVMOpcode(AVMOpcode::AuxPop),
                ) => match immediates {
                    (None, None, None) => {
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Swap1),
                            immediate: None,
                            debug_info: next.debug_info,
                        });
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: next.debug_info,
                        });
                        index += 3;
                        continue;
                    }
                    (None, None, Some(value)) => {
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Swap1),
                            immediate: None,
                            debug_info: next.debug_info,
                        });
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: next.debug_info,
                        });
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                            immediate: Some(value.clone()),
                            debug_info: next.debug_info,
                        });
                        same = false;
                        index += 3;
                        continue;
                    }
                    _ => {}
                },
                (
                    Opcode::AVMOpcode(AVMOpcode::Noop),
                    Opcode::AVMOpcode(AVMOpcode::Noop),
                    Opcode::AVMOpcode(AVMOpcode::Tset),
                ) => match immediates {
                    (Some(value), Some(Value::Tuple(tup)), Some(Value::Int(spot))) => {
                        let spot = spot.to_usize().unwrap();

                        let mut updated = tup.to_vec();
                        updated[spot] = value.clone();

                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                            immediate: Some(Value::Tuple(Arc::new(updated))),
                            debug_info: next.debug_info,
                        });

                        index += 3;
                        continue;
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        
        
        opt.push(curr.clone());
        index += 1;
    }
    opt.extend(block[index..].iter().cloned());
    same = same && block.len() == opt.len();
    (opt, same)
}

///Elides xgets that can be matched to xsets.
pub fn xget_elision(block: &Block, _debug: bool) -> (Block, bool) {
    
    let mut data_stack = vec![];
    let mut aux_stack = vec![];
    let mut modify = HashSet::new();
    
    for index in 0..block.len() {
        let curr = &block[index];
        
        let xset = match curr.opcode {
            Opcode::AVMOpcode(AVMOpcode::Xset) => true,
            _ => false,
        };
        let xget = match curr.opcode {
            Opcode::AVMOpcode(AVMOpcode::Xget) => true,
            _ => false,
        };
        
        let effects = curr.effects();
        
        for effect in effects {
            match effect {
                OpcodeEffect::PushStack => drop(data_stack.push((index, false))),
                OpcodeEffect::PushAux => drop(aux_stack.push((index, xset))),
                OpcodeEffect::PopStack => drop(data_stack.pop()),
                OpcodeEffect::PopAux => drop(aux_stack.pop()),
                OpcodeEffect::SwapStack(depth) => {
                    if depth + 1 > data_stack.len() {
                        data_stack.clear(); // this function does something nasty
                        aux_stack.clear(); //
                    } else if depth == 1 {
                        let lower = data_stack.pop().unwrap().clone();
                        let upper = data_stack.pop().unwrap().clone();
                        data_stack.push(lower);
                        data_stack.push(upper);
                    } else if depth == 2 {
                        let lower = data_stack.pop().unwrap().clone();
                        let still = data_stack.pop().unwrap().clone();
                        let upper = data_stack.pop().unwrap().clone();
                        data_stack.push(lower);
                        data_stack.push(still);
                        data_stack.push(upper);
                    } else {
                        panic!("Found an impossible stack action");
                    }
                }
                OpcodeEffect::ReadStack(depth) => {
                    if depth <= data_stack.len() {
                        let offset = data_stack.len() - depth;
                        data_stack[offset].1 = false;
                    }
                }
                OpcodeEffect::ReadAux => {
                    if let Some((origin, live_xset)) = aux_stack.pop() {
                        aux_stack.push((origin, false));
                        if live_xset && xget {
                            if block[origin].immediate == curr.immediate {
                                modify.insert(origin);
                                modify.insert(index);
                            }
                        }
                    }
                }
                OpcodeEffect::MoveToStack => {
                    match aux_stack.pop() {
                        Some(item) => drop(data_stack.push(item)),
                        None => {
                            data_stack.clear(); // this function does something nasty
                            aux_stack.clear(); //
                        }
                    }
                }
                OpcodeEffect::MoveToAux => {
                    match data_stack.pop() {
                        Some(item) => drop(aux_stack.push(item)),
                        None => {
                            data_stack.clear(); // this function does something nasty
                            aux_stack.clear(); //
                        }
                    }
                }
                OpcodeEffect::Unsure
                | OpcodeEffect::ReadGlobal
                | OpcodeEffect::WriteGlobal => {
                    data_stack.clear();
                    aux_stack.clear();
                }
            }
        }
    }
    
    let mut opt = Vec::with_capacity(block.len());
    
    for index in 0..block.len() {
        let curr = &block[index];
        
        match modify.get(&index) {
            None => drop(opt.push(curr.clone())),
            Some(_) => match curr.opcode {
                Opcode::AVMOpcode(AVMOpcode::Xset) => opt.extend(vec![Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                    immediate: None,
                    debug_info: curr.debug_info,
                }]),
                Opcode::AVMOpcode(AVMOpcode::Xget) => opt.extend(vec![
                    Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                        immediate: None,
                        debug_info: curr.debug_info,
                    },
                    Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Dup0),
                        immediate: None,
                        debug_info: curr.debug_info,
                    },
                    Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Xset),
                        immediate: curr.immediate.clone(),
                        debug_info: curr.debug_info,
                    },
                ]),
                _ => panic!("Oh no! Tried to xget-elide on an incorrect index"),
            },
        }
    }

    let same = opt.len() == block.len();
    (opt, same)
}

///Elides xsets who's inner tuple value is never used.
pub fn xset_tail_elision(block: &Block, _debug: bool) -> (Block, bool) {
    let mut same = true;
    
    if block.len() < 3 {
        return (block.clone(), same);
    }
    
    let mut tails = HashMap::new();
    let mut elide = HashSet::new();
    
    for index in 0..block.len() {
        let curr = &block[index];
        let next = block.get(index + 1);
        
        match curr.opcode {
            Opcode::AVMOpcode(AVMOpcode::Xset) => {
                match &curr.immediate {
                    None => tails.clear(),
                    Some(value) => match value.to_usize() {
                        None => tails.clear(),
                        Some(int) => elide.extend(tails.insert(int, index)), // overwrites last one
                    },
                }
            }
            Opcode::AVMOpcode(AVMOpcode::Xget) => match &curr.immediate {
                None => tails.clear(),
                Some(value) => match value.to_usize() {
                    None => tails.clear(),
                    Some(int) => drop(tails.remove(&int)),
                },
            },
            Opcode::AVMOpcode(AVMOpcode::AuxPush)
                | Opcode::AVMOpcode(AVMOpcode::Swap1)
                | Opcode::AVMOpcode(AVMOpcode::Swap2) => {
                    // This is way too conservative and will change soon.
                    // We can prove from codegen that this isn't needed, but
                    // to keep this from being fragile to codegen changes we'll drop
                    // these opportunities until a more complete analysis is made.
                    tails.clear();
                }
            Opcode::AVMOpcode(AVMOpcode::AuxPop) => {
                if let Some(Opcode::AVMOpcode(AVMOpcode::Pop)) = next.map(|next| next.opcode) {
                    for idx in tails.values() {
                        elide.insert(*idx);
                    }
                }
                tails.clear();
            }
            _ => {}
        }
        
        same = same && elide.len() == 0;
    }
    
    let mut opt = Vec::with_capacity(block.len());
    
    for index in 0..block.len() {
        let curr = &block[index];
        
        match elide.contains(&index) {
            false => opt.push(curr.clone()),
            true => opt.push(Instruction {
                opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                immediate: None,
                debug_info: curr.debug_info,
            }),
        }
    }
    (opt, same)
}

///Breaks up tset-tget pairs. For now this is a peephole strategy, but in the
/// future it'll track the stacks.
pub fn tuple_annihilation(block: &Block, _debug: bool) -> (Block, bool) {
    let mut same = true;
    
    if block.len() < 2 {
        return (block.clone(), same);
    }
    
    let mut opt = Vec::with_capacity(block.len());
    
    let mut index = 0;
    
    while index < block.len() - 1 {
        let curr = &block[index];
        let next = &block[index + 1];
        
        let immediates = (curr.immediate.as_ref(), next.immediate.as_ref());
        
        match (curr.opcode, next.opcode) {
            (Opcode::AVMOpcode(AVMOpcode::Tset), Opcode::AVMOpcode(AVMOpcode::Tget)) => {
                match immediates {
                    (Some(curr_value), Some(next_value)) => {
                        if curr_value != next_value {
                            opt.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::Swap1),
                                immediate: None,
                                debug_info: curr.debug_info,
                            });
                        }
                        
                        opt.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: next.debug_info,
                        });
                        
                        if curr_value != next_value {
                            opt.push(next.clone());
                        }
                        
                        same = false;
                        index += 2;
                        continue;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        
        opt.push(curr.clone());
        index += 1;
    }
    opt.extend(block[index..].iter().cloned());
    (opt, same)
}

#[derive(Clone, Debug, Default)]
pub struct StackEntry {
    created: Option<usize>,         // where this stack entry comes into existence
    moved: Vec<usize>,              // every time entry is moved but not read
    duped: Vec<(usize, Value)>,     // every time entry is duplicated
    used: Vec<usize>,               // every time value is actually used for something
    killed: Option<usize>,          // where this value is ultimately consumed
    value: Value,                   // the actual value of this stack entry
    replaced: Option<Value>,        // what this value gets replaced with
    was_immediate: bool,            // whether value came into existence as an immediate
    children: Vec<StackEntry>,      // the nested child entries
    block_id: Option<usize>,        // unique sequence number to differentiate blocked values
    blocks: Vec<usize>,             // values that can only be eliminated if this gets eliminated
}
impl StackEntry {
    fn blank() -> Self {
        StackEntry::default()
    }
    fn new(created: usize, value: &Value) -> Self {
	StackEntry::from(Some(created), value, false)
    }
    fn new_immediate(created: usize, value: &Value) -> Self {
        StackEntry::from(Some(created), value, true)
    }
    fn from(created: Option<usize>, value: &Value, was_immediate: bool) -> Self {
        let mut entry = StackEntry::blank();
        entry.created = created;
	entry.value = value.clone();
        entry.was_immediate = was_immediate;
        if let Value::Tuple(vec) = value {
            for sub_value in vec.iter() {
                entry.children.push(StackEntry::from(created, sub_value, was_immediate));
            }
        }
        return entry;
    }
    fn use_all(&mut self, user: usize) {
	for child in &mut self.children {
	    child.use_all(user);
	}
	self.used.push(user);
    }
    fn use_children(&mut self, user: usize) {
	self.use_all(user);
	self.used.pop();
    }
}

/// Discovers values that are never used.
pub fn discover_unused(block: &Block, debug: bool) -> Result<BTreeMap<usize, StackEntry>, usize> {
    let mut data_stack: Vec<StackEntry> = vec![];
    let mut aux_stack:  Vec<StackEntry> = vec![];
    let mut eliminate:  BTreeMap<usize, StackEntry> = BTreeMap::new();
    let mut blocked:    BTreeMap<usize, (usize, StackEntry)> = BTreeMap::new();
    let mut next_block: usize = 0;
    
    macro_rules! draw {
        ($stack:expr) => {
            $stack.pop().unwrap_or(StackEntry::blank())
        }
    }
    macro_rules! block {
        ($entry:expr) => {{
            let mut entry = $entry;
            match &mut entry.block_id {
                Some(id) => {
                    let (count, _) = blocked.get_mut(&id).expect("blocked entry not in tree");
                    *count += 1;
                }
                None => {
                    entry.block_id = Some(next_block);
                    blocked.insert(next_block, (1, entry.clone()));
                    next_block += 1;
                }
            }
            entry.block_id.expect("blocked entry was not assigned an id")
        }};
    }
    macro_rules! analyze {
        ($entry:expr) => {
            analyze(&$entry, &mut eliminate, &mut blocked);
        };
    }
    fn analyze(
        entry: &StackEntry,
        eliminate: &mut BTreeMap<usize, StackEntry>,
        blocked: &mut BTreeMap<usize, (usize, StackEntry)>,
    ) {        
        for child in &entry.children {
            analyze(child, eliminate, blocked);
        }
        
        if entry.used.len() == 0 && entry.killed.is_some() {
            
            for id in &entry.blocks {
                let (count, blocked_entry) = blocked.remove(&id).unwrap();
                match count {
                    1 => analyze(&blocked_entry, eliminate, blocked),
                    x => drop(blocked.insert(*id, (x - 1, blocked_entry))),
                }
            }
            
            match entry.created {
                Some(index) => {
                    eliminate.insert(index, entry.clone());    // peel children off
                }
                _ => {}
            }
        }
    }
    
    for index in 0..block.len() {
        
        match &block[index].immediate {
            Some(value) => data_stack.push(StackEntry::new_immediate(index, value)),
            None => {}
        }
        
        match &block[index].opcode {
            Opcode::AVMOpcode(AVMOpcode::Noop) => {}
            Opcode::AVMOpcode(AVMOpcode::Dup0) => {
                let mut a = draw!(data_stack);
                let duplicate = a.value.clone();
                //a.use_children(index);
                
                /*match &duplicate {
                    Some(value) => a.duped.push((index, value.clone())),
                    None => a.used.push(index),
            }*/
                //a.duped.push((index, value.clone()));
                
                data_stack.push(a);
                data_stack.push(StackEntry::new(index, &duplicate));
            }
            Opcode::AVMOpcode(AVMOpcode::Dup1) => {
                let mut a = draw!(data_stack);
                let mut b = draw!(data_stack);
                let duplicate = b.value.clone();
                //b.use_children(index);
                /*match &duplicate {
                    Some(value) => b.duped.push((index, value.clone())),
                    None => b.used.push(index),
                }*/
                a.moved.push(index);
                data_stack.push(b);
                data_stack.push(a);
                data_stack.push(StackEntry::new(index, &duplicate));
            }
            Opcode::AVMOpcode(AVMOpcode::Dup2) => {
                let mut a = draw!(data_stack);
                let mut b = draw!(data_stack);
                let mut c = draw!(data_stack);
                let duplicate = c.value.clone();
                //c.use_children(index);
                /*match &duplicate {
                    Some(value) => c.duped.push((index, value.clone())),
                    None => c.used.push(index),
                }*/
                b.moved.push(index);
                a.moved.push(index);
                data_stack.push(c);
                data_stack.push(b);
                data_stack.push(a);
                data_stack.push(StackEntry::new(index, &duplicate));
            }
            Opcode::AVMOpcode(AVMOpcode::Swap1) => {
                let a = draw!(data_stack);
                let b = draw!(data_stack);
                for mut entry in std::array::IntoIter::new([a, b]) {
                    entry.moved.push(index);
                    data_stack.push(entry);
                }
            }
            Opcode::AVMOpcode(AVMOpcode::Swap2) => {
                let a = draw!(data_stack);
                let b = draw!(data_stack);
                let c = draw!(data_stack);
                for mut entry in std::array::IntoIter::new([a, b, c]) {
                    entry.moved.push(index);
                    data_stack.push(entry);
                }
            }
            Opcode::AVMOpcode(AVMOpcode::Pop) => {
                let mut entry = draw!(data_stack);
		entry.killed = Some(index);
                analyze!(entry);
            }
            Opcode::AVMOpcode(AVMOpcode::Tset) => {
                let mut a = draw!(data_stack);    // index
                let mut b = draw!(data_stack);    // tuple
                let mut c = draw!(data_stack);    // value
                a.used.push(index);
                a.killed = Some(index);
                b.used.push(index);
                c.moved.push(index);
                analyze!(a);
                
                macro_rules! bail {
                    () => {{
                        data_stack.push(StackEntry::new(index, &Value::Unknown(0)));
                        continue;
                    }}
                }
                
                let tuple = match &b.value {
                    Value::Tuple(tuple) => tuple,
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                let offset = match &a.value {
                    Value::Int(offset) => match offset.to_usize() {
                        Some(offset) if offset < tuple.len() => offset,
                        _ => return Err(index),
                    }
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                
                let value = &c.value;
                
                let mut updated = tuple.to_vec();
                updated[offset] = value.clone();
                let updated = Value::new_tuple(updated);
                
		b.children[offset].killed = Some(index);
                b.children[offset].replaced = Some(updated.clone());
                
                //if let Value::Unknown = value {
                    b.blocks.push(block!(b.children[offset].clone()));
            //}
                
                analyze!(b.children[offset]);
                
                b.value = updated;
                b.children[offset] = c;
                data_stack.push(b);
            }
	    Opcode::AVMOpcode(AVMOpcode::Tget) => {
		let mut a = draw!(data_stack);    // index
                let mut b = draw!(data_stack);    // tuple
                a.used.push(index);
                b.used.push(index);
		a.killed = Some(index);
		b.killed = Some(index);
                analyze!(a);
                //analyze!(b);
                
                macro_rules! bail {
                    () => {{
                        data_stack.push(StackEntry::new(index, &Value::Unknown(0)));
                        continue;
                    }}
                }
                
                let tuple = match &b.value {
                    Value::Tuple(tuple) => tuple,
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                let offset = match &a.value {
                    Value::Int(offset) => match offset.to_usize() {
                        Some(offset) if offset < tuple.len() => offset,
                        _ => return Err(index),
                    }
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
		
		for (spot, child) in b.children.iter().enumerate() {
		    if spot != offset {
			analyze!(child);
		    }
		}
                
                let mut pulled = b.children.swap_remove(offset);
                let mut folded = pulled.clone();
                folded.killed = Some(index);
                folded.replaced = Some(folded.value.clone());
                analyze!(folded);
                pulled.created = Some(index);
                data_stack.push(pulled);
                
		/*let mut pulled = b.children.swap_remove(offset);
                pulled.created = Some(index);
                data_stack.push(pulled);*/
	    }
            Opcode::AVMOpcode(AVMOpcode::Plus) => {
                let mut a = draw!(data_stack);
                let mut b = draw!(data_stack);
                a.used.push(index);
                b.used.push(index);
                analyze!(a);
                analyze!(b);
                
                macro_rules! bail {
                    () => {{
                        data_stack.push(StackEntry::new(index, &Value::Unknown(0)));
                        continue;
                    }}
                }
                
                let left = match &a.value {
                    Value::Int(value) => value,
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                let right = match &b.value {
                    Value::Int(value) => value,
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                
                let result = StackEntry::new(index, &Value::Int(left.add(right)));
                let mut folded = result.clone();
                folded.killed = Some(index);
                folded.replaced = Some(folded.value.clone());
                analyze!(folded);
                data_stack.push(result);
            }
            Opcode::AVMOpcode(AVMOpcode::Minus) => {
                let mut a = draw!(data_stack);
                let mut b = draw!(data_stack);
                a.used.push(index);
                b.used.push(index);
                analyze!(a);
                analyze!(b);
                
                macro_rules! bail {
                    () => {{
                        data_stack.push(StackEntry::new(index, &Value::Unknown(0)));
                        continue;
                    }}
                }
                
                let left = match &a.value {
                    Value::Int(value) => value,
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                let right = match &b.value {
                    Value::Int(value) => value,
                    Value::Unknown(_) => bail!(),
                    _ => return Err(index),
                };
                
                let result = StackEntry::new(index, &Value::Int(left.unchecked_sub(right)));
                let mut folded = result.clone();
                folded.killed = Some(index);
                folded.replaced = Some(folded.value.clone());
                analyze!(folded);
                data_stack.push(result);
            }
            _ => {
		return Err(index);
            }
        }
    }
    
    Ok(eliminate)
}

///Discovers unused values and eliminates them from the stacks.
pub fn stack_reduce(block: &Block, debug: bool) -> (Block, bool) {
    let mut same = true;
    let insert_noops = true;

    // Algorithm notes
    //   discover_unused() returns a list of stack items that we can provably elide.
    //   We select the first one and eliminate it by applying a serious of reductions
    //   across each instruction that depends on said item. This is repeated until
    //   there's nothing left to elide. The reason we call discover_unused() after each
    //   elimination is that the indecies (among other subtlties) become incorrect
    //   after elision.
    
    macro_rules! create {
        ($opcode:tt, $debug_info:expr) => {
            Instruction {
		opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		immediate: None,
		debug_info: $debug_info,
	    }
        };
        ($opcode:tt, $value:expr, $debug_info:expr) => {
            Instruction {
		opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		immediate: $value,
		debug_info: $debug_info,
	    }
        };
    }
    
    let mut opt = block.clone();
    let mut color_group = 1;
    loop {
	if debug {
	    print_code(&opt, "before reduction", &BTreeMap::new());
	}
	
	let eliminate = match discover_unused(&opt, debug) {
	    Ok(eliminate) => eliminate,
	    Err(_) => {
		// We encountered some combination of instructions that will
		// raise an error. It's probably best to just not optimize anything.
		return (opt.clone(), true);
	    }
	};
	
	if eliminate.len() == 0 {
	    return (opt, same);
	}
        
	let eliminate: Vec<(usize, StackEntry)> = eliminate.into_iter().collect();
	
	let (created, entry) = eliminate.into_iter().next().unwrap();
	
	let elimination = {
	    let mut elimination = vec![];
	    
	    let reduce_generator = |created: usize| -> Vec<(usize, Vec<Instruction>)> {
		let generator = &opt[created];
		let debug_info = generator.debug_info.color(color_group);
		
		let mut elim = match &generator.opcode {
		    Opcode::AVMOpcode(AVMOpcode::Noop)
                    | Opcode::AVMOpcode(AVMOpcode::Pop)
	            | Opcode::AVMOpcode(AVMOpcode::Swap1) => vec![],
		    Opcode::AVMOpcode(AVMOpcode::Dup0) => {
                        match &generator.immediate {
			    Some(value) => vec![create!(Noop, generator.immediate.clone(), debug_info)],
			    None => vec![],
			}
                    }
		    Opcode::AVMOpcode(AVMOpcode::Dup1) => {
                        match &entry.was_immediate {
			    true => vec![create!(Dup0, debug_info)],
			    false => vec![create!(Noop, generator.immediate.clone(), debug_info)],
		        }
                    }
                    Opcode::AVMOpcode(AVMOpcode::Dup2) => {
			match &entry.was_immediate {
			    true => vec![create!(Dup1, debug_info)],
			    false => vec![create!(Noop, generator.immediate.clone(), debug_info)],
		        }
		    }
		    Opcode::AVMOpcode(AVMOpcode::Swap2) => vec![create!(Swap1, debug_info)],
                    Opcode::AVMOpcode(AVMOpcode::Tget) => {
                        match &generator.immediate {
                            Some(_) => vec![create!(Pop, debug_info)],
                            None => vec![create!(Pop, debug_info), create!(Pop, debug_info)],
                        }
                    }
                    Opcode::AVMOpcode(AVMOpcode::Plus) |
                    Opcode::AVMOpcode(AVMOpcode::Minus) => {
                        match &generator.immediate {
                            Some(_) => vec![create!(Pop, debug_info)],
                            None => vec![create!(Pop, debug_info), create!(Pop, debug_info)],
                        }
                    }
		    x => unreachable!("Insn {} generated a value", x),
		};
		
		if insert_noops && elim.len() == 0 {
		    elim.push(create!(Noop, debug_info));
		}
		vec![(created, elim)]
	    };
	    
	    let reduce_mover = |created: usize, index: usize | -> Vec<(usize, Vec<Instruction>)> {
		let mover = &opt[index];
                let debug_info = mover.debug_info.color(color_group);
		
		// Special case where instruction both creates and moves a value.
		// Dups's and Swap's are already handled in the generator lambda above.
		if index == created {
		    match &mover.opcode {
			Opcode::AVMOpcode(AVMOpcode::Swap1)
			| Opcode::AVMOpcode(AVMOpcode::Swap2)
                        | Opcode::AVMOpcode(AVMOpcode::Dup0)
                        | Opcode::AVMOpcode(AVMOpcode::Dup1)
                        | Opcode::AVMOpcode(AVMOpcode::Dup2) => return vec![],
                        x => unreachable!("Insn {} moved it's own value", x),
		    }
		}
		
		let mut elim = match &mover.opcode {
                    Opcode::AVMOpcode(AVMOpcode::Pop) => vec![],
		    Opcode::AVMOpcode(AVMOpcode::Swap1) => {
			match &mover.immediate {
			    Some(value) => vec![create!(Noop, Some(value.clone()), debug_info)],
			    None => vec![],
			}
		    }
		    Opcode::AVMOpcode(AVMOpcode::Swap2) => {
			// Observe the following identity to understand why
			// a swap1 is always correct. Regardless of which value
			// * is being eliminated, the stack ends up the same way:
			//     a b * => * b a  ==>  swap1
			//     a * b => b * a  ==>  swap1
			match &mover.immediate {
			    Some(value) => vec![create!(Swap1, Some(value.clone()), debug_info)],
			    None => vec![create!(Swap1, debug_info)],
			}
		    }
		    Opcode::AVMOpcode(AVMOpcode::Tset) => {
			let mut reduction = vec![];
			if let None = &mover.immediate {
			    reduction.push(create!(Pop, None, debug_info));
                        }
                        reduction.push(create!(Pop, None, debug_info));
			reduction.push(create!(Pop, None, debug_info));
			reduction
		    }
                    Opcode::AVMOpcode(AVMOpcode::Dup1) => {
                        vec![create!(Dup0, mover.immediate.clone(), debug_info)]
                    }
                    Opcode::AVMOpcode(AVMOpcode::Dup2) => {
                        vec![create!(Dup1, mover.immediate.clone(), debug_info)]
                    }
                    x => unreachable!("A {} made a movement", x),
		};
		
		if insert_noops && elim.len() == 0 {
                    elim.push(create!(Noop, debug_info));
		}
		vec![(index, elim)]
	    };
            
            let reduce_killer = |created: usize, index: usize, replaced: Option<Value>| -> Vec<(usize, Vec<Instruction>)> {
		let killer = &opt[index];
                let debug_info = killer.debug_info.color(color_group);
                let mut elim = vec![];
		
		// Special case where instruction both creates and kills a value.
		// These are already handled in the generator lambda above.
		if index == created {
		    elim.extend(match &killer.opcode {
			Opcode::AVMOpcode(AVMOpcode::Pop) => vec![],
                        Opcode::AVMOpcode(AVMOpcode::Plus) => vec![],
                        Opcode::AVMOpcode(AVMOpcode::Minus) => vec![],
			x => unreachable!("Insn {} killed it's own value", x),
		    });
		}
		
		elim.extend(match &killer.opcode {
                    Opcode::AVMOpcode(AVMOpcode::Pop) => vec![],
		    Opcode::AVMOpcode(AVMOpcode::Swap1) => {
			match &killer.immediate {
			    Some(value) => vec![create!(Noop, Some(value.clone()), debug_info)],
			    None => vec![],
			}
		    }
		    Opcode::AVMOpcode(AVMOpcode::Swap2) => {
			// Observe the following identity to understand why
			// a swap1 is always correct. Regardless of which value
			// * is being eliminated, the stack ends up the same way:
			//     a b * => * b a  ==>  swap1
			//     a * b => b * a  ==>  swap1
			match &killer.immediate {
			    Some(value) => vec![create!(Swap1, Some(value.clone()), debug_info)],
			    None => vec![create!(Swap1, debug_info)],
			}
		    }
		    Opcode::AVMOpcode(AVMOpcode::Tset) => {
                        match &killer.immediate {
                            Some(_) => vec![
                                create!(Pop, debug_info), create!(Pop, debug_info)
                            ],
                            None => vec![
                                create!(Pop, debug_info), create!(Pop, debug_info),
                                create!(Pop, debug_info),
                            ],
                        }
		    }
                    Opcode::AVMOpcode(AVMOpcode::Tget) => {
                        match &killer.immediate {
                            Some(_) => vec![
                                create!(Pop, debug_info)
                            ],
                            None => vec![
                                create!(Pop, debug_info), create!(Pop, debug_info)
                            ],
                        }
                    }
                    Opcode::AVMOpcode(AVMOpcode::Plus)
                    | Opcode::AVMOpcode(AVMOpcode::Minus) => {
                        match &killer.immediate {
                            Some(_) => vec![create!(Pop, debug_info)],
                            None => vec![create!(Pop, debug_info), create!(Pop, debug_info)],
                        }
		    }
                    x => unreachable!("Insn {} killed a value", x),
		});
                
                if let Some(replaced) = replaced {
                    elim.push(create!(Noop, Some(replaced), debug_info));
                }
		
		if insert_noops && elim.len() == 0 {
                    elim.push(create!(Noop, debug_info));
		}
		vec![(index, elim)]
	    };
            
            let reduce_duplicator = |created: usize, index: usize, value: Value| -> Vec<(usize, Vec<Instruction>)> {
		let duplicator = &opt[index];
		let debug_info = duplicator.debug_info.color(color_group);
                
                // Special case where instruction both creates and dupes a value.
		// These are already handled in the generator lambda above.
                if index == created {
		    match &duplicator.opcode {
                        Opcode::AVMOpcode(AVMOpcode::Dup0) => return vec![],
                        x => unreachable!("Insn {} duped it's own value", x),
		    }
		}
		
		let mut elim = match &duplicator.opcode {
		    Opcode::AVMOpcode(AVMOpcode::Dup0)
                    | Opcode::AVMOpcode(AVMOpcode::Dup1)
                    | Opcode::AVMOpcode(AVMOpcode::Dup2) => {
                        match &duplicator.immediate {
                            Some(_) => vec![
                                create!(Noop, duplicator.immediate.clone(), debug_info),
                                create!(Noop, Some(value), debug_info),
                            ],
                            None => vec![create!(Noop, Some(value), debug_info)],
                        }
                    }
		    x => unreachable!("Insn {} duplicated a value", x),
		};
		
		if insert_noops && elim.len() == 0 {
		    elim.push(create!(Noop, debug_info));
		}
		vec![(index, elim)]
	    };

            match &entry.replaced {
                Some(replaced) => {
                    elimination.extend(reduce_killer(created, entry.killed.unwrap(), Some(replaced.clone())));
                }
                None => {
                    elimination.extend(reduce_generator(created));
	            for index in entry.moved {
		        elimination.extend(reduce_mover(created, index));
	            }
                    for (index, value) in entry.duped {
                        elimination.extend(reduce_duplicator(created, index, value));
                    }
                    elimination.extend(reduce_killer(created, entry.killed.unwrap(), None));
                }
            }
	    
	    if debug {
		println!(
		    "Elimination {} {}",
		    created,
		    match &entry.replaced {
			Some(_) => format!("using {}", entry.killed.unwrap()),
			None => String::from(""),
		    }
		);
		for (index, reduction) in &elimination {
		    print!(
			"  - {} {} {}\t=> ",
			index,
			&opt[*index].opcode,
			match &opt[*index].immediate {
			    Some(value) => format!("{} ", value.pretty_print(ConsoleColors::RESET, ConsoleColors::PINK)),
			    None => String::from(""),
			},
		    );
		    for insn in reduction {
			print!(
			    " {}{}",
			    insn.opcode,
			    match &insn.immediate {
				Some(value) => format!(" {} ", value.pretty_print(ConsoleColors::RESET, ConsoleColors::PINK)),
				None => String::from(""),
			    },
			);
		    }
		    println!("");
		}
	    }
	    
	    //color_group += 1;
            color_group = 6;
	    elimination
	};
	
	let mut reduced = Vec::with_capacity(opt.len());
	let mut reductions = BTreeMap::new();
	
	for (index, reduction) in elimination {
	    reductions.insert(index, reduction);
	}
	
	for index in 0..opt.len() {
	    match reductions.get(&index) {
		Some(reduction) => reduced.extend(reduction.clone()),
		None => reduced.push(opt[index].clone()),
	    }
	}

        print_cfg(&create_cfg(&opt), &create_cfg(&reduced), "Elide unused");
	opt = reduced;
        same = false;
    }
}

#[derive(Clone, Debug, Default)]
pub struct StackValue {
    created: Option<usize>,       // where this stack entry comes into existence
    value: Value,                 // the actual value of this stack entry
    which: usize,                 // which output value from its generating insn this came frome
}
impl StackValue {
    fn blank() -> Self {
        StackValue::default()
    }
    fn new(created: usize, value: &Value, which: usize) -> Self {
        StackValue {
            created: Some(created),
            value: value.clone(),
            which: which,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NodeType {
    AVMOpcode(AVMOpcode),
    Value(Value),
    Argument(usize, bool),
    Output,
    GlobalState,
    Function(usize, usize),
}
impl NodeType {
    fn is_prunable(&self) -> bool {
        match self {
            NodeType::AVMOpcode(_) => true,
            NodeType::Value(_) => true,
            NodeType::Argument(..) => false,
            NodeType::Output => false,
            NodeType::GlobalState => true,  // safe since the final global state is the last input to the output node
            NodeType::Function(..) => true,
        }
    }
    fn pretty_print(&self) -> String {
        let reset = ConsoleColors::RESET;
        let pink = ConsoleColors::PINK;
        let mint = ConsoleColors::MINT;
        let grey = ConsoleColors::GREY;
        match self {
            NodeType::AVMOpcode(avm_op) => format!("{}\t", Opcode::AVMOpcode(*avm_op).pretty_print(pink, pink)),
            NodeType::Value(value) => format!("{}\t", value.pretty_print(pink, pink)),
            NodeType::Argument(arg, aux) => format!(
                "Arg {}{}{}\t",
                match aux {
                    false => pink,
                    true => mint,
                }, arg, reset
            ),
            NodeType::Output => String::from("Output\t"),
            NodeType::GlobalState => String::from("Globals\t"),
            NodeType::Function(inputs, outputs) => format!(
                "{}func({}{}{}{},{}{}{}{}){}", grey, reset, pink, inputs, reset, pink, outputs, reset, grey, reset),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EdgeType {
    Connect(usize, usize),
    Meta,
}
impl EdgeType {
    fn input_number(&self) -> usize {
        match &self {
            EdgeType::Connect(_, input_number) => *input_number,
            EdgeType::Meta => usize::MAX,
        }
    }
}

pub type ValueGraph = StableGraph<NodeType, EdgeType>;
pub type PrintGraph = StableGraph<String, (usize, usize)>;

/// Prints the graph in a human-readable format. A prime (') is used to indicate the second output
/// value of a given node, should one exist. Edges in pink are meta.
fn print_graph(graph: &ValueGraph) {
    for node in graph.node_indices() {
        let grey = ConsoleColors::GREY;
        let pink = ConsoleColors::PINK;
        let reset = ConsoleColors::RESET;
        print!("{}{:>3}{}  {}  ", grey, node.index(), reset, graph[node].pretty_print());
        let edges = graph.edges_directed(node, Direction::Outgoing);
        for edge in edges {
            let (_, input) = graph.edge_endpoints(edge.id()).unwrap();
            print!(
                "{}{},{} ",
                match graph.edge_weight(edge.id()).unwrap() {
                    EdgeType::Meta => format!("{}{}{}", pink, input.index(), reset),
                    EdgeType::Connect(output_number, input_number) => 
                        format!(
                            "{}{}", input.index(),
                            match output_number {
                                0 => format!(""),
                                _ => format!("'"), 
                            },
                        ),
                },
                grey, reset
            );
        }
        println!();
    }
    println!();
}

pub fn graph_reduce_block(block: &Block, detail: usize) -> Result<Block, (String, DebugInfo, ValueGraph)> {

    print_code(&block, "graph reduce input block", &BTreeMap::new());
    
    if block.len() == 0 {
        return Ok(block.clone());
    }
    
    let debug_info = block[0].debug_info.propagate(true);
    let unoptimized_block = block.clone();

    macro_rules! err {
        ($msg:ident) => {{
            return Err(String::from($msg), debug_info);
        }};
        ($msg:ident, $debug_info:expr) => {{
            return Err(String::from($msg), $debug_info);
        }};
    }
    
    macro_rules! create {
        ($opcode:ident) => {
            Instruction {
		opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		immediate: None,
		debug_info: debug_info,
	    }
        };
        ($opcode:ident, $value:expr) => {
            Instruction {
		opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		immediate: $value,
		debug_info: debug_info,
	    }
        };
    }
    
    // Move all immediates to a preceeding noop. This avoids special-casing later.
    let mut nooped = Vec::with_capacity(block.len());
    for curr in block.iter() {
        nooped.extend(match &curr.opcode {
            Opcode::AVMOpcode(AVMOpcode::Noop) => vec![curr.clone()],
            opcode => match &curr.immediate {
                Some(value) => vec![
                    create!(Noop, Some(value.clone())),
                    Instruction::from_opcode(opcode.clone(), debug_info),
                ],
                None => vec![curr.clone()],
            }
        });
    }
    let block = nooped;

    if detail > 0 {
        print_code(&block, "noop'd", &BTreeMap::new());
    }
    
    let mut xtransformed = Vec::with_capacity(block.len());
    for curr in block.iter() {
        xtransformed.extend(match &curr.opcode {
            Opcode::AVMOpcode(AVMOpcode::Xset) => vec![
                create!(AuxPop),
                create!(Swap1),
                create!(Tset),
                create!(AuxPush),
            ],
            Opcode::AVMOpcode(AVMOpcode::Xget) => vec![
                create!(AuxPop),
                create!(Dup0),
                create!(AuxPush),
                create!(Swap1),
                create!(Tget),
            ],
            _ => vec![curr.clone()],
        });
    }
    let block = xtransformed;
    
    let mut arg_count = 0;
    let mut data_stack: VecDeque<(isize, usize)> = VecDeque::new();
    let mut aux_stack:  VecDeque<(isize, usize)> = VecDeque::new();
    
    let mut graph = ValueGraph::default();
    let mut index_to_node = HashMap::new();
    let mut global_state = graph.add_node(NodeType::GlobalState);
    let mut global_reads = vec![];
    
    macro_rules! touch {
        ($stack:expr, $count:expr) => {
            let aux = &$stack as *const _ == &aux_stack as *const _;    // check if this is the aux stack
            let stack = &mut $stack;
            let count = $count as usize;
            for touched in 0..(count.saturating_sub(stack.len())) {
                arg_count += 1;
                index_to_node.insert(-arg_count, graph.add_node(NodeType::Argument(arg_count as usize, aux)));
                stack.push_front((-arg_count, 0));
            }
        };
    }
    
    for (index, curr) in block.iter().enumerate().map(|(index, curr)| (index as isize, curr)) {
        
        match &curr.opcode {
            Opcode::AVMOpcode(AVMOpcode::Noop) => {
                match &curr.immediate {
                    Some(value) => index_to_node.insert(index, graph.add_node(NodeType::Value(value.clone()))),
                    None => continue,
                };
            }
            Opcode::AVMOpcode(avm_op) => drop(index_to_node.insert(index, graph.add_node(NodeType::AVMOpcode(avm_op.clone())))),
            x => panic!("Graph reduce should never encounter a virtual opcode {:?}", x),
        };
        
        let effects = curr.effects();
        
        let mut output_number = 0;
        let mut input_number = 0;
        
        for effect in effects {
            match effect {
                OpcodeEffect::PopStack => {
                    touch!(data_stack, 1);
                    data_stack.pop_back();
                }
                OpcodeEffect::PopAux => {
                    touch!(aux_stack, 1);
                    aux_stack.pop_back();
                }
                OpcodeEffect::PushStack => {
                    data_stack.push_back((index, output_number));
                    output_number += 1;
                }
                OpcodeEffect::PushAux => {
                    aux_stack.push_back((index, output_number));
                    output_number += 1;
                }
                OpcodeEffect::MoveToStack => {
                    touch!(aux_stack, 1);
                    data_stack.push_back(aux_stack.pop_back().unwrap());
                }
                OpcodeEffect::MoveToAux => {
                    touch!(data_stack, 1);
                    aux_stack.push_back(data_stack.pop_back().unwrap());
                }
                OpcodeEffect::ReadStack(depth) => {
                    touch!(data_stack, depth);
                    let (producer, which) = data_stack[data_stack.len() - depth];
                    let this_node = *index_to_node.get(&index).unwrap();
                    let that_node = *index_to_node.get(&producer).unwrap();
                    graph.add_edge(this_node, that_node, EdgeType::Connect(which, input_number));
                    input_number += 1;
                }
                OpcodeEffect::ReadAux => {
                    touch!(aux_stack, 1);
                    let (producer, which) = aux_stack[aux_stack.len() - 1];
                    let this_node = *index_to_node.get(&index).unwrap();
                    let that_node = *index_to_node.get(&producer).unwrap();
                    graph.add_edge(this_node, that_node, EdgeType::Connect(which, input_number));
                    input_number += 1;
                }
                OpcodeEffect::SwapStack(depth) => {
                    touch!(data_stack, depth + 1);
                    let swapped = data_stack.swap_remove_back(data_stack.len() - depth - 1).unwrap();
                    data_stack.push_back(swapped);
                }
                OpcodeEffect::ReadGlobal => {
                    let this_node = *index_to_node.get(&index).unwrap();
                    global_reads.push(this_node.clone());
                    graph.add_edge(this_node, global_state, EdgeType::Meta);
                }
                OpcodeEffect::WriteGlobal => {
                    let this_node = *index_to_node.get(&index).unwrap();
                    global_state = this_node;
                    
                    // we must force this rset to happen *after* all prior reads
                    for read in &global_reads {
                        graph.add_edge(this_node, read.clone(), EdgeType::Meta);
                    }
                }
                x => panic!("effect not yet handled: {} {:?}", x.to_name(), &curr.opcode),
            }
        }
    }
    
    // anything left on the stack is necessarily an output of this block
    let output_node = graph.add_node(NodeType::Output);
    
    let mut aux_output_count = 0;
    for (producer, output_number) in aux_stack.iter().rev() {
        let node = *index_to_node.get(&producer).unwrap();
        graph.add_edge(output_node, node, EdgeType::Connect(*output_number, aux_stack.len() - 1 - aux_output_count));
        //graph.add_edge(output_node, node, EdgeType::Connect(*output_number, aux_output_count));
        aux_output_count += 1;
    }
    
    graph.add_edge(output_node, global_state, EdgeType::Meta);
    
    let mut output_count = 0;
    while let Some((producer, output_number)) = data_stack.pop_back() {
        let node = *index_to_node.get(&producer).unwrap();
        graph.add_edge(output_node, node, EdgeType::Connect(output_number, aux_output_count + output_count));
        output_count += 1;
    }
    
    /// Prunes nodes whose values are never consumed.
    fn prune_graph(graph: &mut ValueGraph) {
        loop {
            let node_count = graph.node_count();
            graph.retain_nodes(|this, node| {
                !this[node].is_prunable() || this.neighbors_directed(node, Direction::Incoming).count() > 0
            });
            if graph.node_count() == node_count {
                break;
            }
        }
    }
    
    /// Retrieves all neighbors and the edge taken for each, sorted by input number.
    fn sorted_neighbors(graph: &ValueGraph, node: NodeIndex) -> Vec<(NodeIndex, EdgeIndex)> {
        let mut edges: Vec<_> = graph.edges_directed(node, Direction::Outgoing).collect();
        edges.sort_by_key(|e| e.weight().input_number());
        let mut inputs: Vec<_> = edges.into_iter().map(|e| (graph.edge_endpoints(e.id()).unwrap().1, e.id())).collect();
        inputs
    }
    
    fn typecheck(graph: &ValueGraph) -> Result<(), (String, NodeIndex)> {
        
        for node in graph.node_indices() {
            
            match &graph[node] {
                NodeType::AVMOpcode(AVMOpcode::Tset) => {
                    let [(offset_node, _), (tuple_node, _), (_, _)]: [(NodeIndex, EdgeIndex); 3]
                        = sorted_neighbors(graph, node).try_into().expect("Tset should have 3 inputs");
                    
                    let offset = match &graph[offset_node] {
                        NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => offset.to_usize().unwrap(),
                        NodeType::Value(_) => return Err((format!("Tset {:?} has a provably bad offset.", node), offset_node)),
                        _ => continue,
                    };
                    match &graph[tuple_node] {
                        NodeType::Value(Value::Tuple(vec)) if offset < vec.len() => {}
                        NodeType::AVMOpcode(
                            AVMOpcode::Tset | AVMOpcode::Tget | AVMOpcode::Rget |
                            AVMOpcode::Dup0 | AVMOpcode::Dup1 | AVMOpcode::Dup2
                        ) => {}
                        NodeType::Argument(..) => {}
                        _ => return Err((format!("Tset {:?} has a provably bad tuple.", node), tuple_node)),
                    }
                }
                NodeType::AVMOpcode(AVMOpcode::Tget) => {
                    let [(offset_node, _), (tuple_node, _)]: [(NodeIndex, EdgeIndex); 2]
                        = sorted_neighbors(graph, node).try_into().expect("Tget should have 2 inputs");
                    
                    let offset = match &graph[offset_node] {
                        NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => offset.to_usize().unwrap(),
                        NodeType::Value(_) => return Err((format!("Tget {:?} has a provably bad offset.", node), offset_node)),
                        _ => continue,
                    };
                    let tuple = match &graph[tuple_node] {
                        NodeType::Value(Value::Tuple(vec)) if offset < vec.len() => {}
                        NodeType::AVMOpcode(
                            AVMOpcode::Tset | AVMOpcode::Tget | AVMOpcode::Rget |
                            AVMOpcode::Dup0 | AVMOpcode::Dup1 | AVMOpcode::Dup2
                        ) => {},
                        NodeType::Argument(..) => {},
                        _ => return Err((format!("Tget {:?} has a provably bad tuple value.", node), tuple_node)),
                    };
                }
                _ => {}
            }
        }
        Ok(())
    }
    
    /// Replaces an edge with one deeper in the graph that provably results in the consumer
    /// receiving the same value.
    fn relink_edges(graph: &mut ValueGraph) -> Result<(), (&'static str, NodeIndex)> {
        
        let edges: Vec<_> = graph.edge_indices().collect();
        for edge in edges {
            relink_edge(edge, graph)?;
        }

        return Ok(());
        
        fn relink_edge(edge: EdgeIndex, graph: &mut ValueGraph) -> Result<(), (&'static str, NodeIndex)> {
            
            let (output, mut input) = graph.edge_endpoints(edge).unwrap();
            let connect = match graph.edge_weight(edge).unwrap() {
                EdgeType::Connect(output_number, input_number) => (*output_number, *input_number),
                EdgeType::Meta => return Ok(()),
            };
            graph.remove_edge(edge);
            
            /// Finds which of the producer's output values correspond to following this edge.
            fn edge_output_number(edge: EdgeIndex, graph: &ValueGraph, error_msg: &str) -> usize {
                match graph.edge_weight(edge).unwrap() {
                    EdgeType::Meta => panic!("{}", error_msg),
                    EdgeType::Connect(output_number, _) => *output_number,
                }
            }
            
            let mut anchor = (input, connect.0);    // a relinkable node and its output value
            let mut tuple_offset = None;
            
            let mut same = false;
            while !same {
                same = true;
                match &graph[input] {
                    NodeType::AVMOpcode(AVMOpcode::Dup0)
                    | NodeType::AVMOpcode(AVMOpcode::Dup1)
                    | NodeType::AVMOpcode(AVMOpcode::Dup2) => {
                        let [(original_node, original_edge)]: [(NodeIndex, EdgeIndex); 1] =
                            sorted_neighbors(graph, input).try_into().expect("Dups should have 1 input");
                        
                        input = original_node;
                        anchor = (input, edge_output_number(original_edge, &graph, "Dup has a meta edge"));
                        same = false;
                    }
                    NodeType::AVMOpcode(AVMOpcode::Tget) => {
                        let [(offset_node, offset_edge), (tuple_node, tuple_edge)]: [(NodeIndex, EdgeIndex); 2]
                            = sorted_neighbors(graph, input).try_into().expect("Tget should have 3 inputs");
                        
                        let offset = match &graph[offset_node] {
                            NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => offset.to_usize().unwrap(),
                            NodeType::Value(_) => return Err(("Found a Tget with a provably bad offset.", offset_node)),
                            _ => continue,
                        };
                        let tuple = match &graph[tuple_node] {
                            NodeType::Value(Value::Tuple(vec)) if offset < vec.len() => input = tuple_node,
                            NodeType::AVMOpcode(AVMOpcode::Tset) => input = tuple_node,
                            NodeType::AVMOpcode(AVMOpcode::Tget | AVMOpcode::Rget) => continue,
                            NodeType::Argument(..) => continue,
                            _ => return Err(("Found a Tget with provably bad tuple value.", tuple_node)),
                        };
                        tuple_offset = Some(offset);
                        same = false;
                    }
                    NodeType::AVMOpcode(AVMOpcode::Tset) => {
                        let [(offset_node, _), (tuple_node, _), (value_node, value_edge)]: [(NodeIndex, EdgeIndex); 3]
                            = sorted_neighbors(graph, input).try_into().expect("Tset should have 3 inputs");
                        
                        if let Some(tuple_offset) = tuple_offset {
                            let offset = match &graph[offset_node] {
                                NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => offset.to_usize().unwrap(),
                                NodeType::Value(_) => return Err(("Found a Tset with a provably bad offset.", offset_node)),
                                _ => continue,
                            };
                            let tuple = match &graph[tuple_node] {
                                NodeType::Value(Value::Tuple(vec)) if offset < vec.len() && offset == tuple_offset => {
                                    input = value_node;
                                    anchor = (input, edge_output_number(value_edge, &graph, "Tset has a meta edge"));
                                    break;
                                }
                                NodeType::Value(Value::Tuple(vec)) if offset < vec.len() => input = tuple_node,
                                NodeType::AVMOpcode(AVMOpcode::Tset) if offset == tuple_offset => {
                                    input = value_node;
                                    anchor = (input, edge_output_number(value_edge, &graph, "Tset has a meta edge"));
                                    break;
                                }
                                NodeType::AVMOpcode(AVMOpcode::Tset) => input = tuple_node,
                                NodeType::AVMOpcode(AVMOpcode::Tget | AVMOpcode::Rget) => continue,
                                NodeType::Argument(..) => continue,
                                _ => return Err(("Found a Tset with a provably bad tuple.", tuple_node)),
                            };
                            same = false;
                        }
                    }
                    NodeType::Value(Value::Tuple(vec)) => {
                        if let Some(offset) = tuple_offset {
                            if offset >= vec.len() {
                                return Err(("Found a Tget with a provably bad tuple.", input));
                            }
                            let inner = NodeType::Value(vec[offset].clone());
                            input = graph.add_node(inner);
                            anchor = (input, 0);    // values only have 1 output
                            same = false;
                            break;
                        }
                    }
                    _ => {},
                }
            }
            graph.add_edge(output, anchor.0, EdgeType::Connect(anchor.1, connect.1));
            Ok(())
        }
    }
    
    fn elide_tsets(graph: &mut ValueGraph) {
        
        let tsets: Vec<_> = graph.node_indices().filter(|node| graph[*node] == NodeType::AVMOpcode(AVMOpcode::Tset)).collect();
        
        for tset in tsets {
            
            let [(offset_node, offset_edge), (tuple_node, tuple_edge), (value_node, value_edge)]: [(NodeIndex, EdgeIndex); 3]
                = sorted_neighbors(graph, tset).try_into().expect("Tset should have 3 inputs");
            
            let offset = match &graph[offset_node] {
                NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => offset.to_usize().unwrap(),
                _ => continue,
            };
            let tuple = match &graph[tuple_node] {
                NodeType::Value(Value::Tuple(vec)) if offset < vec.len() => tuple_node,
                NodeType::AVMOpcode(AVMOpcode::Tset) => tuple_node,
                _ => continue,
            };
            
            let mut input = tuple;
            let mut tset_path = vec![];
            loop {
                match graph[input] {
                    NodeType::Value(Value::Tuple(ref vec)) if offset < vec.len() => {
                        // see if we can apply the tset to this tuple using values we know about
                        
                        // TODO build a tset tree where the tuple is the root and walk backwards
                        if tset_path.len() > 0 {
                            break;
                        }
                        
                        let inner = match graph[value_node] {
                            NodeType::Value(ref value) => value.clone(),
                            _ => break,
                        };
                        
                        let mut new_tuple = vec.to_vec();
                        new_tuple[offset] = inner;
                        
                        /*if graph.neighbors_directed(input, Direction::Incoming).count() == 1 {
                            // Nothing else depends on this node's value, so let's update it.
                            graph[input] = NodeType::Value(Value::new_tuple(new_tuple.clone()));
                        }*/
                        
                        graph[tset] = NodeType::Value(Value::new_tuple(new_tuple));
                        graph.remove_edge(offset_edge);
                        graph.remove_edge(tuple_edge);
                        graph.remove_edge(value_edge);
                        break;
                    }
                    NodeType::AVMOpcode(AVMOpcode::Tset) => {
                        // see if this tset kills another
                        
                        let [(offset_node, offset_edge), (tuple_node, _), (value_node, value_edge)]: [(NodeIndex, EdgeIndex); 3]
                            = sorted_neighbors(graph, input).try_into().expect("Tset should have 3 inputs");
                        
                        let prior_offset = match &graph[offset_node] {
                            NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => offset.to_usize().unwrap(),
                            _ => break,
                        };
                        
                        let degree = graph.edges_directed(input, Direction::Incoming).count();
                        
                        if offset == prior_offset && degree == 1 {
                            // this prior tset assigns a value that's never used since
                            // the original overwrites it.
                            graph[input] = NodeType::AVMOpcode(AVMOpcode::Dup0);
                            graph.remove_edge(offset_edge);
                            graph.remove_edge(value_edge);
                            break;
                        }
                        
                        // TODO build a tset tree where the tuple is the root and walk backwards
                        if degree == 1 {
                            tset_path.push(tuple_node);
                            input = tuple_node;
                            continue;
                        }
                    }
                    _ => {},
                }
                break;
            }
        }
    }
    
    fn constant_folding(graph: &mut ValueGraph) -> Result<(), (String, NodeIndex)> {
        
        let nodes: Vec<_> = graph.node_indices().collect();
        
        for node in nodes {
            macro_rules! inputs {
                ($count:expr) => {{
                    let inputs: [(NodeIndex, EdgeIndex); $count] = sorted_neighbors(graph, node)
                        .try_into().expect(&format!("{:?} should have {} inputs", graph[node], $count));
                    inputs
                }};
            }
            macro_rules! compute {
                (1, |$va:ident| $func:expr) => {{
                    let [(a, ea)] = inputs!(1);
                    match &graph[a] {
                        NodeType::Value(Value::Int($va)) => {
                            graph[node] = NodeType::Value(Value::Int($func));
                            graph.remove_edge(ea);
                        }
                        NodeType::Value(_) => {
                            return Err((format!("Found a unary op with a bad input {:?}.", graph[a]), a));
                        }
                        _ => continue,
                    }
                }};
                (2, |$va:ident, $vb:ident| $func:expr) => {{
                    let [(a, ea), (b, eb)] = inputs!(2);
                    match (&graph[a], &graph[b]) {
                        (NodeType::Value(Value::Int($va)), NodeType::Value(Value::Int($vb))) => {
                            graph[node] = NodeType::Value(Value::Int($func));
                            graph.remove_edge(ea);
                            graph.remove_edge(eb);
                        }
                        (NodeType::Value(_), NodeType::Value(_)) => return Err((format!("Found a binary op with bad inputs."), a)),
                        _ => continue,
                    }
                }};
            }
            
            let zero = Uint256::from_usize(0);
            let one = Uint256::from_usize(1);
            
            match graph[node] {
                NodeType::AVMOpcode(AVMOpcode::BitwiseNeg)   => compute!(1, |a   | a.bitwise_neg()   ),
              //NodeType::AVMOpcode(AVMOpcode::Hash)         => compute!(1, |a   | a.avm_hash()      ),
                NodeType::AVMOpcode(AVMOpcode::Plus)         => compute!(2, |a, b| a.add(b)          ),
                NodeType::AVMOpcode(AVMOpcode::Minus)        => compute!(2, |a, b| a.unchecked_sub(b)),
                NodeType::AVMOpcode(AVMOpcode::Mul)          => compute!(2, |a, b| a.mul(b)          ),
                NodeType::AVMOpcode(AVMOpcode::Exp)          => compute!(2, |a, b| a.exp(b)          ),
                NodeType::AVMOpcode(AVMOpcode::BitwiseAnd)   => compute!(2, |a, b| a.bitwise_and(b)),
                NodeType::AVMOpcode(AVMOpcode::BitwiseOr)    => compute!(2, |a, b| a.bitwise_or(b)),
                NodeType::AVMOpcode(AVMOpcode::BitwiseXor)   => compute!(2, |a, b| a.bitwise_xor(b)),
                NodeType::AVMOpcode(AVMOpcode::Hash2)        => compute!(2, |a, b| Uint256::avm_hash2(a, b)),
                NodeType::AVMOpcode(AVMOpcode::LessThan)     => compute!(2, |a, b| if a < b { one } else { zero }),
                NodeType::AVMOpcode(AVMOpcode::GreaterThan)  => compute!(2, |a, b| if a > b { one } else { zero }),
              //NodeType::AVMOpcode(AVMOpcode::Equal)        => compute!(2, |a, b| if a == b { one } else { zero }),
                NodeType::AVMOpcode(AVMOpcode::SLessThan)    => compute!(2, |a, b| if a.s_less_than(b) { one } else { zero }),
                NodeType::AVMOpcode(AVMOpcode::SGreaterThan) => compute!(2, |a, b| if b.s_less_than(a) { one } else { zero }),
                _ => {}
            }
        }
        Ok(())
    }
    
    let original_graph = graph.clone();
    if detail > 0 {
        print_graph(&graph);
    }
    
    for _ in 0..64 {
        let node_count = graph.node_count();
        prune_graph(&mut graph);
        println!("Relink");
        relink_edges(&mut graph).map_err(|(msg, node)| (String::from(msg), debug_info, original_graph.clone()))?;
        prune_graph(&mut graph);
        if detail > 1 {
            print_graph(&graph);
        }

        println!("Elide tsets");
        elide_tsets(&mut graph);
        prune_graph(&mut graph);
        if detail > 1 {
            print_graph(&graph);
        }
        
        println!("Constants");
        constant_folding(&mut graph).map_err(|(msg, node)| (String::from(msg), debug_info, original_graph.clone()))?;
        if detail > 1 {
            print_graph(&graph);
        }
        
        println!("Typecheck");
        typecheck(&graph).map_err(|(msg, node)| (msg, debug_info, original_graph.clone()))?;
        if graph.node_count() == node_count {
            break;
        }
        if detail > 0 {
            print_graph(&graph);
        }
    }
    if detail > 0 {
        print_graph(&graph);
    }
    
    /// Prints the items on the stack, along with their producer's output number.
    fn print_slots(title: &str, slots: &Vec<(NodeIndex, usize)>, aux_slots: &Vec<(NodeIndex, usize)>, node: NodeIndex) {
        println!(
            "  => {} slots for node({}) [{}] [{}]", title,
            node.index(),
            slots.iter().map(
                |slot| format!(
                    "{}{}",
                    slot.0.index(),
                    match slot.1 {
                        0 => "",
                        _ => "'",
                    })
            ).collect::<Vec<_>>().join(", "),
            aux_slots.iter().map(
                |slot| format!(
                    "{}{}",
                    slot.0.index(),
                    match slot.1 {
                        0 => "",
                        _ => "'",
                    })
            ).collect::<Vec<_>>().join(", "),
        );
    }
    
    let mut slots = vec![];
    let mut aux_slots = vec![];
    for node in graph.node_indices() {
        if let NodeType::Argument(arg, aux) = &graph[node] {
            match aux {
                false => slots.push((node, 0)),
                true => aux_slots.push((node, 0)),
            }
        }
    }
    slots.reverse();
    aux_slots.reverse();
    
    /// Creates a new set of instructions from a value graph. Randomization is employed to vary
    /// the order in which values are generated, as changing the order changes the number of
    /// pulls and swaps needed.
    fn codegen(
        original: &ValueGraph,
        graph: &mut ValueGraph,
        slots: &mut Vec<(NodeIndex, usize)>,        // values on the data stack
        aux_slots: &mut Vec<(NodeIndex, usize)>,    // values that are on the aux stack
        aux_outputs: usize,                         // values that should be put on the aux stack at the end
        completed: &mut HashSet<NodeIndex>,
        node: NodeIndex,
        debug_info: DebugInfo,
        entropy: &mut SmallRng,
    ) -> Vec<Instruction> {
        
        macro_rules! create {
            ($opcode:expr) => {
                Instruction {
		    opcode: $opcode,
		    immediate: None,
		    debug_info: debug_info.color(debug_info.attributes.color_group + 1),
	        }
            };
            ($opcode:ident, $value:expr) => {
                Instruction {
		    opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		    immediate: Some($value),
		    debug_info: debug_info.color(debug_info.attributes.color_group + 1),
	        }
            };
        }
        
        let mut code = vec![];
        
        if let NodeType::Output = &graph[node] {

            /*if detail > 0 {
                print_slots("first", slots, aux_slots, node);
            }*/
            
            // Before codegening anything, we need to make sure unused items on the stack
            // created in the callee are removed. We can't just not codegen / prune them,
            // since these were generated elsewhere.
            
            let mut used_args = vec![];
            for arg in slots.iter().rev() {
                let degree = graph.neighbors_directed(arg.0, Direction::Incoming).count();
                if degree != 0 {
                    used_args.push(*arg);
                } else {
                    code.push(create!(Opcode::Pull(used_args.len())));
                    code.push(create!(Opcode::AVMOpcode(AVMOpcode::Pop)));
                }
            }
            used_args.reverse();
            *slots = used_args;
            
            // By eliminating instructions like xset and xget, we've transformed the graph
            // to never have an instruction take an input directly from the aux stack. Hence,
            // we can move each item from the aux stack to the data stack in advance.
            
            let mut used_args = vec![];
            for arg in aux_slots.iter().rev() {
                let degree = graph.neighbors_directed(arg.0, Direction::Incoming).count();
                if degree != 0 {
                    used_args.push(*arg);
                    code.push(create!(Opcode::AVMOpcode(AVMOpcode::AuxPop)));
                } else {
                    code.push(create!(Opcode::AVMOpcode(AVMOpcode::AuxPop)));
                    code.push(create!(Opcode::AVMOpcode(AVMOpcode::Pop)));
                }
            }
            slots.extend(used_args.into_iter());
            aux_slots.clear();
        }
        
        if completed.contains(&node) {
            //println!("We've already done node({})", node.index());
            return vec![];
        }

        /*if detail > 0 {
            print_slots("prior", slots, aux_slots, node);
        }*/
        
        struct Info {
            code: Vec<Instruction>,
            input: NodeIndex,
            edge: EdgeIndex,
            edge_type: EdgeType,
            start_degree: usize,
            prior_degree: usize,
            final_degree: usize,
        }
        
        let mut input_edges: Vec<EdgeIndex> = graph.edges_directed(node, Direction::Outgoing).map(|e| e.id()).collect();
        input_edges.shuffle(entropy);
        
        let mut inputs = vec![];
        
        for edge in input_edges {
            let input = graph.edge_endpoints(edge).unwrap().1;
            inputs.push(Info {
                input: input,
                edge: edge,
                code: codegen(
                    original, graph, slots, aux_slots, aux_outputs, completed, input,
                    debug_info.color(debug_info.attributes.color_group + 1), entropy
                ),
                edge_type: EdgeType::Meta,    // gets overwritten
                start_degree: 0,    // gets overwritten
                prior_degree: 0,    // gets overwritten
                final_degree: 0,    // gets overwritten
            });
        }
        for (mut info) in inputs.iter_mut() {
            info.edge_type = graph.edge_weight(info.edge).unwrap().clone();
        }
        
        for info in inputs.iter() {
            code.extend(info.code.clone());
        }
        
        /// Gets the degree of a given node's output value.
        fn value_degree(info: &Info, graph: &ValueGraph, direction: Direction) -> usize {
            let output = match info.edge_type {
                EdgeType::Connect(output, _) => output,
                _ => return 0,
            };
            graph.edges_directed(info.input, direction)
                .filter(|e| match graph.edge_weight(e.id()) {
                    Some(EdgeType::Connect(output_number, _)) => *output_number == output,
                    _ => false,
                })
                .count()
        }
        
        // inputs are in random order and aren't duplicated
        /*let mut positions = vec![];
        for info in inputs.iter() {
            let output_number = match info.edge_type {
                EdgeType::Meta => continue,
                EdgeType::Connect(output_number, input_number) => output_number,
            };
            let top_slot = slots.len() - 1;
            let slot = top_slot - slots.iter()
                .rev().position(|slot| slot == &(info.input, output_number))
                .expect(&format!("no slot assigned for node {:?}'s {}", info.input, output_number));
            positions.push((info.input, output_number, slot));
        }
        let deepest = positions.iter().map(|(_, _, slot)| slot).min();*/
        
        
        
        inputs.sort_by_key(|a| a.edge_type.input_number());
        inputs.reverse();
        for info in inputs.iter_mut() {
            info.start_degree = value_degree(info, &graph, Direction::Incoming);
        }
        for info in inputs.iter_mut() {
            info.prior_degree = value_degree(info, &graph, Direction::Incoming);
            graph.remove_edge(info.edge);
        }
        for info in inputs.iter_mut() {
            info.final_degree = value_degree(info, &graph, Direction::Incoming);
        }
        
        for info in inputs.iter() {

            let connect = match info.edge_type {
                EdgeType::Meta => continue,
                EdgeType::Connect(output_number, input_number) => (output_number, input_number),
            };
            
            let top_slot = slots.len() - 1;
            let start_slot = top_slot - slots.iter()
                .rev().position(|slot| slot == &(info.input, connect.0))
                .expect(&format!("no slot assigned for node {:?}", info.input));
            
            let should_pull =
                info.final_degree == 0 &&                     // insn consumes the input
                info.start_degree == info.prior_degree;       // this is the first use of the input
            
            /*if detail > 0 {
                println!(
                    "{} {:>2} {} => {} {:?}",
                    match should_pull {
                        true => "pull",
                        false => "dupe",
                    },
                    info.input.index(), start_slot, top_slot,
                    slots.iter().map(|(node, out)| (node.index(), out)).collect::<Vec<_>>()
                );
            }*/
            
            if should_pull {
                if start_slot != top_slot {
                    code.push(create!(Opcode::Pull((top_slot as isize - start_slot as isize).abs() as usize)));
                }
                
                let pulled = slots.remove(start_slot);
                slots.push(pulled);
                
            } else {
                code.push(create!(Opcode::Dup((top_slot as isize - start_slot as isize).abs() as usize)));
                
                let duped = slots[start_slot];
                slots.push(duped);
            }

            /*if detail > 0 {
                println!(
                    "          => {} {:?}",
                    top_slot, slots.iter().map(|(node, out)| (node.index(), out)).collect::<Vec<_>>()
                );
            }*/
        }

        /*if detail > 0 {
            print_slots("inner", slots, aux_slots, node);
        }*/

        let inputs_iter = inputs.into_iter().filter(|info| matches!(info.edge_type, EdgeType::Connect(..))).rev();
        match &graph[node] {
            NodeType::Output => {
                let mut aux_pushes_left = aux_outputs;
                for _ in inputs_iter {
                    match aux_pushes_left {
                        0 => drop(slots.pop()),
                        _ => {
                            aux_slots.push(slots.pop().expect("Tried to AuxPush a value that wasn't there"));
                            code.push(create!(Opcode::AVMOpcode(AVMOpcode::AuxPush)));
                            aux_pushes_left -= 1;
                        }
                    }
                }
            }
            _ =>
                for _ in inputs_iter {
                    slots.pop();
                }
        }
        
        match &graph[node] {
            NodeType::Output => {}
            NodeType::GlobalState => {}
            NodeType::Argument(_, _aux) => {}
            NodeType::AVMOpcode(avm_op) => {
                code.push(create!(Opcode::AVMOpcode(*avm_op)));
                let mut output_number = 0;
                for effect in avm_op.effects() {
                    match effect {
                        OpcodeEffect::PushStack
                        | OpcodeEffect::PushAux => {
                            slots.push((node, output_number));
                            output_number += 1;
                        }
                        _ => {}
                    }
                }
                
                for output in 0..output_number {
                    
                    let degree = original.edges_directed(node, Direction::Incoming)
                        .filter(|e| match original.edge_weight(e.id()) {
                            Some(EdgeType::Connect(output_number, _)) => *output_number == output,
                            _ => false,
                        }).count();
                    
                    if degree == 0 {    // assumes an insn drops at most 1 value
                        code.push(create!(Opcode::Pull(output_number - output - 1)));
                        code.push(create!(Opcode::AVMOpcode(AVMOpcode::Pop)));
                        let spot = slots.len() - (output_number - output);
                        slots.remove(spot);
                    }
                }
                
            }
            NodeType::Value(value) => {
                code.push(create!(Noop, value.clone()));
                slots.push((node, 0));
            }
            x => panic!("not implemented for {:#?}", x),
        }

        /*if detail > 0 {
            print_slots("final", slots, aux_slots, node);
        }*/
        
        completed.insert(node);
        code
    }
    
    let mut opt = unoptimized_block.clone();
    let mut original = graph.clone();
    let mut entropy: SmallRng = SeedableRng::seed_from_u64(2);
    
    for _ in 0..4 {
        // attempt to codegen a better set of instructions than the best found so far.
        
        let mut alt = codegen(
            &original,
            &mut graph.clone(),
            &mut slots.clone(),
            &mut aux_slots.clone(),
            aux_output_count,
            &mut HashSet::new(),
            output_node,
            debug_info,
            &mut entropy
        );
        alt = peephole2(&alt);
        alt = devirtualize(&alt, false);
        loop {
            let (opt, same) = peephole(&alt, false);
            alt = opt;
            if same {
                break;
            }
        }
        
        let alt_cost = alt.iter().map(|insn| insn.opcode.base_cost()).sum::<u64>();
        let opt_cost = opt.iter().map(|insn| insn.opcode.base_cost()).sum::<u64>();
        
        if alt_cost < opt_cost {
            opt = alt;
        }
    }
    
    /// Employes simple optimizations to reduce the burden of pulls and swaps.
    fn peephole2(block: &Block) -> Block {
        
        return block.clone();
        
        let mut block: Vec<_> = block.clone().into_iter().rev().collect();
        
        let mut opt = vec![];
        let mut index = 0;
        while index < block.len() {
            
            let curr = &block[index];
            
            match curr.opcode {
                Opcode::Pull(0) => continue,
                Opcode::Pull(depth) => {

                    // 0 1 2 3 4
                    // 0 2 3 4 1    Pull(3)
                    // 0 3 4 1 2    Pull(3)
                    // 0 4 1 2 3    Pull(3)
                    // 0 4 1 2 3 1  Dup(2)
                    // 0 1 2 3 1 4  Pull(4)
                    
                    // 0 1 2 3 4
                    // 0 2 3 4 1    Pull(3)
                    // 0 2 3 4 1 3  Dup(2)

                    // code coverage + codecov rename things + graph of funcs() + review spec
                    // Pull(3)    
                    // Pull(3)
                    // Pull(3)    Pull(3)
                    // Dup(2)     Dup(2)
                    // Pull(4)    Pull(4)
                    
                    let mut scan = index + 1;
                    let mut can_drop = block.get(index + depth - 1).is_some();
                    if can_drop {
                        for i in scan..(index + depth) {
                            match &block[scan].opcode {
                                Opcode::Pull(d) if *d == depth => {}
                                _ => can_drop = false,
                            }
                        }
                        if can_drop {
                            opt.push(Instruction::from_opcode(Opcode::Pull(depth), curr.debug_info));
                            index += depth;
                            continue;
                        }
                    }
                }
                _ => {}
            }
            
            opt.push(curr.clone());
            index += 1;
        }
        opt.reverse();
        opt
    }
    
    Ok(opt)
}

/// Converts virtual Pulls and Dups into equivalent AVM instructions.
pub fn devirtualize(block: &Block, debug: bool) -> Block {
    let mut opt = vec![];
    
    for curr in block {
        macro_rules! create {
            ($opcode:tt) => {
                Instruction {
		    opcode: Opcode::AVMOpcode(AVMOpcode::$opcode),
		    immediate: None,
		    debug_info: curr.debug_info,
	        }
            };
        }
        
        opt.extend(match &curr.opcode {
            Opcode::Dup(depth) => match depth {
                0 => vec![create!(Dup0)],
                1 => vec![create!(Dup1)],
                2 => vec![create!(Dup2)],
                x => {
                    let mut code = vec![];
                    for _ in 0..(x-2) {
                        code.push(create!(AuxPush));
                    }
                    code.push(create!(Dup2));
                    for _ in 0..(x-2) {
                        code.extend(vec![create!(AuxPop), create!(Swap1)]);
                    }
                    code
                }
            }
            Opcode::Pull(depth) => match depth {
                0 => vec![],
                1 => vec![create!(Swap1)],
                2 => vec![create!(Swap1), create!(Swap2)],
                x => {
                    let mut code = vec![];
                    for _ in 0..(x-2) {
                        code.push(create!(AuxPush));
                    }
                    code.extend(vec![create!(Swap1), create!(Swap2)]);
                    for _ in 0..(x-2) {
                        code.extend(vec![create!(AuxPop), create!(Swap1)]);
                    }
                    code
                }
            }
            _ => vec![curr.clone()],
        });
    }
    
    opt
}

pub fn graph_reduce(code: &Block, detail: bool) -> Result<Block, CompileError> {
    let mut opt = vec![];
    let mut block = vec![];
    
    print_code(&code, "graph reduce input", &BTreeMap::new());
    
    for (index, curr) in code.iter().enumerate() {
        
        macro_rules! split {
            (@internal $opcode:ident) => {
                curr.opcode == Opcode::AVMOpcode(AVMOpcode::$opcode)
            };
            (@internal $opcode:ident, $($remaining:ident),+) => {
                split!(@internal $opcode) || split!(@internal $($remaining),+)
            };
            ($($opcodes:ident),* $(,)?) => {
                let should_split =
                    split!(@internal $($opcodes),*)          // insn isn't supported
                    || match curr.opcode {
                        Opcode::AVMOpcode(_) => false,
                        _ => true,                           // this is a virtual insn
                    };
                
                if should_split {
                    let reduced = graph_reduce_block(&block, 1).map_err(|(msg, debug_info, graph)| {
                        println!("Value Graph for Proof of Incorrectness");
                        print_graph(&graph);
                        CompileError::from_debug("Optimizer proof", msg, debug_info)
                    })?;
                    
                    let (prior_stack, prior_aux, prior_confused) = eval_stacks(&block);
                    let (after_stack, after_aux, after_confused) = eval_stacks(&reduced);
                    
                    if !prior_confused && (after_confused || after_stack != prior_stack || after_aux != prior_aux) {
                        print_code(&block, "deleterious", &BTreeMap::new());
                        panic!("Optimization didn't preserve the stacks!");
                    }
                    
                    opt.extend(reduced);
                    opt.push(curr.clone());
                    block = vec![];
                } else {
                    block.push(curr.clone());
                }
                
                if index == code.len() - 1 {
                    opt.extend(graph_reduce_block(&block, 1).map_err(|(msg, debug_info, graph)| {
                        println!("Value Graph for Proof");
                        print_graph(&graph);
                        CompileError::from_debug("Optimizer proof", msg, debug_info)
                    })?);
                }
            };
        }
        
        split!(
            // those that induce branching
            Jump, Cjump,
            
            // those that are out of the scope of the current optimizer
            GetGas, SetGas, Halt, Panic, Inbox, InboxPeek, Send, Log, Breakpoint, DebugPrint,
            Sideload, OpenInsn, StackEmpty, AuxStackEmpty, ErrCodePoint, ErrSet, Blake2f,
        );
    }
    
    Ok(opt)
}
