/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functions for modifying a sequence of Instructions to improve performance and lower gas
//! costs.

use crate::console::ConsoleColors;
use crate::mavm::{AVMOpcode, Instruction, Opcode, OpcodeEffect, Value};
use crate::link::analyze::{print_code};
use crate::link::analyze::{print_cfg, create_cfg};
use crate::compile::DebugInfo;
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
            (Opcode::AVMOpcode(AVMOpcode::IsZero), Opcode::AVMOpcode(AVMOpcode::IsZero)) => {
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
                    opt.push(Instruction {
                        opcode: next.opcode.clone(),
                        immediate: Some(value.clone()),
                        debug_info: next.debug_info,
                    });
                    index += 2;
                    continue;
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
                OpcodeEffect::Unsure => {
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

#[derive(Clone, Debug)]
pub enum NodeType {
    AVMOpcode(AVMOpcode),
    Value(Value),
    Argument(usize),
    Output,
    GlobalState,
    Function(usize, usize),
}

impl NodeType {
    fn is_prunable(&self) -> bool {
        match self {
            NodeType::AVMOpcode(_) => true,
            NodeType::Value(_) => true,
            NodeType::Argument(_) => false,
            NodeType::Output => false,
            NodeType::GlobalState => false,
            NodeType::Function(..) => true,
        }
    }
    fn pretty_print(&self) -> String {
        let reset = ConsoleColors::RESET;
        let pink = ConsoleColors::PINK;
        let grey = ConsoleColors::GREY;
        match self {
            NodeType::AVMOpcode(avm_op) => format!("{}\t", Opcode::AVMOpcode(*avm_op).pretty_print(pink, pink)),
            NodeType::Value(value) => format!("{}\t", value.pretty_print(pink, pink)),
            NodeType::Argument(arg) => format!("Arg {}{}{}\t", pink, arg, reset),
            NodeType::Output => String::from("Output\t"),
            NodeType::GlobalState => String::from("Global State"),
            NodeType::Function(inputs, outputs) => format!(
                "{}func({}{}{}{},{}{}{}{}){}", grey, reset, pink, inputs, reset, pink, outputs, reset, grey, reset),
        }
    }
}

pub type ValueGraph = StableGraph<NodeType, (usize, usize)>;
pub type PrintGraph = StableGraph<String, (usize, usize)>;

pub fn graph_reduce(block: &Block, debug: bool) -> (Block, bool) {
    let mut same = true;
    
    let debug_info = block[0].debug_info.propagate(true);
    
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
    
    let mut nooped = Vec::with_capacity(block.len());
    for curr in block.iter() {
        nooped.extend(
            match &curr.opcode {
                Opcode::AVMOpcode(AVMOpcode::Noop) => vec![curr.clone()],
                opcode => match &curr.immediate {
                    Some(value) => vec![
                        create!(Noop, Some(value.clone())),
                        Instruction::from_opcode(opcode.clone(), debug_info),
                    ],
                    None => vec![curr.clone()],
                }
            }
        );
    }
    let block = nooped;

    let mut arg_count = 0;
    let mut data_stack: VecDeque<(isize, usize)> = VecDeque::new();
    let mut aux_stack:  VecDeque<(isize, usize)> = VecDeque::new();
    
    let mut graph = ValueGraph::default();
    let mut index_to_node = HashMap::new();
    
    macro_rules! touch {
        ($stack:expr, $count:expr) => {
            let stack = &mut $stack;
            let count = $count as usize;
            for touched in 0..(count.saturating_sub(stack.len())) {
                arg_count += 1;
                index_to_node.insert(-arg_count, graph.add_node(NodeType::Argument(arg_count as usize)));
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
            _ => panic!("Graph reduce should never encounter a virtual opcode"),
        }
        
        let effects = curr.effects();
        
        let mut output_number = 0;
        let mut input_number = 0;
        
        for effect in effects {
            match effect {
                OpcodeEffect::PopStack => {
                    touch!(data_stack, 1);
                    data_stack.pop_back();
                }
                OpcodeEffect::PushStack => {
                    data_stack.push_back((index, output_number));
                    output_number += 1;
                }
                OpcodeEffect::ReadStack(depth) => {
                    touch!(data_stack, depth);
                    let (producer, which) = data_stack[data_stack.len() - depth];
                    let this_node = index_to_node.get(&index).unwrap();
                    let that_node = index_to_node.get(&producer).unwrap();
                    graph.add_edge(*this_node, *that_node, (which, input_number));
                    input_number += 1;
                }
                OpcodeEffect::SwapStack(depth) => {
                    touch!(data_stack, depth);
                    let swapped = data_stack.swap_remove_back(data_stack.len() - depth - 1).unwrap();
                    data_stack.push_back(swapped);
                }
                x => panic!("effect not yet handled: {}", x.to_name()),
            }
        }
    }
    
    let output_node = graph.add_node(NodeType::Output);
    let mut output_count = 0;
    
    while let Some((producer, _)) = data_stack.pop_back() {
        let node = index_to_node.get(&producer).unwrap();
        graph.add_edge(output_node, *node, (0, output_count));    // todo handle multi-value
        output_count += 1;
    }
    
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
    
    fn relink(graph: &mut ValueGraph, node: NodeIndex) {
        
        let input_edges: Vec<EdgeIndex> = graph.edges_directed(node, Direction::Outgoing).map(|e| e.id()).collect();
        
        let mut de_duplicated = vec![];
        let mut tget_elided = vec![];
        //let mut tset_elided = vec![];
        
        // de-duplicate
        for edge in input_edges {
            let (_, input) = graph.edge_endpoints(edge).unwrap();
            de_duplicated.push((*graph.edge_weight(edge).unwrap(), find_original(graph, input)));
            relink(graph, input);
            graph.remove_edge(edge);
        }
        
        // elide tget's
        for (weight, input) in de_duplicated {
            match &graph[input] {
                NodeType::AVMOpcode(avm_op) if avm_op == &AVMOpcode::Tget => {
                    
                    let mut inputs: Vec<_> = graph.neighbors_directed(input, Direction::Outgoing).collect();
                    inputs.sort();
                    let [tuple_node, offset_node]: [NodeIndex; 2] = inputs.try_into().unwrap();
                    
                    match &graph[offset_node] {
                        NodeType::Value(Value::Int(offset)) if offset.to_usize().is_some() => {
                            tget_elided.push((weight, tget_elision(graph, tuple_node, offset.to_usize().unwrap())));
                        }
                        _ => tget_elided.push((weight, input)),
                    }
                }
                _ => tget_elided.push((weight, input)),
            }
        }
        
        for (weight, input) in tget_elided {
            graph.add_edge(node, input, weight);
            //relink(graph, input);
        }
        
        fn find_original(graph: &ValueGraph, node: NodeIndex) -> NodeIndex {
            match &graph[node] {
                NodeType::Value(_) => node,
                NodeType::Argument(_) => node,
                NodeType::AVMOpcode(avm_op) => match avm_op {
                    AVMOpcode::Dup0
                    | AVMOpcode::Dup1
                    | AVMOpcode::Dup2 => {
                        find_original(graph, graph.neighbors_directed(node, Direction::Outgoing).next().unwrap())
                    }
                    AVMOpcode::Noop
                    | AVMOpcode::Swap1
                    | AVMOpcode::Swap2 => unreachable!("{} should have been eliminated", avm_op),
                    _ => node,
                }
                x => panic!("Not implemented for {}", x.pretty_print()),
            }
        }
        
        fn tget_elision(graph: &ValueGraph, node: NodeIndex, offset: usize) -> NodeIndex {
            match &graph[node] {
                NodeType::Value(_) => node,
                NodeType::Argument(_) => node,
                NodeType::AVMOpcode(avm_op) => match avm_op {
                    AVMOpcode::Tset => {
                        let mut inputs: Vec<_> = graph.neighbors_directed(node, Direction::Outgoing).collect();
                        inputs.sort();
                        
                        let [tuple_node, offset_node, value_node]: [NodeIndex; 3] = inputs.try_into().unwrap();
                        
                        println!("{:?}\n{} {:?}\n{:?}", &graph[tuple_node], offset, &graph[offset_node], &graph[value_node]);
                        
                        match &graph[offset_node] {
                            NodeType::Value(Value::Int(set_offset)) if set_offset.to_usize().is_some() => {
                                match offset == set_offset.to_usize().unwrap() {
                                    true => value_node,
                                    false => tget_elision(graph, tuple_node, offset), // need to match against tuple
                                }
                            }
                            _ => node,
                        }
                    }
                    _ => node,
                }
                x => panic!("Not implemented for {}", x.pretty_print()),
            }
        }
    }
    
    
    loop {
        let node_count = graph.node_count();
        prune_graph(&mut graph);
        relink(&mut graph, output_node);
        prune_graph(&mut graph);
        if graph.node_count() == node_count {
            break;
        }
    }
    
    for node in graph.node_indices() {
        let grey = ConsoleColors::GREY;
        let reset = ConsoleColors::RESET;
        let neighbors = graph.neighbors_directed(node, Direction::Outgoing);
        print!("{}{:>3}{}  {}  ", grey, node.index(), reset, graph[node].pretty_print());
        for node in neighbors {
            print!("{}{},{} ", node.index(), grey, reset);
        }
        println!();
    }

    
    let mut node_to_slot = BTreeMap::new();
    let mut slot_to_node = vec![];
    for node in graph.node_indices() {
        if let NodeType::Argument(arg) = &graph[node] {
            node_to_slot.insert(node, arg_count as usize - *arg);
        }
    }
    for node in node_to_slot.keys().rev() {
        slot_to_node.push(*node);
    }
    
    //println!("slots: {:#?}", node_to_slot);
    println!("nodes: {:#?}", slot_to_node);
    
    fn codegen(
        graph: &mut ValueGraph,
        node_to_slot: &mut BTreeMap<NodeIndex, usize>,
        slot_to_node: &mut Vec<NodeIndex>,
        node: NodeIndex,
        debug_info: DebugInfo,
        entropy: &mut SmallRng,
    ) -> Vec<Instruction> {

        if node_to_slot.len() != slot_to_node.len() {
            panic!("Slot assignments are inconsistent");
        }
        
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
        
        match &graph[node] {
            NodeType::Argument(_) => return vec![],
            NodeType::Value(value) => {
                if let None = node_to_slot.get(&node) {
                    node_to_slot.insert(node, node_to_slot.len());
                    slot_to_node.push(node);
                }
                return vec![create!(Noop, value.clone())];
            }
            _ => {}
        }
        
        struct Info {
            code: Vec<Instruction>,
            connect: (usize, usize),
            degree: usize,
            input: NodeIndex,
            slot: usize,
        }
        
        let mut input_edges: Vec<EdgeIndex> = graph.edges_directed(node, Direction::Outgoing).map(|e| e.id()).collect();
        input_edges.sort();
        input_edges.shuffle(entropy);
        
        let mut inputs = vec![];
        
        for edge in input_edges.clone() {
            let input = graph.edge_endpoints(edge).unwrap().1;
            inputs.push(Info {
                input: input,
                code: codegen(
                    graph, node_to_slot, slot_to_node, input, debug_info.color(debug_info.attributes.color_group + 1), entropy
                ),
                connect: (0, 0),    // gets overwritten
                degree: 0,          // gets overwritten
                slot: 0,            // gets overwritten
            });
        }
        for (edge, mut info) in input_edges.iter().zip(inputs.iter_mut()) {
            info.connect = *graph.edge_weight(*edge).unwrap();
            info.degree = graph.neighbors_directed(info.input, Direction::Incoming).count();
            info.slot = *node_to_slot.get(&info.input).expect(&format!("no slot assigned for input {}", info.input.index()));
        }
        for edge in input_edges {
            graph.remove_edge(edge);
        }
        
        //inputs.sort_by(|a, b| a.connect.1.cmp(&b.connect.1));
        
        let mut code = vec![];
        
        for info in inputs.iter() {
            code.extend(info.code.clone());
            /*if info.degree > 1 {
            code.extend(create!(AVMOpcode::Dup0));
            }*/
        }
        
        for info in inputs.iter_mut() {
            
            let top_slot = node_to_slot.len() - 1;
            let dest_slot = top_slot - info.connect.1;
            let start_slot = *node_to_slot.get(&info.input).expect("no slot assigned");
            
            println!("top {} {:?} {} {} {:?}", top_slot, info.input, dest_slot, start_slot, slot_to_node);
            
            if start_slot == dest_slot {
                continue;
            }
            
            // How to swap * and &
            //   0 * 1 2 & 3 4
            //   0 4 1 2 & 3 *  (start, top)
            //   0 4 1 2 * 3 &  (dest, top)
            //   0 & 1 2 * 3 4  (start, top)
            
            // How to swap * and & when dest is top (not the same as above!)
            //   0 * 1 2 &
            //   0 & 1 2 *  (start, top)
            
            // How to swap a dup'd * into &
            //     | 0 * 1 2 & 3 4
            //     | 0 * 1 2 & 3 4 *  (dup start)
            //     | 0 * 1 2 * 3 4 &  (dest, new top)
            //   & | 0 * 1 2 * 3 4    (auxpush => auxpop later)
            
            if dest_slot == top_slot || start_slot == top_slot {
                code.push(create!(Opcode::Swap((dest_slot as isize - start_slot as isize).abs() as usize)));
            } else {
                code.extend(vec![
                    create!(Opcode::Swap(top_slot - start_slot)),
                    create!(Opcode::Swap(top_slot - dest_slot)),
                    create!(Opcode::Swap(top_slot - start_slot)),
                ]);
            }
            
            let tmp = slot_to_node[start_slot];
            slot_to_node[start_slot] = slot_to_node[dest_slot];
            slot_to_node[dest_slot] = tmp;
            
            node_to_slot.insert(slot_to_node[start_slot], start_slot);
            node_to_slot.insert(slot_to_node[dest_slot], dest_slot);
        }
        
        //println!("  => final slots {:?}\n", slot_to_node);
        
        for info in inputs.into_iter() {
            node_to_slot.remove(&slot_to_node.pop().unwrap());
        }
        
        match &graph[node] {
            NodeType::Output => {}
            NodeType::AVMOpcode(avm_op) => {
                code.push(create!(Opcode::AVMOpcode(*avm_op)));
                node_to_slot.insert(node, node_to_slot.len());
                slot_to_node.push(node);
            }
            x => panic!("not implemented for {:#?}", x),
        }
        
        code
    }
    
    //let mut entropy: SmallRng = thread_rng();
    let mut entropy: SmallRng = SeedableRng::seed_from_u64(2);
    
    let mut opt = block.clone();
    
    for _ in 0..8 {
        
        let alt = codegen(
            &mut graph.clone(),
            &mut node_to_slot.clone(),
            &mut slot_to_node.clone(),
            output_node,
            debug_info,
            &mut entropy
        );
        
        let alt_cost = alt.iter().map(|insn| insn.opcode.base_cost()).sum::<u64>();
        let opt_cost = opt.iter().map(|insn| insn.opcode.base_cost()).sum::<u64>();
        
        if alt_cost < opt_cost {
            opt = alt;
        }
    }
    
    println!("{:#?}", node_to_slot);
    println!("{:#?}", slot_to_node);
    
    /*let print_graph = graph.map(
        |_, node| format!("{} {}", node.pretty_print());
        |_, edge| Some(edge)
    );
    println!("{}", format!("{:?}", Dot::with_config(&print_graph, &[Config::EdgeNoLabel]))
             .replace("\\u{1b}", "\x1b")
             .replace("\"", "")
             .replace("\\", "")
             .replace("[ ]", "")
             .replace("[ ", "")
             .replace(" ]", "")
             .replace(" label = ", "\t")
    );*/

    //let opt = block.clone();
    
    (opt, same)
}
