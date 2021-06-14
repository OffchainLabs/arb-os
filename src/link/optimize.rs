/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functions for modifying a sequence of Instructions to improve performance and lower gas
//! costs.

use crate::link::FlowGraph;
use crate::console::ConsoleColors;
use crate::mavm::{AVMOpcode, Instruction, Opcode, OpcodeEffect, Value};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;
use crate::link::analyze::{print_code};

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

pub fn _change_abi(graph: &FlowGraph, _debug: bool) -> (FlowGraph, bool) {
    let mut optimized = FlowGraph::default();
    let mut same = true;

    let mut global_offset = 0;

    for node in graph.node_indices() {
        let block = &graph[node];

        let mut opt = Vec::with_capacity(block.len());

        for index in 0..block.len() {
            let curr = &block[index];

            match &curr.opcode {
                Opcode::AVMOpcode(AVMOpcode::Xset) => opt.push(Instruction {
                    opcode: Opcode::AVMOpcode(AVMOpcode::AuxPush),
                    immediate: None,
                    debug_info: curr.debug_info,
                }),
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
                _ => opt.push(curr.clone()),
            }
        }
        optimized.add_node(opt);
    }

    (optimized, same)
}

#[derive(Clone)]
struct StackEntry {
    created: Option<usize>,
    moved: Vec<usize>,
    used: Vec<usize>,
    value: Option<Value>,
    children: Vec<StackEntry>,
}
impl StackEntry {
    fn new(created: usize, moved: Vec<usize>, used: Vec<usize>, children: Vec<Self>) -> Self {
        StackEntry {
            created: Some(created),
            moved: moved,
            used: used,
            value: None,
            children: vec![],
        }
    }
    fn blank() -> Self {
        StackEntry {
            created: None,
            moved: vec![],
            used: vec![],
            value: None,
            children: vec![],
        }
    }
    fn from(created: Option<usize>, value: &Value) -> Self {
        let mut entry = StackEntry::blank();
        entry.created = created;
        if let Value::Tuple(vec) = value {
            for value in &**vec {
                entry.children.push(StackEntry::from(None, &value));
            }
        }
        return entry;
    }
    fn shift(&mut self, starting_at: usize, amount: isize) {
	if let Some(ref mut created) = self.created {
	    if *created >= starting_at {
		*created = (*created as isize + amount) as usize
	    }
	}
	self.moved.iter_mut().filter(|x| **x >= starting_at).for_each(|x| *x = (*x as isize + amount) as usize);
	self.used.iter_mut().filter(|x| **x >= starting_at).for_each(|x| *x = (*x as isize + amount) as usize);
	//self.moved.iter_mut().for_each(|x| *x = (*x as isize + amount) as usize);
	//self.used.iter_mut().for_each(|x| *x = (*x as isize + amount) as usize);
	for child in &mut self.children {
	    child.shift(starting_at, amount);
	}
    }
}

///Discovers unused values and eliminates them from the stacks.
pub fn stack_reduce(block: &Block, debug: bool) -> (Block, bool) {
    let mut same = true;
    let mut data_stack: Vec<StackEntry> = vec![];
    let mut aux_stack:  Vec<StackEntry> = vec![];
    let mut eliminate: BTreeMap<usize, StackEntry> = BTreeMap::new();
    
    macro_rules! draw {
        ($stack:expr) => {
            $stack.pop().unwrap_or(StackEntry::blank())
        }
    }
    macro_rules! analyze {
        ($entry:expr) => {
            analyze(&$entry, &mut eliminate);
        };
    }
    fn analyze(entry: &StackEntry, eliminate: &mut BTreeMap<usize, StackEntry>) {
        
        for child in &entry.children {
            analyze(child, eliminate);
        }
        
        match entry.created {
            Some(index) if entry.used.len() == 0 => {
                eliminate.insert(index, entry.clone());    // peel children off
            }
            _ => {}
        }
    }
    
    let mut raised_error = false;
    
    for index in 0..block.len() {
        
        match &block[index].immediate {
            Some(value) => data_stack.push(StackEntry::from(Some(index), value)),
            None => {}
        }
        
        match &block[index].opcode {
            Opcode::AVMOpcode(AVMOpcode::Noop) => {}
            Opcode::AVMOpcode(AVMOpcode::Dup0) => {
                match data_stack.len() {
                    x if x >= 1 => {
                        let entry = &mut data_stack[x - 1];
                        let duplicate = entry.clone();
                        entry.used.push(index);
                        data_stack.push(StackEntry::new(index, vec![], vec![], vec![duplicate]));
                    }
                    _ => data_stack.push(StackEntry::new(index, vec![], vec![], vec![])),
                }
            }
            Opcode::AVMOpcode(AVMOpcode::Dup1) => {
                match data_stack.len() {
                    x if x >= 2 => {
                        let entry = &mut data_stack[x - 2];
                        let duplicate = entry.clone();
                        entry.used.push(index);
                        data_stack.push(StackEntry::new(index, vec![], vec![], vec![duplicate]));
                    }
                    _ => data_stack.push(StackEntry::new(index, vec![], vec![], vec![])),
                }
            }
            Opcode::AVMOpcode(AVMOpcode::Dup2) => {
                match data_stack.len() {
                    x if x >= 3 => {
                        let entry = &mut data_stack[x - 3];
                        let duplicate = entry.clone();
                        entry.used.push(index);
                        data_stack.push(StackEntry::new(index, vec![], vec![], vec![duplicate]));
                    }
                    _ => data_stack.push(StackEntry::new(index, vec![], vec![], vec![])),
                }
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
		entry.moved.push(index);
                analyze!(entry);
            }
            Opcode::AVMOpcode(AVMOpcode::Tset) => {
                let mut a = draw!(data_stack);    // index
                let mut b = draw!(data_stack);    // tuple
                let mut c = draw!(data_stack);    // value
                a.used.push(index);
                b.used.push(index);
                c.moved.push(index);
                
                let safe = match &a.value {
                    Some(Value::Int(offset)) => match offset.to_usize() {
                        Some(offset) => match &b.value {
                            Some(Value::Tuple(vec)) if vec.len() > offset => true,
                            _ => false,
                        }
                        _ => false,
                    }
                    _ => false,
                };
                
                if !safe {
                    raised_error = true;
                    break;
                }
                
                let offset = a.value.as_ref().unwrap().to_usize().unwrap();
                analyze!(a);
                analyze!(b.children[offset]);
                b.children[offset] = c;
                data_stack.push(b);
            }
            _ => {
                raised_error = true;
                break;
            }
        }
    }
    
    if raised_error {
        // We encountered some combination of instructions that will
        // raise an error. It's probably best to just not optimize anything.
        return (block.clone(), true);
    }
    
    // Apply the eliminations one at a time in case of overlap.
    let mut opt = block.clone();
    let mut color_group = 1;
    let mut eliminate: Vec<(usize, StackEntry)> = eliminate.into_iter().collect();
    let elim_count = eliminate.len();
    for elim_index in 0..elim_count {
	let (created, entry) = eliminate[elim_index].clone();    // unclone this

	print_code(&opt, "before next", &BTreeMap::new());

	// shifting & debug print
	println!("Eliminating {} {:?} {:?}", created, entry.moved, entry.created);
	
	let elimination = {
	    let mut elimination = vec![];
	    
	    let generator = &opt[created];
	    
	    let mut elim = match &generator.opcode {
		Opcode::AVMOpcode(AVMOpcode::Noop)
		| Opcode::AVMOpcode(AVMOpcode::Swap1) => vec![],
		Opcode::AVMOpcode(AVMOpcode::Dup0)
		| Opcode::AVMOpcode(AVMOpcode::Dup1)
		| Opcode::AVMOpcode(AVMOpcode::Dup2) => {
		    match &generator.immediate {
			Some(value) => vec![Instruction::from_opcode_imm(
			    Opcode::AVMOpcode(AVMOpcode::Noop),
			    value.clone(),
			    generator.debug_info.color(color_group),
			)],
			None => vec![],
		    }
		}
		Opcode::AVMOpcode(AVMOpcode::Swap2) =>
                    vec![Instruction::from_opcode(
			Opcode::AVMOpcode(AVMOpcode::Swap1),
			generator.debug_info.color(color_group),
                    )],
		Opcode::AVMOpcode(AVMOpcode::Tset) => {
		    vec![]
		}
		_ => vec![],
            };
	    
	    if debug && elim.len() == 0 {
		elim.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Noop),
                    generator.debug_info.color(color_group),
		));
            }
	    elimination.push((created, elim));
	    
	    for index in entry.moved {
		let insn = &opt[index];
		
		// Special case where instruction both creates and moves a value.
		// Pop's and Swap's are already handled in the generator block above.
		if index == created {
		    match &insn.opcode {
			Opcode::AVMOpcode(AVMOpcode::Pop)
			| Opcode::AVMOpcode(AVMOpcode::Swap1)
			| Opcode::AVMOpcode(AVMOpcode::Swap2) => continue,
			
			// a noop might seem to "move" a value if it came from a prior reduction.
			Opcode::AVMOpcode(AVMOpcode::Noop) => continue,
			_ => {}
		    }
		}
		
		let mut elim = match &insn.opcode {
                    Opcode::AVMOpcode(AVMOpcode::Pop) => vec![],
		    Opcode::AVMOpcode(AVMOpcode::Swap1) => {
			match &insn.immediate {
			    Some(value) => vec![Instruction::from_opcode_imm(
				Opcode::AVMOpcode(AVMOpcode::Noop),
				value.clone(),
				insn.debug_info.color(color_group),
			    )],
			    None => vec![],
			}
		    }
		    Opcode::AVMOpcode(AVMOpcode::Swap2) => {
			// Observe the following identity to understand why
			// a swap1 is always correct. Regardless of which value
			// * is being eliminated, the stack ends up the same way:
			//     a b * => * b a  ==>  swap1
			//     a * b => b * a  ==>  swap1
			match &insn.immediate {
			    Some(value) => vec![Instruction::from_opcode_imm(
				Opcode::AVMOpcode(AVMOpcode::Swap1),
				value.clone(),
				insn.debug_info.color(color_group),
			    )],
			    None => vec![Instruction::from_opcode(
				Opcode::AVMOpcode(AVMOpcode::Swap1),
				insn.debug_info.color(color_group),
			    )],
			}
		    }
                    _ => vec![],
		};
		
		if debug && elim.len() == 0 {
                    elim.push(Instruction::from_opcode(
			Opcode::AVMOpcode(AVMOpcode::Noop),
			generator.debug_info.color(color_group),
                    ));
		}
		elimination.push((index, elim));
	    };

	    //if debug {
		println!("Elimination {}", created);
		for (index, reduction) in &elimination {
		    print!(
			"  - {} {} {}\t=>",
			index,
			&opt[*index].opcode,
			match &opt[*index].immediate {
			    Some(value) => format!("{} ", value.pretty_print(ConsoleColors::RESET, ConsoleColors::PINK)),
			    None => String::from(""),
			},
		    );
		    for insn in reduction {
			print!(
			    " {} {}",
			    insn.opcode,
			    match &insn.immediate {
				Some(value) => format!("{} ", value.pretty_print(ConsoleColors::RESET, ConsoleColors::PINK)),
				None => String::from(""),
			    },
			);
		    }
		    println!("");
		}
	    //}
	    
	    color_group += 1;
	    elimination
	};
	
	let mut reduced = Vec::with_capacity(opt.len());
	let mut reductions = BTreeMap::new();
	
	for (index, reduction) in elimination {
	    reductions.insert(index, reduction);
	}
	
	for index in 0..opt.len() {
	    match reductions.get(&index) {
		Some(reduction) => {
		    let shift = reduction.len() as isize - 1;
		    //for future in (elim_index+1)..elim_count {
		    for future in (elim_index+1)..elim_count {
			// We need to correct future eliminations since the indices they were built on
			// are now off by the change in instruction count right here.
			if eliminate[future].0 >= index {
			    eliminate[future].0 = (eliminate[future].0 as isize + shift) as usize;
			}
			eliminate[future].1.shift(index, shift);
			// function that never returns
			// do not put a return addr on the stack
		    }
		    reduced.extend(reduction.clone());
		}
		None => reduced.push(opt[index].clone()),
	    }
	}
	
	opt = reduced;
    }
    
    (opt, same)
    
    /*
    for places in replace {
        let mut born = false;
        let mut on_stack = true;

        for (place, insn) in places {
            while let Some(effect) = effects.next() {
                if !born {
                    match effect {
                        OpcodeEffect::PopStack => {
                            on_stack = true;
                            born = true;
                            break;
                        }
                        OpcodeEffect::PopAux => {
                            on_stack = false;
                            born = true;
                            break;
                        }
                        _ => after.push(effect),
                    }
                } else if on_stack {
                    match effect {
                        OpcodeEffect::PushStack => break,
                        OpcodeEffect::MoveToStack => on_stack = false,
                        _ => after.push(effect),
                    }
                } else {
                    match effect {
                        OpcodeEffect::PushAux => break,
                        OpcodeEffect::MoveToAux => on_stack = true,
                        _ => after.push(effect),
                    }
                }
            }
	
            if insn == generator {
                match insn.opcode {
                    Opcode::AVMOpcode(AVMOpcode::Noop)
                        | Opcode::AVMOpcode(AVMOpcode::Rget)
                        | Opcode::AVMOpcode(AVMOpcode::AuxPush)
                        | Opcode::AVMOpcode(AVMOpcode::PushStatic)
                        | Opcode::AVMOpcode(AVMOpcode::Swap1) => {}
                    
                    Opcode::AVMOpcode(AVMOpcode::Swap2) => {}    // def wrong

                    Opcode::AVMOpcode(AVMOpcode::AuxPop) => {
                        equivalent.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                            immediate: None,
                            debug_info: debug_info,
                        });
                    }

                    Opcode::AVMOpcode(AVMOpcode::Dup0)
                        | Opcode::AVMOpcode(AVMOpcode::Dup1)
                        | Opcode::AVMOpcode(AVMOpcode::Dup2) => {
                            if let Some(value) = &insn.immediate {
                                equivalent.push(Instruction {
                                    opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                                    immediate: Some(value.clone()),
                                    debug_info: debug_info,
                                });
                            }
                        }

                    Opcode::AVMOpcode(AVMOpcode::Xset) => {
                        if let None = &insn.immediate {
                            equivalent.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                                immediate: None,
                                debug_info: debug_info,
                            });
                        }
                        equivalent.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: debug_info,
                        });

                        equivalent.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::AuxPop),
                            immediate: None,
                            debug_info: debug_info,
                        });
                        equivalent.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: debug_info,
                        });
                    }
                    
                    Opcode::AVMOpcode(AVMOpcode::StackEmpty) => {
                        if let Some(value) = &insn.immediate {
                            equivalent.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                                immediate: Some(value.clone()),
                                debug_info: debug_info,
                            });
                        }
                    }
                    
                    Opcode::AVMOpcode(AVMOpcode::Plus)
                        | Opcode::AVMOpcode(AVMOpcode::Mul)
                        | Opcode::AVMOpcode(AVMOpcode::Minus)
                        | Opcode::AVMOpcode(AVMOpcode::Div)
                        | Opcode::AVMOpcode(AVMOpcode::Sdiv)
                        | Opcode::AVMOpcode(AVMOpcode::Mod)
                        | Opcode::AVMOpcode(AVMOpcode::Smod)
                        | Opcode::AVMOpcode(AVMOpcode::Exp)
                        | Opcode::AVMOpcode(AVMOpcode::SignExtend)
                        | Opcode::AVMOpcode(AVMOpcode::LessThan)
                        | Opcode::AVMOpcode(AVMOpcode::GreaterThan)
                        | Opcode::AVMOpcode(AVMOpcode::SLessThan)
                        | Opcode::AVMOpcode(AVMOpcode::SGreaterThan)
                        | Opcode::AVMOpcode(AVMOpcode::Equal)
                        | Opcode::AVMOpcode(AVMOpcode::BitwiseAnd)
                        | Opcode::AVMOpcode(AVMOpcode::BitwiseOr)
                        | Opcode::AVMOpcode(AVMOpcode::BitwiseXor)
                        | Opcode::AVMOpcode(AVMOpcode::Byte)
                        | Opcode::AVMOpcode(AVMOpcode::ShiftLeft)
                        | Opcode::AVMOpcode(AVMOpcode::ShiftRight)
                        | Opcode::AVMOpcode(AVMOpcode::ShiftArith)
                        | Opcode::AVMOpcode(AVMOpcode::Hash2)
                        | Opcode::AVMOpcode(AVMOpcode::Tget) => {
                            if let None = &insn.immediate {
                                equivalent.push(Instruction {
                                    opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                                    immediate: None,
                                    debug_info: debug_info,
                                });
                            }
                            equivalent.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                                immediate: None,
                                debug_info: debug_info,
                            });
                        }

                    Opcode::AVMOpcode(AVMOpcode::Tset) => {
                        if let None = &insn.immediate {
                            equivalent.push(Instruction {
                                opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                                immediate: None,
                                debug_info: debug_info,
                            });
                        }
                        equivalent.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: debug_info,
                        });
                        equivalent.push(Instruction {
                            opcode: Opcode::AVMOpcode(AVMOpcode::Pop),
                            immediate: None,
                            debug_info: debug_info,
                        });
                    }
                    _ => {
                        println!("\nEncountered generator {}", insn);
                        panic!("Oh no! Stack Reduction is broken :(");
                    }
                };
            } else {
                if let Some(value) = &insn.immediate {
                    equivalent.push(Instruction {
                        opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                        immediate: Some(value.clone()),
                        debug_info: debug_info,
                    });
                }
 
                equivalent.extend(match insn.opcode {
                    Opcode::AVMOpcode(AVMOpcode::AuxPush)
                        | Opcode::AVMOpcode(AVMOpcode::AuxPop)
                        | Opcode::AVMOpcode(AVMOpcode::Pop) => vec![],
                    Opcode::AVMOpcode(AVMOpcode::Swap1) => vec![], // check this
                    Opcode::AVMOpcode(AVMOpcode::Swap2) => {
                        vec![]
                    }
                    _ => {
                        println!("\nEncountered {}", insn);
                        panic!("Oh no! Stack Reduction is broken :(");
                    }
                });
            }*/
}
