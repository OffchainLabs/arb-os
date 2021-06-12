/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functions for modifying a sequence of Instructions to improve performance and lower gas
//! costs.

use crate::link::FlowGraph;
use crate::mavm::{AVMOpcode, Instruction, Opcode, OpcodeEffect, Value};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;

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

///Discovers unused values and eliminates them from the stacks.
pub fn stack_reduce(block: &Block, debug: bool) -> (Block, bool) {
    let mut same = true;
    
    let mut data_stack = vec![];
    let mut aux_stack = vec![];
    let mut replace: Vec<Vec<usize>> = vec![];
    
    for index in 0..block.len() {
        let effects = block[index].effects();
        
        for effect in effects {
            match effect {
                OpcodeEffect::PushStack => drop(data_stack.push((vec![index], true))),
                OpcodeEffect::PushAux => drop(aux_stack.push((vec![index], true))),
                OpcodeEffect::SwapStack(depth) => {
                    if depth + 1 > data_stack.len() {
                        data_stack.clear(); // this function does something nasty
                        aux_stack.clear();
                    } else if depth == 1 {
                        let mut lower = data_stack.pop().unwrap().clone();
                        let mut upper = data_stack.pop().unwrap().clone();
                        if index != lower.0[0] {
                            lower.0.push(index);
                        }
                        if index != upper.0[0] {
                            upper.0.push(index);
                        }
                        data_stack.push(lower);
                        data_stack.push(upper);
                    } else if depth == 2 {
                        let mut lower = data_stack.pop().unwrap().clone();
                        let mut still = data_stack.pop().unwrap().clone();
                        let mut upper = data_stack.pop().unwrap().clone();
                        if index != lower.0[0] {
                            lower.0.push(index);
                        }
                        if index != still.0[0] {
                            still.0.push(index);
                        }
                        if index != upper.0[0] {
                            upper.0.push(index);
                        }
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
                    if let Some((vec, _alive)) = aux_stack.pop() {
                        aux_stack.push((vec, false));
                    }
                }
                OpcodeEffect::PopStack => {
                    if let Some((mut vec, alive)) = data_stack.pop() {
                        if alive && index != vec[0] {
                            vec.push(index);
                            replace.push(vec);
                        }
                    }
                }
                OpcodeEffect::PopAux => {
                    if let Some((mut vec, alive)) = aux_stack.pop() {
                        if alive && index != vec[0] {
                            vec.push(index);
                            replace.push(vec);
                        }
                    }
                }
                OpcodeEffect::MoveToStack => {
                    match aux_stack.pop() {
                        Some((mut vec, alive)) => {
                            if index != vec[0] {
                                vec.push(index);
                            }
                            data_stack.push((vec, alive));
                        }
                        None => {
                            data_stack.clear(); // this function does something nasty
                            aux_stack.clear(); //
                        }
                    }
                }
                OpcodeEffect::MoveToAux => {
                    match data_stack.pop() {
                        Some((mut vec, alive)) => {
                            if index != vec[0] {
                                vec.push(index);
                            }
                            aux_stack.push((vec, alive));
                        }
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
    
    let mut changes: BTreeMap<usize, Vec<Instruction>> = BTreeMap::new();
    
    for places in replace {
        let generator = &block[places[0]];

        match &generator.opcode {
            Opcode::AVMOpcode(AVMOpcode::Xget) => continue,
            Opcode::AVMOpcode(AVMOpcode::EcMul) => continue,
            _ => {}
        }
        
        if debug {
            println!("Reduction {}", places.len());
            for place in places.iter() {
                print!("  - {} {: <14}\t  ", place, &block[*place]);
                for effect in &block[*place].effects() {
                    print!(" {}", effect.to_name());
                }
                print!("\n");
            }
        }

        let places = places.into_iter().rev().map(|place| (place, &block[place]));

        let mut born = false;
        let mut on_stack = true;

        for (place, insn) in places {
            let mut effects = insn.effects().into_iter().rev();

            let mut after = vec![];

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

            let after: Vec<OpcodeEffect> = after.into_iter().rev().collect();
            let before: Vec<OpcodeEffect> = effects.rev().collect();

            if debug {
                print!("  =       \t  ");
                for effect in &before {
                    print!(" {}", effect.to_name());
                }
                print!(" ---");
                for effect in &after {
                    print!(" {}", effect.to_name());
                }
            }

            let mut equivalent = vec![];

            let mut debug_info = insn.debug_info.clone();
            debug_info.updates += 1;

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
            }

            if debug {
                let grey = "\x1b[90m";
                let reset = "\x1b[0;0m";

                if !equivalent.is_empty() {
                    print!("\t{}=>{} ", grey, reset);
                    for equal in &equivalent {
                        print!(" {}", equal);
                    }
                }
                println!("");
            }

            changes.insert(place, equivalent);
        }

        same = false;
    }
    
    let mut opt = Vec::with_capacity(block.len());
    
    for index in 0..block.len() {
        let curr = &block[index];
        
        match changes.get(&index) {
            None => opt.push(curr.clone()),
            Some(equivalent) => {
                for insn in equivalent {
                    opt.push(insn.clone());
                }
            }
        }
    }
    
    (opt, same)
}
