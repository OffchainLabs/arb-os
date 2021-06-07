/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Provides functions for restructuring and analyzing the program to aid in optimization passes.

use crate::link::FlowGraph;
use crate::uint256::Uint256;
use crate::compile::FileInfo;
use crate::mavm::{Instruction, Opcode, AVMOpcode, Value, Label, OpcodeEffect};
use crate::console::ConsoleColors;
use std::collections::{HashMap, HashSet, BTreeMap};

///Pretty-prints a list of instructions with the lines that generated them.
pub fn print_code(code: &Vec<Instruction>, title: &str, file_info_chart: &BTreeMap<u64, FileInfo>) {
    
    let grey = ConsoleColors::GREY;
    let pink = ConsoleColors::PINK;
    let blue = ConsoleColors::BLUE;
    let red  = ConsoleColors::RED;
    let reset = ConsoleColors::RESET;
    
    println!("{}======={} {}{}{} {}======={}", grey, reset, pink, title, reset, grey, reset);
    
    for (index, curr) in code.iter().enumerate() {
        if curr.debug_info.attributes.codegen_print {
            println!(
                "{: <44}{}",
                format!(
                    "{}{:04}{}  {}{}{} {}", grey, index, reset,
                    match &curr.debug_info.attributes.was_inlined {
                        0 => reset,
                        1 => blue,
                        2 => pink,
                        _ => red,
                    },
                    curr.opcode.pretty_print(reset, pink),
                    reset,
                    match &curr.immediate {
                        None => String::from(""),
                        Some(value) => value.pretty_print(pink, reset),
                    },
                ),
                match &curr.debug_info.location {
                    None => String::from(""),
                    Some(loc) => match file_info_chart.get(&loc.file_id) {
                        None => String::from(""),
                        Some(info) => {
                            match info.contents.get(loc.line.to_usize()) {
                                None => String::from(""),
                                Some(line) => format!(
                                    "\t  \t  \t  {}{}{}{}",
                                    grey,
                                    line.chars().take(32).collect::<String>(),
                                    if line.trim().len() <= 32 {
                                        "".to_string()
                                    } else {
                                        "...".to_string()
                                    },
                                    reset,
                                ),
                            }
                        }
                    }
                },
            );
        }
    }
    
}

///Counts the number of each kind of label.
pub fn count_labels(code: &Vec<Instruction>) -> (HashMap<usize, usize>, HashMap<usize, usize>) {
    
    let mut func_labels = HashMap::new();
    let mut anon_labels = HashMap::new();
    
    for insn in code {
        match &insn.immediate {
            Some(Value::Label(Label::Func(label))) => *func_labels.entry(*label).or_insert(0) += 1,
            Some(Value::Label(Label::Anon(label))) => *anon_labels.entry(*label).or_insert(0) += 1,
            _ => {}
        }
    }
    (func_labels, anon_labels)
}

///Elides labels that are never used.
pub fn elide_useless_labels(code: &Vec<Instruction>, anon_labels: HashMap<usize, usize>) -> Vec<Instruction> {
    
    let mut opt = Vec::with_capacity(code.len());
    
    for insn in code {
        if let Opcode::Label(Label::Anon(label)) = insn.opcode {
            match anon_labels.get(&label) {
                Some(_count) => {}
                None => continue,
            }
        }
        opt.push(insn.clone());
    }
    opt
}

///Opportunistically inlines functions, embedding their stack frames and eliding jumps.
pub fn inline_frames(code: &Vec<Instruction>) -> (Vec<Instruction>, usize) {
    
    let mut splices = HashMap::new();    // maps func labels to instruction contents
    
    for index in 0..(code.len() - 1) {
        
        if let Opcode::Label(Label::Func(label)) = &code[index].opcode {
            
            let mut reader = index + 1;
            let mut splice = vec![];
            
            while reader < code.len() {
                
                let mut insn = code[reader].clone();
                insn.debug_info.attributes.was_inlined = 1;
                
                if let Opcode::Label(Label::Func(_)) = &insn.opcode {
                    break;
                }
                
                splice.push(insn);
                reader += 1;
            }
            
            splices.insert(label, splice);
        }
    }
    
    let (func_labels, _) = count_labels(&code);    // function label counts before inlining
    
    let mut outgoing = HashMap::new();    // maps func labels to those that they call
    let mut incoming = HashMap::new();    // maps func labels to those that call them
    
    let mut current_func = None;
    let mut index = 0;
    while index < code.len() {
        
        if let Opcode::Label(Label::Func(label)) = &code[index].opcode {
            current_func = Some(*label);
        }
        
        let window = (code.get(index), code.get(index + 1), code.get(index + 2), code.get(index + 3));
        
        // This is what a function call looks like.
        if let (Some(a), Some(b), Some(c), Some(d)) = &window {
            if let (
                Opcode::AVMOpcode(AVMOpcode::Noop),    // noop Label(label_x)
                Opcode::AVMOpcode(AVMOpcode::Noop),    // noop Label(function_y)
                Opcode::AVMOpcode(AVMOpcode::Jump),    // jump
                Opcode::Label(Label::Anon(below)),     // label_x
            ) = (a.opcode, b.opcode, c.opcode, d.opcode) {
                
                if let (
                    Some(Value::Label(Label::Anon(back))), 
                    Some(Value::Label(Label::Func(dest)))
                ) = (a.immediate.as_ref(), b.immediate.as_ref()) {
                    if *back == below {
                        let caller = current_func.unwrap();
                        outgoing.entry(caller.clone()).or_insert(vec![]).push(*dest);
                        incoming.entry(*dest).or_insert(vec![]).push(current_func);
                        index += 4;
                        continue;
                    }
                }
            }
        }
        
        index += 1;
    }
    
    
    let mut current_func = None;
    let mut index = 0;
    let mut inlined = vec![];
    let mut inlined_funcs = HashSet::new();
    let mut changed = 0;
    while index < code.len() {
        
        if let Opcode::Label(Label::Func(label)) = &code[index].opcode {
            current_func = Some(*label);
        }
        
        let window = (code.get(index), code.get(index + 1), code.get(index + 2), code.get(index + 3));
        
        if let (Some(a), Some(b), Some(jump), Some(d)) = &window {
            if let (
                Opcode::AVMOpcode(AVMOpcode::Noop),    // noop Label(label_4)
                Opcode::AVMOpcode(AVMOpcode::Noop),    // noop Label(function_0)
                Opcode::AVMOpcode(AVMOpcode::Jump),    // jump
                Opcode::Label(Label::Anon(below)),     // label_4
            ) = (a.opcode, b.opcode, jump.opcode, d.opcode) {
                
                if let (
                    Some(Value::Label(Label::Anon(back))), 
                    Some(Value::Label(Label::Func(dest)))
                ) = (a.immediate.as_ref(), b.immediate.as_ref()) {
                    
                    if *back == below && *dest != current_func.unwrap() {
                        
                        // time to inline :)
                        
                        // here would be where you'd apply your heuristic.
                        // for the time being, we'll inline all funcs f for which
                        //   - f calls no one else
                        //   - f is not recursive
                        //   - f only returns once
                        //   - f has no branches
                        //   - f's function pointer is never created
                        
                        if let None = outgoing.get(&dest) {
                            
                            let mut splice: Vec<Instruction> = splices.get(dest).unwrap().clone()
                                .into_iter()
                                .map(|mut insn| {
                                    insn.debug_info.attributes.codegen_print = 
                                        insn.debug_info.attributes.codegen_print || 
                                        jump.debug_info.attributes.codegen_print;
                                    insn
                                }).collect();
                            
                            let ret = splice.pop().unwrap();    // remove final return
                            
                            match &ret.opcode {
                                Opcode::Return => {}
                                _ => panic!("Stripped something besides a return: {}", ret.opcode),
                            }
                            if let Some(_) = ret.immediate {
                                panic!("Stripped the stack!");
                            }
                            
                            let mut safe_to_inline = 
                                *func_labels.get(dest).unwrap() == incoming.get(dest).unwrap().len();
                            
                            for insn in &splice {
                                match insn.opcode {
                                    Opcode::Return => safe_to_inline = false,
                                    Opcode::Label(Label::Anon(_)) => safe_to_inline = false,
                                    Opcode::Label(Label::External(_)) => safe_to_inline = false,
                                    Opcode::Label(Label::Evm(_)) => safe_to_inline = false,
                                    _ => {}
                                }
                            }
                            
                            if safe_to_inline {
                                let mut debug_info = code[index].debug_info;
                                debug_info.attributes.was_inlined = 2;
                                if let Some(insn) = splice.iter().next() {
                                    debug_info.attributes.codegen_print =
                                        debug_info.attributes.codegen_print ||
                                        insn.debug_info.attributes.codegen_print;
                                }
                                inlined.push(Instruction {
                                    opcode: Opcode::AVMOpcode(AVMOpcode::Noop),
                                    immediate: Some(Value::Int(Uint256::from_usize(0))),
                                    debug_info: debug_info
                                });
                                
                                let mut debug_info = code[index].debug_info;
                                debug_info.attributes.was_inlined = 2;
                                if let Some(insn) = splice.iter().last() {
                                    debug_info.attributes.codegen_print =
                                        debug_info.attributes.codegen_print ||
                                        insn.debug_info.attributes.codegen_print;
                                }
                                inlined.extend(splice);
                                inlined.push(Instruction {
                                    opcode: Opcode::PopFrame,
                                    immediate: None,
                                    debug_info: debug_info,
                                });
                                
                                inlined_funcs.insert(*dest);
                                changed += 1;
                                index += 4;
                                continue;
                            }
                        }
                    }
                }
            }
        }
        
        inlined.push(code[index].clone());
        index += 1;
    }
    
    
    let mut opt = vec![];
    let mut index = 0;
    
    while index < inlined.len() {
        
        let curr = &inlined[index];
        
        if let Opcode::Label(Label::Func(label)) = curr.opcode {
            
            if inlined_funcs.contains(&label) {
                // skip functions that were inlined
                
                loop {
                    index += 1;
                    let insn = &inlined[index];
                    
                    if let Opcode::Label(Label::Func(_)) = insn.opcode {
                        break;
                    }
                    if index == inlined.len() {
                        break;
                    }
                }
                continue;
            }
        }
        opt.push(curr.clone());
        index += 1;
    }    
    (opt, changed)
}

///Creates a Control Flow Graph from a list of instructions.
/// The edges are currently unpopulated.
pub fn create_cfg(code: &Vec<Instruction>) -> FlowGraph {
    
    let mut graph = FlowGraph::default();
    
    let mut vec = vec![];
    
    for curr in code {
        
        match curr.opcode {
            Opcode::Label(_) => {
                graph.add_node(vec);
                vec = vec![curr.clone()];
            }
            Opcode::AVMOpcode(AVMOpcode::Jump) |
            Opcode::AVMOpcode(AVMOpcode::Cjump) => {
                vec.push(curr.clone());
                graph.add_node(vec);
                vec = vec![];
            }
            _ => {
                vec.push(curr.clone());
            }
        }
    }
    
    graph
}

///Converts a Control Flow Graph back to its equivalent list of instructions.
pub fn flatten_cfg(graph: FlowGraph) -> Vec<Instruction> {
    let mut code = vec![];    
    for node in graph.node_indices() {
        code.extend(graph[node].iter().cloned());
    }
    code
}

///Pretty prints a comparison between marked code blocks in two Control Flow Graphs.
pub fn print_cfg(prior: &FlowGraph, after: &FlowGraph, title: &str) {
    
    let blue = ConsoleColors::BLUE;
    let mint = ConsoleColors::MINT;
    let pink = ConsoleColors::PINK;
    let grey = ConsoleColors::GREY;
    let reset = ConsoleColors::RESET;
    
    println!("{}======={} {}{}{} {}======={}", grey, reset, mint, title, reset, grey, reset);
    
    let mut prior_global_index = -1;
    let mut after_global_index = -1;
    
    for node in prior.node_indices() {
        
        let prior_block = &prior[node];
        let after_block = &after[node];
        
        let print_blocks = 
            prior_block.iter().find(|curr| curr.debug_info.attributes.codegen_print).is_some() ||
            after_block.iter().find(|curr| curr.debug_info.attributes.codegen_print).is_some();
        
        if !print_blocks {
            continue;
        }
        
        let prior_cost: u64 = prior_block.iter().map(|insn| insn.opcode.base_cost()).sum();
        let after_cost: u64 = after_block.iter().map(|insn| insn.opcode.base_cost()).sum();
        
        let prior_updates: usize = prior_block.iter().map(|insn| insn.debug_info.updates).max().unwrap();
        
        let (prior_stack, prior_aux, prior_confused) = eval_stacks(&prior_block);
        let (after_stack, after_aux, after_confused) = eval_stacks(&after_block);
        
        println!(
            "{: <30}  {: <52}  {}",
            format!("{}Block {}{}", blue, node.index(), reset),
            format!(
                "{}{} =>{} {}{}{}",
                grey, prior_cost, reset,
                if after_cost < prior_cost {
                    mint
                } else if after_cost > prior_cost {
                    pink
                } else {
                    grey
                },
                after_cost,
                reset,
            ),
            format!(
                "{}{}{}, {}{}{}{}",
                match after_stack == prior_stack {
                    true => grey,
                    false => pink,
                },
                after_stack - prior_stack, grey,
                match after_aux == prior_aux {
                    true => grey,
                    false => pink,
                },
                after_aux - prior_aux,
                match prior_confused || after_confused {
                    true => "?",
                    false => "",
                },
                reset
            ),
        );
        
        let mut prior_iter = prior_block.iter().peekable();
        let mut after_iter = after_block.iter().peekable();
        
        loop {
            
            let (mut prior, mut after) = (vec![], vec![]);
            
            while prior_iter.peek().is_some() {
                prior_global_index += 1;
                if let Some(insn) = prior_iter.next() {
                    if insn.debug_info.attributes.codegen_print {
                        prior = format!(
                            "{}{:04}{}  {} {}", grey, prior_global_index, reset, insn.opcode,
                            match &insn.immediate {
                                None => "".to_string(),
                                Some(immediate) => format!("{}", immediate),
                            },
                        ).lines().map(|s| s.to_string()).collect();
                        break;
                    }
                }
            }
            
            while after_iter.peek().is_some() {
                after_global_index += 1;
                if let Some(insn) = after_iter.next() {
                    if insn.debug_info.attributes.codegen_print {
                        after = format!(
                            "{}{:04}{}  {}{} {}{}", grey, after_global_index, reset,
                            match insn.debug_info.updates > prior_updates {
                                false => reset,
                                true => pink,
                            },
                            insn.opcode,
                            match &insn.immediate {
                                None => "".to_string(),
                                Some(immediate) => format!("{}", immediate),
                            },
                            reset,
                        ).lines().map(|s| s.to_string()).collect();
                        break;
                    }
                }
            }
            
            let mut prior_lines = prior.iter();
            let mut after_lines = after.iter();
            
            let mut prior_line = prior_lines.next();
            let mut after_line = after_lines.next();
            
            while prior_line.is_some() || after_line.is_some() {
                
                let blank = String::from(format!("{}{}", grey, reset));
                
                print!("{: <44}", prior_line.unwrap_or(&blank));
                println!("{}", after_line.unwrap_or(&String::from("")));
                
                prior_line = prior_lines.next();
                after_line = after_lines.next();
            }
            
            if prior.is_empty() && after.is_empty() {
                break;
            }
        }
        
        if (after_stack != prior_stack || after_aux != prior_aux) && !prior_confused {
            panic!("Optimization broke the frame");
        }
    }
}

///Useful for determining peephole opportunities.
pub fn _print_cfg_stats(graph: &FlowGraph) {
    
    let mut pairs: HashMap<((usize, usize, usize), (bool, bool, bool)), usize> = HashMap::new();
    
    for node in graph.node_indices() {
        
        let block = &graph[node];
        
        if block.len() < 3 {
            continue;
        }
        
        for index in 0..(block.len()-3) {
            
            let curr = &block[index];
            let next = &block[index + 1];
            let last = &block[index + 2];
            
            if let (Opcode::AVMOpcode(prior), Opcode::AVMOpcode(after), Opcode::AVMOpcode(later)) = 
                (curr.opcode, next.opcode, last.opcode) {
                
                let pair = (prior.to_number() as usize, after.to_number() as usize, later.to_number() as usize);
                let immediates = (curr.immediate.is_some(), next.immediate.is_some(), last.immediate.is_some());
                
                let count = *pairs.entry((pair, immediates)).or_insert(0);
                
                pairs.insert((pair, immediates), count + 1);
            }
        }
    }
    
    let mut costs = BTreeMap::new();
    
    for ((pair, immediates), count) in pairs {
        
        let cost = (
            Opcode::AVMOpcode(AVMOpcode::from_number(pair.0).unwrap()).base_cost() +
            Opcode::AVMOpcode(AVMOpcode::from_number(pair.1).unwrap()).base_cost() +
            Opcode::AVMOpcode(AVMOpcode::from_number(pair.1).unwrap()).base_cost()
        ) as usize;
        
        costs.insert(count * cost, (pair, count, immediates));
    }
    
    let blue = ConsoleColors::BLUE;
    let grey = ConsoleColors::GREY;
    let reset = ConsoleColors::RESET;
    
    for (total_cost, ((prior, after, later), count, immediates)) in costs {
        println!(
            "{: <6} {: <6} {}{: <12}{} {}{: <12}{} {}{: <12}{}",
            total_cost, count,
            match immediates.0 {
                false => grey,
                true => blue,
            },
            AVMOpcode::from_number(prior).unwrap().to_name(), reset,
            match immediates.1 {
                false => grey,
                true => blue,
            },
            AVMOpcode::from_number(after).unwrap().to_name(), reset,
            match immediates.2 {
                false => grey,
                true => blue,
            },
            AVMOpcode::from_number(later).unwrap().to_name(), reset,
        );
    }
}

///Determines the net change these instructions have
/// on the stack and aux stack, if possible.
pub fn eval_stacks(code: &Vec<Instruction>) -> (isize, isize, bool) {
    
    let mut stack = 0;
    let mut aux = 0;
    let mut confused = false;
    
    for curr in code {
        match &curr.opcode {
            Opcode::Label(_) => continue,
            _ => {}
        }
        for effect in &curr.effects() {
            match effect {
                OpcodeEffect::PushStack => stack += 1,
                OpcodeEffect::PopStack => stack -= 1,
                OpcodeEffect::PushAux => aux += 1,
                OpcodeEffect::PopAux => aux -= 1,
                OpcodeEffect::ReadStack(_) => {}
                OpcodeEffect::ReadAux => {}
                OpcodeEffect::SwapStack(_) => {}
                OpcodeEffect::MoveToStack => { stack += 1; aux -= 1 }
                OpcodeEffect::MoveToAux => { stack -= 1; aux += 1 }
                OpcodeEffect::Unsure => confused = true,
            }
        }
    }
    (stack, aux, confused)
}
