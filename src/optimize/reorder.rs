/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::DebugInfo;
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode};
use crate::optimize::peephole;
use lazy_static::lazy_static;
use parking_lot::Mutex;
use petgraph::graph::NodeIndex;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::hash::{Hash, Hasher};

#[cfg(test)]
use rand::prelude::*;

lazy_static! {
    static ref CACHE: Mutex<HashMap<u64, (Vec<Instruction>, Vec<NodeIndex>)>> =
        Mutex::new(HashMap::new());
}

/// Reorder the stack to place needed values on top.
/// - needs represent the ordered, potentially duplicated, values we want at the top of the stack
/// - kills represent needs we don't need to save future copies of.
pub fn reorder_stack(
    stack: &Vec<NodeIndex>,
    needs: Vec<NodeIndex>,
    kills: HashSet<NodeIndex>,
    print: bool,
) -> Result<(Vec<Instruction>, Vec<NodeIndex>), &'static str> {
    // Algorithm
    //   Annotate the stack & needs with how many copies need to be made & their order
    //   Move a sliding window around, fixing order & duping values as is needed
    //   Optimize to compute an efficient transformation of Swap's, Dup's, and Aux code.

    let mut hasher = DefaultHasher::new();
    stack.hash(&mut hasher);
    needs.hash(&mut hasher);
    let ordered_kills: BTreeSet<_> = kills.clone().into_iter().collect();
    ordered_kills.hash(&mut hasher);
    let hash = hasher.finish();

    if let Some(cached) = CACHE.lock().get(&hash) {
        return Ok(cached.clone());
    }

    // Ensure needs are present on the stack & all kills are needed
    let stack_check: HashSet<_> = stack.clone().into_iter().collect();
    let needs_check: HashSet<_> = needs.clone().into_iter().collect();
    if !stack_check.is_superset(&needs_check) {
        return Err("needs ⊄ stack");
    }
    if !needs_check.is_superset(&kills) {
        return Err("kills ⊄ needs");
    }

    // Determine which values can be treated as "blanks" verses
    // those whose order we must track. Blanks get sifted to the bottom.
    let used: HashSet<_> = needs.clone().into_iter().collect();
    let mut stack: Vec<_> = stack
        .into_iter()
        .map(|item| (*item, 0, used.contains(item)))
        .collect();

    // Differentiate needs based on their copy number.
    let mut need_counts: HashMap<_, usize> = HashMap::new();
    let needs: Vec<_> = needs
        .into_iter()
        .map(|need| {
            // If a value isn't killed, we need to leave a copy of it on the stack.
            // We can do this by saying we need the 1st copy of an unkilled value.
            // Since the 0th version won't be marked as "needed", it'll be left under the stack.
            let start = match kills.contains(&need) {
                true => 0,
                false => 1,
            };
            let count = need_counts.entry(need).or_insert(start);
            let exact = (need, *count, true);
            *count += 1;
            exact
        })
        .collect();

    // The "highest copy" of a need is its latest copy on the stack.
    // The "highest need" of a need is the final copy we'll eventually want on the stack.
    let mut highest_copy: HashMap<_, _> = need_counts.iter().map(|(n, _)| (*n, 0)).collect();
    let highest_need: HashMap<_, _> = need_counts.iter().map(|(n, c)| (*n, c - 1)).collect();

    /// Determine if two items on the stack need to be swapped
    fn out_of_order(
        upper: &(NodeIndex, usize, bool),
        lower: &(NodeIndex, usize, bool),
        needs: &Vec<(NodeIndex, usize, bool)>,
    ) -> bool {
        let upper_pos = (upper.2).then(|| needs.into_iter().position(|n| *n == *upper));
        let lower_pos = (lower.2).then(|| needs.into_iter().position(|n| *n == *lower));

        match (upper_pos, lower_pos) {
            (Some(Some(upper_pos)), Some(Some(lower_pos))) => {
                // two needs whose positions differ
                upper_pos < lower_pos
            }
            (Some(None), Some(Some(_))) => {
                // a 0th copy we need to save vs a true need
                true
            }
            _ => {
                // a blank vs a need
                (!upper.2 || upper_pos.is_none()) && lower.2
            }
        }
    }

    let print_stack = |title: &str, stack: &Vec<(NodeIndex, usize, bool)>| {
        if print {
            print!("{}", title);
            for item in stack {
                match item.2 {
                    true => print!(" {}{}{}", item.0.index(), Color::grey("-"), item.1),
                    false => print!("{}", Color::grey(" ???")),
                }
            }
            println!();
        }
    };
    print_stack("needs", &needs);
    print_stack("start", &stack);

    let mut debug = DebugInfo::default();
    debug.attributes.color_group = 1;
    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
        };
    }

    let mut code = vec![];

    macro_rules! window {
        ($label:tt) => {
            // Fix any out-of-order items on the part of the stack we can access
            // with Dups & Swaps. We call this top part of the stack the "window".

            let len = stack.len();
            let mut top = (len >= 1).then(|| stack[len - 1]);
            let mut mid = (len >= 2).then(|| stack[len - 2]);
            let mut bot = (len >= 3).then(|| stack[len - 3]);

            if let (Some(top_val), Some(bot_val)) = &(top, bot) {
                if out_of_order(top_val, bot_val, &needs) {
                    code.push(opcode!(Swap2));
                    stack.swap(len - 1, len - 3);
                    top = Some(*bot_val);
                    bot = Some(*top_val);
                    print_stack("swap2", &stack);
                }
            }
            if let (Some(top_val), Some(mid_val)) = &(top, mid) {
                if out_of_order(top_val, mid_val, &needs) {
                    code.push(opcode!(Swap1));
                    stack.swap(len - 1, len - 2);
                    top = Some(*mid_val);
                    mid = Some(*top_val);
                    print_stack("swap1", &stack);
                }
            }

            // For safety we update these to point to the right items even if they aren't used again,
            // so we do this assignment to silence the "unused assignment" warnings
            let _ = (top, bot, mid);

            // Now that we've ordered the window, see if there's any dups needed.
            $label: loop {
                for depth in 1..3 {
                    let len = stack.len();
                    let item = (len >= depth).then(|| stack[len - depth]);
                    if let Some((item, _, true)) = item {
                        let high_copy = *highest_copy.get(&item).unwrap();
                        let high_need = *highest_need.get(&item).unwrap();
                        if high_copy < high_need {
                            code.push(match depth {
                                1 => opcode!(Dup0),
                                2 => opcode!(Dup1),
                                3 => opcode!(Dup2),
                                _ => unreachable!(),
                            });
                            stack.push((item, high_copy + 1, true));
                            highest_copy.insert(item, high_copy + 1);
                            print_stack(&format!("dup{} ", depth - 1), &stack);
                            continue $label;
                        }
                    }
                }
                break;
            }
        };
    }

    let mut aux = vec![]; // values temporarily in the aux stack.

    loop {
        // Slide the window up and down via AuxPush & AuxPop, making corrections until
        // what we need is at the top of the stack.

        while stack.len() > 0 {
            window!('going_down);
            code.push(opcode!(AuxPush));
            aux.push(stack.pop().unwrap());
            print_stack("apush", &stack);
        }

        while aux.len() > 0 {
            code.push(opcode!(AuxPop));
            stack.push(aux.pop().unwrap());
            print_stack("aupop", &stack);
            window!('going_up);
        }

        let top_of_stack: Vec<_> = stack.iter().rev().take(needs.len()).rev().collect();
        let what_we_need: Vec<_> = needs.iter().collect();
        if top_of_stack != what_we_need {
            continue;
        }

        print_stack("final", &stack);
        break;
    }

    // Eliminate any spans of AuxPush's and AuxPop's. These cancel each other out,
    // and by eliding them we've essentially transformed the window function to
    // ignore ordered parts of the stack as it intelligently moves the window
    // directly to where values need to be swapped.
    let code = peephole::filter_pair(
        code,
        Opcode::AVMOpcode(AVMOpcode::AuxPush),
        Opcode::AVMOpcode(AVMOpcode::AuxPop),
    );
    let code = peephole::filter_pair(
        code,
        Opcode::AVMOpcode(AVMOpcode::AuxPop),
        Opcode::AVMOpcode(AVMOpcode::AuxPush),
    );

    let stack = stack.into_iter().map(|item| item.0).collect();
    let result = (code, stack);
    CACHE.lock().insert(hash, result.clone());
    Ok(result)
}

#[test]
fn reorder_test() -> Result<(), &'static str> {
    let mut rng = thread_rng();

    for _ in 0..128 {
        let stack: [usize; 32] = rng.gen();
        let stack: HashSet<_> = IntoIterator::into_iter(stack).collect();
        let stack: Vec<NodeIndex> = stack.into_iter().map(NodeIndex::new).collect();

        let mut needs = vec![];
        for item in &stack {
            for _ in 0..(rand::random::<usize>() % 3) {
                needs.push(*item);
            }
        }
        let mut kills = HashSet::new();
        for item in &needs {
            if rand::random::<usize>() % 4 == 0 {
                kills.insert(*item);
            }
        }

        let (code, mut new_stack) = reorder_stack(&stack, needs.clone(), kills, true)?;
        println!("{:?}", stack);
        println!("{:?}", new_stack);
        for curr in code {
            println!("{}", curr.pretty_print(Color::PINK));
        }

        new_stack = new_stack
            .into_iter()
            .rev()
            .take(needs.len())
            .rev()
            .collect();
        assert_eq!(new_stack, needs);
    }
    Ok(())
}
