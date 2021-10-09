/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, SlotNum};
use crate::console::{print_columns, Color};
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use crate::optimize::effects::{Effect, Effects};
use crate::optimize::peephole;
use crate::run::{Machine, MachineState};
use crate::link::fold_tuples;
use petgraph::algo;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::stable_graph::StableGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use rand::prelude::*;
use rand::rngs::SmallRng;
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::convert::TryInto;

pub enum ValueNode {
    Opcode(Opcode),     // a
    Value(Value),       // a simple value
    Arg(usize),         //
    Local(SlotNum),     //
    Meta(&'static str), //
}

impl ValueNode {
    fn prunable(&self) -> bool {
        match self {
            Self::Opcode(_) | Self::Value(_) => true,
            _ => false,
        }
    }
}

pub enum ValueEdge {
    Connect(usize),
    Meta(&'static str),
}

impl ValueEdge {
    fn input_order(&self) -> Reverse<usize> {
        match self {
            Self::Connect(num) => Reverse(1 + *num),
            Self::Meta(_) => Reverse(usize::MAX),
        }
    }
}

fn nodes(graph: &StableGraph<ValueNode, ValueEdge>) -> Vec<NodeIndex> {
    graph.node_indices().collect()
}

fn conn_count(graph: &StableGraph<ValueNode, ValueEdge>, node: NodeIndex) -> usize {
    graph
        .edges_directed(node, Direction::Incoming)
        .filter(|edge| matches!(edge.weight(), ValueEdge::Connect(_)))
        .count()
}

fn node_data(
    graph: &StableGraph<ValueNode, ValueEdge>,
    node: NodeIndex,
) -> (
    Vec<NodeIndex>,
    Vec<NodeIndex>,
    Vec<EdgeIndex>,
    Vec<EdgeIndex>,
) {
    let mut deps = vec![];
    let mut dep_edges = vec![];
    let mut edges: Vec<_> = graph.edges_directed(node, Direction::Outgoing).collect();
    edges.sort_by_key(|edge| edge.weight().input_order());
    for edge in &edges {
        let dep = edge.target();
        deps.push(dep);
        dep_edges.push(edge.id());
    }

    let mut users = vec![];
    let mut user_edges = vec![];
    for edge in graph.edges_directed(node, Direction::Incoming) {
        let user = edge.source();
        users.push(user);
        user_edges.push(edge.id());
    }

    (deps, users, dep_edges, user_edges)
}

pub struct ValueGraph {
    graph: StableGraph<ValueNode, ValueEdge>,
    defs: BTreeMap<SlotNum, NodeIndex>,
    phis: BTreeMap<SlotNum, SlotNum>,
    header: Vec<Instruction>,
    output: NodeIndex,
}

macro_rules! opcode {
    ($opcode:ident) => {
        Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), DebugInfo::default())
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

impl ValueGraph {
    pub fn print_lines(&self) -> Vec<String> {
        let graph = &self.graph;

        let mut output = vec![];

        for node in graph.node_indices() {
            let node_string = format!(
                " {}  {}",
                Color::grey(format!("{:>3}", node.index())),
                match &graph[node] {
                    ValueNode::Opcode(op) => op.pretty_print(Color::PINK),
                    ValueNode::Value(value) => value.pretty_print(Color::PINK),
                    ValueNode::Arg(num) => format!("Arg {}", Color::mint(num)),
                    ValueNode::Local(slot) => format!("Local {}", Color::mint(slot)),
                    ValueNode::Meta(name) => name.to_string(),
                }
            );
            let spacing = 22_usize.saturating_sub(Color::uncolored(&node_string).len());
            let mut line = format!("{} {}", node_string, " ".repeat(spacing));
            let mut edges: Vec<_> = graph.edges_directed(node, Direction::Outgoing).collect();
            edges.sort_by_key(|edge| edge.weight().input_order());

            for edge in edges {
                let input = edge.target();
                line += &format!(
                    "  {}",
                    match edge.weight() {
                        ValueEdge::Connect(_) => Color::grey(input.index()),
                        ValueEdge::Meta(_) => Color::mint(input.index()),
                    }
                );
            }
            output.push(line);
        }
        output
    }

    /// Create a new `ValueGraph` from a set of instructions without control flow
    pub fn new(code: &[Instruction]) -> Option<Self> {
        // Algorithm
        //
        //
        // Assumptions
        //   The instructions must be in SSA, which ensures blocks have:
        //     No more than 1 jump, which can only be at the end of a block
        //     No more than 1 label, which can only be at the start of a block
        //     Locals are accessed at a common auxstack height
        //     Phis happen just before jumping
        //     The order of phis don't matter

        // Separate the metadata instructions from the top. Given our assumptions,
        // these don't impact how values relate to one another.
        let mut header = vec![];
        for curr in code {
            match &curr.opcode {
                Opcode::Label(_) | Opcode::MakeFrame(..) => {
                    header.push(curr.clone());
                }
                _ => break,
            }
        }
        let code = &code[header.len()..];

        // Save the phi-metadata for later. Given our assumptions,
        // these only impact control flow.
        let mut phis = BTreeMap::new();
        for curr in code {
            if let Opcode::MoveLocal(dest, source) = &curr.opcode {
                phis.insert(*dest, *source);
            }
        }

        let mut graph = StableGraph::new();
        let mut locals = BTreeMap::new();
        let mut defs = BTreeMap::new();

        let mut globals = graph.add_node(ValueNode::Meta("globals"));
        let mut global_readers = BTreeSet::new();
        let mut pc_writer = None;

        let mut stack: VecDeque<NodeIndex> = VecDeque::new();
        let mut nargs = 0;

        macro_rules! touch {
            ($count:expr) => {
                let count: usize = $count;
                let missing = count.saturating_sub(stack.len());
                for _ in 0..missing {
                    // Arg wasn't on the stack, so lets push one to the bottom
                    let arg = graph.add_node(ValueNode::Arg(nargs));
                    stack.push_front(arg);
                    nargs += 1;
                }
            };
        }

        for curr in code {
            if let Some(value) = &curr.immediate {
                let node = graph.add_node(ValueNode::Value(value.clone()));
                stack.push_back(node);
            }

            if let Opcode::AVMOpcode(AVMOpcode::EcMul | AVMOpcode::EcAdd) = curr.opcode {
                // Let's not worry about opcodes that produce 2+ values. These are rare and
                // complicate things as you can see in the #compiler-optimization-passes branch.
                return None;
            }

            let node = graph.add_node(ValueNode::Opcode(curr.opcode));
            let effects = curr.opcode.effects();
            let mut input_count = 0;

            for effect in effects {
                match effect {
                    Effect::PushStack => {
                        stack.push_back(node);
                    }
                    Effect::PopStack => {
                        touch!(1);
                        stack.pop_back();
                    }
                    Effect::ReadStack => {
                        touch!(1);
                        let pusher = stack.iter().last().unwrap();
                        graph.add_edge(node, *pusher, ValueEdge::Connect(input_count));
                        input_count += 1;
                    }
                    Effect::DupStack(depth) => {
                        touch!(depth);
                        let pusher = stack[stack.len() - depth];
                        stack.push_back(pusher);
                    }
                    Effect::SwapStack(depth) => {
                        touch!(depth + 1);
                        let swapped = stack.swap_remove_back(stack.len() - depth - 1).unwrap();
                        stack.push_back(swapped);
                    }
                    Effect::ReadLocal(slot) => {
                        let local = match locals.get(&slot) {
                            Some(local) => *local,
                            None => {
                                let local = graph.add_node(ValueNode::Local(slot));
                                locals.insert(slot, local);
                                local
                            }
                        };
                        graph.add_edge(node, local, ValueEdge::Meta("read"));
                    }
                    Effect::WriteLocal(slot) => {
                        let local = graph.add_node(ValueNode::Local(slot));
                        if let Some(_) = locals.get(&slot) {
                            unreachable!("Not SSA: found 2nd write or write-after-read");
                        }
                        graph.add_edge(local, node, ValueEdge::Meta("set"));
                        locals.insert(slot, local);
                        defs.insert(slot, local);
                    }
                    Effect::ReadGlobal => {
                        graph.add_edge(node, globals, ValueEdge::Meta("view"));
                        global_readers.insert(node);
                    }
                    Effect::WriteGlobal => {
                        for reader in global_readers {
                            if reader != node {
                                graph.add_edge(node, reader, ValueEdge::Meta("order"));
                            }
                        }
                        graph.add_edge(node, globals, ValueEdge::Meta("order"));
                        global_readers = BTreeSet::new();
                        globals = graph.add_node(ValueNode::Meta("globals"));
                        graph.add_edge(globals, node, ValueEdge::Meta("write"));
                    }
                    Effect::WritePC => {
                        // this can only happen once since these are basic blocks,
                        // so we save this node so that it can be the output.
                        pc_writer = Some(node);
                    }
                    Effect::PushAux
                    | Effect::PopAux
                    | Effect::ReadAux
                    | Effect::MoveToStack
                    | Effect::MoveToAux
                    | Effect::Unsure => return None,
                }
            }
        }

        macro_rules! col {
            ($($collectable:tt)+) => {
                graph. $($collectable)+ .collect::<Vec<_>>()
            };
        }
        macro_rules! slice {
            ($num:expr, $vec:expr) => {{
                let slice: [_; $num] = $vec.try_into().expect("wrong number");
                slice
            }};
        }

        //
        let output = match pc_writer {
            Some(writer) => writer,
            None => graph.add_node(ValueNode::Meta("output")),
        };

        // shift the output
        let output_edges = col!(edges_directed(output, Direction::Outgoing).map(|e| e.id()));
        for (index, item) in stack.into_iter().rev().enumerate() {
            graph.add_edge(output, item, ValueEdge::Connect(index + output_edges.len()));
        }
        graph.add_edge(output, globals, ValueEdge::Meta("globals"));
        for (_, local) in locals {
            graph.add_edge(output, local, ValueEdge::Meta("locals"));
        }

        fn fold_constants(graph: &mut StableGraph<ValueNode, ValueEdge>) {
            // perform constant folding

            'next: for node in nodes(&graph) {

                if let ValueNode::Opcode(opcode) = &graph[node] {
                    // For AVM opcodes that only touch the stack, we can try to emulate them by
                    // building a machine whose only purpose is to execute the opcode and then halt.

                    if let Opcode::FuncCall(..) = opcode {
                        continue;
                    }
                    
                    let mut pushes = 0;
                    let mut stateful = false;
                    for effect in opcode.effects() {
                        match effect {
                            Effect::PushStack => pushes += 1,
                            Effect::PopStack => {}
                            Effect::ReadStack => {}
                            _ => stateful = true,
                        }
                    }
                    if pushes != 1 || stateful {
                        continue;
                    }

                    // we'll build a machine to run the instruction
                    let (deps, _, dep_edges, _) = node_data(&graph, node);
                    let mut code = vec![];
                    for dep in deps {
                        match &graph[dep] {
                            ValueNode::Value(value) => code.push(opcode!(Noop, value.clone())),
                            _ => continue 'next,
                        }
                    }
                    code.push(Instruction::from_opcode(*opcode, DebugInfo::default()));

                    // fold wide tuples as is needed to compute the result
                    let code = match fold_tuples(code, 0) {
                        Ok(code) => code,
                        _ => continue,
                    };
                    let mut machine = Machine::from(code);
                    machine.start_at_zero(false);
                    machine.run(None);
                    if let MachineState::Error(_) = &machine.state {
                        continue;
                    }

                    match machine.stack.contents.pop_back() {
                        Some(value) => graph[node] = ValueNode::Value(value.clone()),
                        _ => continue,
                    }
                    assert!(machine.stack.contents.is_empty());

                    for edge in dep_edges {
                        graph.remove_edge(edge);
                    }
                }
            }
        }

        /// Prunes nodes whose values are never consumed.
        fn prune_graph(graph: &mut StableGraph<ValueNode, ValueEdge>, output: NodeIndex) {
            loop {
                let node_count = graph.node_count();
                graph.retain_nodes(|this, node| {
                    !this[node].prunable()
                        || this.neighbors_directed(node, Direction::Incoming).count() > 0
                        || node == output
                });
                if graph.node_count() == node_count {
                    break;
                }
            }
        }

        fold_constants(&mut graph);
        prune_graph(&mut graph, output);

        let values = ValueGraph {
            graph,
            defs,
            phis,
            header,
            output,
        };

        if algo::is_cyclic_directed(&values.graph) {
            let ssa = code
                .into_iter()
                .map(|x| x.pretty_print(Color::PINK))
                .collect();
            let values = values.print_lines();
            print_columns(vec![ssa, values], vec!["SSA", "values"]);
            panic!("Value graph is not a DAG!");
        }

        Some(values)
    }

    pub fn codegen(&self) -> (Vec<Instruction>, usize) {
        let mut stack = vec![];
        let graph = &self.graph;

        let mut debug = DebugInfo::default();
        debug.attributes.codegen_print = true;

        macro_rules! opcode {
            ($opcode:ident) => {
                Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
            };
            ($opcode:ident, $immediate:expr) => {
                Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::$opcode),
                    $immediate,
                    debug,
                )
            };
            (@$($opcode:tt)+) => {
                Instruction::from_opcode(Opcode::$($opcode)+, debug)
            };
        }

        let mut header = self.header.clone();

        // Pop unused arguments. We can't just ignore them, since they were created elsewhere.
        for node in graph.node_indices().rev() {
            if let ValueNode::Arg(_) = &graph[node] {
                if conn_count(&graph, node) != 0 {
                    stack.push(node);
                } else {
                    header.push(opcode!(@Pop(stack.len())));
                }
            }
        }

        // determine the number of times each node is used by another
        let mut conn_counts = HashMap::new();
        for node in graph.node_indices() {
            conn_counts.insert(node, conn_count(&graph, node));
        }

        let output = self.output;

        fn descend(
            node: NodeIndex,
            graph: &StableGraph<ValueNode, ValueEdge>,
            stack: &mut Vec<NodeIndex>,
            conn_counts: &mut HashMap<NodeIndex, usize>,
            entropy: &mut SmallRng,
            done: &mut HashSet<NodeIndex>,
        ) -> Vec<Instruction> {
            let mut code = vec![];

            let mut deps: Vec<_> = graph.edges_directed(node, Direction::Outgoing).collect();
            deps.shuffle(entropy);

            // ensure everything this node depends on has been codegened
            for edge in &deps {
                let input = edge.target();
                if !done.contains(&input) {
                    code.extend(descend(input, graph, stack, conn_counts, entropy, done));
                }
            }

            // while codegen can be random, consumption needs to be in order
            deps.sort_by_key(|edge| edge.weight().input_order());

            // drop meta edges since they only enforce ordering
            let mut inputs = vec![];
            let mut edges = vec![];
            for edge in deps {
                if let ValueEdge::Connect(_) = edge.weight() {
                    inputs.push(edge.target());
                    edges.push(edge);
                }
            }

            // find where the needed values exist on the stack
            let mut needs = vec![]; // what's needed and where it is
            let mut kills = HashSet::new(); // values never used again
            for edge in &edges {
                let input = edge.target();
                needs.push(input);

                let remaining = conn_counts.get_mut(&input).unwrap();
                *remaining -= 1;
                if *remaining == 0 {
                    kills.insert(input);
                }
            }

            // reorder the stack to place the inputs at the top and then consume them
            let (trans, new_stack) = reorder_stack(&stack, needs, kills, false);
            *stack = new_stack;
            code.extend(trans);
            for _ in edges {
                stack.pop();
            }

            match &graph[node] {
                ValueNode::Opcode(opcode) => {
                    code.push(Instruction::from_opcode(*opcode, DebugInfo::default()));
                    let pushes = opcode
                        .effects()
                        .into_iter()
                        .filter(|effect| *effect == Effect::PushStack)
                        .count();
                    for _ in 0..pushes {
                        stack.push(node);
                    }
                }
                ValueNode::Value(value) => {
                    code.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Noop),
                        value.clone(),
                        DebugInfo::default(),
                    ));
                    stack.push(node);
                }
                _ => {}
            }

            done.insert(node);
            code
        }

        let mut best = vec![];
        let mut best_cost = usize::MAX;
        let mut entropy: SmallRng = SeedableRng::seed_from_u64(0);

        for _ in 0..64 {
            // attempt to codegen a better set of instructions than the best found so far.

            let mut stack = stack.clone();
            let mut alt = header.clone();
            alt.extend(descend(
                output,
                &graph,
                &mut stack,
                &mut conn_counts.clone(),
                &mut entropy,
                &mut HashSet::new(),
            ));
            let alt_cost = alt.iter().map(|x| x.opcode.base_cost()).sum();

            if alt_cost < best_cost {
                best_cost = alt_cost;
                best = alt;
            }
        }

        if let Some(exit) = best.pop() {
            for (dest, source) in &self.phis {
                best.push(opcode!(@MoveLocal(*dest, *source)));
            }
            best.push(exit);
        }

        (best, best_cost)
    }
}

/// Reorder the stack to place needed values on top.
/// - needs represent the ordered, potentially duplicated, values we want at the top of the stack
/// - kills represent needs we don't need to save future copies of.
fn reorder_stack(
    stack: &Vec<NodeIndex>,
    needs: Vec<NodeIndex>,
    kills: HashSet<NodeIndex>,
    print: bool,
) -> (Vec<Instruction>, Vec<NodeIndex>) {
    // Algorithm
    //   Annotate the stack & needs with how many copies need to be made & their order
    //   Move a sliding window around, fixing order & duping values as is needed
    //   Optimize to compute an efficient transformation of Swap's, Dup's, and Aux code.

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
            (Some(Some(_)), Some(Some(_))) => {
                // two needs whose positions differ
                let upper_pos = needs.into_iter().position(|n| *n == *upper).unwrap();
                let lower_pos = needs.into_iter().position(|n| *n == *lower).unwrap();
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
    (code, stack)
}

#[test]
fn reorder_test() {
    let stack = [0, 1, 2, 3, 4];
    let needs = [3, 1, 0, 1, 1, 0, 2];
    let kills = [1, 3];

    let stack: Vec<_> = std::array::IntoIter::new(stack)
        .map(NodeIndex::new)
        .collect();
    let needs: Vec<_> = std::array::IntoIter::new(needs)
        .map(NodeIndex::new)
        .collect();
    let kills: HashSet<_> = std::array::IntoIter::new(kills)
        .map(NodeIndex::new)
        .collect();

    let (code, mut new_stack) = reorder_stack(&stack, needs.clone(), kills, true);
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
