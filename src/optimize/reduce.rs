/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, SlotNum};
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use crate::optimize::effects::{Effect, Effects};
use crate::optimize::peephole;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use rand::prelude::*;
use rand::rngs::SmallRng;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};

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
    fn input_order(&self) -> usize {
        match self {
            Self::Connect(num) => *num,
            Self::Meta(_) => usize::MAX,
        }
    }
}

pub struct ValueGraph {
    graph: StableGraph<ValueNode, ValueEdge>,
    defs: BTreeMap<SlotNum, NodeIndex>,
    phis: BTreeMap<SlotNum, SlotNum>,
    output: NodeIndex,
    source: Vec<Instruction>,
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
            let spacing = 24_usize.saturating_sub(Color::uncolored(&node_string).len());
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

        if !self.phis.is_empty() {
            let mut line = format!("    ");
            for (dest, source) in &self.phis {
                line += &format!(
                    "  {}",
                    Opcode::MoveLocal(*dest, *source).pretty_print(Color::MINT)
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
        //   The instructions *must* be in SSA

        let mut graph = StableGraph::new();
        let mut locals = HashMap::new();
        let mut defs = BTreeMap::new();
        let mut phis = BTreeMap::new();

        let mut globals = graph.add_node(ValueNode::Meta("globals"));
        let mut global_readers = BTreeSet::new();

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
                    Effect::ReadStack(depth) => {
                        touch!(depth);
                        let pusher = stack[stack.len() - depth];
                        graph.add_edge(node, pusher, ValueEdge::Connect(input_count));
                        input_count += 1;
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
                        let local = match locals.get(&slot) {
                            Some(local) => *local,
                            None => {
                                let local = graph.add_node(ValueNode::Local(slot));
                                locals.insert(slot, local);
                                local
                            }
                        };
                        graph.add_edge(local, node, ValueEdge::Meta("set"));
                    }
                    Effect::ReadGlobal => {
                        graph.add_edge(node, globals, ValueEdge::Meta("view"));
                        global_readers.insert(node);
                    }
                    Effect::WriteGlobal => {
                        for reader in global_readers {
                            graph.add_edge(node, reader, ValueEdge::Meta("order"));
                        }
                        global_readers = BTreeSet::new();
                        globals = graph.add_node(ValueNode::Meta("globals"));
                        graph.add_edge(globals, node, ValueEdge::Meta("write"));
                    }
                    Effect::PhiLocal(dest, source) => {
                        phis.insert(dest, source);
                    }
                    Effect::OpenFrame(_) => {}
                    Effect::PushAux
                    | Effect::PopAux
                    | Effect::ReadAux
                    | Effect::MoveToStack
                    | Effect::MoveToAux
                    | Effect::Unsure => return None,
                }
            }
        }

        let output = graph.add_node(ValueNode::Meta("output"));
        let mut nouts = 0;
        while let Some(item) = stack.pop_back() {
            graph.add_edge(output, item, ValueEdge::Connect(nouts));
            nouts += 1;
        }

        /// Prunes nodes whose values are never consumed.
        fn prune_graph(graph: &mut StableGraph<ValueNode, ValueEdge>) {
            loop {
                let node_count = graph.node_count();
                graph.retain_nodes(|this, node| {
                    !this[node].prunable()
                        || this.neighbors_directed(node, Direction::Incoming).count() > 0
                });
                if graph.node_count() == node_count {
                    break;
                }
            }
        }

        prune_graph(&mut graph);

        Some(ValueGraph {
            graph,
            defs,
            phis,
            output,
            source: code.to_vec(),
        })
    }

    pub fn codegen(&self) -> Vec<Instruction> {
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

        macro_rules! conn_count {
            ($node:expr) => {
                graph
                    .edges_directed($node, Direction::Incoming)
                    .filter(|edge| matches!(edge.weight(), ValueEdge::Connect(_)))
                    .count()
            };
        }

        let mut arg_code = vec![];

        // Pop unused arguments. We can't just ignore them, since they were created elsewhere.
        for node in graph.node_indices() {
            if let ValueNode::Arg(num) = &graph[node] {
                if conn_count!(node) != 0 {
                    stack.push(node);
                } else {
                    arg_code.push(opcode!(@Pop(stack.len())));
                }
            }
        }

        // determine the number of times each node is used by another
        let mut conn_counts = HashMap::new();
        for node in graph.node_indices() {
            conn_counts.insert(node, conn_count!(node));
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

            let (transformation, new_stack) = reorder_stack(&stack, needs, kills);
            *stack = new_stack;
            code.extend(transformation);

            match &graph[node] {
                ValueNode::Opcode(opcode) => {
                    code.push(Instruction::from_opcode(*opcode, DebugInfo::default()));
                    stack.push(node);
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

        let mut best = self.source.clone();
        let mut entropy: SmallRng = SeedableRng::seed_from_u64(0);

        for _ in 0..4 {
            // attempt to codegen a better set of instructions than the best found so far.

            let mut stack = stack.clone();
            let mut alt = arg_code.clone();
            alt.extend(descend(
                output,
                &graph,
                &mut stack,
                &mut conn_counts.clone(),
                &mut entropy,
                &mut HashSet::new(),
            ));

            if alt.len() < best.len() {
                best = alt;
            }
        }

        best
    }
}

fn reorder_stack(
    stack: &Vec<NodeIndex>,
    needs: Vec<NodeIndex>,
    kills: HashSet<NodeIndex>,
) -> (Vec<Instruction>, Vec<NodeIndex>) {
    let mut stack = stack.clone();

    let mut used: HashSet<_> = needs.clone().into_iter().collect();
    let mut stack: Vec<_> = stack
        .into_iter()
        .map(|item| (item, 0, used.contains(&item)))
        .collect();

    let mut need_counts: HashMap<_, usize> = HashMap::new();
    let needs: Vec<_> = needs
        .into_iter()
        .map(|need| {
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
    let mut highest_copy: HashMap<_, _> = need_counts.iter().map(|(n, _)| (*n, 0)).collect();
    let mut highest_need: HashMap<_, _> = need_counts.iter().map(|(n, c)| (*n, c - 1)).collect();

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
                let upper_pos = needs.into_iter().position(|n| *n == *upper).unwrap();
                let lower_pos = needs.into_iter().position(|n| *n == *lower).unwrap();
                upper_pos < lower_pos
            }
            (Some(None), Some(Some(_))) => true,
            _ => (!upper.2 || upper_pos.is_none()) && lower.2,
        }
    }

    fn print_stack(title: &str, stack: &Vec<(NodeIndex, usize, bool)>) {
        print!("{}", title);
        for item in stack {
            match item.2 {
                true => print!(" {}{}{}", item.0.index(), Color::grey("-"), item.1),
                false => print!("{}", Color::grey(" ???")),
            }
        }
        println!();
    }
    print_stack("needs", &needs);
    print_stack("start", &stack);

    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), DebugInfo::default())
        };
    }

    let mut code = vec![];

    macro_rules! window {
        (false) => {
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
        };
        (true) => {
            window!(false);

            'restart: loop {
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
                            continue 'restart;
                        }
                    }
                }
                break;
            }
        };
    }

    let mut aux = vec![];

    loop {
        while stack.len() > 0 {
            window!(false);
            code.push(opcode!(AuxPush));
            aux.push(stack.pop().unwrap());
            print_stack("apush", &stack);
        }

        while aux.len() > 0 {
            code.push(opcode!(AuxPop));
            stack.push(aux.pop().unwrap());
            print_stack("aupop", &stack);
            window!(true);
        }

        let top_of_stack: Vec<_> = stack.iter().rev().take(needs.len()).rev().collect();
        let what_we_need: Vec<_> = needs.iter().collect();
        if top_of_stack != what_we_need {
            continue;
        }

        print_stack("final", &stack);
        break;
    }

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

    let (_, mut new_stack) = reorder_stack(&stack, needs.clone(), kills);
    println!("{:?}", stack);
    println!("{:?}", new_stack);

    new_stack = new_stack
        .into_iter()
        .rev()
        .take(needs.len())
        .rev()
        .collect();
    assert_eq!(new_stack, needs);
}
