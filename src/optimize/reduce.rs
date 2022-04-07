/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, SlotNum};
use crate::console;
use crate::console::Color;
use crate::link::fold_tuples;
use crate::mavm::{AVMOpcode, Instruction, Label, LabelId, Opcode, Value};
use crate::opcode;
use crate::optimize::compute::Computer;
use crate::optimize::effects::{Effect, Effects};
use crate::optimize::reorder::reorder_stack;
use crate::run::{Machine, MachineState};
use petgraph::algo;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::stable_graph::StableGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use rand::prelude::*;
use rand::rngs::SmallRng;
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};

/// Represents a value or value state in a `ValueGraph`
#[derive(Clone)]
pub enum ValueNode {
    Opcode(Opcode),        // An opcode that, when applied to its inputs, produces a value
    Value(Value),          // A compile-time known AVM value
    Arg(usize),            // A value created elsewhere that this valuegraph references
    Local(SlotNum),        // A local variable on the Aux stack
    StackedLocal(SlotNum), // A local variable that's been moved to the data stack
    StackLocal(SlotNum),   // Operation for placing a local on the data stack
    Drop,                  // Mechanism for popping a value
    Alias,                 // Mechanism for causing a node to consume another
    Meta(&'static str),    // A node that's just used to enforce some kind of ordering
}

impl ValueNode {
    /// A node is prunable if, upon discovery that the output is unused, it can be removed.
    fn prunable(&self) -> bool {
        // Value         T => an AVM value that's never read is useless
        // Opcode        T => stateful opcodes will always be read by some Meta or Local node
        // Arg           F => we must _pop_, rather than _prune_, unused args since we don't create them
        // Local         F => you can't elide a local sets/gets variable without global knowledge
        // Stack'd Local F => a future block might use this so we must _pop_ rather than _prune_
        // Stack Local   F => a future block might use this so we must _pop_ rather than _prune_
        // Drop          F => this is used to pop things that aren't prunable
        // Alias         F => if the value this aliases can't be pruned, than neither can this one be

        match self {
            Self::Value(_) | Self::Opcode(_) => true,
            _ => false,
        }
    }
}

/// Represents the relationship between two `ValueNode`s in a `ValueGraph`.
#[derive(Clone, Copy)]
pub enum ValueEdge {
    Connect(usize),     // an edge that causes a node to consume the output of another
    Meta(&'static str), // an edge that induces an ordering
    Order,              // an edge that induces an ordering without implying necessity
}

impl ValueEdge {
    /// Determines the consumption order of a `ValueNode` during codegen
    fn input_order(&self) -> Reverse<usize> {
        match self {
            Self::Connect(num) => Reverse(1 + *num),
            Self::Order | Self::Meta(_) => Reverse(usize::MAX),
        }
    }
}

/// Retrieves the `NodeIndex`s of a graph without borrowing
fn nodes(graph: &StableGraph<ValueNode, ValueEdge>) -> Vec<NodeIndex> {
    graph.node_indices().collect()
}

/// Retrieves the undirected neighbors of a node
fn neighbors(graph: &StableGraph<ValueNode, ValueEdge>, node: NodeIndex) -> Vec<NodeIndex> {
    graph.neighbors_undirected(node).collect()
}

/// Retrieves the directed neighbors of a node
fn directed_neighbors(
    graph: &StableGraph<ValueNode, ValueEdge>,
    node: NodeIndex,
) -> Vec<NodeIndex> {
    graph
        .neighbors_directed(node, Direction::Outgoing)
        .collect()
}

/// Retrieves the number of times other nodes consume this `ValueNode`
fn conn_count(graph: &StableGraph<ValueNode, ValueEdge>, node: NodeIndex) -> usize {
    graph
        .edges_directed(node, Direction::Incoming)
        .filter(|edge| matches!(edge.weight(), ValueEdge::Connect(_)))
        .count()
}

/// Retrieves the number of times other nodes statefully depend on this `ValueNode`
fn meta_count(graph: &StableGraph<ValueNode, ValueEdge>, node: NodeIndex) -> usize {
    graph
        .edges_directed(node, Direction::Incoming)
        .filter(|edge| matches!(edge.weight(), ValueEdge::Meta(_)))
        .count()
}

/// Retrieves the inputs and outputs of a `ValueNode` and the associated `ValueEdge`s
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

type DataGraph = StableGraph<ValueNode, ValueEdge>;

/// Represents how values in a `BasicBlock` relate to one another
/// The header & footer save metadata not directly encoded in the value relationships themselves
#[derive(Default)]
pub struct ValueGraph {
    /// How the values relate to one another
    graph: DataGraph,
    /// Metadata instructions like the entry point & frame size
    header: Vec<Instruction>,
    /// Metadata instructions like φ's and δ's
    footer: Vec<Instruction>,
    /// The output node, who's construction means executing the full basic block
    output: NodeIndex,
    #[allow(dead_code)]
    /// The func that owns the basic block used to make this graph
    func_id: LabelId,
    /// The funcs this graph provably uses
    pub funcs: HashSet<LabelId>,
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
                    ValueNode::StackLocal(slot) => format!("Stack {}", Color::mint(slot)),
                    ValueNode::StackedLocal(slot) => format!("Stack'd {}", Color::mint(slot)),
                    ValueNode::Drop => Color::blue("Drop"),
                    ValueNode::Alias => Color::blue("Alias"),
                    ValueNode::Meta(name) => name.to_string(),
                }
            );
            let spacing = 22_usize.saturating_sub(console::console_width(&node_string));
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
                        ValueEdge::Order => Color::blue(input.index()),
                    }
                );
            }
            output.push(line);
        }
        output
    }

    /// Create a value graph, if possible, for set of instructions without any prior
    /// knowledge of what's on the stack.
    pub fn new(
        code: &[Instruction],
        phis: &HashMap<SlotNum, SlotNum>,
        func_id: LabelId,
        computer: &Computer,
    ) -> Option<Self> {
        Self::with_stack(
            code,
            BTreeSet::new(),
            &BTreeSet::new(),
            &BTreeSet::new(),
            phis,
            func_id,
            computer,
        )
    }

    /// Create a new `ValueGraph` from a set of instructions without control flow
    /// - stacked is the set of stack'd locals added before this block
    /// - unstack is the set of stack'd locals that this block must pop
    /// - tostack is the set of locals that this block will stack
    /// - phis denotes which locals phi which others
    /// - computer is the mechanism for simulating calls to pure funcs
    pub fn with_stack(
        mut block: &[Instruction],
        mut stacked: BTreeSet<SlotNum>,
        unstack: &BTreeSet<SlotNum>,
        tostack: &BTreeSet<SlotNum>,
        phis: &HashMap<SlotNum, SlotNum>,
        func_id: LabelId,
        computer: &Computer,
    ) -> Option<Self> {
        // Algorithm
        //   Strip away metadata so that the code solely reflects the block's actions
        //   Simulate the block's execution to determine the connectivity of values
        //   Where possible pre-compute instructions whose inputs are statically known
        //   Enforce phi and other orderings
        //   Track output values for the exit
        //   Prune provably useless values from the graph
        //
        // Assumptions
        //   The block must be in SSA, which ensures certain properties:
        //     - There's no more than 1 jump, which can only be at the end of a block
        //     - There's no more than 1 label, which can only be at the start of a block
        //     - Phis & Drops may be moved to the end
        //     - The order of phis don't matter
        //   Locals are accessed at a common auxstack height
        //     - There can't be Aux -pushes & -pops that misalign xgets and xsets
        //   Pure funcs can be arbitrarily reordered
        //     - Those that aren't view and write must be marked sensitive

        // Separate the metadata instructions from the top.
        let mut header = vec![];
        for curr in block {
            match &curr.opcode {
                Opcode::Label(_) | Opcode::MakeFrame(..) => {
                    header.push(curr.clone());
                }
                _ => break,
            }
        }
        block = &block[header.len()..];

        // Save Phis and Drops for the footer. Since they have no Opcode `Effects`,
        // they can stay in the slice.
        let mut footer = vec![];
        for curr in block {
            if let Opcode::MoveLocal(..) | Opcode::DropLocal(..) = &curr.opcode {
                footer.push(curr.clone());
            }
        }
        if let Some(last) = block.last() {
            match &last.opcode {
                Opcode::Return
                | Opcode::JumpTo(..)
                | Opcode::CjumpTo(..)
                | Opcode::AVMOpcode(AVMOpcode::Jump | AVMOpcode::Cjump) => {
                    footer.push(last.clone());
                    block = &block[..(block.len() - 1)];
                }
                _ => {}
            }
        }

        let mut graph = StableGraph::new();
        let mut locals = BTreeMap::new();
        let mut local_reads = HashMap::new();

        let mut globals = graph.add_node(ValueNode::Meta("globals"));
        let mut global_readers = BTreeSet::new();

        let mut stack: VecDeque<NodeIndex> = VecDeque::new(); // values on the datastack
        let mut nargs = 0;

        for &slot in &stacked {
            // These are values already on the stack, so we can make nodes for them.
            // We do this now for simplicity instead of being lazy as in for normal locals.
            let local = graph.add_node(ValueNode::StackedLocal(slot));
            locals.insert(slot, local);
        }

        /// Ensures the stack has enough args.
        /// For example, stack(2) ensures at least 2 args are on the stack, and if not prepends them.
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

        use Effect::*;
        for curr in block {
            // Since we track the stack, each instruction informs our relationships between values.
            // For simplicity, we split up an instruction into its immediate & opcode, adding nodes for each.
            // Each opcode has a set of effects, which when stepped through may reveal additional info,
            // including things like the need to create Local nodes, track globals, etc.

            if let Some(value) = &curr.immediate {
                let node = graph.add_node(ValueNode::Value(value.clone()));
                stack.push_back(node);
            }

            if let Opcode::AVMOpcode(AVMOpcode::EcMul | AVMOpcode::EcAdd) = curr.opcode {
                // Let's not worry about opcodes that produce 2+ values. These are rare and
                // complicate things as you can see in the #compiler-optimization-passes branch.
                return None;
            }

            let mut node = graph.add_node(ValueNode::Opcode(curr.opcode));
            let mut input_count = 0;

            for effect in curr.opcode.effects() {
                match effect {
                    PushStack => {
                        stack.push_back(node);
                    }
                    PopStack => {
                        touch!(1);
                        stack.pop_back();
                    }
                    ReadStack => {
                        touch!(1);
                        let pusher = stack.iter().last().unwrap();
                        graph.add_edge(node, *pusher, ValueEdge::Connect(input_count));
                        input_count += 1;
                    }
                    DupStack(depth) => {
                        touch!(depth);
                        let pusher = stack[stack.len() - depth];
                        stack.push_back(pusher);
                    }
                    SwapStack(depth) => {
                        touch!(depth + 1);
                        let swapped = stack.swap_remove_back(stack.len() - depth - 1).unwrap();
                        stack.push_back(swapped);
                    }
                    ReadLocal(slot) => {
                        let local = match locals.get(&slot) {
                            Some(local) => *local,
                            None => {
                                let local = graph.add_node(ValueNode::Local(slot));
                                locals.insert(slot, local);
                                local
                            }
                        };

                        local_reads.entry(slot).or_insert(vec![]).push(node);

                        if stacked.contains(&slot) {
                            // The local has been stack'd, so rather than creating a meta edge,
                            // which just ensures ordering, we instead cause future Push effects
                            // to place a copy of the local _itself_ on the stack.
                            graph.remove_node(node);
                            node = local;
                        } else {
                            graph.add_edge(node, local, ValueEdge::Meta("read"));
                        }
                    }
                    WriteLocal(slot) => {
                        let local = match locals.get(&slot) {
                            Some(local) => *local,
                            None => {
                                let local = graph.add_node(ValueNode::Local(slot));
                                locals.insert(slot, local);
                                local
                            }
                        };

                        assert!(!stacked.contains(&slot), "Not SSA: found write-after-write");

                        if tostack.contains(&slot) {
                            graph.add_edge(local, node, ValueEdge::Connect(0));
                            graph[node] = ValueNode::Alias;
                            graph[local] = ValueNode::StackLocal(slot);
                            stacked.insert(slot);
                        } else {
                            graph.add_edge(local, node, ValueEdge::Meta("write"));
                        }
                    }
                    ReadGlobal => {
                        graph.add_edge(node, globals, ValueEdge::Meta("view"));
                        global_readers.insert(node);
                    }
                    MaybeThrow => {
                        graph.add_edge(node, globals, ValueEdge::Meta("throw"));
                        global_readers.insert(node);
                    }
                    WriteGlobal => {
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
                    WritePC => {
                        unreachable!("Not SSA: jumps should only happen at the end")
                    }
                    PushAux | PopAux | ReadAux | MoveToStack | MoveToAux | Unsure => return None,
                }
            }
        }

        // Enforce phi-orderings. The Local nodes so far only gauruntee that SetLocal()
        // happens before GetLocal() during codegen. We need to further ensure that for
        // each φ(dest, source) each SetLocal(source) happens before all GetLocal(dest)
        for (source, dest) in phis {
            let source = match locals.get(source) {
                Some(node) => *node,
                _ => continue,
            };
            let dest = match locals.get(dest) {
                Some(node) => *node,
                _ => continue,
            };

            for source_access in neighbors(&graph, source) {
                for dest_access in neighbors(&graph, dest) {
                    graph.add_edge(source_access, dest_access, ValueEdge::Meta("φ"));
                }
            }

            graph.add_edge(source, dest, ValueEdge::Meta("φ"));
        }

        // represents the output of the graph just before exiting
        let output = graph.add_node(ValueNode::Meta("output"));

        // locals that are unstack'd by this block need to be dropped
        let mut drops = HashMap::new();
        for slot in unstack.iter() {
            stacked.remove(slot);

            let drop = graph.add_node(ValueNode::Drop);
            let local = *locals.get(slot).unwrap();

            for node in neighbors(&graph, local) {
                // the drop must occur after all uses of the local
                graph.add_edge(drop, node, ValueEdge::Order);
            }

            graph.add_edge(drop, local, ValueEdge::Connect(0));
            graph.add_edge(output, drop, ValueEdge::Meta("drop"));
            drops.insert(slot, drop);
        }

        /// Give up and print the value graph
        macro_rules! bail {
            ($text:expr) => {
                println!("{}", Color::red("Bailing on graph"));
                let ssa = block
                    .into_iter()
                    .map(|x| x.pretty_print(Color::RED))
                    .collect();
                let mut incomplete = ValueGraph::default();
                incomplete.graph = graph.clone();
                let lines = incomplete.print_lines();
                console::print_columns(vec![ssa, lines], vec!["SSA", "values"]);
                panic!("{}", $text);
            };
        }

        let mut footer_inputs = vec![];
        if let Some(last) = footer.last() {
            let reads = last.effects().iter().filter(|e| **e == ReadStack).count();
            let makes = last.effects().iter().filter(|e| **e == PushStack).count();
            touch!(reads - makes);
            for _ in 0..(reads - makes) {
                let input = match stack.pop_back() {
                    Some(input) => input,
                    None => {
                        let msg =
                            format!("{} needs value not on stack", last.pretty_print(Color::RED));
                        bail!(msg);
                    }
                };
                footer_inputs.push(input);
            }
            footer_inputs.reverse();
        }

        // locals still stack'd need to be placed in-order at the top of the data stack after exit
        for slot in &stacked {
            let local = locals.get(slot).unwrap();
            stack.push_back(*local);
        }

        // the footer's needs (like the jump dest) are placed on top & will be consumed during exit
        for input in footer_inputs {
            stack.push_back(input);
        }

        // items left on the stack need to be passed on
        for (index, item) in stack.into_iter().rev().enumerate() {
            graph.add_edge(output, item, ValueEdge::Connect(index));
        }

        // state updates are part of the output
        for (_, local) in &locals {
            graph.add_edge(output, *local, ValueEdge::Meta("locals"));
        }
        if neighbors(&graph, globals).len() > 0 {
            graph.add_edge(output, globals, ValueEdge::Meta("globals"));
        }

        /// Statically analyze the graph for values we can compute at compile time.
        fn fold_constants(graph: &mut DataGraph) {
            // For all nodes that aren't stateful, try to pre-compute their values

            'next: for node in nodes(&graph) {
                if let ValueNode::Opcode(opcode) = &graph[node] {
                    // For AVM opcodes that only touch the stack, we can try to emulate them by
                    // building a machine whose only purpose is to execute the opcode and then halt.

                    if let Opcode::FuncCall(..) | Opcode::AVMOpcode(AVMOpcode::Hash) = opcode {
                        // Hash is unsafe since the emulator isn't to spec.
                        // Func calls require more complex infrusture to safely pre-compute.
                        continue;
                    }

                    let mut pushes = 0;
                    let mut stateful = false;
                    for effect in opcode.effects() {
                        match effect {
                            PushStack => pushes += 1,
                            PopStack => {}
                            ReadStack => {}
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

        fn precompute_funcs(graph: &mut DataGraph, computer: &Computer) {
            // Funcs that are pure are, by definition, dependent only on their args.
            // Hence, knowing the args implies we can statically evaluate the result.
            // This routine evaluates such funcs, eliding calls with their results.

            'next: for node in nodes(&graph) {
                if let ValueNode::Opcode(Opcode::FuncCall(prop)) = &graph[node] {
                    if !prop.is_pure() || prop.nouts != 1 {
                        // An analysis of global state would be necessary to handle impure funcs.
                        // Pure funcs with 0 outputs are already elided during constant folding.
                        continue;
                    }

                    let mut args = vec![];

                    let (deps, _, dep_edges, _) = node_data(&graph, node);
                    for dep in deps {
                        match &graph[dep] {
                            ValueNode::Value(value) => args.push(value.clone()),
                            _ => continue 'next,
                        }
                    }

                    let label = match args.pop() {
                        Some(Value::Label(label)) => label,
                        Some(_) => continue,
                        None => panic!("FuncCalls should always have at least the jump dest"),
                    };

                    let result = match computer.calc(label, args) {
                        Some(result) => result,
                        None => continue,
                    };

                    graph[node] = ValueNode::Value(result);

                    for edge in dep_edges {
                        graph.remove_edge(edge);
                    }
                }
            }
        }

        /// Prunes nodes whose values are never consumed.
        fn prune_graph(graph: &mut DataGraph, output: NodeIndex) {
            loop {
                let node_count = graph.node_count();
                graph.retain_nodes(|this, node| {
                    let need_count = conn_count(&this, node) + meta_count(&this, node);
                    !this[node].prunable() || need_count > 0 || node == output
                });
                if graph.node_count() == node_count {
                    break;
                }
            }

            for node in nodes(&graph) {
                let effects = match &graph[node] {
                    ValueNode::Opcode(opcode) => opcode.effects(),
                    _ => continue,
                };

                let pushes = effects.into_iter().any(|x| x == Effect::PushStack);

                if pushes && conn_count(&graph, node) == 0 {
                    // A node still present that creates an unconsumed value needs to have it popped

                    let drop = graph.add_node(ValueNode::Drop);
                    graph.add_edge(drop, node, ValueEdge::Connect(0));
                    graph.add_edge(output, drop, ValueEdge::Meta("prune"));
                }
            }
        }

        fold_constants(&mut graph);

        let mut funcs = HashSet::new();
        for node in nodes(&graph) {
            funcs.insert(match &graph[node] {
                ValueNode::Value(Value::Label(Label::Func(id))) => *id,
                ValueNode::Value(Value::Label(Label::Closure(id))) => *id,
                _ => continue,
            });
        }

        precompute_funcs(&mut graph, computer);
        fold_constants(&mut graph);
        prune_graph(&mut graph, output);

        for slot in unstack.iter() {
            let consumed = local_reads
                .get(slot)
                .into_iter()
                .flatten()
                .any(|node| graph.contains_node(*node));
            if consumed {
                // Since there exists a node that consumes the stack'd value,
                // we don't need an explicit drop. The last consumer will, in
                // effect, unstack it for us when codegen sees no future use.
                graph.remove_node(*drops.get(slot).unwrap());
            }
        }

        prune_graph(&mut graph, output);

        if algo::is_cyclic_directed(&graph) {
            // By construction, a value graph is a DAG.
            // Each edge represents a dependency: compute this to compute that.
            // For there to be a cycle, computing a value would require computing
            // another that in turn requires computing the first. This is impossible
            // for a basic block since it doesn't have control flow.
            bail!("Value graph is not a DAG!");
        }

        let values = ValueGraph {
            graph,
            header,
            footer,
            output,
            func_id,
            funcs,
        };

        Some(values)
    }

    /// Flatten a `ValueGraph` into an efficient block of equivalent instructions
    pub fn codegen(&self, optimization_level: usize) -> (Vec<Instruction>, usize) {
        let mut stack = vec![];
        let graph = &self.graph;

        // Stack'd locals are always on top of the data stack when entering a basic block.
        // This is something codegen must guarantee later too.
        for node in nodes(&graph) {
            if let ValueNode::StackedLocal(_) = &graph[node] {
                stack.push(node);
            }
        }

        let mut header = self.header.clone();

        // Pop unused arguments. We can't just ignore them, since they were created elsewhere.
        for node in nodes(&graph) {
            if let ValueNode::Arg(_) = &graph[node] {
                if conn_count(&graph, node) != 0 {
                    stack.insert(0, node);
                } else {
                    header.push(opcode!(@Pop(stack.len())));
                }
            }
        }

        // Determine the number of times each node is used by another.
        let mut conn_counts = HashMap::new();
        for node in nodes(&graph) {
            conn_counts.insert(node, conn_count(&graph, node));
        }

        /// Determine which nodes should employ randomization
        fn randomize_check(
            graph: &DataGraph,
            node: NodeIndex,
            seen: &mut HashSet<NodeIndex>,
        ) -> HashSet<NodeIndex> {
            let mut results = HashSet::new();
            let mut include = false;

            for child in directed_neighbors(graph, node) {
                if !seen.contains(&child) {
                    results.extend(randomize_check(graph, child, seen));
                }
                include |= conn_count(graph, child) > 1; // child's presence is stack-stateful
                include |= meta_count(graph, child) != 0; // child's presence is order-stateful
                include |= results.contains(&child); // a descendant's presence is stateful
            }

            if include {
                results.insert(node);
            }
            seen.insert(node);
            results
        }
        let randomize = randomize_check(&self.graph, self.output, &mut HashSet::new());

        let mut entropy: SmallRng = SeedableRng::seed_from_u64(0);

        /// Walk the `ValueGraph`, building the instructions in a bottom-up manner.
        fn descend(
            node: NodeIndex,
            graph: &DataGraph,
            stack: &mut Vec<NodeIndex>,
            conn_counts: &mut HashMap<NodeIndex, usize>,
            randomize: &HashSet<NodeIndex>,
            entropy: &mut SmallRng,
            code: &mut Vec<Instruction>,
            done: &mut HashSet<NodeIndex>,
        ) {
            let mut deps: Vec<_> = graph.edges_directed(node, Direction::Outgoing).collect();
            if randomize.contains(&node) {
                deps.shuffle(entropy);
            }

            // ensure everything this node depends on has been codegened
            for edge in &deps {
                let input = edge.target();
                if !done.contains(&input) {
                    descend(
                        input,
                        graph,
                        stack,
                        conn_counts,
                        randomize,
                        entropy,
                        code,
                        done,
                    );
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
            match reorder_stack(&stack, needs, kills, false) {
                Ok((trans, new_stack)) => {
                    *stack = new_stack;
                    code.extend(trans);
                }
                Err(msg) => {
                    let mut fake = ValueGraph::default();
                    fake.graph = (*graph).clone();
                    let lines = fake.print_lines();
                    let sofar = code
                        .into_iter()
                        .map(|x| x.pretty_print(Color::PINK))
                        .collect();
                    console::print_columns(vec![lines, sofar], vec!["Values", "Crash"]);
                    println!("Could not reorder stack: {}", Color::red(msg));
                    panic!();
                }
            }

            for _ in edges {
                stack.pop();
            }

            match &graph[node] {
                ValueNode::Opcode(opcode) => {
                    code.push(Instruction::from(*opcode));
                    let pushes = opcode
                        .effects()
                        .into_iter()
                        .filter(|e| *e == Effect::PushStack)
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
                ValueNode::StackLocal(_) => {
                    stack.push(node);
                }
                ValueNode::Alias => {
                    stack.push(node);
                }
                ValueNode::Drop => {
                    code.push(Instruction::from(Opcode::AVMOpcode(AVMOpcode::Pop)));
                }
                _ => {}
            }

            done.insert(node);
        }

        let mut best = vec![];
        let mut best_cost = usize::MAX;
        let mut rounds = 1;

        if !randomize.is_empty() {
            rounds += optimization_level;
        }

        for _ in 0..rounds {
            // Attempt to codegen a better set of instructions than the best found so far.
            // There's a single-pass, dynamic programming variant of this but we'd need to
            // make reorder_stack() pure and after around -O4096 the optimal solution is found.

            let mut stack = stack.clone();
            let mut alt = header.clone();
            descend(
                self.output,
                &graph,
                &mut stack,
                &mut conn_counts.clone(),
                &randomize,
                &mut entropy,
                &mut alt,
                &mut HashSet::new(),
            );
            let alt_cost = alt.iter().map(|x| x.opcode.base_cost()).sum();

            if alt_cost < best_cost {
                best_cost = alt_cost;
                best = alt;
            }
        }

        // write the exit code for this block
        best.extend(self.footer.clone());
        (best, best_cost)
    }
}
