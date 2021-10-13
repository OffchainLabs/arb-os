/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{FrameSize, SlotNum};
use crate::console;
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use petgraph::algo;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::stable_graph::StableGraph;
use petgraph::visit::{Dfs, IntoNodeReferences};
use petgraph::{Direction, Undirected};
use rand::prelude::*;
use rand::rngs::SmallRng;
use reduce::ValueGraph;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::hash::Hasher;

mod effects;
mod peephole;
mod reduce;
mod reorder;

/// Represents a block of instructions that has no control flow
pub enum BasicBlock {
    Code(Vec<Instruction>),
    Meta(&'static str),
}

impl BasicBlock {
    fn get_code(&self) -> &[Instruction] {
        match self {
            BasicBlock::Code(code) => &code,
            BasicBlock::Meta(_) => &[],
        }
    }
    fn get_code_mut(&mut self) -> &mut [Instruction] {
        match self {
            BasicBlock::Code(ref mut code) => code,
            BasicBlock::Meta(_) => &mut [],
        }
    }
}

/// Represents control flow between two blocks.
pub enum BasicEdge {
    Forward,
    Jump,
}

/// Represents control flow about basic blocks
pub struct BasicGraph {
    /// Basic blocks and the edges that connect them
    graph: StableGraph<BasicBlock, BasicEdge>,
    /// The output node of the graph
    entry: NodeIndex,
    /// The output node of the graph
    output: NodeIndex,
    /// Whether this graph contains a cycle
    cyclic: bool,
    /// Whether this graph contains code that's been marked for printing
    should_print: bool,
}

impl BasicGraph {
    /// Creates a `BasicGraph` from a list of `Instruction`s.
    pub fn new(code: Vec<Instruction>) -> Self {
        // Algorithm:
        //   Create a control flow graph by walking the instructions in a few passes.
        //     Split on labels & jumps to form basic-blocks of instructions.
        //     Add forward edges between blocks where execution flow could proceed to the next.
        //     Add edges for jump targets. If the target is unknown, treat it as a return.
        //
        // Assumptions:
        //   We assume the first instruction is the entry point.
        //   We assume all Return opcodes jump to the same location.
        //   We assume all label-less jumps are not reentrant

        let mut graph: StableGraph<BasicBlock, BasicEdge> = StableGraph::new();
        let entry = graph.add_node(BasicBlock::Meta("Entry"));

        let should_print = code.iter().any(|x| x.debug_info.attributes.codegen_print);

        // create basic blocks by splitting on jumps and labels
        let mut block_data = vec![];
        for curr in code {
            match curr.opcode {
                Opcode::Label(_) => {
                    if block_data.len() > 0 {
                        graph.add_node(BasicBlock::Code(block_data));
                    }
                    block_data = vec![curr];
                }
                Opcode::Return
                | Opcode::JumpTo(_)
                | Opcode::CjumpTo(_)
                | Opcode::AVMOpcode(AVMOpcode::Cjump | AVMOpcode::Jump) => {
                    block_data.push(curr);
                    graph.add_node(BasicBlock::Code(block_data));
                    block_data = vec![];
                }
                _ => {
                    block_data.push(curr);
                }
            }
        }
        let output = graph.add_node(BasicBlock::Meta("Output"));

        // associate labels to blocks
        let mut label_to_block = HashMap::new();
        for (node, block) in graph.node_references() {
            if let Some(Opcode::Label(label)) = block.get_code().get(0).map(|x| x.opcode) {
                label_to_block.insert(label, node);
            }
        }

        // make forward edges
        let nodes: Vec<_> = graph.node_indices().collect();
        let mut last_node = None;

        for &node in &nodes {
            if let Some(last) = last_node {
                graph.add_edge(last, node, BasicEdge::Forward);
            }
            match &graph[node].get_code().last().map(|insn| insn.opcode) {
                Some(Opcode::Return | Opcode::JumpTo(_) | Opcode::AVMOpcode(AVMOpcode::Jump)) => {
                    last_node = None;
                }
                _ => {
                    last_node = Some(node);
                }
            }
        }

        // add jump edges
        for node in nodes {
            let last = match &graph[node] {
                BasicBlock::Code(code) => match code.iter().last().cloned() {
                    Some(insn) => insn,
                    _ => continue,
                },
                _ => continue,
            };

            match &last.opcode {
                Opcode::Return => {
                    graph.add_edge(node, output, BasicEdge::Jump);
                }
                Opcode::JumpTo(label) | Opcode::CjumpTo(label) => {
                    let dest = match label_to_block.get(label) {
                        Some(dest) => *dest,
                        _ => output,
                    };
                    graph.add_edge(node, dest, BasicEdge::Jump);
                }
                Opcode::AVMOpcode(AVMOpcode::Jump | AVMOpcode::Cjump) => {
                    let label = match &last.immediate {
                        Some(Value::Label(label)) => label,
                        _ => {
                            graph.add_edge(node, output, BasicEdge::Jump);
                            continue;
                        }
                    };
                    let dest = match label_to_block.get(label) {
                        Some(dest) => *dest,
                        _ => output,
                    };
                    graph.add_edge(node, dest, BasicEdge::Jump);
                }
                _ => {}
            }
        }

        let cyclic = algo::is_cyclic_directed(&graph);
        BasicGraph {
            graph,
            entry,
            output,
            cyclic,
            should_print,
        }
    }

    /// Flattens a basic graph into an equivalent vector of `Instruction`s
    pub fn flatten(self) -> Vec<Instruction> {
        let mut code = vec![];
        for node in self.graph.node_indices() {
            let block = &self.graph[node];
            match block {
                BasicBlock::Code(block) => code.extend(block.clone()),
                _ => {}
            }
        }
        code
    }

    /// Prints a basic graph with colors
    pub fn _print(&self) {
        let graph = &self.graph;
        let mut block_num = 0;
        let mut insn_num = 0;
        for node in graph.node_indices() {
            println!(
                "{}",
                match &graph[node] {
                    BasicBlock::Code(_) => Color::blue(format!("Block {}", block_num)),
                    BasicBlock::Meta(text) => Color::blue(format!("{} Block", text)),
                }
            );

            block_num += 1;
            let code = match &graph[node] {
                BasicBlock::Code(code) => code,
                _ => continue,
            };
            for curr in code {
                if curr.debug_info.attributes.codegen_print {
                    println!(
                        "{}  {}",
                        Color::grey(format!("{:04}", insn_num)),
                        curr.pretty_print(Color::PINK)
                    );
                }
                insn_num += 1;
            }
        }
        println!(
            "{} {}\n",
            Color::grey("cyclic:"),
            Color::color_if(self.cyclic, Color::MINT, Color::GREY)
        );
    }

    /// Removes local variables that are provably useless.
    pub fn pop_useless_locals(&mut self) {
        let mut locals = HashSet::new();
        let nodes: Vec<_> = self.graph.node_indices().collect();

        for node in &nodes {
            for curr in self.graph[*node].get_code() {
                if let Opcode::SetLocal(slot) | Opcode::ReserveCapture(slot, _) = curr.opcode {
                    locals.insert(slot);
                }
            }
        }
        for node in &nodes {
            for curr in self.graph[*node].get_code() {
                if let Opcode::GetLocal(slot) | Opcode::MoveLocal(_, slot) = curr.opcode {
                    locals.remove(&slot);
                }
            }
        }
        for node in nodes {
            for curr in self.graph[node].get_code_mut() {
                let debug = curr.debug_info;
                match curr.opcode {
                    Opcode::SetLocal(slot) if locals.contains(&slot) => {
                        *curr = Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Pop), debug);
                    }
                    Opcode::ReserveCapture(slot, _) if locals.contains(&slot) => {
                        *curr = Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Noop), debug);
                    }
                    Opcode::MoveLocal(dest, _) if locals.contains(&dest) => {
                        *curr = Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Noop), debug);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Shrinks the frame size to elide unused frame slots.
    pub fn shrink_frame(&mut self) -> FrameSize {
        let mut locals = BTreeMap::new();
        let nodes: Vec<_> = self.graph.node_indices().collect();

        for node in &nodes {
            for curr in self.graph[*node].get_code() {
                match curr.opcode {
                    Opcode::SetLocal(slot)
                    | Opcode::GetLocal(slot)
                    | Opcode::ReserveCapture(slot, _) => {
                        *locals.entry(slot).or_insert(0) += 1;
                    }
                    Opcode::MoveLocal(dest, source) => {
                        *locals.entry(dest).or_insert(0) += 1;
                        *locals.entry(source).or_insert(0) += 1;
                    }
                    _ => {}
                }
            }
        }

        // Frequently used variables should get the lowest slots
        let mut locals: Vec<_> = locals.into_iter().collect();
        locals.sort_by_key(|(_, count)| *count);

        let replace: HashMap<_, _> = locals
            .into_iter()
            .rev()
            .map(|(slot, _count)| slot)
            .enumerate()
            .map(|(new, slot)| (slot, new as u32))
            .collect();

        for node in &nodes {
            for curr in self.graph[*node].get_code_mut() {
                match &mut curr.opcode {
                    Opcode::SetLocal(ref mut slot)
                    | Opcode::GetLocal(ref mut slot)
                    | Opcode::ReserveCapture(ref mut slot, _) => {
                        *slot = *replace.get(slot).unwrap();
                    }
                    Opcode::MoveLocal(ref mut dest, ref mut source) => {
                        *dest = *replace.get(dest).unwrap();
                        *source = *replace.get(source).unwrap();
                    }
                    Opcode::MakeFrame(ref mut space, ..) => {
                        *space = replace.len() as FrameSize;
                    }
                    _ => {}
                }
            }
        }

        replace.len() as FrameSize
    }

    /// Efficiently assign frame slots to minimize the frame size.
    pub fn color(&mut self, frame_size: FrameSize, optimization_level: usize) {
        // Algorithm
        //   Determine which values would overwrite each other if reassigned the same slot.
        //   Reassign values using as few slots as possible given this knowledge
        //
        // Steps
        //   - Create a graph of which values are phi'd together.
        //   - Build a conflict graph whose nodes represent values in the local function frame
        //     and whose edges represent the inability to give each the same slot.
        //   - Contract phi nodes so that each side gets the same slot assignment.
        //   - Color the contracted graph with as few colors as best we can.
        //   - Color the original graph 1-to-1 using the contracted graph.
        //   - Modify each instruction to reflect the new slot assignments.

        let mut conflicts: StableGraph<SlotNum, (), Undirected> = StableGraph::default();
        let mut phi_graph: UnGraph<SlotNum, ()> = UnGraph::default();

        for slot in 0..frame_size {
            // we do this so that slot & index are interchangeable.
            // conflicts is a stable graph to preserve this property when we remove nodes.
            conflicts.add_node(slot);
            phi_graph.add_node(slot);
        }

        for node in self.graph.node_indices() {
            for curr in self.graph[node].get_code() {
                if let Opcode::MoveLocal(dest, source) = &curr.opcode {
                    phi_graph.add_edge(NodeIndex::from(*dest), NodeIndex::from(*source), ());
                }
            }
        }

        // Walking the graph backwards is heuristically faster by order O(n).
        let mut work_list: VecDeque<_> = self.graph.node_indices().rev().collect();
        let mut work_set: HashSet<_> = work_list.clone().into_iter().collect();
        let mut node_needs = HashMap::new();

        while let Some(node) = work_list.pop_front() {
            // Create conflict graph algorithm
            //   Keep updating the liveliness information for each block until all conflicts are found.
            //   At each block, check if the needs of its successors should be propagated backwards.
            //   If so, add conflict information & place the predecessors to the end of the work list.

            let outgoing = self.graph.neighbors_directed(node, Direction::Outgoing);
            let incoming = self.graph.neighbors_directed(node, Direction::Incoming);

            let mut alive: BTreeSet<SlotNum> = BTreeSet::new(); // values that haven't been set yet

            macro_rules! conflict {
                ($slot:expr) => {
                    for slot in &alive {
                        if $slot != *slot {
                            conflicts.update_edge(
                                NodeIndex::from($slot),
                                NodeIndex::from(*slot),
                                (),
                            );
                        }
                    }
                };
            }

            for output in outgoing {
                for &need in node_needs.get(&output).into_iter().flatten() {
                    conflict!(need);
                    alive.insert(need);
                }
            }

            for curr in self.graph[node].get_code().into_iter().rev() {
                match curr.opcode {
                    Opcode::SetLocal(local) | Opcode::ReserveCapture(local, _) => {
                        alive.remove(&local);
                        conflict!(local);
                    }
                    Opcode::GetLocal(local) => {
                        if alive.insert(local) {
                            conflict!(local);
                        }
                    }
                    Opcode::MoveLocal(dest, source) => {
                        conflict!(dest);
                        conflict!(source);
                        alive.remove(&dest);
                        alive.insert(source);
                    }
                    _ => {}
                }
            }

            let mut changed = false;

            if node_needs.get(&node) != Some(&alive) {
                changed = alive.len() != 0;
                node_needs.insert(node, alive);
            }

            // can't be inside if since this node could be its own successor
            work_set.remove(&node);

            if changed {
                for input in incoming {
                    if work_set.insert(input) {
                        work_list.push_back(input);
                    }
                }
            }
        }

        // Graph-contract all phi nodes. We can do this since mini is a scoped language.
        // What this does is force both sides of a phi to be colored the same way.
        for component in algo::kosaraju_scc(&phi_graph) {
            let mut nodes = component.into_iter();
            let slot = nodes.next().unwrap();

            for alias in nodes {
                // Contract the graph such that the neighbors of the alias are now the
                // neighbors of the slot.

                let others: Vec<_> = conflicts.neighbors_undirected(alias).collect();
                for other in others {
                    conflicts.add_edge(slot, other, ());
                }
                conflicts.remove_node(alias);
            }
        }

        // Coloring a graph its chromatic number is NP-complete.
        // We find the following algorithm approximates this well for the kinds of graphs
        // SSA tends to produce. How this works is we successively pick the least-used color
        // we can for each node in the graph. When no color is available, we make a new one.
        // We repeat this process against a random ording some number of times and take the best.
        let mut rng = SmallRng::seed_from_u64(0);
        let mut best_assignments = HashMap::new();
        let mut ncolors = usize::MAX;

        for _ in 0..(24 + 2 * optimization_level) {
            let mut colors: BTreeMap<u32, usize> = BTreeMap::new(); // colors to usage counts
            let mut assignments: HashMap<SlotNum, u32> = HashMap::new(); // slots to colors

            let mut nodes: Vec<_> = conflicts.node_indices().collect();
            nodes.shuffle(&mut rng);

            for node in nodes {
                let mut available = colors.clone();

                for neighbor in conflicts.neighbors_undirected(node) {
                    let slot = neighbor.index() as u32;
                    if let Some(color) = assignments.get(&slot) {
                        available.remove(color);
                    }
                }

                if available.is_empty() {
                    let new = colors.len() as u32;
                    available.insert(new, 0);
                    colors.insert(new, 0);
                }

                let best = *available.iter().min_by_key(|(_, uses)| *uses).unwrap().0;
                assignments.insert(node.index() as u32, best);
                colors.entry(best).or_insert(1);
            }

            if colors.len() < ncolors {
                ncolors = colors.len();
                best_assignments = assignments;
            }
        }

        // Apply colors to phi'd slots. Since each came from a strongly connected
        // component, no two Dfs's will re-assign the same node.
        for node in conflicts.node_indices() {
            let slot = node.index() as u32;
            let color = *best_assignments.get(&slot).unwrap();

            let mut dfs = Dfs::new(&phi_graph, node);
            while let Some(alias) = dfs.next(&phi_graph) {
                best_assignments.insert(alias.index() as u32, color);
            }
        }

        // apply the colors
        let nodes: Vec<_> = self.graph.node_indices().collect();
        for node in nodes {
            for curr in self.graph[node].get_code_mut() {
                match &mut curr.opcode {
                    Opcode::GetLocal(ref mut slot)
                    | Opcode::SetLocal(ref mut slot)
                    | Opcode::ReserveCapture(ref mut slot, _) => {
                        *slot = *best_assignments.get(slot).expect("no color!");
                    }
                    Opcode::MoveLocal(ref mut dest, ref mut source) => {
                        *dest = *best_assignments.get(dest).expect("no color!");
                        *source = *best_assignments.get(source).expect("no color!");
                    }
                    _ => {}
                }
            }
        }

        // The frame should be much smaller now, so it makes sense to shrink it.
        self.shrink_frame();

        if self.should_print {
            //self.print();
        }
    }

    pub fn graph_reduce(&mut self, optimization_level: usize) {
        // Algorithm
        //   Where possible, create a value graph for each basic block.
        //   Apply reductions like constant folding to each value graph.
        //   Apply cross-block reductions like local-variable elision using the graphs.
        //   Re-construct a, hopefully more performant, vec of instructions.

        // Phi data is needed to ensure locals are never rearanged across phi-boundries.
        // Since this requires knowledge that's often not local to a specific block, we must
        // collect this info before constructing the value graphs.
        let mut phis = HashMap::new();
        let mut phid = HashSet::new();
        for node in self.graph.node_indices() {
            for curr in self.graph[node].get_code() {
                if let Opcode::MoveLocal(dest, source) = curr.opcode {
                    // A dest can be phi'd by many sources, but a source can only
                    // phi a single dest. Hence, this map has to be source-to-dest.
                    phis.insert(source, dest);
                    phid.insert(dest);
                }
            }
        }

        // The proposed value graph for each basic block.
        // These are replaced as better ones are created.
        let mut graphs = HashMap::new();

        // We use environment variables to make debugging easier.
        // GR_BISECT takes a bitstring & only applies graph_reduce() on blocks whose hashes match it.
        // GR_PRINT pretty-prints every block we apply graph_reduce() on. Together these find bugs.
        let bisect =
            std::env::var("GR_BISECT").map(|x| (u64::from_str_radix(&x, 2).unwrap_or(0), x.len()));
        let always_print = std::env::var_os("GR_PRINT")
            .filter(|x| !x.is_empty())
            .is_some();

        // Compute each block's value graph where possible. In the rare case that a
        // block contains code for which correctness cannot be easily varified, we
        // simply don't optimize it.
        for node in self.graph.node_indices() {
            let block = self.graph[node].get_code();

            if let Ok((bitstring, length)) = bisect {
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                for insn in block.iter() {
                    if let Opcode::AVMOpcode(opcode) = insn.opcode {
                        hasher.write_u8(opcode.to_number());
                    }
                    let mut immediate_hash = [0u8; 32];
                    if let Some(value) = &insn.immediate {
                        if let Value::Int(x) = value.avm_hash() {
                            immediate_hash.copy_from_slice(&x.to_bytes_be());
                        }
                    }
                    hasher.write(&immediate_hash);
                }
                let hash = hasher.finish();
                if hash % (1 << length) != bitstring {
                    // Block's hash didn't match the bitstring, so we skip it.
                    continue;
                }
            }

            if let Some(value_graph) = ValueGraph::new(block, &phis) {
                graphs.insert(node, value_graph);
            }
        }

        macro_rules! show_all {
            () => {
                // Pretty print SSA, the value graph, and graph-codegened results.
                if self.should_print || always_print {
                    for node in self.graph.node_indices() {
                        if let BasicBlock::Meta(_) = &self.graph[node] {
                            continue;
                        }
                        if let Some(graph) = graphs.get(&node) {
                            let code = self.graph[node].get_code();
                            let ssa = code.iter().map(|x| x.pretty_print(Color::PINK)).collect();
                            let values = graph.print_lines();
                            let reduced = graph
                                .codegen(optimization_level)
                                .0
                                .into_iter()
                                .map(|x| x.pretty_print(Color::PINK))
                                .collect();
                            if ssa != reduced {
                                console::print_columns(
                                    vec![ssa, values, reduced],
                                    vec!["SSA", "values", "reduced"],
                                );
                                println!();
                            }
                        }
                    }
                }
            };
        }

        show_all!();

        let nodes: Vec<_> = self.graph.node_indices().collect();

        // See if there's any simple local substitutions we can make
        let mut known_locals = BTreeMap::new();
        for node in &nodes {
            if let Some(values) = graphs.get(node) {
                known_locals.extend(values.known_locals());
            }
        }
        // Give up on phi'd & phi'ing values. A future extension of this algo could handle them,
        // but it's not as trivial as it may seem.
        known_locals = known_locals
            .into_iter()
            .filter(|(slot, _)| !phid.contains(slot) && !phis.contains_key(slot))
            .collect();

        for &node in &nodes {
            let mut block = self.graph[node].get_code_mut();
            for index in 0..block.len() {
                match block[index].opcode {
                    Opcode::GetLocal(slot) => {
                        if let Some(value) = known_locals.get(&slot) {
                            let mut insn = block.get_mut(index).unwrap();
                            insn.opcode = Opcode::AVMOpcode(AVMOpcode::Noop);
                            insn.immediate = Some(value.clone());
                        }
                    }
                    Opcode::SetLocal(slot) => {
                        if let Some(_) = known_locals.get(&slot) {
                            let mut insn = block.get_mut(index).unwrap();
                            insn.opcode = Opcode::AVMOpcode(AVMOpcode::Pop);
                        }
                    }
                    _ => {}
                }
            }

            if !graphs.contains_key(&node) {
                continue;
            }

            if let Some(value_graph) = ValueGraph::new(&block, &phis) {
                graphs.insert(node, value_graph);
            }
        }

        show_all!();

        if self.cyclic {
            // While there's a reasonable path toward making the rest work for
            // loops, these are rare and so we'll restrict our analysis to DAG's.
            // The next best thing is to use these graphs to improve blocks independently.

            // Update blocks where value graphs produce more performant code
            for &node in &nodes {
                if let Some(values) = graphs.get(&node) {
                    let code = self.graph[node].get_code();
                    let cost = code.iter().map(|x| x.opcode.base_cost()).sum();
                    let (reduced, reduced_cost) = values.codegen(optimization_level);
                    if reduced_cost < cost {
                        self.graph[node] = BasicBlock::Code(reduced);
                    }
                }
            }
            return;
        }

        let mut defs = HashMap::new(); // the assigner for each assignment
        let mut uses = HashMap::new(); // the users for each assignment
        let mut dels = HashMap::new(); // the dropper for each assignment
        for node in self.graph.node_indices() {
            for curr in self.graph[node].get_code() {
                match curr.opcode {
                    Opcode::SetLocal(slot) => {
                        // Since the graph is SSA, a slot can only be assigned once.
                        defs.insert(slot, node);
                    }
                    Opcode::GetLocal(slot) => {
                        let set = uses.entry(slot).or_insert(HashSet::new());
                        set.insert(node);
                    }
                    Opcode::DropLocal(slot) => {
                        // Since mini is a scoped language, only one drop is ever needed.
                        dels.insert(slot, node);
                    }
                    _ => {}
                }
            }
        }

        // Stacking a local means moving it from the aux stack to the data stack.
        // Using the stack'd value means passing it from value graph to value graph as an arg.
        // This is only safe, however, if we can determine where exactly to pop the value.
        // There are two kinds of places we must pop a given def
        //   Whenever we exit the basic graph.
        //     We can find this by dfs'ing from the def to all exits
        //   Whenever a value goes out of scope
        //     We place DropLocal annotations during codegen for this purpose

        // Find which locals we can move from the aux stack to the data stack.
        let mut pops = HashMap::new();
        let exits: HashSet<_> = self.graph.neighbors_undirected(self.output).collect();

        for (slot, node) in defs.clone() {
            let mut poppers = BTreeSet::new();
            let mut dfs = Dfs::new(&self.graph, node);
            while let Some(alive) = dfs.next(&self.graph) {
                if exits.contains(&alive) {
                    // This block is an exit, so we need to make sure we pop this value
                    // to clean up the stack.
                    poppers.insert(alive);
                }
                if let None = graphs.get(&alive) {
                    // We can't place this local on the stack since we don't understand
                    // how the values in a down-stream block relate to one another.
                    defs.remove(&slot);
                    break;
                }
            }
            if defs.contains_key(&slot) {
                let dropper = *dels.get(&slot).expect("no dropper");

                let mut dfs = Dfs::new(&self.graph, dropper);
                while let Some(node) = dfs.next(&self.graph) {
                    // This exit happens after the annotated drop, so
                    // we don't actually want to issue a pop.
                    poppers.remove(&node);
                }

                pops.entry(dropper).or_insert(BTreeSet::new()).insert(slot);
                for popper in poppers {
                    pops.entry(popper).or_insert(BTreeSet::new()).insert(slot);
                }
            }
        }

        let mut who_stacks = HashMap::new();
        for (slot, def) in defs {
            who_stacks
                .entry(def)
                .or_insert(BTreeSet::new())
                .insert(slot);
        }

        /// Stack the locals we can
        fn stack_locals(
            node: NodeIndex,
            blocks: &StableGraph<BasicBlock, BasicEdge>,
            phis: &HashMap<SlotNum, SlotNum>,
            graphs: &mut HashMap<NodeIndex, ValueGraph>,
            who_stacks: &HashMap<NodeIndex, BTreeSet<SlotNum>>,
            who_pops: &HashMap<NodeIndex, BTreeSet<SlotNum>>,
            stacked: &BTreeSet<SlotNum>,
            done: &mut HashSet<NodeIndex>,
        ) {
            let block = blocks[node].get_code();
            let mut stacked = stacked.clone();

            let tostack = match who_stacks.get(&node) {
                Some(slots) => slots.clone(),
                None => BTreeSet::new(),
            };
            let unstack = match who_pops.get(&node) {
                Some(slots) => slots.clone(),
                None => BTreeSet::new(),
            };

            let check_stack: BTreeSet<_> = stacked.union(&tostack).cloned().collect();
            assert!(unstack.is_subset(&check_stack), "unstack ⊄ stack ∪ tostack");

            match ValueGraph::with_stack(block, stacked.clone(), &unstack, &tostack, phis) {
                Some(values) => {
                    //graphs.insert(node, values);
                }
                None => {
                    // No value graph exists, but we can restart this analysis since, by construction,
                    // a local is only stack'd when its descendents have value graphs.
                    assert!(
                        !graphs.contains_key(&node),
                        "assumption about construction is wrong"
                    );
                    stacked = BTreeSet::new();
                }
            };

            stacked.extend(tostack);
            for slot in unstack {
                stacked.remove(&slot);
            }

            for child in blocks.neighbors_directed(node, Direction::Outgoing) {
                if !done.contains(&child) {
                    stack_locals(
                        child, blocks, phis, graphs, who_stacks, who_pops, &stacked, done,
                    );
                }
            }
            done.insert(node);
        }

        stack_locals(
            self.entry,
            &self.graph,
            &phis,
            &mut graphs,
            &who_stacks,
            &pops,
            &BTreeSet::new(),
            &mut HashSet::new(),
        );

        for (node, values) in &graphs {
            self.graph[*node] = BasicBlock::Code(values.codegen(optimization_level).0);
        }

        show_all!();
    }
}
