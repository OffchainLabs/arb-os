/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{FrameSize, SlotNum};
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use petgraph::algo::{is_cyclic_directed, kosaraju_scc};
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::stable_graph::StableGraph;
use petgraph::visit::{Dfs, IntoNodeReferences};
use petgraph::{Direction, Undirected};
use rand::prelude::*;
use rand::rngs::SmallRng;
use std::collections::{HashMap, HashSet, VecDeque};

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

pub enum BasicEdge {
    Forward,
    Jump,
}

/// Represents control flow about basic blocks
pub struct BasicGraph {
    /// Basic blocks and the edges that connect them
    graph: StableGraph<BasicBlock, BasicEdge>,
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
        let _entry = graph.add_node(BasicBlock::Meta("Entry"));

        let should_print = code.iter().any(|x| x.debug_info.attributes.codegen_print);

        // Create basic blocks by splitting on jumps and labels
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

        // Add jump edges
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

        let cyclic = is_cyclic_directed(&graph);
        BasicGraph {
            graph,
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
    pub fn print(&self) {
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
                if let Opcode::SetLocal(slot) = curr.opcode {
                    locals.insert(slot);
                }
            }
        }
        for node in &nodes {
            for curr in self.graph[*node].get_code() {
                match curr.opcode {
                    Opcode::GetLocal(slot) => {
                        locals.remove(&slot);
                    }
                    Opcode::MoveLocal(_, source) => {
                        locals.remove(&source);
                    }
                    _ => {}
                }
            }
        }
        for node in nodes {
            for curr in self.graph[node].get_code_mut() {
                match curr.opcode {
                    Opcode::SetLocal(slot) if locals.contains(&slot) => {
                        let debug = curr.debug_info;
                        *curr = Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Pop), debug);
                    }
                    Opcode::MoveLocal(dest, _) if locals.contains(&dest) => {
                        let debug = curr.debug_info;
                        *curr = Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::Noop), debug);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Shrinks the frame size to elide unused frame slots.
    pub fn shrink_frame(&mut self) -> FrameSize {
        let mut locals = HashMap::new();
        let nodes: Vec<_> = self.graph.node_indices().collect();

        for node in &nodes {
            for curr in self.graph[*node].get_code() {
                match curr.opcode {
                    Opcode::SetLocal(slot) | Opcode::GetLocal(slot) => {
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
                    Opcode::SetLocal(ref mut slot) | Opcode::GetLocal(ref mut slot) => {
                        *slot = *replace.get(slot).unwrap();
                    }
                    Opcode::MoveLocal(ref mut dest, ref mut source) => {
                        *dest = *replace.get(dest).unwrap();
                        *source = *replace.get(source).unwrap();
                    }
                    Opcode::MakeFrame(ref mut space, _) => {
                        *space = replace.len() as FrameSize;
                    }
                    _ => {}
                }
            }
        }

        replace.len() as FrameSize
    }

    /// Efficiently assign frame slots to minimize the frame size.
    pub fn color(&mut self, frame_size: FrameSize) {
        let mut conflicts: StableGraph<SlotNum, (), Undirected> = StableGraph::default();
        let mut phi_graph: UnGraph<SlotNum, ()> = UnGraph::default();

        for slot in 0..frame_size {
            // we do this so that slot & index are the same
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

        // Walking the graph backwards is heuristically faster.
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

            let mut alive: HashSet<SlotNum> = HashSet::new(); // values that haven't been set yet

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
                    Opcode::SetLocal(local) => {
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
        for component in kosaraju_scc(&phi_graph) {
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

        // Coloring a graph is NP-complete
        let mut rng = SmallRng::seed_from_u64(0);
        let mut best_assignments = HashMap::new();
        let mut ncolors = usize::MAX;

        for _ in 0..24 {
            let mut colors: HashMap<u32, usize> = HashMap::new(); // colors to usage counts
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

        // apply colors to phi'd slots
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
                    Opcode::GetLocal(ref mut slot) | Opcode::SetLocal(ref mut slot) => {
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

        self.shrink_frame();

        if self.should_print {
            self.print();
        }
    }
}
