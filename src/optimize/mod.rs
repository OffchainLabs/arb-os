/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{FrameSize, SlotNum};
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use petgraph::algo::{is_cyclic_directed, kosaraju_scc};
use petgraph::graph::{EdgeIndex, NodeIndex, UnGraph};
use petgraph::stable_graph::StableGraph;
use petgraph::visit::{Dfs, EdgeRef};
use petgraph::{Direction, Undirected};
use std::collections::{BTreeSet, HashMap, HashSet};

pub fn outgoing_edges<N, E>(graph: &StableGraph<N, E>, node: NodeIndex) -> Vec<EdgeIndex> {
    graph
        .edges_directed(node, Direction::Outgoing)
        .map(|e| e.id())
        .collect()
}

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
    /// Whether this graph includes a jump to some unknown place
    wild: bool,
    /// Whether this graph contains a cycle
    cyclic: bool,
}

impl BasicGraph {
    /// Creates a `BasicGraph` from a list of `Instruction`s.
    pub fn new(code: Vec<Instruction>) -> Self {
        // Algorithm:
        //   Create a control flow graph by walking the instructions in two passes.
        //     Split on labels & jumps to form sequentially-connected basic-blocks.
        //     Drop sequential jumps for instructions that unconditionally branch elsewhere.
        //     Add edges for jump targets. If the target is unknown, declare the graph wild.
        //
        // Assumptions:
        //   We assume the first instruction is the entry point.
        //   We assume all Return opcodes jump to the same location.

        let mut graph: StableGraph<BasicBlock, BasicEdge> = StableGraph::new();
        let entry = graph.add_node(BasicBlock::Meta("Entry"));

        let mut wild = false; // not wild until proven otherwise

        // First pass
        //   make blocks and connect them sequentially.
        //
        let mut last_block = entry;
        let mut block_data = vec![];
        let mut label_to_block = HashMap::new();
        for curr in code.clone() {
            match curr.opcode {
                Opcode::Label(label) => {
                    let block = graph.add_node(BasicBlock::Code(block_data));
                    graph.add_edge(last_block, block, BasicEdge::Forward);
                    last_block = block;
                    block_data = vec![curr];
                    label_to_block.insert(label, block);
                }
                Opcode::Return
                | Opcode::JumpTo(_)
                | Opcode::CjumpTo(_)
                | Opcode::AVMOpcode(AVMOpcode::Cjump | AVMOpcode::Jump) => {
                    block_data.push(curr);
                    let block = graph.add_node(BasicBlock::Code(block_data));
                    graph.add_edge(last_block, block, BasicEdge::Forward);
                    last_block = block;
                    block_data = vec![];
                }
                _ => block_data.push(curr),
            }
        }
        let tail = graph.add_node(BasicBlock::Code(block_data));
        let output = graph.add_node(BasicBlock::Meta("Output"));
        graph.add_edge(last_block, tail, BasicEdge::Forward);
        graph.add_edge(tail, output, BasicEdge::Forward);

        // Second pass
        //   add jump edges and prune forward Edges that shouldn't exist
        //
        let nodes: Vec<_> = graph.node_indices().collect();
        for node in nodes {
            let last = match &graph[node] {
                BasicBlock::Code(code) => match code.iter().last().cloned() {
                    Some(insn) => insn,
                    _ => continue,
                },
                _ => continue,
            };

            let op = last.opcode;

            if let Opcode::Return | Opcode::JumpTo(_) | Opcode::AVMOpcode(AVMOpcode::Jump) = &op {
                // these should only point to their target destinations
                for edge in outgoing_edges(&graph, node) {
                    graph.remove_edge(edge);
                }
            }

            match &op {
                Opcode::Return => {
                    graph.add_edge(node, output, BasicEdge::Jump);
                }
                Opcode::JumpTo(label) | Opcode::CjumpTo(label) => {
                    let dest = match label_to_block.get(label) {
                        Some(dest) => *dest,
                        _ => {
                            wild = true;
                            continue;
                        }
                    };
                    graph.add_edge(node, dest, BasicEdge::Jump);
                }
                Opcode::AVMOpcode(AVMOpcode::Jump | AVMOpcode::Cjump) => {
                    let label = match &last.immediate {
                        Some(Value::Label(label)) => label,
                        _ => {
                            wild = true;
                            continue;
                        }
                    };
                    let dest = match label_to_block.get(label) {
                        Some(dest) => *dest,
                        _ => {
                            wild = true;
                            continue;
                        }
                    };
                    graph.add_edge(node, dest, BasicEdge::Jump);
                }
                _ => {}
            }
        }

        let cyclic = is_cyclic_directed(&graph);

        let graph = BasicGraph {
            graph,
            wild,
            cyclic,
        };

        let should_print = code.iter().any(|x| x.debug_info.attributes.codegen_print);
        if should_print {
            graph.print();
        }

        graph
    }

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
            "{} {}",
            Color::grey("cyclic:"),
            Color::color_if(self.cyclic, Color::MINT, Color::GREY)
        );
        println!(
            "{} {}\n",
            Color::grey("wild:  "),
            Color::color_if(self.wild, Color::MINT, Color::GREY)
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

    pub fn shrink_frame(&mut self) -> FrameSize {
        let mut locals = BTreeSet::new();
        let nodes: Vec<_> = self.graph.node_indices().collect();

        for node in &nodes {
            for curr in self.graph[*node].get_code() {
                match curr.opcode {
                    Opcode::SetLocal(slot) | Opcode::GetLocal(slot) => {
                        locals.insert(slot);
                    }
                    Opcode::MoveLocal(dest, source) => {
                        locals.insert(dest);
                        locals.insert(source);
                    }
                    _ => {}
                }
            }
        }

        let replace: HashMap<_, _> = locals
            .into_iter()
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
                        if *space as usize != replace.len() {
                            eprintln!("FRAME {} {}", space, replace.len());
                        }
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
        let mut should_print = false;
        for node in self.graph.node_indices() {
            let code = self.graph[node].get_code();
            should_print =
                should_print || code.iter().any(|x| x.debug_info.attributes.codegen_print);
        }

        /*if self.wild {
            // For now, don't try to be smart about funcs with wild jumps.
            println!("Found a wild frame");
            return;
        }*/

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

        let mut node_kills = HashMap::new();
        let mut node_needs = HashMap::new();

        for node in self.graph.node_indices() {
            let code = self.graph[node].get_code();

            let mut alive = HashSet::new();
            let mut kills = HashSet::new();

            macro_rules! kill {
                ($slot:expr) => {{
                    kills.insert($slot);
                    for other in &alive {
                        conflicts.update_edge(NodeIndex::from(*other), NodeIndex::from($slot), ());
                    }
                    //alive.remove(&$slot);
                }};
            }
            macro_rules! make {
                ($slot:expr) => {{
                    if !alive.insert($slot) {
                        continue;
                    }
                    for other in &alive {
                        conflicts.update_edge(NodeIndex::from(*other), NodeIndex::from($slot), ());
                    }
                }};
            }

            for curr in code.iter().rev() {
                match curr.opcode {
                    Opcode::SetLocal(slot) => {
                        kill!(slot);
                    }
                    Opcode::GetLocal(slot) => {
                        make!(slot);
                    }
                    Opcode::MoveLocal(dest, source) => {
                        kill!(dest);
                        make!(source);
                    }
                    _ => {}
                }
            }

            if should_print {
                println!("kills {:?}", &kills);
                println!("alive {:?}", &alive);

                for curr in code {
                    println!("{}", curr.pretty_print(Color::MINT));
                }
            }

            node_kills.insert(node, kills);
            node_needs.insert(node, alive);
        }

        // Walking the graph backwards is heuristically faster.
        let mut work_list: Vec<_> = self.graph.node_indices().rev().collect();

        while let Some(node) = work_list.pop() {
            // Create conflict graph algorithm
            //   Keep updating the liveliness information for each block until all conflicts are found.
            //   At each block, check if the needs of its successors should be propagated backwards.
            //   If so, add conflict information & place the successors to the end of the work list.

            let outgoing = self.graph.neighbors_directed(node, Direction::Outgoing);
            let incoming = self.graph.neighbors_directed(node, Direction::Incoming);

            let my_kills = node_kills.get(&node).unwrap();

            let mut successor_needs: Vec<SlotNum> = vec![];

            for output in outgoing {
                let output_needs = node_needs.get(&output).unwrap().into_iter();
                successor_needs.extend(output_needs.filter(|need| !my_kills.contains(need)));
            }

            let my_needs = node_needs.get_mut(&node).unwrap();

            let mut changed = false;
            for need in successor_needs {
                if my_needs.contains(&need) {
                    continue;
                }

                let disallowed = NodeIndex::from(need);
                for slot in my_needs.iter() {
                    conflicts.update_edge(NodeIndex::from(*slot), disallowed, ());
                }
                for slot in my_kills {
                    conflicts.update_edge(NodeIndex::from(*slot), disallowed, ());
                }

                my_needs.insert(need);
                changed = true;
            }

            if changed {
                for input in incoming {
                    work_list.push(input);
                }
            }
        }

        // Graph-contract all phi nodes. We can do this since mini is a scoped language.
        // What this does is force both sides of a phi node to be colored the same way.
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

        let mut colors: HashMap<u32, usize> = HashMap::new(); // colors to usage counts
        let mut assignments: HashMap<SlotNum, u32> = HashMap::new(); // slots to colors
        colors.insert(0, 0);

        for node in conflicts.node_indices() {
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

            let best = available.into_keys().min().unwrap();
            assignments.insert(node.index() as u32, best);
            colors.entry(best).or_insert(1);

            let mut dfs = Dfs::new(&phi_graph, node);
            while let Some(alias) = dfs.next(&phi_graph) {
                assignments.insert(alias.index() as u32, best);
            }
        }

        // apply the colors
        let nodes: Vec<_> = self.graph.node_indices().collect();
        for node in nodes {
            for curr in self.graph[node].get_code_mut() {
                match &mut curr.opcode {
                    Opcode::GetLocal(ref mut slot) | Opcode::SetLocal(ref mut slot) => {
                        *slot = *assignments.get(slot).expect("no color!");
                    }
                    Opcode::MoveLocal(ref mut dest, ref mut source) => {
                        *dest = *assignments.get(dest).expect("no color!");
                        *source = *assignments.get(source).expect("no color!");
                    }
                    _ => {}
                }
            }
        }

        if should_print {
            self.print();
        }
    }
}
