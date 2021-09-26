/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{FrameSize, SlotNum};
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use petgraph::algo::is_cyclic_directed;
use petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use petgraph::stable_graph::StableGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::collections::HashMap;

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

            if let Opcode::Return | Opcode::AVMOpcode(AVMOpcode::Jump) = &last.opcode {
                // these should only point to their target destinations
                for edge in outgoing_edges(&graph, node) {
                    graph.remove_edge(edge);
                }
            }

            match &last.opcode {
                Opcode::Return => {
                    graph.add_edge(node, output, BasicEdge::Jump);
                }
                Opcode::CjumpTo(label) => {
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

    /// Efficiently assign frame slots to minimize the frame size.
    pub fn color(&mut self, frame_size: FrameSize) {
        if self.wild {
            // For now, don't try to be smart about funcs with wild jumps.
            return;
        }

        let mut lifetimes: DiGraph<SlotNum, ()> = DiGraph::new();
        let mut phi_graph: DiGraph<SlotNum, ()> = DiGraph::new();

        for slot in 0..frame_size {
            lifetimes.add_node(slot);
            phi_graph.add_node(slot);
        }

        for node in self.graph.node_indices() {
            for curr in self.graph[node].get_code() {
                if let Opcode::MoveLocal(dest, source) = &curr.opcode {
                    phi_graph.add_edge(NodeIndex::from(*source), NodeIndex::from(*dest), ());
                }
            }
        }

        let nodes: Vec<_> = self.graph.node_indices().rev().collect();

        for node in nodes {
            let code = self.graph[node].get_code();

            for curr in code.iter().rev() {
                match curr.opcode {
                    Opcode::SetLocal(_var) => {}
                    Opcode::GetLocal(_var) => {}
                    _ => {}
                }
            }
        }
    }
}
