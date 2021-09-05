/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

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

pub struct BasicGraph {
    graph: StableGraph<BasicBlock, BasicEdge>,
    output: NodeIndex,
    complete: bool,
    cyclic: bool,
    source: Vec<Instruction>,
}

impl BasicGraph {
    pub fn new(code: Vec<Instruction>) -> Self {
        let mut graph: StableGraph<BasicBlock, BasicEdge> = StableGraph::new();
        let mut complete = true;
        let entry = graph.add_node(BasicBlock::Meta("Entry")); // entry point to the graph
        let output = graph.add_node(BasicBlock::Meta("Output")); // output of the graph

        let should_print = code.iter().any(|x| x.debug_info.attributes.codegen_print);
        for (index, curr) in code.iter().enumerate() {
            if curr.debug_info.attributes.codegen_print {
                println!(
                    "{}  {}",
                    Color::grey(format!("{:04}", index)),
                    curr.pretty_print(Color::PINK)
                );
            }
        }
        if should_print {
            println!("");
        }

        // first pass: create the nodes, connecting each to its successor with Forward edges.

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
                Opcode::Return | Opcode::AVMOpcode(AVMOpcode::Cjump | AVMOpcode::Jump) => {
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
        graph.add_edge(last_block, tail, BasicEdge::Forward);
        graph.add_edge(tail, output, BasicEdge::Forward);

        // second pass: add jump edges and prune forward Edges that shouldn't exist

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
                Opcode::AVMOpcode(AVMOpcode::Jump | AVMOpcode::Cjump) => {
                    let label = match &last.immediate {
                        Some(Value::Label(label)) => label,
                        _ => {
                            complete = false;
                            continue;
                        }
                    };
                    let dest = match label_to_block.get(label) {
                        Some(dest) => *dest,
                        _ => {
                            complete = false;
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
            output,
            complete,
            cyclic,
            source: code,
        };

        if should_print {
            graph.print();
        }

        graph
    }

    pub fn flatten(self) -> Vec<Instruction> {
        if !self.complete {
            return self.source;
        }

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
                    BasicBlock::Meta(text) => Color::mint(text),
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
        println!("is dag:   {}", !self.cyclic);
        println!("complete: {}\n", self.complete);
    }

    pub fn color(&mut self) {
        if self.cyclic {
            // loops are rare and algos analyzing them must track stack
            // differentials. We'll just skip the ~11 of them in ArbOS.
            return;
        }

        let mut _lifetimes: DiGraph<usize, ()> = DiGraph::new(); // the actual values (line #)

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
