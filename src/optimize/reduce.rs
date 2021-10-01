/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use std::collections::{HashMap, VecDeque};

pub enum ValueNode {
    Opcode(Opcode), // a
    Value(Value),   // a simple value
    Arg(usize),
    Meta(&'static str), //
}

pub enum ValueEdge {
    Connect,
    Meta(&'static str),
}

pub struct ValueGraph {
    graph: StableGraph<ValueNode, ValueEdge>,
}

impl ValueGraph {
    /// Create a new `ValueGraph` from a set of instructions without control flow
    pub fn new(code: &[Instruction]) -> Self {
        type Index = isize;

        let mut graph = StableGraph::new();

        let mut stack: VecDeque<Index> = VecDeque::new();
        let mut offset_to_node: HashMap<Index, NodeIndex> = HashMap::new();
        let mut nargs = 0;

        macro_rules! touch {
            ($count:expr) => {
                let count: usize = $count;
                let missing = count.saturating_sub(stack.len());
                for touched in 0..missing {
                    // Arg wasn't on the stack, so lets push one to the bottom
                    nargs += 1;
                    offset_to_node.insert(-nargs, graph.add_node(ValueNode::Arg(nargs as usize)));
                    stack.push_front(-nargs);
                }
            };
        }

        let mut offset = 0;
        macro_rules! push {
            ($($item:tt)+) => {{
                let node = graph.add_node(ValueNode::$($item)+);
                offset_to_node.insert(offset, node);
                stack.push_back(offset);
                offset += 1;
            }};
        }

        for curr in code {
            if let Some(value) = &curr.immediate {
                push!(Value(value.clone()));
            }

            macro_rules! avm {
                ($first:ident $(,$opcode:ident)*) => {
                    Opcode::AVMOpcode(AVMOpcode::$first $(| AVMOpcode::$opcode)*)
                };
            }

            macro_rules! value {
                ($read:expr, $output:expr) => {{}};
                ($read:expr) => {
                    value!($read, true)
                };
            }
        }

        ValueGraph { graph }
    }
}
