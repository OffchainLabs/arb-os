/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::{DebugInfo, SlotNum};
use crate::console::Color;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use crate::optimize::effects::{Effect, Effects};
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

        macro_rules! degree {
            ($node:expr) => {
                graph.neighbors_directed($node, Direction::Incoming).count()
            };
        }

        let mut arg_code = vec![];

        // Pop unused arguments. We can't just ignore them, since they were created elsewhere.
        for node in graph.node_indices() {
            if let ValueNode::Arg(num) = &graph[node] {
                if degree!(node) != 0 {
                    stack.push(node);
                } else {
                    arg_code.push(opcode!(@Pull(stack.len())));
                    arg_code.push(opcode!(Pop));
                }
            }
        }

        let output = self.output;

        fn descend(
            node: NodeIndex,
            graph: &StableGraph<ValueNode, ValueEdge>,
            produced: &mut HashSet<NodeIndex>,
            entropy: &mut SmallRng,
            debug: DebugInfo,
        ) -> Vec<Instruction> {
            let mut code = vec![];

            if produced.contains(&node) {
                // We've already produced a value for this node,
                // which should be on the stack.
                return vec![];
            }

            produced.insert(node);
            code
        }

        let mut entropy: SmallRng = SeedableRng::seed_from_u64(2);

        let mut best = self.source.clone();
        for _ in 0..4 {
            let mut alt = arg_code.clone();
            alt.extend(descend(
                output,
                &graph,
                &mut HashSet::new(),
                &mut entropy,
                debug,
            ));

            if alt.len() < best.len() {
                best = alt;
            }
        }

        best
    }
}
