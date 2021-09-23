/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Converts non-type checked ast nodes to type checked versions, and other related utilities.

use super::ast::{
    AssignRef, Attributes, BinaryOp, CodeBlock, Constant, DebugInfo, Expr, ExprKind, Func,
    GlobalVar, Statement, StatementKind, StructField, TopLevelDecl, TrinaryOp, Type, TypeTree,
    UnaryOp,
};
use crate::compile::ast::{FieldInitializer, FuncProperties};
use crate::compile::{CompileError, ErrorSystem};
use crate::console::{human_readable_index, Color};
use crate::link::Import;
use crate::mavm::{Instruction, Value};
use crate::pos::Location;
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

type TypeTable = HashMap<usize, Type>;

/// Trait for all nodes in the AST, currently only implemented for type checked versions.
pub trait AbstractSyntaxTree {
    /// Returns a list of direct children of `self`
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        vec![]
    }
    /// Applies `func` to `self` recursively, stopping when `func` returns `false`.  The `state` and
    /// `mut_state` arguments are accessible to all nodes called by this method, and `mut_state` can
    /// be modified by `func`.  The modifications will only be visible to the child nodes.
    fn recursive_apply<F, S, MS>(&mut self, func: F, state: &S, mut_state: &mut MS)
    where
        F: Fn(&mut TypeCheckedNode, &S, &mut MS) -> bool + Copy,
        MS: Clone,
    {
        let mut children = self.child_nodes();
        for child in &mut children {
            let mut child_state = (*mut_state).clone();
            let recurse = func(child, state, &mut child_state);
            if recurse {
                child.recursive_apply(func, state, &mut child_state);
            }
        }
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool;
    fn is_write(&mut self, type_tree: &TypeTree) -> bool;
}

/// Represents a mutable reference to any AST node.
#[derive(Debug)]
pub enum TypeCheckedNode<'a> {
    Statement(&'a mut TypeCheckedStatement),
    Expression(&'a mut TypeCheckedExpr),
    Type(&'a mut Type),
}

impl<'a> AbstractSyntaxTree for TypeCheckedNode<'a> {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            TypeCheckedNode::Statement(stat) => stat.child_nodes(),
            TypeCheckedNode::Expression(exp) => exp.child_nodes(),
            TypeCheckedNode::Type(tipe) => tipe.child_nodes(),
        }
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool {
        match self {
            TypeCheckedNode::Statement(stat) => stat.is_view(type_tree),
            TypeCheckedNode::Expression(exp) => exp.is_view(type_tree),
            TypeCheckedNode::Type(_) => false,
        }
    }
    fn is_write(&mut self, type_tree: &TypeTree) -> bool {
        match self {
            TypeCheckedNode::Statement(stat) => stat.is_write(type_tree),
            TypeCheckedNode::Expression(exp) => exp.is_write(type_tree),
            TypeCheckedNode::Type(_) => false,
        }
    }
}

impl<'a> TypeCheckedNode<'a> {
    /// Propagates attributes down the AST.
    pub fn propagate_attributes(mut nodes: Vec<TypeCheckedNode>, attributes: &Attributes) {
        for node in nodes.iter_mut() {
            match node {
                TypeCheckedNode::Statement(stat) => {
                    stat.debug_info.attributes.codegen_print =
                        stat.debug_info.attributes.codegen_print || attributes.codegen_print;
                    let child_attributes = stat.debug_info.attributes.clone();
                    TypeCheckedNode::propagate_attributes(stat.child_nodes(), &child_attributes);
                    if let TypeCheckedStatementKind::Asm(ref mut vec, _) = stat.kind {
                        for insn in vec {
                            insn.debug_info.attributes.codegen_print =
                                stat.debug_info.attributes.codegen_print
                                    || attributes.codegen_print;
                        }
                    }
                }
                TypeCheckedNode::Expression(expr) => {
                    expr.debug_info.attributes.codegen_print =
                        expr.debug_info.attributes.codegen_print || attributes.codegen_print;
                    let child_attributes = expr.debug_info.attributes.clone();
                    TypeCheckedNode::propagate_attributes(expr.child_nodes(), &child_attributes);
                }
                _ => {}
            }
        }
    }
}

pub type TypeCheckedFunc = Func<TypeCheckedStatement>;

impl AbstractSyntaxTree for TypeCheckedFunc {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        self.code
            .iter_mut()
            .map(|stat| TypeCheckedNode::Statement(stat))
            .collect()
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool {
        self.code
            .iter_mut()
            .any(|statement| statement.is_view(type_tree))
    }
    fn is_write(&mut self, type_tree: &TypeTree) -> bool {
        self.code
            .iter_mut()
            .any(|statement| statement.is_write(type_tree))
    }
}

/// Discovers which import statements have been used
fn flowcheck_imports(mut nodes: Vec<TypeCheckedNode>, imports: &mut BTreeMap<usize, Import>) {
    for node in &mut nodes {
        if let TypeCheckedNode::Expression(expr) = node {
            let nominals = match &expr.kind {
                TypeCheckedExprKind::Cast(_, tipe)
                | TypeCheckedExprKind::Const(_, tipe)
                | TypeCheckedExprKind::ClosureLoad(.., tipe) => tipe.find_nominals(),
                _ => vec![],
            };
            for nominal in &nominals {
                imports.remove(nominal);
            }

            // observe any function calls or pointers
            if let TypeCheckedExprKind::FuncRef(id, _) = &expr.kind {
                imports.remove(&id);
            }
        }

        flowcheck_imports(node.child_nodes(), imports);
    }
}

/// Discovers code segments that could never be executed
fn flowcheck_reachability<T: AbstractSyntaxTree>(node: &mut T) -> Vec<CompileError> {
    let mut children = node.child_nodes();
    let mut child_iter = children.iter_mut();

    let mut warnings = vec![];
    let mut locations = vec![];

    for child in &mut child_iter {
        match child {
            TypeCheckedNode::Statement(stat) => match &mut stat.kind {
                TypeCheckedStatementKind::Return(_) | TypeCheckedStatementKind::ReturnVoid() => {
                    locations.extend(stat.debug_info.location);
                    break;
                }
                TypeCheckedStatementKind::Expression(expr) => match &mut expr.kind {
                    TypeCheckedExprKind::If(_, block, else_block, ..)
                    | TypeCheckedExprKind::IfLet(_, _, block, else_block, ..) => {
                        warnings.extend(flowcheck_reachability(block));

                        if let Some(branch) = else_block {
                            warnings.extend(flowcheck_reachability(branch));
                        }

                        continue;
                    }
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }

        warnings.extend(flowcheck_reachability(child));
    }

    match child_iter.next() {
        Some(TypeCheckedNode::Statement(issue)) => locations.extend(issue.debug_info.location),
        _ => {}
    };

    match child_iter.last() {
        Some(TypeCheckedNode::Statement(issue)) => locations.extend(issue.debug_info.location),
        _ => {}
    };

    if locations.len() <= 1 {
        return warnings;
    }

    warnings.push(CompileError::new_warning(
        "Compile warning",
        if locations.len() == 2 {
            "found unreachable statement"
        } else {
            "found unreachable statements"
        },
        locations,
    ));

    warnings
}

/// Discovers assigned values that are never used
fn flowcheck_liveliness(
    mut nodes: Vec<TypeCheckedNode>,
    problems: &mut Vec<(Location, StringId)>,
    loop_pass: bool,
) -> (BTreeSet<StringId>, BTreeMap<StringId, Location>) {
    let mut node_iter = nodes.iter_mut();
    let mut alive = BTreeMap::new(); // values that are alive and need to die in this scope
    let mut reborn = BTreeMap::new(); // values that are brought back to life within this scope
    let mut born = BTreeSet::<StringId>::new(); // values brought to life within this scope
    let mut killed = BTreeSet::<StringId>::new(); // values this scope has killed
    let mut rescue = BTreeSet::<StringId>::new(); // values from parental scopes we provably must not kill

    // algorithm notes:
    //   anything still alive at the end of scope is unused
    //   scopes return everything they've killed
    //   a scope cannot kill that which it overwrites
    //   loops are unrolled to give assignments a second chance to be killed before leaving scope
    //   we assume conditionals could evaluate either true or false
    //   you can never kill a value that's been reborn since the original value might be used later
    //   we make no garuntee that all mistakes with the same variable are caught, only that one of them is

    macro_rules! process {
        ($child_changes:expr) => {
            let (child_killed, child_reborn) = $child_changes;
            for id in child_killed.iter() {
                alive.remove(id);
                reborn.remove(id);
            }
            for (id, loc) in child_reborn {
                if born.contains(&id) {
                    alive.insert(id, loc);
                } else {
                    reborn.insert(id, loc);
                }
            }
            killed.extend(child_killed);
        };
        ($child_nodes:expr, $problems:expr, $loop_pass:expr $(,)?) => {
            process!(flowcheck_liveliness($child_nodes, $problems, $loop_pass));
        };
    }

    for node in &mut node_iter {
        let repeat = match node {
            TypeCheckedNode::Statement(stat) => match &mut stat.kind {
                TypeCheckedStatementKind::SetLocals(assigned, expr) => {
                    process!(vec![TypeCheckedNode::Expression(expr)], problems, false);

                    for local in assigned {
                        let id = local.id;
                        let loc = local.debug_info.locs()[0];

                        if local.shadow {
                            if let Some(_) = born.get(&id) {
                                if let Some(loc) = alive.get(&id) {
                                    problems.push((*loc, id))
                                }
                            }
                            if !alive.contains_key(&id)
                                && !born.contains(&id)
                                && !killed.contains(&id)
                            {
                                rescue.insert(id);
                            }
                            born.insert(id);
                            alive.insert(id, loc);
                        } else {
                            if let Some(loc) = alive.get(&id) {
                                problems.push((*loc, id));
                            }
                            if let None = born.get(&id) {
                                reborn.insert(id, stat.debug_info.location.unwrap());
                            }
                            if !alive.contains_key(&id)
                                && !born.contains(&id)
                                && !killed.contains(&id)
                            {
                                rescue.insert(id);
                            }
                            alive.insert(id, loc);
                        }
                    }
                    continue;
                }
                TypeCheckedStatementKind::While(..) => true,
                TypeCheckedStatementKind::Break(optional_expr, _) => {
                    process!(
                        optional_expr
                            .iter_mut()
                            .map(|x| TypeCheckedNode::Expression(x))
                            .collect(),
                        problems,
                        false,
                    );
                    continue;
                }
                _ => false,
            },

            TypeCheckedNode::Expression(expr) => match &mut expr.kind {
                TypeCheckedExprKind::LocalVariableRef(id, ..) => {
                    killed.insert(id.clone());
                    alive.remove(id);
                    reborn.remove(id);
                    false
                }
                TypeCheckedExprKind::IfLet(_, cond, block, else_block, _)
                | TypeCheckedExprKind::If(cond, block, else_block, _) => {
                    let (mut if_killed, mut if_reborn) = (
                        BTreeSet::<StringId>::new(),
                        BTreeMap::<StringId, Location>::new(),
                    );

                    macro_rules! extend {
                        ($child_nodes:expr, $problems:expr, $loop_pass:expr $(,)?) => {
                            let (child_killed, child_reborn) =
                                flowcheck_liveliness($child_nodes, $problems, $loop_pass);
                            if_killed.extend(child_killed);
                            if_reborn.extend(child_reborn);
                        };
                    }

                    extend!(vec![TypeCheckedNode::Expression(cond)], problems, false);
                    extend!(block.child_nodes(), problems, false);

                    if let Some(branch) = else_block {
                        extend!(branch.child_nodes(), problems, false);
                    }

                    process!((if_killed, if_reborn));
                    continue;
                }
                TypeCheckedExprKind::Loop(_body, _) => true,
                TypeCheckedExprKind::ClosureLoad(_, _, _, captures, _) => {
                    // In the future we'll walk into the closure in case a captured value is overwritten,
                    // but because child_nodes() requires a *mutable* reference we can't do that cheaply.
                    // Hence, we'll just claim that every captured value is used.

                    killed.extend(captures.iter().clone());
                    false
                }
                _ => false,
            },
            _ => false,
        };

        process!(node.child_nodes(), problems, repeat);
    }

    if loop_pass {
        let mut node_iter = nodes.iter_mut();

        for node in &mut node_iter {
            let repeat = match node {
                TypeCheckedNode::Statement(stat) => match &mut stat.kind {
                    TypeCheckedStatementKind::While(..) => true,
                    _ => false,
                },
                TypeCheckedNode::Expression(expr) => match &mut expr.kind {
                    TypeCheckedExprKind::Loop(..) => true,
                    TypeCheckedExprKind::LocalVariableRef(id, ..) => {
                        // a variable born in this scope shouldn't get a second chance when unrolling
                        if !born.contains(id) {
                            alive.remove(id);
                            reborn.remove(id);
                        }
                        false
                    }
                    _ => false,
                },
                _ => false,
            };

            // we've done already walked these nodes, so all errors are repeated and should be elided
            let mut duplicate_problems = vec![];

            let (child_killed, _) =
                flowcheck_liveliness(node.child_nodes(), &mut duplicate_problems, repeat);
            for id in child_killed.iter() {
                // a variable born in this scope shouldn't get a second chance when unrolling
                if !born.contains(id) {
                    alive.remove(id);
                    reborn.remove(id);
                }
            }
        }
    }

    // check if variables are still alive and we're going out of scope
    for (id, loc) in alive.iter() {
        if let Some(_) = born.get(id) {
            problems.push((loc.clone(), id.clone()));
        }
    }

    for id in &rescue {
        killed.remove(id);
    }

    return (killed, reborn);
}

impl TypeCheckedFunc {
    pub fn flowcheck(
        &mut self,
        imports: &mut BTreeMap<usize, Import>,
        string_table: &mut StringTable,
        error_system: &ErrorSystem,
    ) -> Vec<CompileError> {
        let mut flowcheck_warnings = vec![];

        flowcheck_imports(self.child_nodes(), imports);

        for id in self.tipe.find_nominals() {
            imports.remove(&id);
        }

        flowcheck_warnings.extend(flowcheck_reachability(self));

        let mut unused_assignments = vec![];

        let (killed, reborn) =
            flowcheck_liveliness(self.child_nodes(), &mut unused_assignments, false);

        for arg in self.args.iter() {
            // allow intentional lack of use
            if !string_table.name_from_id(arg.name.clone()).starts_with('_') {
                if !killed.contains(&arg.name) {
                    flowcheck_warnings.push(CompileError::new_warning(
                        String::from("Compile warning"),
                        format!(
                            "func {}'s argument {} is declared but never used",
                            Color::color(error_system.warn_color, &self.name),
                            Color::color(
                                error_system.warn_color,
                                string_table.name_from_id(arg.name.clone())
                            ),
                        ),
                        arg.debug_info.location.into_iter().collect(),
                    ));
                }

                if let Some(loc) = reborn.get(&arg.name) {
                    flowcheck_warnings.push(CompileError::new_warning(
                        String::from("Compile warning"),
                        format!(
                            "func {}'s argument {} is assigned but never used",
                            Color::color(error_system.warn_color, &self.name),
                            Color::color(
                                error_system.warn_color,
                                string_table.name_from_id(arg.name.clone())
                            ),
                        ),
                        vec![*loc],
                    ));
                }
            }
        }

        for &(loc, id) in unused_assignments.iter() {
            // allow intentional lack of use
            if !string_table.name_from_id(id.clone()).starts_with('_') {
                flowcheck_warnings.push(CompileError::new_warning(
                    String::from("Compile warning"),
                    format!(
                        "value {} is assigned but never used",
                        Color::color(error_system.warn_color, string_table.name_from_id(id)),
                    ),
                    vec![loc],
                ));
            }
        }

        flowcheck_warnings
    }
}

/// A mini statement that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeCheckedStatement {
    pub kind: TypeCheckedStatementKind,
    pub debug_info: DebugInfo,
}

/// A mini statement that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeCheckedStatementKind {
    ReturnVoid(),
    Return(TypeCheckedExpr),
    Break(Option<TypeCheckedExpr>, String),
    Expression(TypeCheckedExpr),
    SetLocals(Vec<AssignRef>, TypeCheckedExpr),
    AssignGlobal(StringId, TypeCheckedExpr),
    While(TypeCheckedExpr, TypeCheckedCodeBlock),
    Asm(Vec<Instruction>, Vec<TypeCheckedExpr>),
    DebugPrint(TypeCheckedExpr),
    Assert(TypeCheckedExpr),
}

impl AbstractSyntaxTree for TypeCheckedStatement {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match &mut self.kind {
            TypeCheckedStatementKind::ReturnVoid() => vec![],
            TypeCheckedStatementKind::Return(exp)
            | TypeCheckedStatementKind::Expression(exp)
            | TypeCheckedStatementKind::SetLocals(_, exp)
            | TypeCheckedStatementKind::AssignGlobal(_, exp)
            | TypeCheckedStatementKind::Assert(exp)
            | TypeCheckedStatementKind::DebugPrint(exp) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedStatementKind::While(exp, block) => vec![TypeCheckedNode::Expression(exp)]
                .into_iter()
                .chain(block.child_nodes())
                .collect(),
            TypeCheckedStatementKind::Asm(_, exps) => exps
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedStatementKind::Break(oexp, _) => {
                oexp.iter_mut().flat_map(|exp| exp.child_nodes()).collect()
            }
        }
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool {
        match &mut self.kind {
            TypeCheckedStatementKind::Asm(insns, args) => {
                insns.iter().any(|insn| insn.is_view(type_tree))
                    || args.iter_mut().any(|expr| expr.is_view(type_tree))
            }
            _ => self
                .child_nodes()
                .iter_mut()
                .any(|node| node.is_view(type_tree)),
        }
    }
    fn is_write(&mut self, type_tree: &TypeTree) -> bool {
        match &mut self.kind {
            TypeCheckedStatementKind::AssignGlobal(_, _) => true,
            TypeCheckedStatementKind::Asm(insns, args) => {
                insns.iter().any(|insn| insn.is_write(type_tree))
                    || args.iter_mut().any(|expr| expr.is_write(type_tree))
            }
            _ => self
                .child_nodes()
                .iter_mut()
                .any(|node| node.is_write(type_tree)),
        }
    }
}

/// A mini expression with associated `DebugInfo` that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeCheckedExpr {
    pub kind: TypeCheckedExprKind,
    pub debug_info: DebugInfo,
}

/// A mini expression that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeCheckedExprKind {
    NewBuffer,
    Quote(Vec<u8>),
    UnaryOp(UnaryOp, Box<TypeCheckedExpr>, Type),
    Binary(BinaryOp, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
    Trinary(
        TrinaryOp,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
    ),
    ShortcutOr(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>),
    ShortcutAnd(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>),
    LocalVariableRef(StringId, Type),
    GlobalVariableRef(StringId, Type),
    Variant(Box<TypeCheckedExpr>),
    FuncRef(StringId, Type),
    TupleRef(Box<TypeCheckedExpr>, usize, usize, Type),
    Const(Value, Type),
    FunctionCall(
        Box<TypeCheckedExpr>,
        Vec<TypeCheckedExpr>,
        Type,
        FuncProperties,
    ),
    CodeBlock(TypeCheckedCodeBlock),
    FixedArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, usize, Type),
    ClosureLoad(StringId, usize, usize, BTreeSet<StringId>, Type),
    Tuple(Vec<TypeCheckedExpr>, Type),
    NewFixedArray(usize, Box<TypeCheckedExpr>, Type),
    FixedArrayMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        usize,
        Type,
    ),
    StructMod(
        Box<TypeCheckedExpr>,
        usize,
        usize,
        Box<TypeCheckedExpr>,
        Type,
    ),
    Cast(Box<TypeCheckedExpr>, Type),
    Asm(Type, Vec<Instruction>, Vec<TypeCheckedExpr>),
    Error,
    GetGas,
    SetGas(Box<TypeCheckedExpr>),
    Try(Box<TypeCheckedExpr>, Type),
    If(
        Box<TypeCheckedExpr>,
        TypeCheckedCodeBlock,
        Option<TypeCheckedCodeBlock>,
        Type,
    ),
    IfLet(
        StringId,
        Box<TypeCheckedExpr>,
        TypeCheckedCodeBlock,
        Option<TypeCheckedCodeBlock>,
        Type,
    ),
    Loop(TypeCheckedCodeBlock, Type),
}

impl AbstractSyntaxTree for TypeCheckedExpr {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match &mut self.kind {
            TypeCheckedExprKind::LocalVariableRef(..)
            | TypeCheckedExprKind::GlobalVariableRef(..)
            | TypeCheckedExprKind::FuncRef(..)
            | TypeCheckedExprKind::ClosureLoad(..)
            | TypeCheckedExprKind::Const(..)
            | TypeCheckedExprKind::NewBuffer
            | TypeCheckedExprKind::Quote(..)
            | TypeCheckedExprKind::GetGas
            | TypeCheckedExprKind::Error => vec![],
            TypeCheckedExprKind::UnaryOp(_, exp, _)
            | TypeCheckedExprKind::Variant(exp)
            | TypeCheckedExprKind::SetGas(exp)
            | TypeCheckedExprKind::TupleRef(exp, ..)
            | TypeCheckedExprKind::NewFixedArray(_, exp, _)
            | TypeCheckedExprKind::Cast(exp, _)
            | TypeCheckedExprKind::Try(exp, _) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedExprKind::Trinary(_, a, b, c, _) => vec![
                TypeCheckedNode::Expression(a),
                TypeCheckedNode::Expression(b),
                TypeCheckedNode::Expression(c),
            ],
            TypeCheckedExprKind::Binary(_, lexp, rexp, _)
            | TypeCheckedExprKind::ShortcutOr(lexp, rexp)
            | TypeCheckedExprKind::ShortcutAnd(lexp, rexp)
            | TypeCheckedExprKind::FixedArrayRef(lexp, rexp, _, _)
            | TypeCheckedExprKind::StructMod(lexp, _, _, rexp, _) => vec![
                TypeCheckedNode::Expression(lexp),
                TypeCheckedNode::Expression(rexp),
            ],
            TypeCheckedExprKind::FunctionCall(name_exp, arg_exps, _, _) => {
                vec![TypeCheckedNode::Expression(name_exp)]
                    .into_iter()
                    .chain(
                        arg_exps
                            .iter_mut()
                            .map(|exp| TypeCheckedNode::Expression(exp)),
                    )
                    .collect()
            }
            TypeCheckedExprKind::Loop(block, _) | TypeCheckedExprKind::CodeBlock(block) => {
                block.child_nodes()
            }
            TypeCheckedExprKind::Tuple(exps, _) | TypeCheckedExprKind::Asm(_, _, exps) => exps
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedExprKind::FixedArrayMod(exp1, exp2, exp3, _, _) => vec![
                TypeCheckedNode::Expression(exp1),
                TypeCheckedNode::Expression(exp2),
                TypeCheckedNode::Expression(exp3),
            ],
            TypeCheckedExprKind::If(cond, block, else_block, _)
            | TypeCheckedExprKind::IfLet(_, cond, block, else_block, _) => {
                vec![TypeCheckedNode::Expression(cond)]
                    .into_iter()
                    .chain(block.child_nodes().into_iter())
                    .chain(
                        else_block
                            .into_iter()
                            .map(|n| n.child_nodes().into_iter())
                            .flatten(),
                    )
                    .collect()
            }
        }
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool {
        match &mut self.kind {
            TypeCheckedExprKind::FunctionCall(func, args, _, prop) => {
                prop.view
                    || func.is_view(type_tree)
                    || args.iter_mut().any(|expr| expr.is_view(type_tree))
            }
            TypeCheckedExprKind::Asm(_, insns, args) => {
                insns.iter().any(|insn| insn.is_view(type_tree))
                    || args.iter_mut().any(|expr| expr.is_view(type_tree))
            }
            TypeCheckedExprKind::GetGas | TypeCheckedExprKind::GlobalVariableRef(_, _) => true,
            _ => self
                .child_nodes()
                .iter_mut()
                .any(|node| node.is_view(type_tree)),
        }
    }
    fn is_write(&mut self, type_tree: &TypeTree) -> bool {
        match &mut self.kind {
            TypeCheckedExprKind::FunctionCall(func, args, _, prop) => {
                prop.write
                    || func.is_write(type_tree)
                    || args.iter_mut().any(|expr| expr.is_write(type_tree))
            }
            TypeCheckedExprKind::Asm(_, insns, args) => {
                insns.iter().any(|insn| insn.is_write(type_tree))
                    || args.iter_mut().any(|expr| expr.is_write(type_tree))
            }
            TypeCheckedExprKind::SetGas(_) => true,
            _ => self
                .child_nodes()
                .iter_mut()
                .any(|node| node.is_write(type_tree)),
        }
    }
}

impl TypeCheckedExpr {
    /// Creates a `TypeCheckedExpr` from its component fields
    pub fn new(kind: TypeCheckedExprKind, debug_info: DebugInfo) -> Self {
        Self { kind, debug_info }
    }

    /// Make a reference to to a builtin func with the types altered for safety.
    pub fn builtin_ref(
        name: &str,
        args: Vec<&Type>,
        ret: &Type,
        func_table: &HashMap<usize, Type>,
        string_table: &StringTable,
        debug_info: DebugInfo,
    ) -> Result<Self, CompileError> {
        let args = args.into_iter().cloned().collect();
        let ret = ret.clone();

        let error_msg = Err(CompileError::new(
            "Internal Error",
            format!("Builtin {} does not exist", name),
            debug_info.locs(),
        ));

        let builtin_id = match string_table.get_if_exists(name) {
            Some(id) => id,
            none => return error_msg,
        };

        let mut builtin_type = match func_table.get(&builtin_id) {
            Some(Type::Func(prop, ..)) => Type::Func(prop.clone(), args, Box::new(ret)),
            _ => return error_msg,
        };
        Ok(TypeCheckedExpr::new(
            TypeCheckedExprKind::FuncRef(builtin_id, builtin_type),
            debug_info,
        ))
    }

    /// Extracts the type returned from the expression.
    pub fn get_type(&self) -> Type {
        match &self.kind {
            TypeCheckedExprKind::NewBuffer => Type::Buffer,
            TypeCheckedExprKind::Quote(_) => Type::Tuple(vec![Type::Uint, Type::Buffer]),
            TypeCheckedExprKind::Error => Type::Every,
            TypeCheckedExprKind::GetGas => Type::Uint,
            TypeCheckedExprKind::SetGas(_t) => Type::Void,
            TypeCheckedExprKind::UnaryOp(_, _, t) => t.clone(),
            TypeCheckedExprKind::Binary(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::Trinary(_, _, _, _, t) => t.clone(),
            TypeCheckedExprKind::ShortcutOr(_, _) | TypeCheckedExprKind::ShortcutAnd(_, _) => {
                Type::Bool
            }
            TypeCheckedExprKind::LocalVariableRef(.., t) => t.clone(),
            TypeCheckedExprKind::GlobalVariableRef(.., t) => t.clone(),
            TypeCheckedExprKind::FuncRef(.., t) => t.clone(),
            TypeCheckedExprKind::TupleRef(.., t) => t.clone(),
            TypeCheckedExprKind::Variant(t) => Type::Option(Box::new(t.get_type())),
            TypeCheckedExprKind::Const(.., t) => t.clone(),
            TypeCheckedExprKind::FunctionCall(.., t, _) => t.clone(),
            TypeCheckedExprKind::CodeBlock(block) => block.get_type(),
            TypeCheckedExprKind::FixedArrayRef(.., t) => t.clone(),
            TypeCheckedExprKind::ClosureLoad(.., t) => t.clone(),
            TypeCheckedExprKind::Tuple(.., t) => t.clone(),
            TypeCheckedExprKind::NewFixedArray(.., t) => t.clone(),
            TypeCheckedExprKind::FixedArrayMod(.., t) => t.clone(),
            TypeCheckedExprKind::StructMod(.., t) => t.clone(),
            TypeCheckedExprKind::Cast(.., t) => t.clone(),
            TypeCheckedExprKind::Asm(t, ..) => t.clone(),
            TypeCheckedExprKind::Try(.., t) => t.clone(),
            TypeCheckedExprKind::If(.., t) => t.clone(),
            TypeCheckedExprKind::IfLet(.., t) => t.clone(),
            TypeCheckedExprKind::Loop(.., t) => t.clone(),
        }
    }
}

type TypeCheckedFieldInitializer = FieldInitializer<TypeCheckedExpr>;

impl AbstractSyntaxTree for TypeCheckedFieldInitializer {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        vec![TypeCheckedNode::Expression(&mut self.value)]
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool {
        self.value.is_view(type_tree)
    }
    fn is_write(&mut self, type_tree: &TypeTree) -> bool {
        self.value.is_write(type_tree)
    }
}

/// Returns a vector of `ImportFuncDecl`s corresponding to the builtins as defined by string_table,
/// if they are not defined in string_table, they are inserted.
fn builtin_func_decls() -> Vec<Import> {
    vec![
        Import::new_builtin("assert", "builtin_assert"),
        Import::new_builtin("array", "builtin_arrayNew"),
        Import::new_builtin("array", "builtin_arrayGet"),
        Import::new_builtin("array", "builtin_arraySet"),
        Import::new_builtin("kvs", "builtin_kvsNew"),
        Import::new_builtin("kvs", "builtin_kvsGet"),
        Import::new_builtin("kvs", "builtin_kvsSet"),
        Import::new_builtin("assert", "builtin_assert"),
    ]
}

/// Sorts the `TopLevelDecl`s into collections based on their type
pub fn sort_top_level_decls(
    parsed: (Vec<TopLevelDecl>, BTreeMap<StringId, Func>),
    file_path: Vec<String>,
    string_table: &mut StringTable,
    builtins: bool,
) -> (
    Vec<Import>,
    Vec<Func>,
    HashMap<usize, Type>,
    Vec<GlobalVar>,
    HashMap<usize, Type>,
) {
    let (decls, closures) = parsed;

    let mut imports = if builtins {
        builtin_func_decls()
            .into_iter()
            .filter(|imp| imp.path != file_path)
            .collect()
    } else {
        vec![]
    };

    // we wait till now to assign stringIDs to keep the upgrade loop happy
    for import in &mut imports {
        import.id = Some(string_table.get(import.name.clone()));
    }

    //let mut imports = vec![];
    let mut funcs = vec![];
    let mut named_types = HashMap::new();
    let mut func_table = HashMap::new();
    let mut globals = vec![];

    for decl in decls {
        match decl {
            TopLevelDecl::UseDecl(ud) => {
                imports.push(ud);
            }
            TopLevelDecl::FuncDecl(fd) => {
                func_table.insert(fd.id, fd.tipe.clone());
                funcs.push(fd);
            }
            TopLevelDecl::TypeDecl(td) => {
                named_types.insert(td.name, td.tipe);
            }
            TopLevelDecl::VarDecl(vd) => {
                globals.push(vd);
            }
            TopLevelDecl::ConstDecl => {}
        }
    }

    for (id, closure) in &closures {
        func_table.insert(*id, closure.tipe.clone());
    }

    (imports, funcs, named_types, globals, func_table)
}

/// Performs typechecking various top level declarations, `FuncDecl`s,
/// named `Type`s, and global variables.
pub fn typecheck_top_level_decls(
    funcs: Vec<Func>,
    named_types: &HashMap<usize, Type>,
    mut global_vars: Vec<GlobalVar>,
    imports: &Vec<Import>,
    string_table: StringTable,
    func_table: HashMap<usize, Type>,
    type_tree: &TypeTree,
    path: &Vec<String>,
) -> Result<
    (
        BTreeMap<StringId, TypeCheckedFunc>,
        Vec<GlobalVar>,
        StringTable,
    ),
    CompileError,
> {
    if let Some(var) = global_vars
        .iter()
        .position(|var| &var.name == "__fixedLocationGlobal")
    {
        global_vars.swap(0, var)
    }
    let global_vars_map = global_vars
        .iter()
        .map(|var| (var.id, var.tipe.clone()))
        .collect::<HashMap<_, _>>();

    let type_table: HashMap<_, _> = named_types.clone().into_iter().collect();

    let mut undefinable_ids = HashMap::new(); // ids no one is allowed to define
    for import in imports {
        undefinable_ids.insert(
            string_table.get_if_exists(&import.name).unwrap(),
            import.location,
        );
    }

    let mut checked_funcs = BTreeMap::new();
    let mut checked_closures = BTreeMap::new();

    for func in &funcs {
        let mut type_tree = type_tree.clone();
        for (index, generic) in func.generics.iter().enumerate() {
            type_tree.insert(
                (path.clone(), *generic),
                (
                    Type::Generic(index),
                    string_table.name_from_id(*generic).clone(),
                ),
            );
        }
        checked_funcs.insert(
            func.id,
            typecheck_function(
                &func,
                &type_table,
                &global_vars_map,
                &func_table,
                &type_tree,
                &string_table,
                &mut checked_closures,
                &mut undefinable_ids,
            )?,
        );
    }

    checked_funcs.extend(checked_closures);

    let mut res_global_vars = Vec::new();
    for global_var in global_vars {
        res_global_vars.push(global_var);
    }

    Ok((checked_funcs, res_global_vars, string_table))
}

/// If successful, produces a `TypeCheckedFunc` from `FuncDecl` reference fd, according to global
/// state defined by type_table, global_vars, and func_table.
///
/// If not successful the function returns a `CompileError`.
pub fn typecheck_function(
    func: &Func,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, Type>,
    func_table: &TypeTable,
    type_tree: &TypeTree,
    string_table: &StringTable,
    closures: &mut BTreeMap<StringId, TypeCheckedFunc>,
    undefinable_ids: &mut HashMap<StringId, Option<Location>>,
) -> Result<TypeCheckedFunc, CompileError> {
    let mut func = func.clone();

    func.ret_type = func.ret_type.commit_generic_slots();
    func.args
        .iter_mut()
        .for_each(|arg| arg.tipe = arg.tipe.commit_generic_slots());

    let mut hm = HashMap::new();

    if let Some(location_option) = undefinable_ids.get(&func.id) {
        return Err(CompileError::new_type_error(
            format!(
                "Func {} has the same name as another top-level symbol",
                Color::red(string_table.name_from_id(func.id)),
            ),
            location_option
                .iter()
                .chain(func.debug_info.location.iter())
                .cloned()
                .collect(),
        ));
    }
    undefinable_ids.insert(func.id, func.debug_info.location);

    for arg in func.args.iter() {
        arg.tipe.rep(type_tree).map_err(|_| {
            CompileError::new_type_error(
                format!(
                    "Func {}'s argument {} has the unknown type {}",
                    Color::red(string_table.name_from_id(func.id)),
                    Color::red(string_table.name_from_id(arg.name)),
                    Color::red(arg.tipe.print(type_tree)),
                ),
                arg.debug_info.location.into_iter().collect(),
            )
        })?;
        if let Some(location_option) = undefinable_ids.get(&arg.name) {
            return Err(CompileError::new_type_error(
                format!(
                    "Func {}'s argument {} has the same name as a top-level symbol",
                    Color::red(string_table.name_from_id(func.id)),
                    Color::red(string_table.name_from_id(arg.name)),
                ),
                location_option
                    .iter()
                    .chain(arg.debug_info.location.iter())
                    .cloned()
                    .collect(),
            ));
        }
        hm.insert(arg.name, arg.tipe.clone());
    }

    let mut inner_type_table = type_table.clone();
    inner_type_table.extend(hm);
    let mut tc_stats = typecheck_statement_sequence(
        &func.code,
        &func,
        &inner_type_table,
        global_vars,
        func_table,
        type_tree,
        string_table,
        undefinable_ids,
        closures,
        &mut vec![],
    )?;

    if func.ret_type == Type::Void {
        if tc_stats.last().cloned().map(|s| s.kind) != Some(TypeCheckedStatementKind::ReturnVoid())
        {
            tc_stats.push(TypeCheckedStatement {
                kind: TypeCheckedStatementKind::ReturnVoid(),
                debug_info: func.debug_info,
            });
        }
    } else {
        if func.code.len() == 0 {
            return Err(CompileError::new_type_error(
                format!(
                    "Func {} never returns",
                    Color::red(string_table.name_from_id(func.id))
                ),
                func.debug_info.locs(),
            ));
        }
        if let Some(stat) = func.code.last() {
            match &stat.kind {
                StatementKind::Return(_) => {}
                _ => {
                    return Err(CompileError::new_type_error(
                        format!(
                            "Func {}'s last statement does not a return a value",
                            Color::red(string_table.name_from_id(func.id)),
                        ),
                        func.debug_info
                            .location
                            .into_iter()
                            .chain(stat.debug_info.location.into_iter())
                            .collect(),
                    ))
                }
            }
        }
    }

    Ok(TypeCheckedFunc {
        name: func.name.clone(),
        id: func.id,
        args: func.args,
        ret_type: func.ret_type,
        code: tc_stats,
        tipe: func.tipe.clone(),
        public: func.public,
        captures: BTreeSet::new(),
        generics: func.generics.clone(),
        frame_size: 0,
        unique_id: func.unique_id,
        properties: func.properties,
        debug_info: DebugInfo::from(func.debug_info),
    })
}

/// If successful, produces a `Vec<TypeCheckedStatement>` corresponding to the items in statements
/// after type checking has been performed sequentially.  Bindings produced by a statement are
/// visible to all statements at a higher index, and no previous statements. If not successful, this
/// function produces a `CompileError`.
///
/// This function is not designed to handle additional variable bindings, for example arguments to
/// functions, for this use case, prefer `typecheck_statement_sequence_with_bindings`.
///
/// Takes return_type to ensure that `Return` statements produce the correct type, type_table,
/// global_vars, and func_table should correspond to the types, globals, and functions available
/// to the statement sequence.
fn typecheck_statement_sequence(
    statements: &[Statement],
    func: &Func,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, Type>,
    func_table: &TypeTable,
    type_tree: &TypeTree,
    string_table: &StringTable,
    undefinable_ids: &mut HashMap<StringId, Option<Location>>,
    closures: &mut BTreeMap<StringId, TypeCheckedFunc>,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<Vec<TypeCheckedStatement>, CompileError> {
    typecheck_statement_sequence_with_bindings(
        &statements,
        &func,
        type_table,
        global_vars,
        func_table,
        &[],
        type_tree,
        string_table,
        undefinable_ids,
        closures,
        scopes,
    )
}

/// Operates identically to `typecheck_statement_sequence`, except that the pairs in bindings are
/// added to type_table.
fn typecheck_statement_sequence_with_bindings<'a>(
    statements: &'a [Statement],
    func: &Func,
    type_table: &'a TypeTable,
    global_vars: &'a HashMap<StringId, Type>,
    func_table: &TypeTable,
    bindings: &[(StringId, Type)],
    type_tree: &TypeTree,
    string_table: &StringTable,
    undefinable_ids: &mut HashMap<StringId, Option<Location>>,
    closures: &mut BTreeMap<StringId, TypeCheckedFunc>,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<Vec<TypeCheckedStatement>, CompileError> {
    let mut inner_type_table = type_table.clone();
    for (sid, tipe) in bindings {
        inner_type_table.insert(*sid, tipe.clone());
    }
    let mut output = vec![];
    for stat in statements {
        let (tcs, bindings) = typecheck_statement(
            stat,
            &func,
            &inner_type_table,
            global_vars,
            func_table,
            type_tree,
            string_table,
            undefinable_ids,
            closures,
            scopes,
        )?;
        output.push(tcs);
        for (sid, bind) in bindings {
            inner_type_table.insert(sid, bind);
        }
    }
    Ok(output)
}

/// Performs type checking on statement.
///
/// If successful, returns tuple containing a `TypeCheckedStatement` and a `Vec<(StringId, Type)>`
/// representing the bindings produced by the statement.  Otherwise returns a `CompileError`.
///
/// The argument loc provide the correct location to `CompileError` if the function fails.
fn typecheck_statement<'a>(
    statement: &'a Statement,
    func: &Func,
    type_table: &'a TypeTable,
    global_vars: &'a HashMap<StringId, Type>,
    func_table: &TypeTable,
    type_tree: &TypeTree,
    string_table: &StringTable,
    undefinable_ids: &mut HashMap<StringId, Option<Location>>,
    closures: &mut BTreeMap<StringId, TypeCheckedFunc>,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<(TypeCheckedStatement, Vec<(StringId, Type)>), CompileError> {
    let kind = &statement.kind;
    let debug_info = statement.debug_info;
    let locs = debug_info.locs();

    macro_rules! error {
        ($text:expr $(,$args:expr)* $(,)?) => {
            return Err(CompileError::new("Typecheck error", format!($text, $(Color::red($args),)*), debug_info.locs()));
        };
    }

    let (stat, binds) = match kind {
        StatementKind::ReturnVoid() => {
            if Type::Void.assignable(&func.ret_type, type_tree, HashSet::new()) {
                Ok((TypeCheckedStatementKind::ReturnVoid(), vec![]))
            } else {
                Err(CompileError::new_type_error(
                    format!(
                        "Tried to return without type in function that returns {}",
                        Color::red(&func.ret_type.print(type_tree))
                    ),
                    debug_info.location.into_iter().collect(),
                ))
            }
        }
        StatementKind::Return(expr) => {
            let expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;

            let tipe = expr.get_type().rep(type_tree)?;
            let ret_type = func.ret_type.rep(type_tree)?;

            if ret_type.assignable(&tipe, type_tree, HashSet::new()) {
                Ok((TypeCheckedStatementKind::Return(expr), vec![]))
            } else {
                Err(CompileError::new_type_error(
                    format!(
                        "return statement has wrong type:\nencountered {}\ninstead of  {}",
                        Color::red(tipe.print(type_tree)),
                        Color::red(ret_type.print(type_tree)),
                    ),
                    debug_info.locs(),
                ))
            }
        }
        StatementKind::Break(exp, scope) => Ok((
            {
                let te = exp
                    .clone()
                    .map(|expr| {
                        typecheck_expr(
                            &expr,
                            type_table,
                            global_vars,
                            func_table,
                            func,
                            type_tree,
                            string_table,
                            undefinable_ids,
                            closures,
                            scopes,
                        )
                    })
                    .transpose()?;
                let key = scope.clone().unwrap_or("_".to_string());
                let (_name, tipe) = scopes
                    .iter_mut()
                    .rev()
                    .find(|(s, _)| key == *s)
                    .ok_or_else(|| {
                        CompileError::new_type_error(
                            "No valid scope to break from".to_string(),
                            debug_info.location.into_iter().collect(),
                        )
                    })?;
                if let Some(t) = tipe {
                    if *t
                        != te
                            .clone()
                            .map(|te| te.get_type())
                            .unwrap_or(Type::Tuple(vec![]))
                    {
                        return Err(CompileError::new_type_error(
                            format!(
                                "mismatched types in break statement {}",
                                te.map(|te| te.get_type())
                                    .unwrap_or(Type::Tuple(vec![]))
                                    .mismatch_string(
                                        &tipe.clone().unwrap_or(Type::Tuple(vec![])),
                                        type_tree
                                    )
                                    .expect("Did not find type mismatch")
                            ),
                            debug_info.locs(),
                        ));
                    } else {
                        *t = te
                            .clone()
                            .map(|te| te.get_type())
                            .unwrap_or(Type::Tuple(vec![]));
                    }
                }
                TypeCheckedStatementKind::Break(
                    exp.clone()
                        .map(|expr| {
                            typecheck_expr(
                                &expr,
                                type_table,
                                global_vars,
                                func_table,
                                func,
                                type_tree,
                                string_table,
                                undefinable_ids,
                                closures,
                                scopes,
                            )
                        })
                        .transpose()?,
                    scope.clone().unwrap_or("_".to_string()),
                )
            },
            vec![],
        )),
        StatementKind::Expression(expr) => {
            let expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;
            let tipe = expr.get_type();
            if !matches!(tipe, Type::Void | Type::Every) {
                error!("Statement discards {} value", tipe.print(type_tree));
            }
            Ok((TypeCheckedStatementKind::Expression(expr), vec![]))
        }
        StatementKind::Let(assigned, expr) => {
            let expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;

            let types = match expr.get_type() {
                Type::Tuple(vec) if assigned.len() > 1 => vec.clone(),
                x => vec![x.clone()],
            };

            if types.len() != assigned.len() {
                return Err(CompileError::new_type_error(
                    format!(
                        "Left side needs {} items but right has {}",
                        Color::red(assigned.len()),
                        Color::red(types.len()),
                    ),
                    locs,
                ));
            }

            let mut bindings = vec![];

            for (assigned, tipe) in assigned.iter().zip(types.into_iter()) {
                let id = assigned.id;
                let name = Color::red(string_table.name_from_id(id));

                if let Some(location_option) = undefinable_ids.get(&id) {
                    Err(CompileError::new_type_error(
                        String::from("Variable has the same name as a top-level symbol"),
                        location_option
                            .iter()
                            .chain(statement.debug_info.location.iter())
                            .cloned()
                            .collect(),
                    ))?
                }

                if let Some(_) = global_vars.get(&id) {
                    Err(CompileError::new_type_error(
                        match assigned.shadow {
                            true => format!("Tried to shadow global variable {}", name),
                            false => format!("Tried to let-assign global variable {}", name),
                        },
                        assigned.debug_info.locs(),
                    ))?
                }

                if tipe == Type::Void {
                    Err(CompileError::new_type_error(
                        format!("Tried to assign something void to {}", name),
                        assigned.debug_info.locs(),
                    ))?
                }

                if assigned.shadow {
                    bindings.push((id, tipe));
                } else {
                    match type_table.get(&id) {
                        Some(var_type) => {
                            if !var_type.assignable(&tipe, type_tree, HashSet::new()) {
                                Err(CompileError::new_type_error(
                                    format!(
                                        "Cannot make assignment for {}\n {}",
                                        name,
                                        var_type
                                            .mismatch_string(&tipe, type_tree)
                                            .unwrap_or("Did not find mismatch".to_string())
                                    ),
                                    assigned.debug_info.locs(),
                                ))?
                            }
                        }
                        None => Err(CompileError::new_type_error(
                            format!("assignment to undeclared variable {}", name),
                            assigned.debug_info.locs(),
                        ))?,
                    }
                }
            }

            let stat = TypeCheckedStatementKind::SetLocals(assigned.clone(), expr);

            Ok((stat, bindings))
        }
        StatementKind::Assign(id, expr) => {
            let expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;

            let tipe = expr.get_type().rep(type_tree)?;

            if let Some(var_type) = type_table.get(id) {
                if var_type.assignable(&tipe, type_tree, HashSet::new()) {
                    let assigned = vec![AssignRef::new(*id, false, debug_info)];
                    Ok((TypeCheckedStatementKind::SetLocals(assigned, expr), vec![]))
                } else {
                    Err(CompileError::new_type_error(
                        format!(
                            "mismatched types in assignment statement {}",
                            var_type
                                .mismatch_string(&tipe, type_tree)
                                .unwrap_or("Did not find mismatch".to_string())
                        ),
                        locs,
                    ))
                }
            } else if let Some(var_type) = global_vars.get(id) {
                if var_type.assignable(&tipe, type_tree, HashSet::new()) {
                    Ok((TypeCheckedStatementKind::AssignGlobal(*id, expr), vec![]))
                } else {
                    Err(CompileError::new_type_error(
                        format!(
                            "mismatched types in assignment statement {}",
                            var_type
                                .mismatch_string(&tipe, type_tree)
                                .unwrap_or("Did not find mismatch".to_string())
                        ),
                        locs,
                    ))
                }
            } else {
                Err(CompileError::new_type_error(
                    format!(
                        "assignment to undeclared variable {}",
                        Color::red(string_table.name_from_id(*id))
                    ),
                    locs,
                ))
            }
        }
        StatementKind::While(cond, body) => {
            let tc_cond = typecheck_expr(
                cond,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;
            match tc_cond.get_type() {
                Type::Bool => {
                    let tc_body = typecheck_codeblock(
                        body,
                        type_table,
                        global_vars,
                        func_table,
                        func,
                        type_tree,
                        string_table,
                        undefinable_ids,
                        closures,
                        scopes,
                    )?;
                    Ok((TypeCheckedStatementKind::While(tc_cond, tc_body), vec![]))
                }
                _ => Err(CompileError::new_type_error(
                    format!(
                        "while condition must be bool, found {}",
                        Color::red(tc_cond.get_type().print(type_tree))
                    ),
                    debug_info.location.into_iter().collect(),
                )),
            }
        }
        StatementKind::Asm(insns, unchecked_args) => {
            let mut args = vec![];
            for (index, unchecked) in unchecked_args.into_iter().enumerate() {
                let arg = typecheck_expr(
                    unchecked,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;

                if arg.get_type().rep(type_tree)? == Type::Void {
                    error!("Asm's {} arg is void", human_readable_index(index + 1));
                }
                
                args.push(arg);
            }
            Ok((
                TypeCheckedStatementKind::Asm(insns.to_vec(), args),
                vec![],
            ))
        }
        StatementKind::DebugPrint(e) => {
            let tce = typecheck_expr(
                e,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;
            Ok((TypeCheckedStatementKind::DebugPrint(tce), vec![]))
        }
        StatementKind::Assert(expr) => {
            let tce = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?;
            match tce.get_type() {
                Type::Tuple(vec) if vec.len() == 2 && vec[0] == Type::Bool => {
                    Ok((TypeCheckedStatementKind::Assert(tce), vec![]))
                }
                _ => Err(CompileError::new_type_error(
                    format!(
                        "assert condition must be of type (bool, any), found {}",
                        Color::red(tce.get_type().print(type_tree))
                    ),
                    debug_info.location.into_iter().collect(),
                )),
            }
        }
    }?;
    Ok((
        TypeCheckedStatement {
            kind: stat,
            debug_info,
        },
        binds,
    ))
}

/// Performs type checking on the expression expr.  Returns `TypeCheckedExpr` if successful, and
/// `CompileError` otherwise.
///
/// The arguments type_table, global_vars, and func_table represent the variables, globals, and
/// functions available to the expression, and return_type represents the return type of the
/// containing function. This last argument is needed as Try and CodeBlock expressions may return
/// from the function.
fn typecheck_expr(
    expr: &Expr,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, Type>,
    func_table: &TypeTable,
    func: &Func,
    type_tree: &TypeTree,
    string_table: &StringTable,
    undefinable_ids: &mut HashMap<StringId, Option<Location>>,
    closures: &mut BTreeMap<StringId, TypeCheckedFunc>,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<TypeCheckedExpr, CompileError> {
    let debug_info = expr.debug_info;
    let loc = debug_info.location;

    macro_rules! error {
        ($text:expr $(,$args:expr)* $(,)?) => {
            return Err(CompileError::new("Typecheck error", format!($text, $(Color::red($args),)*), debug_info.locs()));
        };
    }

    Ok(TypeCheckedExpr {
        kind: match &expr.kind {
            ExprKind::NewBuffer => Ok(TypeCheckedExprKind::NewBuffer),
            ExprKind::Quote(buf) => Ok(TypeCheckedExprKind::Quote(buf.clone())),
            ExprKind::Error => Ok(TypeCheckedExprKind::Error),
            ExprKind::UnaryOp(op, subexpr) => {
                let tc_sub = typecheck_expr(
                    subexpr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                typecheck_unary_op(*op, tc_sub, loc, type_tree)
            }
            ExprKind::Binary(op, sub1, sub2) => {
                let tc_sub1 = typecheck_expr(
                    sub1,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                typecheck_binary_op(*op, tc_sub1, tc_sub2, type_tree, loc)
            }
            ExprKind::Trinary(op, sub1, sub2, sub3) => {
                let tc_sub1 = typecheck_expr(
                    sub1,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tc_sub3 = typecheck_expr(
                    sub3,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                typecheck_trinary_op(*op, tc_sub1, tc_sub2, tc_sub3, type_tree, loc)
            }
            ExprKind::ShortcutOr(sub1, sub2) => {
                let tc_sub1 = typecheck_expr(
                    sub1,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                if (tc_sub1.get_type(), tc_sub2.get_type()) != (Type::Bool, Type::Bool) {
                    error!(
                        "operands to logical or must be boolean, got {} and {}",
                        tc_sub1.get_type().print(type_tree),
                        tc_sub2.get_type().print(type_tree),
                    );
                }
                Ok(TypeCheckedExprKind::ShortcutOr(
                    Box::new(tc_sub1),
                    Box::new(tc_sub2),
                ))
            }
            ExprKind::ShortcutAnd(sub1, sub2) => {
                let tc_sub1 = typecheck_expr(
                    sub1,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                if (tc_sub1.get_type(), tc_sub2.get_type()) != (Type::Bool, Type::Bool) {
                    error!(
                        "operands to logical and must be boolean, got {} and {}",
                        tc_sub1.get_type().print(type_tree),
                        tc_sub2.get_type().print(type_tree)
                    );
                }
                Ok(TypeCheckedExprKind::ShortcutAnd(
                    Box::new(tc_sub1),
                    Box::new(tc_sub2),
                ))
            }
            ExprKind::OptionInitializer(inner) => {
                Ok(TypeCheckedExprKind::Variant(Box::new(typecheck_expr(
                    inner,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?)))
            }
            ExprKind::VariableRef(id, spec) => {
                if let Some(tipe) = func_table.get(id) {
                    let template_type = tipe.rep(type_tree)?;
                    let num_generic_params = tipe.count_generic_slots();

                    if spec.len() != num_generic_params {
                        return Err(CompileError::new(
                            "Generics error",
                            format!(
                                "Func {} has {} generic args but was passed {}",
                                Color::red(string_table.name_from_id(*id)),
                                Color::red(num_generic_params),
                                Color::red(spec.len()),
                            ),
                            debug_info.locs(),
                        ));
                    }

                    let tipe = template_type.make_specific(spec)?;
                    Ok(TypeCheckedExprKind::FuncRef(*id, tipe))
                } else if let Some(tipe) = type_table.get(id) {
                    if !spec.is_empty() {
                        return Err(CompileError::new(
                            "Generics error",
                            format!(
                                "Variable {} doesn't need specialization",
                                Color::red(string_table.name_from_id(*id)),
                            ),
                            debug_info.locs(),
                        ));
                    }

                    let tipe = tipe.rep(type_tree)?;
                    Ok(TypeCheckedExprKind::LocalVariableRef(*id, tipe))
                } else if let Some(tipe) = global_vars.get(id) {
                    let tipe = tipe.rep(type_tree)?;

                    if !spec.is_empty() {
                        return Err(CompileError::new(
                            "Generics error",
                            format!(
                                "Global variable {} doesn't need specialization",
                                Color::red(string_table.name_from_id(*id)),
                            ),
                            debug_info.locs(),
                        ));
                    }

                    Ok(TypeCheckedExprKind::GlobalVariableRef(*id, tipe))
                } else {
                    error!(
                        "reference to unrecognized identifier {}",
                        string_table.name_from_id(*id)
                    );
                }
            }
            ExprKind::TupleRef(tuple_expr, offset_value) => {
                let tuple_expr = typecheck_expr(
                    &*tuple_expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let offset = offset_value.to_usize().unwrap();

                let tipe = match tuple_expr.get_type().rep(type_tree)? {
                    Type::Tuple(tup) => tup,
                    wrong => error!("{} isn't a tuple", wrong.print(type_tree)),
                };

                if offset >= tipe.len() {
                    error!("tuple is not wide enough")
                }

                Ok(TypeCheckedExprKind::TupleRef(
                    Box::new(tuple_expr),
                    offset,
                    tipe.len(),
                    tipe[offset].clone(),
                ))
            }
            ExprKind::DotRef(expr, name) => {
                let expr = typecheck_expr(
                    &*expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tipe = expr.get_type().rep(type_tree)?;

                let fields = match &tipe {
                    Type::Struct(fields) => fields,
                    _ => {
                        error!(
                            "can't lookup .{} for non-struct {}",
                            name,
                            tipe.print(type_tree)
                        )
                    }
                };

                let slot = fields.into_iter().position(|field| &field.name == name);

                let slot = match slot {
                    Some(slot) => slot,
                    None => {
                        error!("There's no field .{} in {}", name, tipe.print(type_tree))
                    }
                };

                Ok(TypeCheckedExprKind::TupleRef(
                    Box::new(expr),
                    slot,
                    fields.len(),
                    fields[slot].tipe.clone(),
                ))
            }
            ExprKind::StructMod(struc, name, item) => {
                let struc = typecheck_expr(
                    struc,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let item = typecheck_expr(
                    item,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;

                let struc_type = struc.get_type().rep(type_tree)?;
                let item_type = item.get_type().rep(type_tree)?;

                let fields = match &struc_type {
                    Type::Struct(fields) => fields,
                    _ => {
                        error!(
                            "can't edit .{} for non-struct {}",
                            name,
                            struc_type.print(type_tree)
                        )
                    }
                };

                let slot = fields.into_iter().position(|field| &field.name == name);

                let slot = match slot {
                    Some(slot) => slot,
                    None => {
                        error!(
                            "There's no field .{} in {}",
                            name,
                            struc_type.print(type_tree)
                        )
                    }
                };

                let field_type = &fields[slot].tipe;

                if !item_type.assignable(&field_type, type_tree, HashSet::new()) {
                    error!(
                        "incorrect value type in struct modifier, {}",
                        field_type
                            .mismatch_string(&item_type, type_tree)
                            .unwrap_or("Did not find type mismatch".to_string())
                    );
                }

                Ok(TypeCheckedExprKind::StructMod(
                    Box::new(struc),
                    slot,
                    fields.len(),
                    Box::new(item),
                    struc_type,
                ))
            }
            ExprKind::Constant(constant) => Ok(match constant {
                Constant::Uint(n) => TypeCheckedExprKind::Const(Value::Int(n.clone()), Type::Uint),
                Constant::Int(n) => TypeCheckedExprKind::Const(Value::Int(n.clone()), Type::Int),
                Constant::Bool(b) => {
                    TypeCheckedExprKind::Const(Value::Int(Uint256::from_bool(*b)), Type::Bool)
                }
                Constant::Option(o) => TypeCheckedExprKind::Const(o.value(), o.type_of()),
            }),
            ExprKind::FunctionCall(expr, args) => {
                let expr = typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;

                let args = args
                    .iter()
                    .map(|arg| {
                        typecheck_expr(
                            arg,
                            type_table,
                            global_vars,
                            func_table,
                            func,
                            type_tree,
                            string_table,
                            undefinable_ids,
                            closures,
                            scopes,
                        )
                    })
                    .collect::<Result<_, _>>()?;

                Ok(build_function_call(expr, args, string_table, type_tree)?)
            }
            ExprKind::CodeBlock(block) => Ok(TypeCheckedExprKind::CodeBlock(typecheck_codeblock(
                block,
                &type_table,
                global_vars,
                func_table,
                func,
                type_tree,
                string_table,
                undefinable_ids,
                closures,
                scopes,
            )?)),
            ExprKind::Closure(closure_func) => {
                let mut closure_func = closure_func.clone();

                // a closures inherits its parent's generics
                closure_func.generics = func.generics.clone();

                let id = closure_func.id;
                let tipe = closure_func.tipe.clone();

                // The closure must capture only variables that are in scope above it.
                // The type table as of this moment includes exactly these variables.
                let capture_table = type_table;

                let mut closure = typecheck_function(
                    &closure_func,
                    &capture_table,
                    global_vars,
                    func_table,
                    type_tree,
                    string_table,
                    closures,
                    undefinable_ids,
                )?;

                fn find_captures(
                    mut nodes: Vec<TypeCheckedNode>, // nodes of the same scope
                    mut local: HashSet<StringId>,    // those that have been defined locally
                ) -> BTreeSet<StringId> {
                    let mut captures = BTreeSet::new(); // non-local variables we use

                    for node in &mut nodes {
                        match node {
                            TypeCheckedNode::Statement(stat) => match &mut stat.kind {
                                TypeCheckedStatementKind::SetLocals(assigned, ref mut expr) => {
                                    let right = vec![TypeCheckedNode::Expression(expr)];
                                    captures.extend(find_captures(right, local.clone()));
                                    local.extend(assigned.iter().map(|x| x.id));
                                    continue;
                                }
                                _ => {}
                            },
                            TypeCheckedNode::Expression(expr) => match &mut expr.kind {
                                TypeCheckedExprKind::IfLet(id, ..) => {
                                    local.insert(*id);
                                }
                                TypeCheckedExprKind::LocalVariableRef(id, _tipe) => {
                                    if !local.contains(&id) {
                                        captures.insert(*id);
                                    }
                                }
                                _ => {}
                            },
                            TypeCheckedNode::Type(_) => continue,
                        }

                        captures.extend(find_captures(node.child_nodes(), local.clone()));
                    }

                    captures
                }

                fn all_idents(
                    mut nodes: Vec<TypeCheckedNode>, // nodes of the same scope
                ) -> HashSet<StringId> {
                    let mut idents = HashSet::new();

                    for node in &mut nodes {
                        match node {
                            TypeCheckedNode::Statement(stat) => match &mut stat.kind {
                                TypeCheckedStatementKind::SetLocals(assigned, ..) => {
                                    let ids = assigned.iter().map(|x| x.id);
                                    idents.extend(ids);
                                }
                                _ => {}
                            },
                            TypeCheckedNode::Expression(expr) => match &mut expr.kind {
                                TypeCheckedExprKind::IfLet(id, ..) => {
                                    idents.insert(*id);
                                }
                                TypeCheckedExprKind::LocalVariableRef(id, ..) => {
                                    idents.insert(*id);
                                }
                                _ => {}
                            },
                            TypeCheckedNode::Type(_) => continue,
                        }
                        idents.extend(all_idents(node.child_nodes()));
                    }
                    idents
                }

                let args: HashSet<StringId> = closure.args.iter().map(|arg| arg.name).collect();
                let arg_count = args.len();
                let captures = find_captures(closure.child_nodes(), args);

                // We don't yet have an AST-walker that efficiently assigns slot numbers.
                // The plan is to soon do this: instead of determining slot assignments for
                // locals, args, and captures at codegen, we'll analyze the AST to efficiently pick
                // a position for each identifier. Then, ClosureLoad(), MakeFrame(), and non-closure
                // functions in general will both cost less gas and pay down technical debt.
                //
                // So, for the very short term, we'll just take the maximum use possible.
                let frame_size = arg_count + all_idents(closure.child_nodes()).len() + 1;

                closure.frame_size = frame_size;
                closure.captures = captures.clone();
                closures.insert(id, closure);

                // We only get one opportunity to load in the captured values,
                // so we do that at declaration.
                Ok(TypeCheckedExprKind::ClosureLoad(
                    id, arg_count, frame_size, captures, tipe,
                ))
            }
            ExprKind::NewArray(size_expr, tipe) => {
                let size_expr = typecheck_expr(
                    &size_expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;

                let fill = TypeCheckedExpr::new(
                    TypeCheckedExprKind::Const(tipe.default_value(type_tree), tipe.clone()),
                    debug_info,
                );

                // In order to best simulate a call to the builtin, we alter the signature
                //   In array.mini   func builtin_arrayNew(uint, any) -> Array
                //   Best effort     func builtin_arrayNew(uint, v) -> []v

                let builtin_ref = TypeCheckedExpr::builtin_ref(
                    "builtin_arrayNew",
                    vec![&Type::Uint, tipe],
                    &Type::Array(Box::new(tipe.clone())),
                    func_table,
                    string_table,
                    debug_info,
                )?;

                Ok(build_function_call(
                    builtin_ref,
                    vec![size_expr, fill],
                    string_table,
                    type_tree,
                )?)
            }
            ExprKind::NewFixedArray(size, expr) => {
                let expr = typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tipe = expr.get_type();
                Ok(TypeCheckedExprKind::NewFixedArray(
                    *size,
                    Box::new(expr.clone()),
                    Type::FixedArray(Box::new(tipe), *size),
                ))
            }
            ExprKind::NewUnion(types, expr) => {
                let tc_expr = typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tc_type = tc_expr.get_type();
                if types
                    .iter()
                    .any(|t| t.assignable(&tc_type, type_tree, HashSet::new()))
                {
                    Ok(TypeCheckedExprKind::Cast(
                        Box::new(tc_expr),
                        Type::Union(types.clone()),
                    ))
                } else {
                    error!(
                        "Type {} is not a member of type union: {}",
                        tc_type.print(type_tree),
                        Type::Union(types.clone()).print(type_tree)
                    );
                }
            }
            ExprKind::Tuple(fields) => {
                let mut tc_fields = Vec::new();
                let mut types = Vec::new();
                for field in fields {
                    let tc_field = typecheck_expr(
                        field,
                        type_table,
                        global_vars,
                        func_table,
                        func,
                        type_tree,
                        string_table,
                        undefinable_ids,
                        closures,
                        scopes,
                    )?;
                    types.push(tc_field.get_type().clone());
                    tc_fields.push(tc_field);
                }
                Ok(TypeCheckedExprKind::Tuple(tc_fields, Type::Tuple(types)))
            }
            ExprKind::StructInitializer(fieldvec) => {
                let mut fields = vec![];
                let mut types = vec![];
                for field in fieldvec {
                    let expr = typecheck_expr(
                        &field.value,
                        type_table,
                        global_vars,
                        func_table,
                        func,
                        type_tree,
                        string_table,
                        undefinable_ids,
                        closures,
                        scopes,
                    )?;
                    types.push(StructField::new(field.name.clone(), expr.get_type()));
                    fields.push(expr);
                }
                Ok(TypeCheckedExprKind::Tuple(fields, Type::Struct(types)))
            }
            ExprKind::NewMap(key_type, value_type) => {
                // In order to best simulate a call to the builtin, we alter the signature
                //   In kvs.mini   func builtin_kvsNew() -> Kvs
                //   Best effort   func builtin_kvsNew() -> map<k,v>

                let builtin_ref = TypeCheckedExpr::builtin_ref(
                    "builtin_kvsNew",
                    vec![],
                    &Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone())),
                    func_table,
                    string_table,
                    debug_info,
                )?;

                Ok(build_function_call(
                    builtin_ref,
                    vec![],
                    string_table,
                    type_tree,
                )?)
            }
            ExprKind::ArrayOrMapRef(unchecked_store, unchecked_key) => {
                let store = typecheck_expr(
                    &*unchecked_store,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let key = typecheck_expr(
                    &*unchecked_key,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;

                let store_type = store.get_type().rep(type_tree)?;
                let key_type = key.get_type().rep(type_tree)?;

                match store_type.clone() {
                    Type::FixedArray(inner_type, size) => {
                        if key_type != Type::Uint {
                            error!(
                                "{} index must be uint, found {}",
                                "[]",
                                key_type.print(type_tree)
                            );
                        }

                        Ok(TypeCheckedExprKind::FixedArrayRef(
                            Box::new(store),
                            Box::new(key),
                            size,
                            *inner_type,
                        ))
                    }
                    Type::Array(inner_type) => {
                        // In order to best simulate a call to the builtin, we alter the signature
                        //   In array.mini   func builtin_arrayGet(Array, uint) -> any
                        //   Best effort     func builtin_arrayGet([]v, uint) -> v

                        let builtin_ref = TypeCheckedExpr::builtin_ref(
                            "builtin_arrayGet",
                            vec![&store_type, &Type::Uint],
                            &*inner_type,
                            func_table,
                            string_table,
                            debug_info,
                        )?;

                        Ok(build_function_call(
                            builtin_ref,
                            vec![store, key],
                            string_table,
                            type_tree,
                        )?)
                    }
                    Type::Map(store_key_type, store_value_type) => {
                        if !store_key_type.assignable(&key_type, type_tree, HashSet::new()) {
                            error!(
                                "tried to {} lookup {} in map with {} keys",
                                "[]",
                                key_type.print(type_tree),
                                store_key_type.print(type_tree),
                            );
                        }

                        // In order to best simulate a call to the builtin, we alter the signature
                        //   In array.mini   func builtin_kvsGet(Kvs, k') -> option<any>
                        //   Best effort     func builtin_kvsGet(map<k,v>, k) -> option<v>

                        let builtin_ref = TypeCheckedExpr::builtin_ref(
                            "builtin_kvsGet",
                            vec![&store_type, &*store_key_type],
                            &Type::Option(store_value_type),
                            func_table,
                            string_table,
                            debug_info,
                        )?;

                        Ok(build_function_call(
                            builtin_ref,
                            vec![store, key],
                            string_table,
                            type_tree,
                        )?)
                    }
                    _ => error!(
                        "{} lookup in non-array & non-map type {}",
                        "[]",
                        store_type.print(type_tree)
                    ),
                }
            }
            ExprKind::ArrayOrMapMod(unchecked_store, unchecked_key, unchecked_item) => {
                let store = typecheck_expr(
                    unchecked_store,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let key = typecheck_expr(
                    unchecked_key,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let item = typecheck_expr(
                    unchecked_item,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;

                let store_type = store.get_type().rep(type_tree)?;
                let key_type = key.get_type().rep(type_tree)?;
                let item_type = item.get_type().rep(type_tree)?;

                match store_type.clone() {
                    Type::FixedArray(inner_type, size) => {
                        if key_type != Type::Uint {
                            error!(
                                "array modifier requires {} index, found {}",
                                "uint",
                                key_type.print(type_tree)
                            );
                        }
                        if !inner_type.assignable(&item_type, type_tree, HashSet::new()) {
                            error!(
                                "fixed array doesn't have this type, {}",
                                inner_type
                                    .mismatch_string(&item_type, type_tree)
                                    .unwrap_or("Did not find type mismatch".to_string()),
                            );
                        }
                        Ok(TypeCheckedExprKind::FixedArrayMod(
                            Box::new(store),
                            Box::new(key),
                            Box::new(item),
                            size,
                            Type::FixedArray(inner_type, size),
                        ))
                    }
                    Type::Array(inner_type) => {
                        if !inner_type.assignable(&item_type, type_tree, HashSet::new()) {
                            error!(
                                "mismatched types in array modifier, {}",
                                inner_type
                                    .mismatch_string(&item_type, type_tree)
                                    .unwrap_or("Did not find type mismatch".to_string())
                            );
                        }

                        // In order to best simulate a call to the builtin, we alter the signature
                        //   In array.mini   func builtin_arraySet(Array, uint, any) -> Array
                        //   Best effort     func builtin_arraySet([]v, uint, v) -> []v

                        let builtin_ref = TypeCheckedExpr::builtin_ref(
                            "builtin_arraySet",
                            vec![&store_type, &Type::Uint, &*inner_type],
                            &store_type,
                            func_table,
                            string_table,
                            debug_info,
                        )?;

                        Ok(build_function_call(
                            builtin_ref,
                            vec![store, key, item],
                            string_table,
                            type_tree,
                        )?)
                    }
                    Type::Map(store_key_type, store_value_type) => {
                        if !store_key_type.assignable(&key_type, type_tree, HashSet::new()) {
                            error!(
                                "tried to {} lookup {} in map with {} keys",
                                "[]",
                                key_type.print(type_tree),
                                store_key_type.print(type_tree),
                            );
                        }

                        if !store_value_type.assignable(&item_type, type_tree, HashSet::new()) {
                            error!(
                                "tried to {} set {} in map with {} values",
                                "[]",
                                item_type.print(type_tree),
                                store_value_type.print(type_tree),
                            );
                        }

                        // In order to best simulate a call to the builtin, we alter the signature
                        //   In kvs.mini   func builtin_kvsSet(Kvs, any, any) -> Kvs
                        //   Best effort   func builtin_kvsSet(Map<k,v>, k', v') -> Map<k,v>

                        let builtin_ref = TypeCheckedExpr::builtin_ref(
                            "builtin_kvsSet",
                            vec![&store_type, &*store_key_type, &*store_value_type],
                            &store_type,
                            func_table,
                            string_table,
                            debug_info,
                        )?;

                        Ok(build_function_call(
                            builtin_ref,
                            vec![store, key, item],
                            string_table,
                            type_tree,
                        )?)
                    }
                    other => error!(
                        "{} mod must operate on array or block, found {}",
                        "[]",
                        other.print(type_tree)
                    ),
                }
            }
            ExprKind::Cast(expr, t) => {
                let expr = typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                if t.castable(&expr.get_type(), type_tree, HashSet::new()) {
                    Ok(TypeCheckedExprKind::Cast(Box::new(expr), t.clone()))
                } else {
                    error!(
                        "Cannot cast from type {} to type {}",
                        expr.get_type().print(type_tree),
                        t.print(type_tree)
                    );
                }
            }
            ExprKind::UnsafeCast(expr, t) => Ok(TypeCheckedExprKind::Cast(
                Box::new(typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?),
                t.clone(),
            )),
            ExprKind::Asm(ret_type, insns, args) => {
                if *ret_type == Type::Void {
                    error!("asm expression cannot return void");
                }
                let mut tc_args = Vec::new();
                for arg in args {
                    tc_args.push(typecheck_expr(
                        arg,
                        type_table,
                        global_vars,
                        func_table,
                        func,
                        type_tree,
                        string_table,
                        undefinable_ids,
                        closures,
                        scopes,
                    )?);
                }
                Ok(TypeCheckedExprKind::Asm(
                    ret_type.clone(),
                    insns.to_vec(),
                    tc_args,
                ))
            }
            ExprKind::Try(inner) => {
                match &func.ret_type {
                    Type::Option(_) | Type::Any => {}
                    ret => {
                        error!(
                            "Can only use {} operator in functions that can return option, found {}",
                            "?",
                            ret.print(type_tree),
                        );
                    }
                }
                let res = typecheck_expr(
                    inner,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                match res.get_type().rep(type_tree)? {
                    Type::Option(t) => Ok(TypeCheckedExprKind::Try(Box::new(res), *t)),
                    other => error!(
                        "Try expression requires option type, found {}",
                        other.print(type_tree)
                    ),
                }
            }
            ExprKind::GetGas => Ok(TypeCheckedExprKind::GetGas),
            ExprKind::SetGas(expr) => {
                let expr = typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                if expr.get_type() != Type::Uint {
                    error!(
                        "SetGas(_) requires a uint, found a {}",
                        expr.get_type().print(type_tree)
                    );
                } else {
                    Ok(TypeCheckedExprKind::SetGas(Box::new(expr)))
                }
            }
            ExprKind::If(cond, block, else_block) => {
                let cond_expr = typecheck_expr(
                    cond,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let block = typecheck_codeblock(
                    block,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let else_block = else_block
                    .clone()
                    .map(|e| {
                        typecheck_codeblock(
                            &e,
                            type_table,
                            global_vars,
                            func_table,
                            func,
                            type_tree,
                            string_table,
                            undefinable_ids,
                            closures,
                            scopes,
                        )
                    })
                    .transpose()?;
                if cond_expr.get_type() != Type::Bool {
                    error!(
                        "Condition of if expression must be bool: found {}",
                        cond_expr.get_type().print(type_tree)
                    );
                } else {
                    let block_type = block.get_type();
                    let else_type = else_block
                        .clone()
                        .map(|b| b.get_type())
                        .unwrap_or(Type::Void);
                    let if_type = if block_type.assignable(&else_type, type_tree, HashSet::new()) {
                        block_type
                    } else if else_type.assignable(&block_type, type_tree, HashSet::new()) {
                        else_type
                    } else {
                        error!(
                            "Mismatch of if and else types found: {} and {}",
                            block_type.print(type_tree),
                            else_type.print(type_tree)
                        );
                    };
                    Ok(TypeCheckedExprKind::If(
                        Box::new(cond_expr),
                        block,
                        else_block,
                        if_type,
                    ))
                }
            }
            ExprKind::IfLet(l, r, if_block, else_block) => {
                let tcr = typecheck_expr(
                    r,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let tct = match tcr.get_type() {
                    Type::Option(t) => *t,
                    unexpected => {
                        error!("Expected option type got: {}", unexpected.print(type_tree));
                    }
                };
                let mut inner_type_table = type_table.clone();
                inner_type_table.insert(*l, tct);
                let checked_block = typecheck_codeblock(
                    if_block,
                    &inner_type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                let checked_else = else_block
                    .clone()
                    .map(|block| {
                        typecheck_codeblock(
                            &block,
                            type_table,
                            global_vars,
                            func_table,
                            func,
                            type_tree,
                            string_table,
                            undefinable_ids,
                            closures,
                            scopes,
                        )
                    })
                    .transpose()?;
                let block_type = checked_block.get_type();
                let else_type = checked_else
                    .clone()
                    .map(|b| b.get_type())
                    .unwrap_or(Type::Void);
                let if_let_type = if block_type.assignable(&else_type, type_tree, HashSet::new()) {
                    block_type
                } else if else_type.assignable(&block_type, type_tree, HashSet::new()) {
                    else_type
                } else {
                    error!(
                        "Mismatch of if and else types found: {} and {}",
                        block_type.print(type_tree),
                        else_type.print(type_tree)
                    );
                };
                Ok(TypeCheckedExprKind::IfLet(
                    *l,
                    Box::new(tcr),
                    checked_block,
                    checked_else,
                    if_let_type,
                ))
            }
            ExprKind::Loop(block, tipe) => {
                let expr = typecheck_codeblock(
                    block,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                Ok(TypeCheckedExprKind::Loop(expr, tipe.clone()))
            }
            ExprKind::UnionCast(expr, tipe) => {
                let tc_expr = typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )?;
                if let Type::Union(types) = tc_expr.get_type().rep(type_tree)? {
                    if types.iter().any(|t| t == tipe) {
                        Ok(TypeCheckedExprKind::Cast(Box::new(tc_expr), tipe.clone()))
                    } else {
                        error!(
                            "Type {} is not a member of {}",
                            tipe.print(type_tree),
                            tc_expr.get_type().print(type_tree)
                        );
                    }
                } else {
                    error!(
                        "Tried to unioncast from non-union type {}",
                        tc_expr.get_type().print(type_tree)
                    );
                }
            }
        }?,
        debug_info,
    })
}

/// Attempts to apply the `UnaryOp` op, to `TypeCheckedExpr` sub_expr, producing a `TypeCheckedExpr`
/// if successful, and a `CompileError` otherwise.  The argument loc is used to record the location of
/// op for use in formatting the `CompileError`.
fn typecheck_unary_op(
    op: UnaryOp,
    sub_expr: TypeCheckedExpr,
    loc: Option<Location>,
    type_tree: &TypeTree,
) -> Result<TypeCheckedExprKind, CompileError> {
    let tc_type = sub_expr.get_type().rep(type_tree)?;
    match op {
        UnaryOp::Minus => match tc_type {
            Type::Int => {
                if let TypeCheckedExprKind::Const(Value::Int(ui), _) = sub_expr.kind {
                    Ok(TypeCheckedExprKind::Const(
                        Value::Int(ui.unary_minus().unwrap()),
                        Type::Int,
                    ))
                } else {
                    Ok(TypeCheckedExprKind::UnaryOp(
                        UnaryOp::Minus,
                        Box::new(sub_expr),
                        Type::Int,
                    ))
                }
            }
            other => Err(CompileError::new_type_error(
                format!(
                    "invalid operand type {} for unary minus",
                    Color::red(other.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        UnaryOp::BitwiseNeg => {
            if let TypeCheckedExprKind::Const(Value::Int(ui), _) = sub_expr.kind {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 => Ok(TypeCheckedExprKind::Const(
                        Value::Int(ui.bitwise_neg()),
                        tc_type,
                    )),
                    other => Err(CompileError::new_type_error(
                        format!(
                            "invalid operand type {} for bitwise negation",
                            Color::red(other.print(type_tree))
                        ),
                        loc.into_iter().collect(),
                    )),
                }
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 => Ok(TypeCheckedExprKind::UnaryOp(
                        UnaryOp::BitwiseNeg,
                        Box::new(sub_expr),
                        tc_type,
                    )),
                    other => Err(CompileError::new_type_error(
                        format!(
                            "invalid operand type {} for bitwise negation",
                            Color::red(other.print(type_tree))
                        ),
                        loc.into_iter().collect(),
                    )),
                }
            }
        }
        UnaryOp::Not => match tc_type {
            Type::Bool => {
                if let TypeCheckedExprKind::Const(Value::Int(ui), _) = sub_expr.kind {
                    let b = ui.to_usize().unwrap();
                    Ok(TypeCheckedExprKind::Const(
                        Value::Int(Uint256::from_usize(1 - b)),
                        Type::Bool,
                    ))
                } else {
                    Ok(TypeCheckedExprKind::UnaryOp(
                        UnaryOp::Not,
                        Box::new(sub_expr),
                        Type::Bool,
                    ))
                }
            }
            other => Err(CompileError::new_type_error(
                format!(
                    "invalid operand type {} for logical negation",
                    Color::red(other.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        UnaryOp::Hash => {
            if let TypeCheckedExprKind::Const(Value::Int(ui), _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(
                    Value::Int(ui.avm_hash()),
                    Type::Bytes32,
                ))
            } else {
                Ok(TypeCheckedExprKind::UnaryOp(
                    UnaryOp::Hash,
                    Box::new(sub_expr),
                    Type::Bytes32,
                ))
            }
        }
        UnaryOp::Len => match tc_type {
            Type::Tuple(tv) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_usize(tv.len())),
                Type::Uint,
            )),
            Type::FixedArray(_, sz) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_usize(sz)),
                Type::Uint,
            )),
            Type::Array(_) => Ok(TypeCheckedExprKind::UnaryOp(
                UnaryOp::Len,
                Box::new(sub_expr),
                Type::Uint,
            )),
            other => Err(CompileError::new_type_error(
                format!(
                    "invalid operand type {} for len",
                    Color::red(other.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        UnaryOp::ToUint => {
            if let TypeCheckedExprKind::Const(Value::Int(val), _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(Value::Int(val), Type::Uint))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExprKind::UnaryOp(
                            UnaryOp::ToUint,
                            Box::new(sub_expr),
                            Type::Uint,
                        ))
                    }
                    other => Err(CompileError::new_type_error(
                        format!(
                            "invalid operand type {} for uint()",
                            Color::red(other.print(type_tree))
                        ),
                        loc.into_iter().collect(),
                    )),
                }
            }
        }
        UnaryOp::ToInt => {
            if let TypeCheckedExprKind::Const(Value::Int(val), _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(Value::Int(val), Type::Int))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => Ok(
                        TypeCheckedExprKind::UnaryOp(UnaryOp::ToInt, Box::new(sub_expr), Type::Int),
                    ),
                    other => Err(CompileError::new_type_error(
                        format!(
                            "invalid operand type {} for int()",
                            Color::red(other.print(type_tree))
                        ),
                        loc.into_iter().collect(),
                    )),
                }
            }
        }
        UnaryOp::ToBytes32 => {
            if let TypeCheckedExprKind::Const(Value::Int(val), _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(Value::Int(val), Type::Bytes32))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExprKind::UnaryOp(
                            UnaryOp::ToBytes32,
                            Box::new(sub_expr),
                            Type::Bytes32,
                        ))
                    }
                    other => Err(CompileError::new_type_error(
                        format!(
                            "invalid operand type {} for bytes32()",
                            Color::red(other.print(type_tree))
                        ),
                        loc.into_iter().collect(),
                    )),
                }
            }
        }
        UnaryOp::ToAddress => {
            if let TypeCheckedExprKind::Const(Value::Int(val), _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(
                    Value::Int(
                        val.modulo(
                            &Uint256::from_string_hex(
                                "1__0000_0000__0000_0000__0000_0000__0000_0000__0000_0000",
                            ) //2^160, 1+max address
                            .unwrap(), //safe because we know this str is valid
                        )
                        .unwrap(), //safe because we know this str isn't 0
                    ),
                    Type::EthAddress,
                ))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExprKind::UnaryOp(
                            UnaryOp::ToAddress,
                            Box::new(sub_expr),
                            Type::EthAddress,
                        ))
                    }
                    other => Err(CompileError::new_type_error(
                        format!(
                            "invalid operand type {} for address cast",
                            Color::red(other.print(type_tree))
                        ),
                        loc.into_iter().collect(),
                    )),
                }
            }
        }
    }
}

/// Attempts to apply the `BinaryOp` op, to `TypeCheckedExpr`s tcs1 on the left, and tcs2 on the
/// right.
///
/// This produces a `TypeCheckedExpr` if successful, and a `CompileError` otherwise.  The argument loc
/// is used to record the location of op for use in formatting the `CompileError`.
fn typecheck_binary_op(
    mut op: BinaryOp,
    mut tcs1: TypeCheckedExpr,
    mut tcs2: TypeCheckedExpr,
    type_tree: &TypeTree,
    loc: Option<Location>,
) -> Result<TypeCheckedExprKind, CompileError> {
    if let TypeCheckedExprKind::Const(Value::Int(val2), t2) = tcs2.kind.clone() {
        if let TypeCheckedExprKind::Const(Value::Int(val1), t1) = tcs1.kind.clone() {
            // both args are constants, so we can do the op at compile time
            match op {
                BinaryOp::GetBuffer256 | BinaryOp::GetBuffer64 | BinaryOp::GetBuffer8 => {}
                _ => {
                    return typecheck_binary_op_const(op, val1, t1, val2, t2, type_tree, loc);
                }
            }
        } else {
            match op {
                BinaryOp::Plus
                | BinaryOp::Times
                | BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::BitwiseAnd
                | BinaryOp::BitwiseOr
                | BinaryOp::BitwiseXor => {
                    // swap the args, so code generator will be able to supply the constant as an immediate
                    std::mem::swap(&mut tcs1, &mut tcs2);
                }
                BinaryOp::LessThan => {
                    op = BinaryOp::GreaterThan;
                    std::mem::swap(&mut tcs1, &mut tcs2);
                }
                BinaryOp::GreaterThan => {
                    op = BinaryOp::LessThan;
                    std::mem::swap(&mut tcs1, &mut tcs2);
                }
                BinaryOp::LessEq => {
                    op = BinaryOp::GreaterEq;
                    std::mem::swap(&mut tcs1, &mut tcs2)
                }
                BinaryOp::GreaterEq => {
                    op = BinaryOp::LessEq;
                    std::mem::swap(&mut tcs1, &mut tcs2)
                }
                _ => {}
            }
        }
    }
    let subtype1 = tcs1.get_type().rep(type_tree)?;
    let subtype2 = tcs2.get_type().rep(type_tree)?;
    match op {
        BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to binary op: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree)),
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Div => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                BinaryOp::Sdiv,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to divide: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree)),
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GetBuffer8 => match (subtype1, subtype2) {
            (Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to getbuffer8: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GetBuffer64 => match (subtype1, subtype2) {
            (Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to getbuffer64: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GetBuffer256 => match (subtype1, subtype2) {
            (Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to getbuffer256: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Mod => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                BinaryOp::Smod,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to mod: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::LessThan => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                BinaryOp::SLessThan,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to <: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GreaterThan => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                BinaryOp::SGreaterThan,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to >: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::LessEq => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                BinaryOp::SLessEq,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to <=: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GreaterEq => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                BinaryOp::SGreaterEq,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to >=: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Equal | BinaryOp::NotEqual => {
            let mutual = subtype1.assignable(&subtype2, type_tree, HashSet::new())
                && subtype2.assignable(&subtype1, type_tree, HashSet::new());

            if mutual {
                Ok(TypeCheckedExprKind::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Bool,
                ))
            } else {
                Err(CompileError::new_type_error(
                    format!(
                        "invalid argument types to equality comparison: {} and {}",
                        Color::red(subtype1.print(type_tree)),
                        Color::red(subtype2.print(type_tree))
                    ),
                    loc.into_iter().collect(),
                ))
            }
        }
        BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::BitwiseXor
        | BinaryOp::ShiftLeft
        | BinaryOp::ShiftRight => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
            )),
            (Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bytes32,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to binary bitwise operator: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Hash => match (subtype1, subtype2) {
            (Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bytes32,
            )),
            (subtype1, subtype2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to binary hash operator: {} and {}",
                    Color::red(subtype1.print(type_tree)),
                    Color::red(subtype2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Smod
        | BinaryOp::Sdiv
        | BinaryOp::SLessThan
        | BinaryOp::SGreaterThan
        | BinaryOp::SLessEq
        | BinaryOp::SGreaterEq => {
            panic!("unexpected op in typecheck_binary_op");
        }
    }
}

fn typecheck_trinary_op(
    op: TrinaryOp,
    tcs1: TypeCheckedExpr,
    tcs2: TypeCheckedExpr,
    tcs3: TypeCheckedExpr,
    type_tree: &TypeTree,
    loc: Option<Location>,
) -> Result<TypeCheckedExprKind, CompileError> {
    let subtype1 = tcs1.get_type().rep(type_tree)?;
    let subtype2 = tcs2.get_type().rep(type_tree)?;
    let subtype3 = tcs3.get_type().rep(type_tree)?;
    match op {
        TrinaryOp::SetBuffer8 | TrinaryOp::SetBuffer64 | TrinaryOp::SetBuffer256 => {
            match (subtype1, subtype2, subtype3) {
                (Type::Uint, Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Trinary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Box::new(tcs3),
                    Type::Buffer,
                )),
                (t1, t2, t3) => Err(CompileError::new_type_error(
                    format!(
                        "invalid argument types to 3-ary op: {}, {} and {}",
                        Color::red(t1.print(type_tree)),
                        Color::red(t2.print(type_tree)),
                        Color::red(t3.print(type_tree))
                    ),
                    loc.into_iter().collect(),
                )),
            }
        }
    }
}

/// Version of `typecheck_binary_op` for when both sub expressions are constant integer types.
///
/// This is used internally by `typecheck_binary_op`, so this generally does not need to be called
/// directly.
///
/// The arguments val1, and t1 represent the value of the left subexpression, and its type, and val2
/// and t2 represent the value and type of the right subexpression, loc is used to format the
/// `CompileError` in case of failure.
fn typecheck_binary_op_const(
    op: BinaryOp,
    val1: Uint256,
    t1: Type,
    val2: Uint256,
    t2: Type,
    type_tree: &TypeTree,
    loc: Option<Location>,
) -> Result<TypeCheckedExprKind, CompileError> {
    match op {
        BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times => match (&t1, &t2) {
            (Type::Uint, Type::Uint) | (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Const(
                Value::Int(match op {
                    BinaryOp::Plus => val1.add(&val2),
                    BinaryOp::Minus => {
                        if let Some(val) = val1.sub(&val2) {
                            val
                        } else {
                            return Err(CompileError::new_type_error(
                                "underflow on substraction".to_string(),
                                loc.into_iter().collect(),
                            ));
                        }
                    }
                    BinaryOp::Times => val1.mul(&val2),
                    _ => {
                        panic!();
                    }
                }),
                t1,
            )),
            _ => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to binary op: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Div => match (&t1, &t2) {
            (Type::Uint, Type::Uint) => match val1.div(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(CompileError::new_type_error(
                    "divide by constant zero".to_string(),
                    loc.into_iter().collect(),
                )),
            },
            (Type::Int, Type::Int) => match val1.sdiv(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(CompileError::new_type_error(
                    "divide by constant zero".to_string(),
                    loc.into_iter().collect(),
                )),
            },
            _ => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to divide: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Mod => match (&t1, &t2) {
            (Type::Uint, Type::Uint) => match val1.modulo(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(CompileError::new_type_error(
                    "divide by constant zero".to_string(),
                    loc.into_iter().collect(),
                )),
            },
            (Type::Int, Type::Int) => match val1.smodulo(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(CompileError::new_type_error(
                    "divide by constant zero".to_string(),
                    loc.into_iter().collect(),
                )),
            },
            _ => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to mod: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::LessThan => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(val1 < val2)),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(val1.s_less_than(&val2))),
                Type::Bool,
            )),
            (t1, t2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to <: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GreaterThan => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(val1 > val2)),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(val2.s_less_than(&val1))),
                Type::Bool,
            )),
            (t1, t2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to >: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::LessEq => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(val1 <= val2)),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(!val2.s_less_than(&val1))),
                Type::Bool,
            )),
            (t1, t2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to <=: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::GreaterEq => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(val1 >= val2)),
                Type::Bool,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Const(
                Value::Int(Uint256::from_bool(!val1.s_less_than(&val2))),
                Type::Bool,
            )),
            (t1, t2) => Err(CompileError::new_type_error(
                format!(
                    "invalid argument types to >=: {} and {}",
                    Color::red(t1.print(type_tree)),
                    Color::red(t2.print(type_tree))
                ),
                loc.into_iter().collect(),
            )),
        },
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::BitwiseXor
        | BinaryOp::Hash => {
            if t1 == t2 {
                Ok(TypeCheckedExprKind::Const(
                    Value::Int(match op {
                        BinaryOp::Equal => Uint256::from_bool(val1 == val2),
                        BinaryOp::NotEqual => Uint256::from_bool(val1 != val2),
                        BinaryOp::BitwiseAnd => val1.bitwise_and(&val2),
                        BinaryOp::BitwiseOr => val1.bitwise_or(&val2),
                        BinaryOp::BitwiseXor => val1.bitwise_xor(&val2),
                        BinaryOp::Hash => {
                            if let Type::Bytes32 = t1 {
                                return Ok(TypeCheckedExprKind::Const(
                                    Value::avm_hash2(&Value::Int(val1), &Value::Int(val2)),
                                    Type::Bool,
                                ));
                            } else {
                                return Err(CompileError::new_type_error(
                                    format!(
                                        "invalid argument types to binary op: {} and {}",
                                        Color::red(t1.print(type_tree)),
                                        Color::red(t2.print(type_tree))
                                    ),
                                    loc.into_iter().collect(),
                                ));
                            }
                        }
                        _ => {
                            panic!();
                        }
                    }),
                    Type::Bool,
                ))
            } else {
                Err(CompileError::new_type_error(
                    format!(
                        "invalid argument types to binary op: {} and {}",
                        Color::red(t1.print(type_tree)),
                        Color::red(t2.print(type_tree))
                    ),
                    loc.into_iter().collect(),
                ))
            }
        }
        BinaryOp::ShiftLeft | BinaryOp::ShiftRight => {
            if t1 == Type::Uint {
                Ok(TypeCheckedExprKind::Const(
                    Value::Int(match t2 {
                        Type::Uint | Type::Int | Type::Bytes32 => {
                            let x = val1.to_usize().ok_or_else(|| {
                                CompileError::new_type_error(
                                    format!(
                                        "Attempt to shift {} left by {}, causing overflow",
                                        val2, val1
                                    ),
                                    loc.into_iter().collect(),
                                )
                            })?;
                            if op == BinaryOp::ShiftLeft {
                                val2.shift_left(x)
                            } else {
                                val2.shift_right(x)
                            }
                        }
                        _ => {
                            return Err(CompileError::new_type_error(
                                format!(
                                    "Attempt to shift a {} by a {}, must shift an integer type by a uint",
                                    Color::red(t2.print(type_tree)),
                                    Color::red(t1.print(type_tree))
                                ),
                                loc.into_iter().collect(),
                            ))
                        }
                    }),
                    t1,
                ))
            } else {
                Err(CompileError::new_type_error(
                    format!(
                        "Attempt to shift a {} by a {}, must shift an integer type by a uint",
                        Color::red(t2.print(type_tree)),
                        Color::red(t1.print(type_tree))
                    ),
                    loc.into_iter().collect(),
                ))
            }
        }
        BinaryOp::Smod
        | BinaryOp::GetBuffer8
        | BinaryOp::GetBuffer64
        | BinaryOp::GetBuffer256
        | BinaryOp::Sdiv
        | BinaryOp::SLessThan
        | BinaryOp::SGreaterThan
        | BinaryOp::SLessEq
        | BinaryOp::SGreaterEq => {
            panic!("unexpected op in typecheck_binary_op");
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeCheckedCodeBlock {
    pub body: Vec<TypeCheckedStatement>,
    pub ret_expr: Option<Box<TypeCheckedExpr>>,
    pub scope: Option<String>,
}

impl TypeCheckedCodeBlock {
    fn new(
        body: Vec<TypeCheckedStatement>,
        ret_expr: Option<Box<TypeCheckedExpr>>,
        scope: Option<String>,
    ) -> Self {
        Self {
            body,
            ret_expr,
            scope,
        }
    }
    pub fn get_type(&self) -> Type {
        self.ret_expr
            .clone()
            .map(|r| r.get_type())
            .unwrap_or(Type::Void)
    }
}

impl AbstractSyntaxTree for TypeCheckedCodeBlock {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        self.body
            .iter_mut()
            .map(|stat| TypeCheckedNode::Statement(stat))
            .chain(
                self.ret_expr
                    .iter_mut()
                    .map(|exp| TypeCheckedNode::Expression(exp)),
            )
            .collect()
    }
    fn is_view(&mut self, type_tree: &TypeTree) -> bool {
        self.body
            .iter_mut()
            .any(|statement| statement.is_view(type_tree))
            || self
                .ret_expr
                .as_mut()
                .map(|expr| expr.is_view(type_tree))
                .unwrap_or(false)
    }
    fn is_write(&mut self, type_tree: &TypeTree) -> bool {
        self.body
            .iter_mut()
            .any(|statement| statement.is_write(type_tree))
            || self
                .ret_expr
                .as_mut()
                .map(|expr| expr.is_write(type_tree))
                .unwrap_or(false)
    }
}

fn typecheck_codeblock(
    block: &CodeBlock,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, Type>,
    func_table: &TypeTable,
    func: &Func,
    type_tree: &TypeTree,
    string_table: &StringTable,
    undefinable_ids: &mut HashMap<StringId, Option<Location>>,
    closures: &mut BTreeMap<StringId, TypeCheckedFunc>,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<TypeCheckedCodeBlock, CompileError> {
    let mut output = Vec::new();
    let mut block_bindings = Vec::new();
    scopes.push(("_".to_string(), None));
    for statement in &block.body {
        let mut inner_type_table = type_table.clone();
        inner_type_table.extend(
            block_bindings
                .clone()
                .into_iter()
                .collect::<HashMap<_, _>>(),
        );
        let (statement, bindings) = typecheck_statement(
            &statement,
            func,
            &inner_type_table,
            global_vars,
            func_table,
            type_tree,
            string_table,
            undefinable_ids,
            closures,
            scopes,
        )?;
        output.push(statement);
        for (key, value) in bindings {
            block_bindings.push((key, value));
        }
    }
    let mut inner_type_table = type_table.clone();
    inner_type_table.extend(
        block_bindings
            .clone()
            .into_iter()
            .collect::<HashMap<_, _>>(),
    );
    Ok(TypeCheckedCodeBlock {
        body: output,
        ret_expr: block
            .ret_expr
            .clone()
            .map(|x| {
                typecheck_expr(
                    &*x,
                    &inner_type_table,
                    global_vars,
                    func_table,
                    func,
                    type_tree,
                    string_table,
                    undefinable_ids,
                    closures,
                    scopes,
                )
            })
            .transpose()?
            .map(Box::new),
        scope: None,
    })
}

fn build_function_call(
    func_expr: TypeCheckedExpr,
    args: Vec<TypeCheckedExpr>,
    string_table: &StringTable,
    type_tree: &TypeTree,
) -> Result<TypeCheckedExprKind, CompileError> {
    let func_name = match &func_expr.kind {
        TypeCheckedExprKind::FuncRef(id, _) => {
            format!(" {}", string_table.name_from_id(*id))
        }
        _ => String::new(),
    };

    let func_expr_type = func_expr.get_type().rep(type_tree)?;

    let (prop, arg_types, ret_type) = match func_expr_type {
        Type::Func(prop, arg_types, ret_type) => (prop, arg_types, ret_type),
        wrong => {
            return Err(CompileError::new(
                "Typecheck Error",
                format!(
                    "tried to call non-func {}",
                    Color::red(wrong.print(type_tree))
                ),
                func_expr.debug_info.locs(),
            ));
        }
    };

    if args.len() != arg_types.len() {
        return Err(CompileError::new(
            "Typecheck Error",
            format!(
                "wrong number of args passed to func{}",
                Color::red(func_name)
            ),
            func_expr.debug_info.locs(),
        ));
    }

    for (index, (arg, tipe)) in args.iter().zip(arg_types).enumerate() {
        let arg_type = arg.get_type();

        if !tipe.assignable(&arg_type, type_tree, HashSet::new()) {
            return Err(CompileError::new_type_error(
                format!(
                    "func{}'s {} arg has wrong type:\nencountered {}\ninstead of  {}",
                    Color::red(func_name),
                    Color::red(human_readable_index(index + 1)),
                    Color::red(arg_type.print(type_tree)),
                    Color::red(tipe.print(type_tree)),
                ),
                arg.debug_info.locs(),
            ));
        }
    }

    Ok(TypeCheckedExprKind::FunctionCall(
        Box::new(func_expr),
        args,
        *ret_type,
        prop,
    ))
}
