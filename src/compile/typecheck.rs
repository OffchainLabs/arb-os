/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Converts non-type checked ast nodes to type checked versions, and other related utilities.

use super::ast::{
    BinaryOp, Constant, Expr, FuncArg, FuncDecl, FuncDeclKind, GlobalVarDecl, IfArm,
    ImportFuncDecl, MatchPattern, Statement, StatementKind, StructField, TopLevelDecl, Type,
    UnaryOp,
};
use super::{symtable::SymTable, MiniProperties};
use crate::compile::ast::{DebugInfo, ExprKind, TypeTree};
use crate::link::{ExportedFunc, Import, ImportedFunc};
use crate::mavm::{Instruction, Label, Value};
use crate::pos::Location;
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::HashMap;

pub trait AbstractSyntaxTree {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        vec![]
    }
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
}

#[derive(Debug)]
pub enum TypeCheckedNode<'a> {
    Statement(&'a mut TypeCheckedStatement),
    Expression(&'a mut TypeCheckedExpr),
    IfArm(&'a mut TypeCheckedIfArm),
    StructField(&'a mut TypeCheckedStructField),
}

impl<'a> AbstractSyntaxTree for TypeCheckedNode<'a> {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            TypeCheckedNode::Statement(stat) => stat.child_nodes(),
            TypeCheckedNode::Expression(exp) => exp.child_nodes(),
            TypeCheckedNode::IfArm(arm) => arm.child_nodes(),
            TypeCheckedNode::StructField(field) => field.child_nodes(),
        }
    }
}

///An error encountered during typechecking
#[derive(Debug)]
pub struct TypeError {
    pub reason: String,
    pub location: Option<Location>,
}

pub fn new_type_error(msg: String, location: Option<Location>) -> TypeError {
    TypeError {
        reason: msg,
        location,
    }
}

///Keeps track of compiler enforced properties, currently only tracks purity, may be extended to
/// keep track of potential to throw or other properties.
#[derive(Debug, Clone)]
pub struct PropertiesList {
    pub pure: bool,
}

///A mini function that has been type checked.
#[derive(Debug, Clone)]
pub struct TypeCheckedFunc {
    pub name: StringId,
    pub args: Vec<FuncArg>,
    pub ret_type: Type,
    pub code: Vec<TypeCheckedStatement>,
    pub tipe: Type,
    pub imported: bool,
    pub debug_info: DebugInfo,
    pub properties: PropertiesList,
}

impl AbstractSyntaxTree for TypeCheckedFunc {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        self.code
            .iter_mut()
            .map(|stat| TypeCheckedNode::Statement(stat))
            .collect()
    }
}

impl MiniProperties for TypeCheckedFunc {
    fn is_pure(&self) -> bool {
        self.code.iter().all(|statement| statement.is_pure())
    }
}

fn strip_returns(to_strip: &mut TypeCheckedNode, _state: &(), _mut_state: &mut ()) -> bool {
    if let TypeCheckedNode::Statement(stat) = to_strip {
        if let TypeCheckedStatementKind::Return(exp) = &mut stat.kind {
            stat.kind = TypeCheckedStatementKind::Break(Some(exp.clone()), "_inline".to_string());
        } else if let TypeCheckedStatementKind::ReturnVoid() = &mut stat.kind {
            stat.kind = TypeCheckedStatementKind::Break(None, "_inline".to_string());
        }
    }
    true
}

fn inline(
    to_do: &mut TypeCheckedNode,
    state: &(&Vec<TypeCheckedFunc>, &Vec<ImportedFunc>, &StringTable),
    _mut_state: &mut (),
) -> bool {
    if let TypeCheckedNode::Statement(stat) = to_do {
        stat.debug_info.attributes.inline
    } else if let TypeCheckedNode::Expression(exp) = to_do {
        if let TypeCheckedExpr {
            kind: TypeCheckedExprKind::FunctionCall(name, args, _, _),
            debug_info: _,
        } = exp
        {
            let (code, block_exp) = if let TypeCheckedExpr {
                kind: TypeCheckedExprKind::FuncRef(id, ref func_ref_type),
                debug_info: _,
            } = **name
            {
                let found_func = state.0.iter().find(|func| func.name == id);
                if let Some(func) = found_func {
                    let mut code: Vec<_> = args
                        .iter()
                        .zip(func.args.iter())
                        .map(|(arg, otherarg)| TypeCheckedStatement {
                            kind: TypeCheckedStatementKind::Let(
                                TypeCheckedMatchPattern::Simple(
                                    otherarg.name,
                                    otherarg.tipe.clone(),
                                ),
                                arg.clone(),
                            ),
                            debug_info: DebugInfo::default(),
                        })
                        .collect();
                    code.append(&mut func.code.clone());
                    let last = code.pop();
                    let block_exp = match last {
                        Some(TypeCheckedStatement {
                            kind: TypeCheckedStatementKind::Return(exp),
                            debug_info: _,
                        }) => Some(Box::new(exp)),
                        Some(TypeCheckedStatement {
                            kind: TypeCheckedStatementKind::If(_),
                            debug_info,
                        })
                        | Some(TypeCheckedStatement {
                            kind: TypeCheckedStatementKind::IfLet(_, _, _, _),
                            debug_info,
                        }) => {
                            if let Some(statement) = last {
                                code.push(statement);
                            }
                            Some(Box::new(TypeCheckedExpr {
                                kind: TypeCheckedExprKind::Const(
                                    if let Type::Func(_, _, ret) = func_ref_type {
                                        ret.default_value().0
                                    } else {
                                        panic!()
                                    },
                                    if let Type::Func(_, _, ret) = func_ref_type {
                                        *ret.clone()
                                    } else {
                                        panic!()
                                    },
                                ),
                                debug_info,
                            }))
                        }
                        _ => {
                            if let Some(statement) = last {
                                code.push(statement);
                            }
                            None
                        }
                    };
                    (Some(code), block_exp)
                } else {
                    (None, None)
                }
            } else {
                (None, None)
            };
            if let Some(mut code) = code {
                for statement in code.iter_mut().rev() {
                    statement.recursive_apply(strip_returns, &(), &mut ())
                }
                exp.kind =
                    TypeCheckedExprKind::CodeBlock(code, block_exp, Some("_inline".to_string()));
            }
            false
        } else {
            true
        }
    } else {
        true
    }
}

impl TypeCheckedFunc {
    pub fn inline(
        &mut self,
        funcs: &Vec<TypeCheckedFunc>,
        imported_funcs: &Vec<ImportedFunc>,
        string_table: &StringTable,
    ) {
        self.recursive_apply(inline, &(funcs, imported_funcs, string_table), &mut ());
    }
}

///A mini statement that has been type checked.
#[derive(Debug, Clone)]
pub struct TypeCheckedStatement {
    pub kind: TypeCheckedStatementKind,
    pub debug_info: DebugInfo,
}

///A mini statement that has been type checked.
#[derive(Debug, Clone)]
pub enum TypeCheckedStatementKind {
    Noop(),
    Panic(),
    ReturnVoid(),
    Return(TypeCheckedExpr),
    Break(Option<TypeCheckedExpr>, String),
    Expression(TypeCheckedExpr),
    Let(TypeCheckedMatchPattern, TypeCheckedExpr),
    AssignLocal(StringId, TypeCheckedExpr),
    AssignGlobal(usize, TypeCheckedExpr),
    Loop(Vec<TypeCheckedStatement>),
    While(TypeCheckedExpr, Vec<TypeCheckedStatement>),
    If(TypeCheckedIfArm),
    IfLet(
        StringId,
        TypeCheckedExpr,
        Vec<TypeCheckedStatement>,
        Option<Vec<TypeCheckedStatement>>,
    ),
    Asm(Vec<Instruction>, Vec<TypeCheckedExpr>),
    DebugPrint(TypeCheckedExpr),
}

impl MiniProperties for TypeCheckedStatement {
    fn is_pure(&self) -> bool {
        match &self.kind {
            TypeCheckedStatementKind::Noop()
            | TypeCheckedStatementKind::Panic()
            | TypeCheckedStatementKind::ReturnVoid() => true,
            TypeCheckedStatementKind::Return(something) => something.is_pure(),
            TypeCheckedStatementKind::Break(exp, _) => {
                exp.clone().map(|exp| exp.is_pure()).unwrap_or(true)
            }
            TypeCheckedStatementKind::Expression(expr) => expr.is_pure(),
            TypeCheckedStatementKind::Let(_, exp) => exp.is_pure(),
            TypeCheckedStatementKind::AssignLocal(_, exp) => exp.is_pure(),
            TypeCheckedStatementKind::AssignGlobal(_, _) => false,
            TypeCheckedStatementKind::Loop(code) => {
                code.iter().all(|statement| statement.is_pure())
            }
            TypeCheckedStatementKind::While(exp, block) => {
                exp.is_pure() && block.iter().all(|statement| statement.is_pure())
            }
            TypeCheckedStatementKind::If(if_arm) => if_arm.is_pure(),
            TypeCheckedStatementKind::IfLet(_, expr, block, eblock) => {
                expr.is_pure()
                    && block.iter().all(|statement| statement.is_pure())
                    && eblock
                        // This clone can most likely be avoided and it would probably be good idea to do so
                        .clone()
                        .map(|statements| statements.iter().all(|statement| statement.is_pure()))
                        .unwrap_or(true)
            }
            TypeCheckedStatementKind::Asm(instrs, exprs) => {
                instrs.iter().all(|instr| instr.is_pure())
                    && exprs.iter().all(|expr| expr.is_pure())
            }
            TypeCheckedStatementKind::DebugPrint(_) => true,
        }
    }
}

impl AbstractSyntaxTree for TypeCheckedStatement {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match &mut self.kind {
            TypeCheckedStatementKind::Noop()
            | TypeCheckedStatementKind::Panic()
            | TypeCheckedStatementKind::ReturnVoid() => vec![],
            TypeCheckedStatementKind::Return(exp)
            | TypeCheckedStatementKind::Expression(exp)
            | TypeCheckedStatementKind::Let(_, exp)
            | TypeCheckedStatementKind::AssignLocal(_, exp)
            | TypeCheckedStatementKind::AssignGlobal(_, exp)
            | TypeCheckedStatementKind::DebugPrint(exp) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedStatementKind::Loop(stats) => stats
                .iter_mut()
                .map(|stat| TypeCheckedNode::Statement(stat))
                .collect(),
            TypeCheckedStatementKind::While(exp, stats) => vec![TypeCheckedNode::Expression(exp)]
                .into_iter()
                .chain(
                    stats
                        .iter_mut()
                        .map(|stat| TypeCheckedNode::Statement(stat)),
                )
                .collect(),
            TypeCheckedStatementKind::If(arm) => vec![TypeCheckedNode::IfArm(arm)],
            TypeCheckedStatementKind::IfLet(_, exp, stats, ostats) => {
                vec![TypeCheckedNode::Expression(exp)]
                    .into_iter()
                    .chain(
                        stats
                            .iter_mut()
                            .map(|stat| TypeCheckedNode::Statement(stat)),
                    )
                    .chain(
                        ostats
                            .iter_mut()
                            .flatten()
                            .map(|stat| TypeCheckedNode::Statement(stat)),
                    )
                    .collect()
            }
            TypeCheckedStatementKind::Asm(_, exps) => exps
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedStatementKind::Break(oexp, _) => {
                oexp.iter_mut().flat_map(|exp| exp.child_nodes()).collect()
            }
        }
    }
}

///A `MatchPattern` that has gone through type checking.
#[derive(Debug, Clone)]
pub enum TypeCheckedMatchPattern {
    Simple(StringId, Type),
    Tuple(Vec<TypeCheckedMatchPattern>, Type),
}

///An `IfArm` that has been type checked.
#[derive(Debug, Clone)]
pub enum TypeCheckedIfArm {
    Cond(
        TypeCheckedExpr,
        Vec<TypeCheckedStatement>,
        Option<Box<TypeCheckedIfArm>>,
        DebugInfo,
    ),
    Catchall(Vec<TypeCheckedStatement>, DebugInfo),
}

impl AbstractSyntaxTree for TypeCheckedIfArm {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            TypeCheckedIfArm::Cond(exp, stats, alt_stats, _) => {
                vec![TypeCheckedNode::Expression(exp)]
                    .into_iter()
                    .chain(
                        stats
                            .iter_mut()
                            .map(|stat| TypeCheckedNode::Statement(stat)),
                    )
                    .chain(alt_stats.iter_mut().map(|arm| TypeCheckedNode::IfArm(arm)))
                    .collect()
            }
            TypeCheckedIfArm::Catchall(stats, _) => stats
                .iter_mut()
                .map(|stat| TypeCheckedNode::Statement(stat))
                .collect(),
        }
    }
}

impl MiniProperties for TypeCheckedIfArm {
    fn is_pure(&self) -> bool {
        match self {
            TypeCheckedIfArm::Cond(expr, statements, else_block, _) => {
                expr.is_pure()
                    && statements.iter().all(|statement| statement.is_pure())
                    && if let Some(block) = else_block {
                        block.is_pure()
                    } else {
                        true
                    }
            }
            TypeCheckedIfArm::Catchall(statements, _) => {
                statements.iter().all(|statement| statement.is_pure())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckedExpr {
    pub kind: TypeCheckedExprKind,
    pub debug_info: DebugInfo,
}

///A mini expression that has been type checked.
#[derive(Debug, Clone)]
pub enum TypeCheckedExprKind {
    UnaryOp(UnaryOp, Box<TypeCheckedExpr>, Type),
    Binary(BinaryOp, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
    ShortcutOr(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>),
    ShortcutAnd(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>),
    LocalVariableRef(StringId, Type),
    GlobalVariableRef(usize, Type),
    Variant(Box<TypeCheckedExpr>),
    FuncRef(usize, Type),
    TupleRef(Box<TypeCheckedExpr>, Uint256, Type),
    DotRef(Box<TypeCheckedExpr>, StringId, usize, Type),
    Const(Value, Type),
    FunctionCall(
        Box<TypeCheckedExpr>,
        Vec<TypeCheckedExpr>,
        Type,
        PropertiesList,
    ),
    CodeBlock(
        Vec<TypeCheckedStatement>,
        Option<Box<TypeCheckedExpr>>,
        Option<String>,
    ),
    StructInitializer(Vec<TypeCheckedStructField>, Type),
    ArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
    FixedArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, usize, Type),
    MapRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
    Tuple(Vec<TypeCheckedExpr>, Type),
    NewArray(Box<TypeCheckedExpr>, Type, Type),
    NewFixedArray(usize, Option<Box<TypeCheckedExpr>>, Type),
    NewMap(Type),
    ArrayMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
    ),
    FixedArrayMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        usize,
        Type,
    ),
    MapMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
    ),
    StructMod(Box<TypeCheckedExpr>, usize, Box<TypeCheckedExpr>, Type),
    Cast(Box<TypeCheckedExpr>, Type),
    Asm(Type, Vec<Instruction>, Vec<TypeCheckedExpr>),
    Try(Box<TypeCheckedExpr>, Type),
}

impl MiniProperties for TypeCheckedExpr {
    fn is_pure(&self) -> bool {
        match &self.kind {
            TypeCheckedExprKind::UnaryOp(_, expr, _) => expr.is_pure(),
            TypeCheckedExprKind::Binary(_, left, right, _) => left.is_pure() && right.is_pure(),
            TypeCheckedExprKind::ShortcutOr(left, right) => left.is_pure() && right.is_pure(),
            TypeCheckedExprKind::ShortcutAnd(left, right) => left.is_pure() && right.is_pure(),
            TypeCheckedExprKind::LocalVariableRef(_, _) => true,
            TypeCheckedExprKind::GlobalVariableRef(_, _) => false,
            TypeCheckedExprKind::Variant(expr) => expr.is_pure(),
            TypeCheckedExprKind::FuncRef(_, func_type) => {
                if let Type::Func(impure, _, _) = func_type {
                    !*impure
                } else {
                    panic!("Internal error: func ref has non function type")
                }
            }
            TypeCheckedExprKind::TupleRef(expr, _, _) => expr.is_pure(),
            TypeCheckedExprKind::DotRef(expr, _, _, _) => expr.is_pure(),
            TypeCheckedExprKind::Const(_, _) => true,
            TypeCheckedExprKind::FunctionCall(name_expr, fields_exprs, _, properties) => {
                name_expr.is_pure()
                    && fields_exprs.iter().all(|statement| statement.is_pure())
                    && properties.pure
            }
            TypeCheckedExprKind::CodeBlock(statements, return_expr, _) => {
                statements.iter().all(|statement| statement.is_pure())
                    && return_expr
                        .as_ref()
                        .map(|expr| expr.is_pure())
                        .unwrap_or(true)
            }
            TypeCheckedExprKind::StructInitializer(fields, _) => {
                fields.iter().all(|field| field.value.is_pure())
            }
            TypeCheckedExprKind::ArrayRef(expr, expr2, _) => expr.is_pure() && expr2.is_pure(),
            TypeCheckedExprKind::FixedArrayRef(expr, expr2, _, _) => {
                expr.is_pure() && expr2.is_pure()
            }
            TypeCheckedExprKind::MapRef(expr, expr2, _) => expr.is_pure() && expr2.is_pure(),
            TypeCheckedExprKind::Tuple(exprs, _) => exprs.iter().all(|expr| expr.is_pure()),
            TypeCheckedExprKind::NewArray(expr, _, _) => expr.is_pure(),
            TypeCheckedExprKind::NewFixedArray(_, opt_expr, _) => {
                if let Some(expr) = opt_expr {
                    expr.is_pure()
                } else {
                    true
                }
            }
            TypeCheckedExprKind::NewMap(_) => true,
            TypeCheckedExprKind::ArrayMod(arr, index, val, _) => {
                arr.is_pure() && index.is_pure() && val.is_pure()
            }
            TypeCheckedExprKind::FixedArrayMod(arr, index, val, _, _) => {
                arr.is_pure() && index.is_pure() && val.is_pure()
            }
            TypeCheckedExprKind::MapMod(map, key, val, _) => {
                map.is_pure() && key.is_pure() && val.is_pure()
            }
            TypeCheckedExprKind::StructMod(the_struct, _, val, _) => {
                the_struct.is_pure() && val.is_pure()
            }
            TypeCheckedExprKind::Cast(expr, _) => expr.is_pure(),
            TypeCheckedExprKind::Asm(_, instrs, args) => {
                instrs.iter().all(|inst| inst.is_pure()) && args.iter().all(|expr| expr.is_pure())
            }
            TypeCheckedExprKind::Try(expr, _) => expr.is_pure(),
        }
    }
}

impl AbstractSyntaxTree for TypeCheckedExpr {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match &mut self.kind {
            TypeCheckedExprKind::LocalVariableRef(_, _)
            | TypeCheckedExprKind::GlobalVariableRef(_, _)
            | TypeCheckedExprKind::FuncRef(_, _)
            | TypeCheckedExprKind::Const(_, _)
            | TypeCheckedExprKind::NewMap(_) => vec![],
            TypeCheckedExprKind::UnaryOp(_, exp, _)
            | TypeCheckedExprKind::Variant(exp)
            | TypeCheckedExprKind::TupleRef(exp, _, _)
            | TypeCheckedExprKind::DotRef(exp, _, _, _)
            | TypeCheckedExprKind::NewArray(exp, _, _)
            | TypeCheckedExprKind::Cast(exp, _)
            | TypeCheckedExprKind::Try(exp, _) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedExprKind::Binary(_, lexp, rexp, _)
            | TypeCheckedExprKind::ShortcutOr(lexp, rexp)
            | TypeCheckedExprKind::ShortcutAnd(lexp, rexp)
            | TypeCheckedExprKind::ArrayRef(lexp, rexp, _)
            | TypeCheckedExprKind::FixedArrayRef(lexp, rexp, _, _)
            | TypeCheckedExprKind::MapRef(lexp, rexp, _)
            | TypeCheckedExprKind::StructMod(lexp, _, rexp, _) => vec![
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
            TypeCheckedExprKind::CodeBlock(stats, oexpr, _) => oexpr
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .chain(
                    stats
                        .iter_mut()
                        .map(|stat| TypeCheckedNode::Statement(stat)),
                )
                .collect(),
            TypeCheckedExprKind::StructInitializer(fields, _) => fields
                .iter_mut()
                .map(|field| TypeCheckedNode::StructField(field))
                .collect(),
            TypeCheckedExprKind::Tuple(exps, _) | TypeCheckedExprKind::Asm(_, _, exps) => exps
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedExprKind::NewFixedArray(_, oexp, _) => oexp
                .into_iter()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedExprKind::ArrayMod(exp1, exp2, exp3, _)
            | TypeCheckedExprKind::FixedArrayMod(exp1, exp2, exp3, _, _)
            | TypeCheckedExprKind::MapMod(exp1, exp2, exp3, _) => vec![
                TypeCheckedNode::Expression(exp1),
                TypeCheckedNode::Expression(exp2),
                TypeCheckedNode::Expression(exp3),
            ],
        }
    }
}

impl TypeCheckedExpr {
    ///Extracts the type returned from the expression.
    pub fn get_type(&self) -> Type {
        match &self.kind {
            TypeCheckedExprKind::UnaryOp(_, _, t) => t.clone(),
            TypeCheckedExprKind::Binary(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::ShortcutOr(_, _) | TypeCheckedExprKind::ShortcutAnd(_, _) => {
                Type::Bool
            }
            TypeCheckedExprKind::LocalVariableRef(_, t) => t.clone(),
            TypeCheckedExprKind::GlobalVariableRef(_, t) => t.clone(),
            TypeCheckedExprKind::FuncRef(_, t) => t.clone(),
            TypeCheckedExprKind::TupleRef(_, _, t) => t.clone(),
            TypeCheckedExprKind::Variant(t) => Type::Option(Box::new(t.get_type())),
            TypeCheckedExprKind::DotRef(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::Const(_, t) => t.clone(),
            TypeCheckedExprKind::FunctionCall(_, _, t, _) => t.clone(),
            TypeCheckedExprKind::CodeBlock(_, expr, _) => expr
                .clone()
                .map(|exp| exp.get_type())
                .unwrap_or_else(|| Type::Tuple(vec![])),
            TypeCheckedExprKind::StructInitializer(_, t) => t.clone(),
            TypeCheckedExprKind::ArrayRef(_, _, t) => t.clone(),
            TypeCheckedExprKind::FixedArrayRef(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::MapRef(_, _, t) => t.clone(),
            TypeCheckedExprKind::Tuple(_, t) => t.clone(),
            TypeCheckedExprKind::NewArray(_, _, t) => t.clone(),
            TypeCheckedExprKind::NewFixedArray(_, _, t) => t.clone(),
            TypeCheckedExprKind::NewMap(t) => t.clone(),
            TypeCheckedExprKind::ArrayMod(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::FixedArrayMod(_, _, _, _, t) => t.clone(),
            TypeCheckedExprKind::MapMod(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::StructMod(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::Cast(_, t) => t.clone(),
            TypeCheckedExprKind::Asm(t, _, _) => t.clone(),
            TypeCheckedExprKind::Try(_, t) => t.clone(),
        }
    }
}

///A `StructField` that has been type checked.
#[derive(Debug, Clone)]
pub struct TypeCheckedStructField {
    pub name: String,
    pub value: TypeCheckedExpr,
}

impl AbstractSyntaxTree for TypeCheckedStructField {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        self.value.child_nodes()
    }
}

impl TypeCheckedStructField {
    pub fn new(name: String, value: TypeCheckedExpr) -> Self {
        TypeCheckedStructField { name, value }
    }
}

///Returns a vector of `ImportFuncDecl`s corresponding to the builtins as defined by string_table,
/// if they are not defined in string_table, they are inserted.
fn builtin_func_decls(mut string_table: StringTable) -> (Vec<ImportFuncDecl>, StringTable) {
    let imps = vec![
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayNew".to_string()),
            false,
            vec![Type::Uint, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayGet".to_string()),
            false,
            vec![Type::Any, Type::Uint],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arraySet".to_string()),
            false,
            vec![Type::Any, Type::Uint, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsNew".to_string()),
            false,
            vec![],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsHasKey".to_string()),
            false,
            vec![Type::Any, Type::Any],
            Type::Bool,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsGet".to_string()),
            false,
            vec![Type::Any, Type::Any],
            Type::Option(Box::new(Type::Any)),
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsSet".to_string()),
            false,
            vec![Type::Any, Type::Any, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsDelete".to_string()),
            false,
            vec![Type::Any, Type::Any],
            Type::Any,
        ),
    ];
    (imps, string_table)
}

///Sorts the `TopLevelDecl`s into collections based on their type
pub fn sort_top_level_decls(
    decls: &[TopLevelDecl],
    string_table_in: StringTable,
) -> (
    Vec<Import>,
    Vec<ImportedFunc>,
    Vec<FuncDecl>,
    HashMap<usize, Type>,
    Vec<GlobalVarDecl>,
    StringTable,
    HashMap<usize, Type>,
) {
    let mut imports = vec![];
    let mut imported_funcs = Vec::new();
    let mut funcs = Vec::new();
    let mut named_types = HashMap::new();
    let mut func_table = HashMap::new();
    let mut global_vars = Vec::new();

    let (builtin_fds, string_table) = builtin_func_decls(string_table_in);
    for fd in builtin_fds.iter() {
        func_table.insert(fd.name, fd.tipe.clone());
        imported_funcs.push(ImportedFunc::new(
            imported_funcs.len(),
            fd.name,
            &string_table,
            fd.arg_types.clone(),
            fd.ret_type.clone(),
            fd.is_impure,
        ));
    }
    for decl in decls.iter() {
        match decl {
            TopLevelDecl::TypeDecl(td) => {
                named_types.insert(td.name, td.tipe.clone());
            }
            TopLevelDecl::FuncDecl(fd) => {
                funcs.push(fd.clone());
                func_table.insert(fd.name, fd.tipe.clone());
            }
            TopLevelDecl::VarDecl(vd) => {
                global_vars.push(vd.clone());
            }
            TopLevelDecl::ImpFuncDecl(fd) => {
                func_table.insert(fd.name, fd.tipe.clone());
                imported_funcs.push(ImportedFunc::new(
                    imported_funcs.len(),
                    fd.name,
                    &string_table,
                    fd.arg_types.clone(),
                    fd.ret_type.clone(),
                    fd.is_impure,
                ));
            }
            TopLevelDecl::ImpTypeDecl(itd) => {
                named_types.insert(itd.name, itd.tipe.clone());
            }
            TopLevelDecl::UseDecl(path, filename) => {
                imports.push(Import::new(path.clone(), filename.clone()));
            }
        }
    }
    (
        imports,
        imported_funcs,
        funcs,
        named_types,
        global_vars,
        string_table,
        func_table,
    )
}

pub fn typecheck_top_level_decls(
    imported_funcs: Vec<ImportedFunc>,
    funcs: Vec<FuncDecl>,
    named_types: HashMap<usize, Type>,
    global_vars: Vec<GlobalVarDecl>,
    string_table: StringTable,
    func_map: HashMap<usize, Type>,
    checked_funcs: &mut Vec<TypeCheckedFunc>,
    type_tree: &TypeTree,
) -> Result<
    (
        Vec<ExportedFunc>,
        Vec<ImportedFunc>,
        Vec<GlobalVarDecl>,
        StringTable,
    ),
    TypeError,
> {
    let global_vars_map = global_vars
        .iter()
        .enumerate()
        .map(|(idx, var)| (var.name, (var.tipe.clone(), idx)))
        .collect::<HashMap<_, _>>();
    let mut exported_funcs = Vec::new();

    let type_table = SymTable::<Type>::new();
    let type_table = type_table.push_multi(named_types.iter().map(|(k, v)| (*k, v)).collect());

    let mut resolved_global_vars_map = HashMap::new();
    for (name, (tipe, slot_num)) in global_vars_map {
        resolved_global_vars_map.insert(name, (tipe.resolve_types(&type_table, None)?, slot_num));
    }

    let func_table = SymTable::<Type>::new();
    let func_table = func_table.push_multi(func_map.iter().map(|(k, v)| (*k, v)).collect());

    for func in funcs.iter() {
        match func.resolve_types(&type_table, func.location) {
            Ok(f) => {
                match typecheck_function(
                    &f,
                    &type_table,
                    &resolved_global_vars_map,
                    &func_table,
                    type_tree,
                ) {
                    Ok(f) => match func.kind {
                        FuncDeclKind::Public => {
                            exported_funcs.push(ExportedFunc::new(
                                f.name,
                                Label::Func(f.name),
                                f.tipe.clone(),
                                &string_table,
                            ));
                            checked_funcs.push(f);
                        }
                        FuncDeclKind::Private => {
                            checked_funcs.push(f);
                        }
                    },
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            Err(e) => {
                return Err(e);
            }
        }
    }

    let mut res_global_vars = Vec::new();
    for global_var in global_vars {
        res_global_vars.push(global_var.resolve_types(&type_table)?);
    }

    Ok((
        exported_funcs,
        imported_funcs,
        res_global_vars,
        string_table,
    ))
}

///If successful, produces a `TypeCheckedFunc` from `FuncDecl` reference fd, according to global
/// state defined by type_table, global_vars, and func_table.
///
/// If not successful the function returns a `TypeError`.
pub fn typecheck_function<'a>(
    fd: &'a FuncDecl,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &'a SymTable<'a, Type>,
    type_tree: &TypeTree,
) -> Result<TypeCheckedFunc, TypeError> {
    match fd.kind {
        FuncDeclKind::Public | FuncDeclKind::Private => {
            let mut hm = HashMap::new();
            for arg in fd.args.iter() {
                hm.insert(arg.name, &arg.tipe);
            }
            let inner_type_table = type_table.push_multi(hm);
            let tc_stats = typecheck_statement_sequence(
                &fd.code,
                &fd.ret_type,
                &inner_type_table,
                global_vars,
                func_table,
                type_tree,
                &mut vec![],
            )?;
            Ok(TypeCheckedFunc {
                name: fd.name,
                args: fd.args.clone(),
                ret_type: fd.ret_type.clone(),
                code: tc_stats,
                tipe: fd.tipe.clone(),
                imported: false,
                debug_info: DebugInfo::from(fd.location),
                properties: PropertiesList {
                    pure: !fd.is_impure,
                },
            })
        }
    }
}

///If successful, produces a `Vec<TypeCheckedStatement>` corresponding to the items in statements
/// after type checking has been performed sequentially.  Bindings produced by a statement are
/// visible to all statements at a higher index, and no previous statements. If not successful, this
/// function produces a `TypeError`.
///
/// This function is not designed to handle additional variable bindings, for example arguments to
/// functions, for this use case, prefer `typecheck_statement_sequence_with_bindings`.
///
///Takes return_type to ensure that `Return` statements produce the correct type, type_table,
/// global_vars, and func_table should correspond to the types, globals, and functions available
/// to the statement sequence.
fn typecheck_statement_sequence<'a>(
    statements: &'a [Statement],
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
    if statements.is_empty() {
        return Ok(Vec::new());
    }
    let first_stat = &statements[0];
    let rest_of_stats = &statements[1..];

    let (tcs, bindings) = typecheck_statement(
        &first_stat,
        return_type,
        type_table,
        global_vars,
        func_table,
        type_tree,
        scopes,
    )?;
    let mut rest_result = typecheck_statement_sequence_with_bindings(
        rest_of_stats,
        return_type,
        type_table,
        global_vars,
        func_table,
        &bindings,
        type_tree,
        scopes,
    )?;
    rest_result.insert(0, tcs);
    Ok(rest_result)
}

///Operates identically to `typecheck_statement_sequence`, except that the pairs in bindings are
/// added to type_table.
fn typecheck_statement_sequence_with_bindings<'a>(
    statements: &'a [Statement],
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    bindings: &[(StringId, Type)],
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
    if bindings.is_empty() {
        typecheck_statement_sequence(
            statements,
            return_type,
            type_table,
            global_vars,
            func_table,
            type_tree,
            scopes,
        )
    } else {
        let (sid, tipe) = &bindings[0];
        let inner_type_table = type_table.push_one(*sid, &tipe);
        typecheck_statement_sequence_with_bindings(
            statements,
            return_type,
            &inner_type_table,
            global_vars,
            func_table,
            &bindings[1..],
            type_tree,
            scopes,
        )
    }
}

///Performs type checking on statement.
///
/// If successful, returns tuple containing a `TypeCheckedStatement` and a `Vec<(StringId, Type)>`
/// representing the bindings produced by the statement.  Otherwise returns a `TypeError`.
///
/// The argument loc provide the correct location to `TypeError` if the function fails.
fn typecheck_statement<'a>(
    statement: &'a Statement,
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<(TypeCheckedStatement, Vec<(StringId, Type)>), TypeError> {
    let kind = &statement.kind;
    let debug_info = statement.debug_info;
    let (stat, binds) = match kind {
        StatementKind::Noop() => Ok((TypeCheckedStatementKind::Noop(), vec![])),
        StatementKind::Panic() => Ok((TypeCheckedStatementKind::Panic(), vec![])),
        StatementKind::ReturnVoid() => Ok((TypeCheckedStatementKind::ReturnVoid(), vec![])),
        StatementKind::Return(expr) => {
            let tc_expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            if return_type.get_representation(type_tree)?.assignable(
                &tc_expr.get_type().get_representation(type_tree)?,
                type_tree,
            ) {
                Ok((TypeCheckedStatementKind::Return(tc_expr), vec![]))
            } else {
                Err(new_type_error(
                    format!(
                        "return statement has wrong type, expected: \"{:?}\", got: \"{:?}\"",
                        return_type.get_representation(type_tree)?,
                        tc_expr.get_type().get_representation(type_tree)?
                    ),
                    debug_info.location,
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
                            return_type,
                            type_tree,
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
                        new_type_error(
                            "No valid scope to break from".to_string(),
                            debug_info.location,
                        )
                    })?;
                if let Some(t) = tipe {
                    if *t
                        != te
                            .clone()
                            .map(|te| te.get_type())
                            .unwrap_or(Type::Tuple(vec![]))
                    {
                        return Err(new_type_error(
                            format!(
                                "mismatched types in break statement expected {:?}, got {:?}",
                                te.map(|te| te.get_type()).unwrap_or(Type::Tuple(vec![])),
                                tipe
                            ),
                            debug_info.location,
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
                                return_type,
                                type_tree,
                                scopes,
                            )
                        })
                        .transpose()?,
                    scope.clone().unwrap_or("_".to_string()),
                )
            },
            vec![],
        )),
        StatementKind::Expression(expr) => Ok((
            TypeCheckedStatementKind::Expression(typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?),
            vec![],
        )),
        StatementKind::Let(pat, expr) => {
            let tc_expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            let tce_type = tc_expr.get_type();
            match pat {
                MatchPattern::Simple(name) => Ok((
                    TypeCheckedStatementKind::Let(
                        TypeCheckedMatchPattern::Simple(*name, tce_type.clone()),
                        tc_expr,
                    ),
                    vec![(*name, tce_type)],
                )),
                MatchPattern::Tuple(pats) => {
                    let (tc_pats, bindings) =
                        typecheck_patvec(tce_type.clone(), pats.to_vec(), debug_info.location)?;
                    Ok((
                        TypeCheckedStatementKind::Let(
                            TypeCheckedMatchPattern::Tuple(tc_pats, tce_type),
                            tc_expr,
                        ),
                        bindings,
                    ))
                }
            }
        }
        StatementKind::Assign(name, expr) => {
            let tc_expr = typecheck_expr(
                expr,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            match type_table.get(*name) {
                Some(var_type) => {
                    if var_type.get_representation(type_tree)?.assignable(
                        &tc_expr.get_type().get_representation(type_tree)?,
                        type_tree,
                    ) {
                        Ok((
                            TypeCheckedStatementKind::AssignLocal(*name, tc_expr),
                            vec![],
                        ))
                    } else {
                        Err(new_type_error(
                            "mismatched types in assignment statement".to_string(),
                            debug_info.location,
                        ))
                    }
                }
                None => match global_vars.get(&*name) {
                    Some((var_type, idx)) => {
                        if var_type.assignable(
                            &tc_expr.get_type().get_representation(type_tree)?,
                            type_tree,
                        ) {
                            Ok((
                                TypeCheckedStatementKind::AssignGlobal(*idx, tc_expr),
                                vec![],
                            ))
                        } else {
                            Err(new_type_error(
                                "mismatched types in assignment statement".to_string(),
                                debug_info.location,
                            ))
                        }
                    }
                    None => Err(new_type_error(
                        "assignment to non-existent variable".to_string(),
                        debug_info.location,
                    )),
                },
            }
        }
        StatementKind::Loop(body) => {
            scopes.push(("_".to_string(), Some(Type::Tuple(vec![]))));
            let tc_body = typecheck_statement_sequence(
                body,
                return_type,
                type_table,
                global_vars,
                func_table,
                type_tree,
                scopes,
            )?;
            scopes.pop();
            Ok((TypeCheckedStatementKind::Loop(tc_body), vec![]))
        }
        StatementKind::While(cond, body) => {
            let tc_cond = typecheck_expr(
                cond,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            match tc_cond.get_type() {
                Type::Bool => {
                    let tc_body = typecheck_statement_sequence(
                        body,
                        return_type,
                        type_table,
                        global_vars,
                        func_table,
                        type_tree,
                        scopes,
                    )?;
                    Ok((TypeCheckedStatementKind::While(tc_cond, tc_body), vec![]))
                }
                _ => Err(new_type_error(
                    "while condition is not bool".to_string(),
                    debug_info.location,
                )),
            }
        }
        StatementKind::If(arm) => Ok((
            TypeCheckedStatementKind::If(typecheck_if_arm(
                arm,
                return_type,
                type_table,
                global_vars,
                func_table,
                type_tree,
                scopes,
            )?),
            vec![],
        )),
        StatementKind::Asm(insns, args) => {
            let mut tc_args = Vec::new();
            for arg in args {
                tc_args.push(typecheck_expr(
                    arg,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?);
            }
            Ok((
                TypeCheckedStatementKind::Asm(insns.to_vec(), tc_args),
                vec![],
            ))
        }
        StatementKind::DebugPrint(e) => {
            let tce = typecheck_expr(
                e,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            Ok((TypeCheckedStatementKind::DebugPrint(tce), vec![]))
        }
        StatementKind::IfLet(l, r, if_block, else_block) => {
            let tcr = typecheck_expr(
                r,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            let tct = match tcr.get_type() {
                Type::Option(t) => *t,
                unexpected => {
                    return Err(new_type_error(
                        format!("Expected option type got: {:?}", unexpected),
                        debug_info.location,
                    ))
                }
            };
            Ok((
                TypeCheckedStatementKind::IfLet(
                    *l,
                    tcr,
                    typecheck_statement_sequence_with_bindings(
                        if_block,
                        return_type,
                        type_table,
                        global_vars,
                        func_table,
                        &[(*l, tct.clone())],
                        type_tree,
                        scopes,
                    )?,
                    else_block
                        .clone()
                        .map(|block| {
                            typecheck_statement_sequence(
                                &block,
                                return_type,
                                type_table,
                                global_vars,
                                func_table,
                                type_tree,
                                scopes,
                            )
                        })
                        .transpose()?,
                ),
                vec![(*l, tct)],
            ))
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

///Type checks a `Vec<MatchPattern>`, representing a tuple match pattern against `Type` rhs_type.
///
/// This is used in let bindings, and may have other uses in the future.
///
/// If successful this function returns a tuple containing a `TypeCheckedMatchPattern`, and a
/// `Vec<(StringId, Type)>` representing the bindings produced from this match pattern.  Otherwise
/// the function returns a `TypeError`
fn typecheck_patvec(
    rhs_type: Type,
    patterns: Vec<MatchPattern>,
    location: Option<Location>,
) -> Result<(Vec<TypeCheckedMatchPattern>, Vec<(StringId, Type)>), TypeError> {
    if let Type::Tuple(tvec) = rhs_type {
        if tvec.len() == patterns.len() {
            let mut tc_pats = Vec::new();
            let mut bindings = Vec::new();
            for (i, rhs_type) in tvec.iter().enumerate() {
                let pat = &patterns[i];
                match pat {
                    MatchPattern::Simple(name) => {
                        tc_pats.push(TypeCheckedMatchPattern::Simple(*name, rhs_type.clone()));
                        bindings.push((*name, rhs_type.clone()));
                    }
                    MatchPattern::Tuple(_) => {
                        //TODO: implement this properly
                        return Err(new_type_error(
                            "nested pattern not yet supported in let".to_string(),
                            location,
                        ));
                    }
                }
            }
            Ok((tc_pats, bindings))
        } else {
            Err(new_type_error(
                "tuple-match let must receive tuple of equal size".to_string(),
                location,
            ))
        }
    } else {
        Err(new_type_error(
            "tuple-match let must receive tuple value".to_string(),
            location,
        ))
    }
}

///Performs type checking on `IfArm` arm, returning a `TypeCheckedIfArm` if successful, and a
/// `TypeError` otherwise.
///
/// This function takes the the return type of the containing function as arm may contain Return
/// statements.  Also type_table, global_vars, and func_table contain the variables, globals, and
/// functions available to the arm.
fn typecheck_if_arm(
    arm: &IfArm,
    return_type: &Type,
    type_table: &SymTable<Type>,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<TypeCheckedIfArm, TypeError> {
    match arm {
        IfArm::Cond(cond, body, orest, debug_info) => {
            let loc = debug_info.location;
            let tc_cond = typecheck_expr(
                cond,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            match tc_cond.get_type() {
                Type::Bool => Ok(TypeCheckedIfArm::Cond(
                    tc_cond,
                    typecheck_statement_sequence(
                        body,
                        return_type,
                        type_table,
                        global_vars,
                        func_table,
                        type_tree,
                        scopes,
                    )?,
                    match orest {
                        Some(rest) => Some(Box::new(typecheck_if_arm(
                            rest,
                            return_type,
                            type_table,
                            global_vars,
                            func_table,
                            type_tree,
                            scopes,
                        )?)),
                        None => None,
                    },
                    debug_info.clone(),
                )),
                _ => Err(new_type_error(
                    "if condition must be boolean".to_string(),
                    loc,
                )),
            }
        }
        IfArm::Catchall(body, loc) => Ok(TypeCheckedIfArm::Catchall(
            typecheck_statement_sequence(
                body,
                return_type,
                type_table,
                global_vars,
                func_table,
                type_tree,
                scopes,
            )?,
            *loc,
        )),
    }
}

///Performs type checking on the expression expr.  Returns `TypeCheckedExpr` if successful, and
/// `TypeError` otherwise.
///
/// The arguments type_table, global_vars, and func_table represent the variables, globals, and
/// functions available to the expression, and return_type represents the return type of the
/// containing function. This last argument is needed as Try and CodeBlock expressions may return
/// from the function.
fn typecheck_expr(
    expr: &Expr,
    type_table: &SymTable<Type>,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    return_type: &Type,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<TypeCheckedExpr, TypeError> {
    let debug_info = expr.debug_info;
    let loc = debug_info.location;
    Ok(TypeCheckedExpr {
        kind: match &expr.kind {
            ExprKind::UnaryOp(op, subexpr) => {
                let tc_sub = typecheck_expr(
                    subexpr,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
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
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                typecheck_binary_op(*op, tc_sub1, tc_sub2, type_tree, loc)
            }
            ExprKind::ShortcutOr(sub1, sub2) => {
                let tc_sub1 = typecheck_expr(
                    sub1,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                if tc_sub1.get_type() != Type::Bool {
                    return Err(new_type_error(
                        "operands to logical or must be boolean".to_string(),
                        loc,
                    ));
                }
                if tc_sub2.get_type() != Type::Bool {
                    return Err(new_type_error(
                        "operands to logical or must be boolean".to_string(),
                        loc,
                    ));
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
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_sub2 = typecheck_expr(
                    sub2,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                if tc_sub1.get_type() != Type::Bool {
                    return Err(new_type_error(
                        "operands to logical and must be boolean".to_string(),
                        loc,
                    ));
                }
                if tc_sub2.get_type() != Type::Bool {
                    return Err(new_type_error(
                        "operands to logical and must be boolean".to_string(),
                        loc,
                    ));
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
                    return_type,
                    type_tree,
                    scopes,
                )?)))
            }
            ExprKind::VariableRef(name) => match func_table.get(*name) {
                Some(t) => Ok(TypeCheckedExprKind::FuncRef(*name, t.clone())),
                None => match type_table.get(*name) {
                    Some(t) => Ok(TypeCheckedExprKind::LocalVariableRef(*name, t.clone())),
                    None => match global_vars.get(name) {
                        Some((t, idx)) => {
                            Ok(TypeCheckedExprKind::GlobalVariableRef(*idx, t.clone()))
                        }
                        None => Err(new_type_error(
                            "reference to unrecognized identifier".to_string(),
                            loc,
                        )),
                    },
                },
            },
            ExprKind::TupleRef(tref, idx) => {
                let tc_sub = typecheck_expr(
                    &*tref,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let uidx = idx.to_usize().unwrap();
                if let Type::Tuple(tv) = tc_sub.get_type() {
                    if uidx < tv.len() {
                        Ok(TypeCheckedExprKind::TupleRef(
                            Box::new(tc_sub),
                            idx.clone(),
                            tv[uidx].clone(),
                        ))
                    } else {
                        Err(new_type_error(
                            "tuple field access to non-existent field".to_string(),
                            loc,
                        ))
                    }
                } else {
                    Err(new_type_error(
                        "tuple field access to non-tuple value".to_string(),
                        loc,
                    ))
                }
            }
            ExprKind::DotRef(sref, name) => {
                let tc_sub = typecheck_expr(
                    &*sref,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                if let Type::Struct(v) = tc_sub.get_type().get_representation(type_tree)? {
                    for sf in v.iter() {
                        if *name == sf.name {
                            let slot_num = tc_sub
                                .get_type()
                                .get_representation(type_tree)?
                                .get_struct_slot_by_name(name.clone())
                                .ok_or(new_type_error(
                                    "Could not find name of struct field".to_string(),
                                    loc,
                                ))?;
                            return Ok(TypeCheckedExpr {
                                kind: TypeCheckedExprKind::DotRef(
                                    Box::new(tc_sub),
                                    slot_num,
                                    v.len(),
                                    sf.tipe.clone(),
                                ),
                                debug_info,
                            });
                        }
                    }
                    Err(new_type_error(
                        "reference to non-existent struct field".to_string(),
                        loc,
                    ))
                } else {
                    Err(new_type_error(
                        "struct field access to non-struct value".to_string(),
                        loc,
                    ))
                }
            }
            ExprKind::Constant(constant) => Ok(match constant {
                Constant::Uint(n) => TypeCheckedExprKind::Const(Value::Int(n.clone()), Type::Uint),
                Constant::Int(n) => TypeCheckedExprKind::Const(Value::Int(n.clone()), Type::Int),
                Constant::Bool(b) => {
                    TypeCheckedExprKind::Const(Value::Int(Uint256::from_bool(*b)), Type::Bool)
                }
                Constant::Option(o) => TypeCheckedExprKind::Const(o.value(), o.type_of()),
                Constant::Null => TypeCheckedExprKind::Const(Value::none(), Type::Any),
            }),
            ExprKind::FunctionCall(fexpr, args) => {
                let tc_fexpr = typecheck_expr(
                    fexpr,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                match tc_fexpr.get_type().get_representation(type_tree)? {
                    Type::Func(impure, arg_types, ret_type) => {
                        let ret_type = ret_type.resolve_types(type_table, loc)?;
                        if args.len() == arg_types.len() {
                            let mut tc_args = Vec::new();
                            for i in 0..args.len() {
                                let tc_arg = typecheck_expr(
                                    &args[i],
                                    type_table,
                                    global_vars,
                                    func_table,
                                    return_type,
                                    type_tree,
                                    scopes,
                                )?;
                                tc_args.push(tc_arg);
                                let resolved_arg_type =
                                    arg_types[i].resolve_types(&type_table, loc)?;
                                if !resolved_arg_type.assignable(
                                    &tc_args[i].get_type().get_representation(type_tree)?,
                                    type_tree,
                                ) {
                                    println!(
                                        "expected {:?}",
                                        resolved_arg_type.get_representation(type_tree)?
                                    );
                                    println!(
                                        "actual   {:?}",
                                        tc_args[i].get_type().get_representation(type_tree)?
                                    );
                                    return Err(new_type_error(
                                        "wrong argument type in function call".to_string(),
                                        loc,
                                    ));
                                }
                            }
                            Ok(TypeCheckedExprKind::FunctionCall(
                                Box::new(tc_fexpr),
                                tc_args,
                                ret_type,
                                PropertiesList { pure: !impure },
                            ))
                        } else {
                            Err(new_type_error(
                                "wrong number of args passed to function".to_string(),
                                loc,
                            ))
                        }
                    }
                    _ => Err(new_type_error(
                        "function call to value that is not a function".to_string(),
                        loc,
                    )),
                }
            }
            ExprKind::CodeBlock(body, ret_expr) => {
                let mut output = Vec::new();
                let mut block_bindings = Vec::new();
                scopes.push(("_".to_string(), None));
                for statement in body {
                    let inner_type_table = type_table
                        .push_multi(block_bindings.iter().map(|(k, v)| (*k, v)).collect());
                    let (statement, bindings) = typecheck_statement(
                        &statement,
                        return_type,
                        &inner_type_table,
                        global_vars,
                        func_table,
                        type_tree,
                        scopes,
                    )?;
                    output.push(statement);
                    for (key, value) in bindings {
                        block_bindings.push((key, value));
                    }
                }
                let inner_type_table =
                    type_table.push_multi(block_bindings.iter().map(|(k, v)| (*k, v)).collect());
                Ok(TypeCheckedExprKind::CodeBlock(
                    output,
                    ret_expr
                        .clone()
                        .map(|x| {
                            typecheck_expr(
                                &*x,
                                &inner_type_table,
                                global_vars,
                                func_table,
                                return_type,
                                type_tree,
                                scopes,
                            )
                        })
                        .transpose()?
                        .map(Box::new),
                    None,
                ))
            }
            ExprKind::ArrayOrMapRef(array, index) => {
                let tc_arr = typecheck_expr(
                    &*array,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_idx = typecheck_expr(
                    &*index,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                match tc_arr.get_type().get_representation(type_tree)? {
                    Type::Array(t) => {
                        if tc_idx.get_type() == Type::Uint {
                            Ok(TypeCheckedExprKind::ArrayRef(
                                Box::new(tc_arr),
                                Box::new(tc_idx),
                                *t,
                            ))
                        } else {
                            Err(new_type_error("array index must be Uint".to_string(), loc))
                        }
                    }
                    Type::FixedArray(t, sz) => {
                        if tc_idx.get_type() == Type::Uint {
                            Ok(TypeCheckedExprKind::FixedArrayRef(
                                Box::new(tc_arr),
                                Box::new(tc_idx),
                                sz,
                                *t,
                            ))
                        } else {
                            Err(new_type_error(
                                "fixedarray index must be Uint".to_string(),
                                loc,
                            ))
                        }
                    }
                    Type::Map(kt, vt) => {
                        if tc_idx.get_type() == *kt {
                            Ok(TypeCheckedExprKind::MapRef(
                                Box::new(tc_arr),
                                Box::new(tc_idx),
                                Type::Option(Box::new(*vt)),
                            ))
                        } else {
                            Err(new_type_error(
                                "invalid key value in map lookup".to_string(),
                                loc,
                            ))
                        }
                    }
                    _ => Err(new_type_error(
                        "fixedarray lookup in non-array type".to_string(),
                        loc,
                    )),
                }
            }
            ExprKind::NewArray(size_expr, tipe) => Ok(TypeCheckedExprKind::NewArray(
                Box::new(typecheck_expr(
                    size_expr,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?),
                tipe.get_representation(type_tree)?,
                Type::Array(Box::new(tipe.clone())),
            )),
            ExprKind::NewFixedArray(size, maybe_expr) => match maybe_expr {
                Some(expr) => {
                    let tc_expr = typecheck_expr(
                        expr,
                        type_table,
                        global_vars,
                        func_table,
                        return_type,
                        type_tree,
                        scopes,
                    )?;
                    Ok(TypeCheckedExprKind::NewFixedArray(
                        *size,
                        Some(Box::new(tc_expr.clone())),
                        Type::FixedArray(Box::new(tc_expr.get_type()), *size),
                    ))
                }
                None => Ok(TypeCheckedExprKind::NewFixedArray(
                    *size,
                    None,
                    Type::FixedArray(Box::new(Type::Any), *size),
                )),
            },
            ExprKind::NewMap(key_type, value_type) => Ok(TypeCheckedExprKind::NewMap(Type::Map(
                Box::new(key_type.clone()),
                Box::new(value_type.clone()),
            ))),
            ExprKind::StructInitializer(fieldvec) => {
                let mut tc_fields = Vec::new();
                let mut tc_fieldtypes = Vec::new();
                for field in fieldvec {
                    let tc_expr = typecheck_expr(
                        &field.value,
                        type_table,
                        global_vars,
                        func_table,
                        return_type,
                        type_tree,
                        scopes,
                    )?;
                    tc_fields.push(TypeCheckedStructField::new(
                        field.name.clone(),
                        tc_expr.clone(),
                    ));
                    tc_fieldtypes.push(StructField::new(field.name.clone(), tc_expr.get_type()));
                }
                Ok(TypeCheckedExprKind::StructInitializer(
                    tc_fields,
                    Type::Struct(tc_fieldtypes),
                ))
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
                        return_type,
                        type_tree,
                        scopes,
                    )?;
                    types.push(tc_field.get_type().clone());
                    tc_fields.push(tc_field);
                }
                Ok(TypeCheckedExprKind::Tuple(tc_fields, Type::Tuple(types)))
            }
            ExprKind::ArrayOrMapMod(arr, index, val) => {
                let tc_arr = typecheck_expr(
                    arr,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_index = typecheck_expr(
                    index,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_val = typecheck_expr(
                    val,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                match tc_arr.get_type().get_representation(type_tree)? {
                    Type::Array(t) => {
                        if t.assignable(&tc_val.get_type(), type_tree) {
                            if tc_index.get_type() != Type::Uint {
                                Err(new_type_error(
                                    "array modifier requires uint index".to_string(),
                                    loc,
                                ))
                            } else {
                                Ok(TypeCheckedExprKind::ArrayMod(
                                    Box::new(tc_arr),
                                    Box::new(tc_index),
                                    Box::new(tc_val),
                                    Type::Array(t),
                                ))
                            }
                        } else {
                            Err(new_type_error(
                                "mismatched types in array modifier".to_string(),
                                loc,
                            ))
                        }
                    }
                    Type::FixedArray(t, sz) => {
                        if tc_index.get_type() != Type::Uint {
                            Err(new_type_error(
                                "array modifier requires uint index".to_string(),
                                loc,
                            ))
                        } else {
                            Ok(TypeCheckedExprKind::FixedArrayMod(
                                Box::new(tc_arr),
                                Box::new(tc_index),
                                Box::new(tc_val),
                                sz,
                                Type::FixedArray(t, sz),
                            ))
                        }
                    }
                    Type::Map(kt, vt) => {
                        if tc_index.get_type() == *kt {
                            if vt.assignable(&tc_val.get_type(), type_tree) {
                                Ok(TypeCheckedExprKind::MapMod(
                                    Box::new(tc_arr),
                                    Box::new(tc_index),
                                    Box::new(tc_val),
                                    Type::Map(kt, vt),
                                ))
                            } else {
                                Err(new_type_error(
                                    "invalid value type for map modifier".to_string(),
                                    loc,
                                ))
                            }
                        } else {
                            Err(new_type_error(
                                "invalid key type for map modifier".to_string(),
                                loc,
                            ))
                        }
                    }
                    _ => Err(new_type_error(
                        "[] modifier must operate on array or block".to_string(),
                        loc,
                    )),
                }
            }
            ExprKind::StructMod(struc, name, val) => {
                let tc_struc = typecheck_expr(
                    struc,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tc_val = typecheck_expr(
                    val,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tcs_type = tc_struc.get_type().get_representation(type_tree)?;
                if let Type::Struct(fields) = &tcs_type {
                    match tcs_type.get_struct_slot_by_name(name.clone()) {
                        Some(index) => {
                            if fields[index].tipe.assignable(&tc_val.get_type(), type_tree) {
                                Ok(TypeCheckedExprKind::StructMod(
                                    Box::new(tc_struc),
                                    index,
                                    Box::new(tc_val),
                                    tcs_type,
                                ))
                            } else {
                                Err(new_type_error(
                                    "incorrect value type in struct modifier".to_string(),
                                    loc,
                                ))
                            }
                        }
                        None => Err(new_type_error(
                            "struct modifier must use valid field name".to_string(),
                            loc,
                        )),
                    }
                } else {
                    Err(new_type_error(
                        "struct modifier must operate on a struct".to_string(),
                        loc,
                    ))
                }
            }
            ExprKind::UnsafeCast(expr, t) => Ok(TypeCheckedExprKind::Cast(
                Box::new(typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?),
                t.clone(),
            )),
            ExprKind::Asm(ret_type, insns, args) => {
                if *ret_type == Type::Void {
                    return Err(new_type_error(
                        "asm expression cannot return void".to_string(),
                        loc,
                    ));
                }
                let mut tc_args = Vec::new();
                for arg in args {
                    tc_args.push(typecheck_expr(
                        arg,
                        type_table,
                        global_vars,
                        func_table,
                        return_type,
                        type_tree,
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
                match return_type {
                    Type::Option(_) | Type::Any => {}
                    _ => {
                        return Err(new_type_error(
                            "Can only use \"?\" operator in functions that can return option"
                                .to_string(),
                            loc,
                        ))
                    }
                }
                let res = typecheck_expr(
                    inner,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                match res.get_type().get_representation(type_tree)? {
                    Type::Option(t) => Ok(TypeCheckedExprKind::Try(Box::new(res), *t)),
                    other => Err(new_type_error(
                        format!("Try expression requires option type, found \"{:?}\"", other),
                        loc,
                    )),
                }
            }
        }?,
        debug_info,
    })
}

///Attempts to apply the `UnaryOp` op, to `TypeCheckedExpr` sub_expr, producing a `TypeCheckedExpr`
/// if successful, and a `TypeError` otherwise.  The argument loc is used to record the location of
/// op for use in formatting the `TypeError`.
fn typecheck_unary_op(
    op: UnaryOp,
    sub_expr: TypeCheckedExpr,
    loc: Option<Location>,
    type_tree: &TypeTree,
) -> Result<TypeCheckedExprKind, TypeError> {
    let tc_type = sub_expr.get_type().get_representation(type_tree)?;
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
            _ => Err(new_type_error(
                "invalid operand type for unary minus".to_string(),
                loc,
            )),
        },
        UnaryOp::BitwiseNeg => {
            if let TypeCheckedExprKind::Const(Value::Int(ui), _) = sub_expr.kind {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 => Ok(TypeCheckedExprKind::Const(
                        Value::Int(ui.bitwise_neg()),
                        tc_type,
                    )),
                    _ => Err(new_type_error(
                        "invalid operand type for bitwise negation".to_string(),
                        loc,
                    )),
                }
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 => Ok(TypeCheckedExprKind::UnaryOp(
                        UnaryOp::BitwiseNeg,
                        Box::new(sub_expr),
                        tc_type,
                    )),
                    _ => Err(new_type_error(
                        "invalid operand type for bitwise negation".to_string(),
                        loc,
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
            _ => Err(new_type_error(
                "invalid operand type for logical negation".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid operand type for len".to_string(),
                loc,
            )),
        },
        UnaryOp::ToUint => {
            if let TypeCheckedExprKind::Const(val, _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(val, Type::Uint))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExprKind::UnaryOp(
                            UnaryOp::ToUint,
                            Box::new(sub_expr),
                            Type::Uint,
                        ))
                    }
                    _ => Err(new_type_error(
                        "invalid operand type for uint()".to_string(),
                        loc,
                    )),
                }
            }
        }
        UnaryOp::ToInt => {
            if let TypeCheckedExprKind::Const(val, _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(val, Type::Int))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => Ok(
                        TypeCheckedExprKind::UnaryOp(UnaryOp::ToInt, Box::new(sub_expr), Type::Int),
                    ),
                    _ => Err(new_type_error(
                        "invalid operand type for int()".to_string(),
                        loc,
                    )),
                }
            }
        }
        UnaryOp::ToBytes32 => {
            if let TypeCheckedExprKind::Const(val, _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(val, Type::Bytes32))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExprKind::UnaryOp(
                            UnaryOp::ToBytes32,
                            Box::new(sub_expr),
                            Type::Bytes32,
                        ))
                    }
                    _ => Err(new_type_error(
                        "invalid operand type for bytes32()".to_string(),
                        loc,
                    )),
                }
            }
        }
        UnaryOp::ToAddress => {
            if let TypeCheckedExprKind::Const(val, _) = sub_expr.kind {
                Ok(TypeCheckedExprKind::Const(val, Type::EthAddress))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExprKind::UnaryOp(
                            UnaryOp::ToAddress,
                            Box::new(sub_expr),
                            Type::EthAddress,
                        ))
                    }
                    _ => Err(new_type_error(
                        "invalid operand type for bytes32()".to_string(),
                        loc,
                    )),
                }
            }
        }
    }
}

///Attempts to apply the `BinaryOp` op, to `TypeCheckedExpr`s tcs1 on the left, and tcs2 on the
/// right.
///
/// This produces a `TypeCheckedExpr` if successful, and a `TypeError` otherwise.  The argument loc
/// is used to record the location of op for use in formatting the `TypeError`.
fn typecheck_binary_op(
    mut op: BinaryOp,
    mut tcs1: TypeCheckedExpr,
    mut tcs2: TypeCheckedExpr,
    type_tree: &TypeTree,
    loc: Option<Location>,
) -> Result<TypeCheckedExprKind, TypeError> {
    if let TypeCheckedExprKind::Const(Value::Int(val2), t2) = tcs2.kind.clone() {
        if let TypeCheckedExprKind::Const(Value::Int(val1), t1) = tcs1.kind.clone() {
            // both args are constants, so we can do the op at compile time
            return typecheck_binary_op_const(op, val1, t1, val2, t2, loc);
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
    let subtype1 = tcs1.get_type().get_representation(type_tree)?;
    let subtype2 = tcs2.get_type().get_representation(type_tree)?;
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
            _ => Err(new_type_error(
                "invalid argument types to binary op".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to divide".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to mod".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to <".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to >".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to <=".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to >=".to_string(),
                loc,
            )),
        },
        BinaryOp::Equal | BinaryOp::NotEqual => {
            if (subtype1 == Type::Any) || (subtype2 == Type::Any) || (subtype1 == subtype2) {
                Ok(TypeCheckedExprKind::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Bool,
                ))
            } else {
                Err(new_type_error(
                    format!(
                        "invalid argument types to equality comparison: {:?} and {:?}",
                        subtype1, subtype2
                    ),
                    loc,
                ))
            }
        }
        BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor => {
            match (subtype1, subtype2) {
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
                _ => Err(new_type_error(
                    "invalid argument types to binary bitwise operator".to_string(),
                    loc,
                )),
            }
        }
        BinaryOp::_LogicalAnd | BinaryOp::LogicalOr => match (subtype1, subtype2) {
            (Type::Bool, Type::Bool) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            _ => Err(new_type_error(
                "invalid argument types to binary logical operator".to_string(),
                loc,
            )),
        },
        BinaryOp::Hash => match (subtype1, subtype2) {
            (Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bytes32,
            )),
            _ => Err(new_type_error(
                "invalid argument types to binary hash operator".to_string(),
                loc,
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

///Version of `typecheck_binary_op` for when both sub expressions are constant integer types.
///
/// This is used internally by `typecheck_binary_op`, so this generally does not need to be called
/// directly.
///
/// The arguments val1, and t1 represent the value of the left subexpression, and its type, and val2
/// and t2 represent the value and type of the right subexpression, loc is used to format the
/// `TypeError` in case of failure.
fn typecheck_binary_op_const(
    op: BinaryOp,
    val1: Uint256,
    t1: Type,
    val2: Uint256,
    t2: Type,
    loc: Option<Location>,
) -> Result<TypeCheckedExprKind, TypeError> {
    match op {
        BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times => match (&t1, &t2) {
            (Type::Uint, Type::Uint) | (Type::Int, Type::Int) => Ok(TypeCheckedExprKind::Const(
                Value::Int(match op {
                    BinaryOp::Plus => val1.add(&val2),
                    BinaryOp::Minus => {
                        if let Some(val) = val1.sub(&val2) {
                            val
                        } else {
                            return Err(new_type_error(
                                "underflow on substraction".to_string(),
                                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to binary op".to_string(),
                loc,
            )),
        },
        BinaryOp::Div => match (&t1, &t2) {
            (Type::Uint, Type::Uint) => match val1.div(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            (Type::Int, Type::Int) => match val1.sdiv(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            _ => Err(new_type_error(
                "invalid argument types to divide".to_string(),
                loc,
            )),
        },
        BinaryOp::Mod => match (&t1, &t2) {
            (Type::Uint, Type::Uint) => match val1.modulo(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            (Type::Int, Type::Int) => match val1.smodulo(&val2) {
                Some(v) => Ok(TypeCheckedExprKind::Const(Value::Int(v), t1)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            _ => Err(new_type_error(
                "invalid argument types to mod".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to <".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to >".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to <=".to_string(),
                loc,
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
            _ => Err(new_type_error(
                "invalid argument types to >=".to_string(),
                loc,
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
                                return Err(new_type_error(
                                    "invalid argument types to binary op".to_string(),
                                    loc,
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
                Err(new_type_error(
                    "invalid argument types to binary op".to_string(),
                    loc,
                ))
            }
        }
        BinaryOp::_LogicalAnd => {
            if (t1 == Type::Bool) && (t2 == Type::Bool) {
                Ok(TypeCheckedExprKind::Const(
                    Value::Int(Uint256::from_bool(!val1.is_zero() && !val2.is_zero())),
                    Type::Bool,
                ))
            } else {
                Err(new_type_error(
                    "invalid argument types to logical and".to_string(),
                    loc,
                ))
            }
        }
        BinaryOp::LogicalOr => {
            if (t1 == Type::Bool) && (t2 == Type::Bool) {
                Ok(TypeCheckedExprKind::Const(
                    Value::Int(Uint256::from_bool(!val1.is_zero() || !val2.is_zero())),
                    Type::Bool,
                ))
            } else {
                Err(new_type_error(
                    "invalid argument types to logical or".to_string(),
                    loc,
                ))
            }
        }
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
