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
use crate::compile::ast::TypeTree;
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
    pub location: Option<Location>,
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

fn inline(
    to_do: &mut TypeCheckedNode,
    state: &(&Vec<TypeCheckedFunc>, &StringTable),
    _mut_state: &mut (),
) -> bool {
    if let TypeCheckedNode::Expression(exp) = to_do {
        if let TypeCheckedExpr::FunctionCall(name, args, _, _, _) = exp {
            let (code, block_exp) = if let TypeCheckedExpr::FuncRef(id, _, _) = **name {
                let found_func = state.0.iter().find(|func| func.name == id);
                if let Some(func) = found_func {
                    let mut code: Vec<_> = args
                        .iter()
                        .zip(func.args.iter())
                        .map(|(arg, otherarg)| {
                            TypeCheckedStatement::Let(
                                TypeCheckedMatchPattern::Simple(
                                    otherarg.name,
                                    otherarg.tipe.clone(),
                                ),
                                arg.clone(),
                                None,
                            )
                        })
                        .collect();
                    code.append(&mut func.code.clone());
                    let last = code.pop();
                    let block_exp = if let Some(TypeCheckedStatement::Return(exp, _)) = last {
                        Some(Box::new(exp))
                    } else {
                        if let Some(statement) = last {
                            code.push(statement);
                        }
                        None
                    };
                    (code, block_exp)
                } else {
                    println!("fail 1");
                    (vec![], None)
                }
            } else {
                println!("fail 2");
                (vec![], None)
            };
            **exp = TypeCheckedExpr::CodeBlock(code, block_exp, None);
            println!("changed to: {:?}", exp);
            false
        } else {
            true
        }
    } else {
        true
    }
}

impl TypeCheckedFunc {
    pub fn inline(&mut self, funcs: &Vec<TypeCheckedFunc>, string_table: &StringTable) {
        self.recursive_apply(inline, &(funcs, string_table), &mut ());
    }
}

///A mini statement that has been type checked.
#[derive(Debug, Clone)]
pub enum TypeCheckedStatement {
    Noop(Option<Location>),
    Panic(Option<Location>),
    ReturnVoid(Option<Location>),
    Return(TypeCheckedExpr, Option<Location>),
    Break(Option<TypeCheckedExpr>, String, Option<Location>),
    Expression(TypeCheckedExpr, Option<Location>),
    Let(TypeCheckedMatchPattern, TypeCheckedExpr, Option<Location>),
    AssignLocal(StringId, TypeCheckedExpr, Option<Location>),
    AssignGlobal(usize, TypeCheckedExpr, Option<Location>),
    Loop(Vec<TypeCheckedStatement>, Option<Location>),
    While(TypeCheckedExpr, Vec<TypeCheckedStatement>, Option<Location>),
    If(TypeCheckedIfArm),
    IfLet(
        StringId,
        TypeCheckedExpr,
        Vec<TypeCheckedStatement>,
        Option<Vec<TypeCheckedStatement>>,
        Option<Location>,
    ),
    Asm(Vec<Instruction>, Vec<TypeCheckedExpr>, Option<Location>),
    DebugPrint(TypeCheckedExpr, Option<Location>),
}

impl MiniProperties for TypeCheckedStatement {
    fn is_pure(&self) -> bool {
        match self {
            TypeCheckedStatement::Noop(_)
            | TypeCheckedStatement::Panic(_)
            | TypeCheckedStatement::ReturnVoid(_) => true,
            TypeCheckedStatement::Return(something, _) => something.is_pure(),
            TypeCheckedStatement::Break(exp, _, _) => {
                exp.clone().map(|exp| exp.is_pure()).unwrap_or(true)
            }
            TypeCheckedStatement::Expression(expr, _) => expr.is_pure(),
            TypeCheckedStatement::Let(_, exp, _) => exp.is_pure(),
            TypeCheckedStatement::AssignLocal(_, exp, _) => exp.is_pure(),
            TypeCheckedStatement::AssignGlobal(_, _, _) => false,
            TypeCheckedStatement::Loop(code, _) => code.iter().all(|statement| statement.is_pure()),
            TypeCheckedStatement::While(exp, block, _) => {
                exp.is_pure() && block.iter().all(|statement| statement.is_pure())
            }
            TypeCheckedStatement::If(if_arm) => if_arm.is_pure(),
            TypeCheckedStatement::IfLet(_, expr, block, eblock, _) => {
                expr.is_pure()
                    && block.iter().all(|statement| statement.is_pure())
                    && eblock
                        // This clone can most likely be avoided and it would probably be good idea to do so
                        .clone()
                        .map(|statements| statements.iter().all(|statement| statement.is_pure()))
                        .unwrap_or(true)
            }
            TypeCheckedStatement::Asm(instrs, exprs, _) => {
                instrs.iter().all(|instr| instr.is_pure())
                    && exprs.iter().all(|expr| expr.is_pure())
            }
            TypeCheckedStatement::DebugPrint(_, _) => true,
        }
    }
}

impl AbstractSyntaxTree for TypeCheckedStatement {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            TypeCheckedStatement::Noop(_)
            | TypeCheckedStatement::Panic(_)
            | TypeCheckedStatement::ReturnVoid(_) => vec![],
            TypeCheckedStatement::Return(exp, _)
            | TypeCheckedStatement::Expression(exp, _)
            | TypeCheckedStatement::Let(_, exp, _)
            | TypeCheckedStatement::AssignLocal(_, exp, _)
            | TypeCheckedStatement::AssignGlobal(_, exp, _)
            | TypeCheckedStatement::DebugPrint(exp, _) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedStatement::Loop(stats, _) => stats
                .iter_mut()
                .map(|stat| TypeCheckedNode::Statement(stat))
                .collect(),
            TypeCheckedStatement::While(exp, stats, _) => vec![TypeCheckedNode::Expression(exp)]
                .into_iter()
                .chain(
                    stats
                        .iter_mut()
                        .map(|stat| TypeCheckedNode::Statement(stat)),
                )
                .collect(),
            TypeCheckedStatement::If(arm) => vec![TypeCheckedNode::IfArm(arm)],
            TypeCheckedStatement::IfLet(_, exp, stats, ostats, _) => {
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
            TypeCheckedStatement::Asm(_, exps, _) => exps
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedStatement::Break(oexp, _, _) => {
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
        Option<Location>,
    ),
    Catchall(Vec<TypeCheckedStatement>, Option<Location>),
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

///A mini expression that has been type checked.
#[derive(Debug, Clone)]
pub enum TypeCheckedExpr {
    UnaryOp(UnaryOp, Box<TypeCheckedExpr>, Type, Option<Location>),
    Binary(
        BinaryOp,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
        Option<Location>,
    ),
    ShortcutOr(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Option<Location>),
    ShortcutAnd(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Option<Location>),
    LocalVariableRef(StringId, Type, Option<Location>),
    GlobalVariableRef(usize, Type, Option<Location>),
    Variant(Box<TypeCheckedExpr>, Option<Location>),
    FuncRef(usize, Type, Option<Location>),
    TupleRef(Box<TypeCheckedExpr>, Uint256, Type, Option<Location>),
    DotRef(
        Box<TypeCheckedExpr>,
        StringId,
        usize,
        Type,
        Option<Location>,
    ),
    Const(Value, Type, Option<Location>),
    FunctionCall(
        Box<TypeCheckedExpr>,
        Vec<TypeCheckedExpr>,
        Type,
        PropertiesList,
        Option<Location>,
    ),
    CodeBlock(
        Vec<TypeCheckedStatement>,
        Option<Box<TypeCheckedExpr>>,
        Option<Location>,
    ),
    StructInitializer(Vec<TypeCheckedStructField>, Type, Option<Location>),
    ArrayRef(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
        Option<Location>,
    ),
    FixedArrayRef(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        usize,
        Type,
        Option<Location>,
    ),
    MapRef(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
        Option<Location>,
    ),
    Tuple(Vec<TypeCheckedExpr>, Type, Option<Location>),
    NewArray(Box<TypeCheckedExpr>, Type, Type, Option<Location>),
    NewFixedArray(usize, Option<Box<TypeCheckedExpr>>, Type, Option<Location>),
    NewMap(Type, Option<Location>),
    ArrayMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
        Option<Location>,
    ),
    FixedArrayMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        usize,
        Type,
        Option<Location>,
    ),
    MapMod(
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Box<TypeCheckedExpr>,
        Type,
        Option<Location>,
    ),
    StructMod(
        Box<TypeCheckedExpr>,
        usize,
        Box<TypeCheckedExpr>,
        Type,
        Option<Location>,
    ),
    Cast(Box<TypeCheckedExpr>, Type, Option<Location>),
    Asm(
        Type,
        Vec<Instruction>,
        Vec<TypeCheckedExpr>,
        Option<Location>,
    ),
    Try(Box<TypeCheckedExpr>, Type, Option<Location>),
}

impl MiniProperties for TypeCheckedExpr {
    fn is_pure(&self) -> bool {
        match self {
            TypeCheckedExpr::UnaryOp(_, expr, _, _) => expr.is_pure(),
            TypeCheckedExpr::Binary(_, left, right, _, _) => left.is_pure() && right.is_pure(),
            TypeCheckedExpr::ShortcutOr(left, right, _) => left.is_pure() && right.is_pure(),
            TypeCheckedExpr::ShortcutAnd(left, right, _) => left.is_pure() && right.is_pure(),
            TypeCheckedExpr::LocalVariableRef(_, _, _) => true,
            TypeCheckedExpr::GlobalVariableRef(_, _, _) => false,
            TypeCheckedExpr::Variant(expr, _) => expr.is_pure(),
            TypeCheckedExpr::FuncRef(_, func_type, _) => {
                if let Type::Func(impure, _, _) = func_type {
                    !*impure
                } else {
                    panic!("Internal error: func ref has non function type")
                }
            }
            TypeCheckedExpr::TupleRef(expr, _, _, _) => expr.is_pure(),
            TypeCheckedExpr::DotRef(expr, _, _, _, _) => expr.is_pure(),
            TypeCheckedExpr::Const(_, _, _) => true,
            TypeCheckedExpr::FunctionCall(name_expr, fields_exprs, _, properties, _) => {
                name_expr.is_pure()
                    && fields_exprs.iter().all(|statement| statement.is_pure())
                    && properties.pure
            }
            TypeCheckedExpr::CodeBlock(statements, return_expr, _) => {
                statements.iter().all(|statement| statement.is_pure())
                    && return_expr
                        .as_ref()
                        .map(|expr| expr.is_pure())
                        .unwrap_or(true)
            }
            TypeCheckedExpr::StructInitializer(fields, _, _) => {
                fields.iter().all(|field| field.value.is_pure())
            }
            TypeCheckedExpr::ArrayRef(expr, expr2, _, _) => expr.is_pure() && expr2.is_pure(),
            TypeCheckedExpr::FixedArrayRef(expr, expr2, _, _, _) => {
                expr.is_pure() && expr2.is_pure()
            }
            TypeCheckedExpr::MapRef(expr, expr2, _, _) => expr.is_pure() && expr2.is_pure(),
            TypeCheckedExpr::Tuple(exprs, _, _) => exprs.iter().all(|expr| expr.is_pure()),
            TypeCheckedExpr::NewArray(expr, _, _, _) => expr.is_pure(),
            TypeCheckedExpr::NewFixedArray(_, opt_expr, _, _) => {
                if let Some(expr) = opt_expr {
                    expr.is_pure()
                } else {
                    true
                }
            }
            TypeCheckedExpr::NewMap(_, _) => true,
            TypeCheckedExpr::ArrayMod(arr, index, val, _, _) => {
                arr.is_pure() && index.is_pure() && val.is_pure()
            }
            TypeCheckedExpr::FixedArrayMod(arr, index, val, _, _, _) => {
                arr.is_pure() && index.is_pure() && val.is_pure()
            }
            TypeCheckedExpr::MapMod(map, key, val, _, _) => {
                map.is_pure() && key.is_pure() && val.is_pure()
            }
            TypeCheckedExpr::StructMod(the_struct, _, val, _, _) => {
                the_struct.is_pure() && val.is_pure()
            }
            TypeCheckedExpr::Cast(expr, _, _) => expr.is_pure(),
            TypeCheckedExpr::Asm(_, instrs, args, _) => {
                instrs.iter().all(|inst| inst.is_pure()) && args.iter().all(|expr| expr.is_pure())
            }
            TypeCheckedExpr::Try(expr, _, _) => expr.is_pure(),
        }
    }
}

impl AbstractSyntaxTree for TypeCheckedExpr {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            TypeCheckedExpr::LocalVariableRef(_, _, _)
            | TypeCheckedExpr::GlobalVariableRef(_, _, _)
            | TypeCheckedExpr::FuncRef(_, _, _)
            | TypeCheckedExpr::Const(_, _, _)
            | TypeCheckedExpr::NewMap(_, _) => vec![],
            TypeCheckedExpr::UnaryOp(_, exp, _, _)
            | TypeCheckedExpr::Variant(exp, _)
            | TypeCheckedExpr::TupleRef(exp, _, _, _)
            | TypeCheckedExpr::DotRef(exp, _, _, _, _)
            | TypeCheckedExpr::NewArray(exp, _, _, _)
            | TypeCheckedExpr::Cast(exp, _, _)
            | TypeCheckedExpr::Try(exp, _, _) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedExpr::Binary(_, lexp, rexp, _, _)
            | TypeCheckedExpr::ShortcutOr(lexp, rexp, _)
            | TypeCheckedExpr::ShortcutAnd(lexp, rexp, _)
            | TypeCheckedExpr::ArrayRef(lexp, rexp, _, _)
            | TypeCheckedExpr::FixedArrayRef(lexp, rexp, _, _, _)
            | TypeCheckedExpr::MapRef(lexp, rexp, _, _)
            | TypeCheckedExpr::StructMod(lexp, _, rexp, _, _) => vec![
                TypeCheckedNode::Expression(lexp),
                TypeCheckedNode::Expression(rexp),
            ],
            TypeCheckedExpr::FunctionCall(name_exp, arg_exps, _, _, _) => {
                vec![TypeCheckedNode::Expression(name_exp)]
                    .into_iter()
                    .chain(
                        arg_exps
                            .iter_mut()
                            .map(|exp| TypeCheckedNode::Expression(exp)),
                    )
                    .collect()
            }
            TypeCheckedExpr::CodeBlock(stats, oexpr, _) => oexpr
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .chain(
                    stats
                        .iter_mut()
                        .map(|stat| TypeCheckedNode::Statement(stat)),
                )
                .collect(),
            TypeCheckedExpr::StructInitializer(fields, _, _) => fields
                .iter_mut()
                .map(|field| TypeCheckedNode::StructField(field))
                .collect(),
            TypeCheckedExpr::Tuple(exps, _, _) | TypeCheckedExpr::Asm(_, _, exps, _) => exps
                .iter_mut()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedExpr::NewFixedArray(_, oexp, _, _) => oexp
                .into_iter()
                .map(|exp| TypeCheckedNode::Expression(exp))
                .collect(),
            TypeCheckedExpr::ArrayMod(exp1, exp2, exp3, _, _)
            | TypeCheckedExpr::FixedArrayMod(exp1, exp2, exp3, _, _, _)
            | TypeCheckedExpr::MapMod(exp1, exp2, exp3, _, _) => vec![
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
        match self {
            TypeCheckedExpr::UnaryOp(_, _, t, _) => t.clone(),
            TypeCheckedExpr::Binary(_, _, _, t, _) => t.clone(),
            TypeCheckedExpr::ShortcutOr(_, _, _) | TypeCheckedExpr::ShortcutAnd(_, _, _) => {
                Type::Bool
            }
            TypeCheckedExpr::LocalVariableRef(_, t, _) => t.clone(),
            TypeCheckedExpr::GlobalVariableRef(_, t, _) => t.clone(),
            TypeCheckedExpr::FuncRef(_, t, _) => t.clone(),
            TypeCheckedExpr::TupleRef(_, _, t, _) => t.clone(),
            TypeCheckedExpr::Variant(t, _) => Type::Option(Box::new(t.get_type())),
            TypeCheckedExpr::DotRef(_, _, _, t, _) => t.clone(),
            TypeCheckedExpr::Const(_, t, _) => t.clone(),
            TypeCheckedExpr::FunctionCall(_, _, t, _, _) => t.clone(),
            TypeCheckedExpr::CodeBlock(_, expr, _) => expr
                .clone()
                .map(|exp| exp.get_type())
                .unwrap_or_else(|| Type::Tuple(vec![])),
            TypeCheckedExpr::StructInitializer(_, t, _) => t.clone(),
            TypeCheckedExpr::ArrayRef(_, _, t, _) => t.clone(),
            TypeCheckedExpr::FixedArrayRef(_, _, _, t, _) => t.clone(),
            TypeCheckedExpr::MapRef(_, _, t, _) => t.clone(),
            TypeCheckedExpr::Tuple(_, t, _) => t.clone(),
            TypeCheckedExpr::NewArray(_, _, t, _) => t.clone(),
            TypeCheckedExpr::NewFixedArray(_, _, t, _) => t.clone(),
            TypeCheckedExpr::NewMap(t, _) => t.clone(),
            TypeCheckedExpr::ArrayMod(_, _, _, t, _) => t.clone(),
            TypeCheckedExpr::FixedArrayMod(_, _, _, _, t, _) => t.clone(),
            TypeCheckedExpr::MapMod(_, _, _, t, _) => t.clone(),
            TypeCheckedExpr::StructMod(_, _, _, t, _) => t.clone(),
            TypeCheckedExpr::Cast(_, t, _) => t.clone(),
            TypeCheckedExpr::Asm(t, _, _, _) => t.clone(),
            TypeCheckedExpr::Try(_, t, _) => t.clone(),
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
                location: fd.location,
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
        &first_stat.kind,
        &first_stat.debug_info.location,
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
    statement: &'a StatementKind,
    loc: &Option<Location>,
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<(TypeCheckedStatement, Vec<(StringId, Type)>), TypeError> {
    match statement {
        StatementKind::Noop() => Ok((TypeCheckedStatement::Noop(*loc), vec![])),
        StatementKind::Panic() => Ok((TypeCheckedStatement::Panic(*loc), vec![])),
        StatementKind::ReturnVoid() => Ok((TypeCheckedStatement::ReturnVoid(*loc), vec![])),
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
                Ok((TypeCheckedStatement::Return(tc_expr, *loc), vec![]))
            } else {
                Err(new_type_error(
                    format!(
                        "return statement has wrong type, expected: \"{:?}\", got: \"{:?}\"",
                        return_type.get_representation(type_tree)?,
                        tc_expr.get_type().get_representation(type_tree)?
                    ),
                    *loc,
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
                        new_type_error("No valid scope to break from".to_string(), *loc)
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
                            *loc,
                        ));
                    } else {
                        *t = te
                            .clone()
                            .map(|te| te.get_type())
                            .unwrap_or(Type::Tuple(vec![]));
                    }
                }
                TypeCheckedStatement::Break(
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
                    *loc,
                )
            },
            vec![],
        )),
        StatementKind::Expression(expr) => Ok((
            TypeCheckedStatement::Expression(
                typecheck_expr(
                    expr,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?,
                *loc,
            ),
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
                    TypeCheckedStatement::Let(
                        TypeCheckedMatchPattern::Simple(*name, tce_type.clone()),
                        tc_expr,
                        *loc,
                    ),
                    vec![(*name, tce_type)],
                )),
                MatchPattern::Tuple(pats) => {
                    let (tc_pats, bindings) =
                        typecheck_patvec(tce_type.clone(), pats.to_vec(), *loc)?;
                    Ok((
                        TypeCheckedStatement::Let(
                            TypeCheckedMatchPattern::Tuple(tc_pats, tce_type),
                            tc_expr,
                            *loc,
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
                            TypeCheckedStatement::AssignLocal(*name, tc_expr, *loc),
                            vec![],
                        ))
                    } else {
                        Err(new_type_error(
                            "mismatched types in assignment statement".to_string(),
                            *loc,
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
                                TypeCheckedStatement::AssignGlobal(*idx, tc_expr, *loc),
                                vec![],
                            ))
                        } else {
                            Err(new_type_error(
                                "mismatched types in assignment statement".to_string(),
                                *loc,
                            ))
                        }
                    }
                    None => Err(new_type_error(
                        "assignment to non-existent variable".to_string(),
                        *loc,
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
            Ok((TypeCheckedStatement::Loop(tc_body, *loc), vec![]))
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
                    Ok((TypeCheckedStatement::While(tc_cond, tc_body, *loc), vec![]))
                }
                _ => Err(new_type_error(
                    "while condition is not bool".to_string(),
                    *loc,
                )),
            }
        }
        StatementKind::If(arm) => Ok((
            TypeCheckedStatement::If(typecheck_if_arm(
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
                TypeCheckedStatement::Asm(insns.to_vec(), tc_args, *loc),
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
            Ok((TypeCheckedStatement::DebugPrint(tce, *loc), vec![]))
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
                        *loc,
                    ))
                }
            };
            Ok((
                TypeCheckedStatement::IfLet(
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
                    *loc,
                ),
                vec![(*l, tct)],
            ))
        }
    }
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
        IfArm::Cond(cond, body, orest, loc) => {
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
                    *loc,
                )),
                _ => Err(new_type_error(
                    "if condition must be boolean".to_string(),
                    *loc,
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
    match expr {
        Expr::UnaryOp(op, subexpr, loc) => {
            let tc_sub = typecheck_expr(
                subexpr,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?;
            typecheck_unary_op(*op, tc_sub, *loc, type_tree)
        }
        Expr::Binary(op, sub1, sub2, loc) => {
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
            typecheck_binary_op(*op, tc_sub1, tc_sub2, type_tree, *loc)
        }
        Expr::ShortcutOr(sub1, sub2, loc) => {
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
                    *loc,
                ));
            }
            if tc_sub2.get_type() != Type::Bool {
                return Err(new_type_error(
                    "operands to logical or must be boolean".to_string(),
                    *loc,
                ));
            }
            Ok(TypeCheckedExpr::ShortcutOr(
                Box::new(tc_sub1),
                Box::new(tc_sub2),
                *loc,
            ))
        }
        Expr::ShortcutAnd(sub1, sub2, loc) => {
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
                    *loc,
                ));
            }
            if tc_sub2.get_type() != Type::Bool {
                return Err(new_type_error(
                    "operands to logical and must be boolean".to_string(),
                    *loc,
                ));
            }
            Ok(TypeCheckedExpr::ShortcutAnd(
                Box::new(tc_sub1),
                Box::new(tc_sub2),
                *loc,
            ))
        }
        Expr::OptionInitializer(inner, loc) => Ok(TypeCheckedExpr::Variant(
            Box::new(typecheck_expr(
                inner,
                type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?),
            *loc,
        )),
        Expr::VariableRef(name, loc) => match func_table.get(*name) {
            Some(t) => Ok(TypeCheckedExpr::FuncRef(*name, t.clone(), *loc)),
            None => match type_table.get(*name) {
                Some(t) => Ok(TypeCheckedExpr::LocalVariableRef(*name, t.clone(), *loc)),
                None => match global_vars.get(name) {
                    Some((t, idx)) => Ok(TypeCheckedExpr::GlobalVariableRef(*idx, t.clone(), *loc)),
                    None => Err(new_type_error(
                        "reference to unrecognized identifier".to_string(),
                        *loc,
                    )),
                },
            },
        },
        Expr::TupleRef(tref, idx, loc) => {
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
                    Ok(TypeCheckedExpr::TupleRef(
                        Box::new(tc_sub),
                        idx.clone(),
                        tv[uidx].clone(),
                        *loc,
                    ))
                } else {
                    Err(new_type_error(
                        "tuple field access to non-existent field".to_string(),
                        *loc,
                    ))
                }
            } else {
                Err(new_type_error(
                    "tuple field access to non-tuple value".to_string(),
                    *loc,
                ))
            }
        }
        Expr::DotRef(sref, name, loc) => {
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
                            .ok_or(new_type_error("this one".to_string(), None))?;
                        return Ok(TypeCheckedExpr::DotRef(
                            Box::new(tc_sub),
                            slot_num,
                            v.len(),
                            sf.tipe.clone(),
                            *loc,
                        ));
                    }
                }
                Err(new_type_error(
                    "reference to non-existent struct field".to_string(),
                    *loc,
                ))
            } else {
                Err(new_type_error(
                    "struct field access to non-struct value".to_string(),
                    *loc,
                ))
            }
        }
        Expr::Constant(constant, loc) => Ok(match constant {
            Constant::Uint(n) => TypeCheckedExpr::Const(Value::Int(n.clone()), Type::Uint, *loc),
            Constant::Int(n) => TypeCheckedExpr::Const(Value::Int(n.clone()), Type::Int, *loc),
            Constant::Bool(b) => {
                TypeCheckedExpr::Const(Value::Int(Uint256::from_bool(*b)), Type::Bool, *loc)
            }
            Constant::Option(o) => TypeCheckedExpr::Const(o.value(), o.type_of(), *loc),
            Constant::Null => TypeCheckedExpr::Const(Value::none(), Type::Any, *loc),
        }),
        Expr::FunctionCall(fexpr, args, loc) => {
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
                    let ret_type = ret_type.resolve_types(type_table, *loc)?;
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
                                arg_types[i].resolve_types(&type_table, *loc)?;
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
                                    *loc,
                                ));
                            }
                        }
                        Ok(TypeCheckedExpr::FunctionCall(
                            Box::new(tc_fexpr),
                            tc_args,
                            ret_type,
                            PropertiesList { pure: !impure },
                            *loc,
                        ))
                    } else {
                        Err(new_type_error(
                            "wrong number of args passed to function".to_string(),
                            *loc,
                        ))
                    }
                }
                _ => Err(new_type_error(
                    "function call to value that is not a function".to_string(),
                    *loc,
                )),
            }
        }
        Expr::CodeBlock(body, ret_expr, loc) => {
            let mut output = Vec::new();
            let mut block_bindings = Vec::new();
            scopes.push(("_".to_string(), None));
            for statement in body {
                let inner_type_table =
                    type_table.push_multi(block_bindings.iter().map(|(k, v)| (*k, v)).collect());
                let (statement, bindings) = typecheck_statement(
                    &statement.kind,
                    loc,
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
            Ok(TypeCheckedExpr::CodeBlock(
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
                *loc,
            ))
        }
        Expr::ArrayOrMapRef(array, index, loc) => {
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
                        Ok(TypeCheckedExpr::ArrayRef(
                            Box::new(tc_arr),
                            Box::new(tc_idx),
                            *t,
                            *loc,
                        ))
                    } else {
                        Err(new_type_error("array index must be Uint".to_string(), *loc))
                    }
                }
                Type::FixedArray(t, sz) => {
                    if tc_idx.get_type() == Type::Uint {
                        Ok(TypeCheckedExpr::FixedArrayRef(
                            Box::new(tc_arr),
                            Box::new(tc_idx),
                            sz,
                            *t,
                            *loc,
                        ))
                    } else {
                        Err(new_type_error(
                            "fixedarray index must be Uint".to_string(),
                            *loc,
                        ))
                    }
                }
                Type::Map(kt, vt) => {
                    if tc_idx.get_type() == *kt {
                        Ok(TypeCheckedExpr::MapRef(
                            Box::new(tc_arr),
                            Box::new(tc_idx),
                            Type::Option(Box::new(*vt)),
                            *loc,
                        ))
                    } else {
                        Err(new_type_error(
                            "invalid key value in map lookup".to_string(),
                            *loc,
                        ))
                    }
                }
                _ => Err(new_type_error(
                    "fixedarray lookup in non-array type".to_string(),
                    *loc,
                )),
            }
        }
        Expr::NewArray(size_expr, tipe, loc) => Ok(TypeCheckedExpr::NewArray(
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
            *loc,
        )),
        Expr::NewFixedArray(size, maybe_expr, loc) => match maybe_expr {
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
                Ok(TypeCheckedExpr::NewFixedArray(
                    *size,
                    Some(Box::new(tc_expr.clone())),
                    Type::FixedArray(Box::new(tc_expr.get_type()), *size),
                    *loc,
                ))
            }
            None => Ok(TypeCheckedExpr::NewFixedArray(
                *size,
                None,
                Type::FixedArray(Box::new(Type::Any), *size),
                *loc,
            )),
        },
        Expr::NewMap(key_type, value_type, loc) => Ok(TypeCheckedExpr::NewMap(
            Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone())),
            *loc,
        )),
        Expr::StructInitializer(fieldvec, loc) => {
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
            Ok(TypeCheckedExpr::StructInitializer(
                tc_fields,
                Type::Struct(tc_fieldtypes),
                *loc,
            ))
        }
        Expr::Tuple(fields, loc) => {
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
            Ok(TypeCheckedExpr::Tuple(tc_fields, Type::Tuple(types), *loc))
        }
        Expr::ArrayOrMapMod(arr, index, val, loc) => {
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
                                *loc,
                            ))
                        } else {
                            Ok(TypeCheckedExpr::ArrayMod(
                                Box::new(tc_arr),
                                Box::new(tc_index),
                                Box::new(tc_val),
                                Type::Array(t),
                                *loc,
                            ))
                        }
                    } else {
                        Err(new_type_error(
                            "mismatched types in array modifier".to_string(),
                            *loc,
                        ))
                    }
                }
                Type::FixedArray(t, sz) => {
                    if tc_index.get_type() != Type::Uint {
                        Err(new_type_error(
                            "array modifier requires uint index".to_string(),
                            *loc,
                        ))
                    } else {
                        Ok(TypeCheckedExpr::FixedArrayMod(
                            Box::new(tc_arr),
                            Box::new(tc_index),
                            Box::new(tc_val),
                            sz,
                            Type::FixedArray(t, sz),
                            *loc,
                        ))
                    }
                }
                Type::Map(kt, vt) => {
                    if tc_index.get_type() == *kt {
                        if vt.assignable(&tc_val.get_type(), type_tree) {
                            Ok(TypeCheckedExpr::MapMod(
                                Box::new(tc_arr),
                                Box::new(tc_index),
                                Box::new(tc_val),
                                Type::Map(kt, vt),
                                *loc,
                            ))
                        } else {
                            Err(new_type_error(
                                "invalid value type for map modifier".to_string(),
                                *loc,
                            ))
                        }
                    } else {
                        Err(new_type_error(
                            "invalid key type for map modifier".to_string(),
                            *loc,
                        ))
                    }
                }
                _ => Err(new_type_error(
                    "[] modifier must operate on array or block".to_string(),
                    *loc,
                )),
            }
        }
        Expr::StructMod(struc, name, val, loc) => {
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
                            Ok(TypeCheckedExpr::StructMod(
                                Box::new(tc_struc),
                                index,
                                Box::new(tc_val),
                                tcs_type,
                                *loc,
                            ))
                        } else {
                            Err(new_type_error(
                                "incorrect value type in struct modifier".to_string(),
                                *loc,
                            ))
                        }
                    }
                    None => Err(new_type_error(
                        "struct modifier must use valid field name".to_string(),
                        *loc,
                    )),
                }
            } else {
                Err(new_type_error(
                    "struct modifier must operate on a struct".to_string(),
                    *loc,
                ))
            }
        }
        Expr::UnsafeCast(expr, t, loc) => Ok(TypeCheckedExpr::Cast(
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
            *loc,
        )),
        Expr::Asm(ret_type, insns, args, loc) => {
            if *ret_type == Type::Void {
                return Err(new_type_error(
                    "asm expression cannot return void".to_string(),
                    *loc,
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
            Ok(TypeCheckedExpr::Asm(
                ret_type.clone(),
                insns.to_vec(),
                tc_args,
                *loc,
            ))
        }
        Expr::Try(inner, loc) => {
            match return_type {
                Type::Option(_) | Type::Any => {}
                _ => {
                    return Err(new_type_error(
                        "Can only use \"?\" operator in functions that can return option"
                            .to_string(),
                        *loc,
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
                Type::Option(t) => Ok(TypeCheckedExpr::Try(Box::new(res), *t, *loc)),
                other => Err(new_type_error(
                    format!("Try expression requires option type, found \"{:?}\"", other),
                    *loc,
                )),
            }
        }
    }
}

///Attempts to apply the `UnaryOp` op, to `TypeCheckedExpr` sub_expr, producing a `TypeCheckedExpr`
/// if successful, and a `TypeError` otherwise.  The argument loc is used to record the location of
/// op for use in formatting the `TypeError`.
fn typecheck_unary_op(
    op: UnaryOp,
    sub_expr: TypeCheckedExpr,
    loc: Option<Location>,
    type_tree: &TypeTree,
) -> Result<TypeCheckedExpr, TypeError> {
    let tc_type = sub_expr.get_type().get_representation(type_tree)?;
    match op {
        UnaryOp::Minus => match tc_type {
            Type::Int => {
                if let TypeCheckedExpr::Const(Value::Int(ui), _, loc) = sub_expr {
                    Ok(TypeCheckedExpr::Const(
                        Value::Int(ui.unary_minus().unwrap()),
                        Type::Int,
                        loc,
                    ))
                } else {
                    Ok(TypeCheckedExpr::UnaryOp(
                        UnaryOp::Minus,
                        Box::new(sub_expr),
                        Type::Int,
                        loc,
                    ))
                }
            }
            _ => Err(new_type_error(
                "invalid operand type for unary minus".to_string(),
                loc,
            )),
        },
        UnaryOp::BitwiseNeg => {
            if let TypeCheckedExpr::Const(Value::Int(ui), _, loc) = sub_expr {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 => Ok(TypeCheckedExpr::Const(
                        Value::Int(ui.bitwise_neg()),
                        tc_type,
                        loc,
                    )),
                    _ => Err(new_type_error(
                        "invalid operand type for bitwise negation".to_string(),
                        loc,
                    )),
                }
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 => Ok(TypeCheckedExpr::UnaryOp(
                        UnaryOp::BitwiseNeg,
                        Box::new(sub_expr),
                        tc_type,
                        loc,
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
                if let TypeCheckedExpr::Const(Value::Int(ui), _, loc) = sub_expr {
                    let b = ui.to_usize().unwrap();
                    Ok(TypeCheckedExpr::Const(
                        Value::Int(Uint256::from_usize(1 - b)),
                        Type::Bool,
                        loc,
                    ))
                } else {
                    Ok(TypeCheckedExpr::UnaryOp(
                        UnaryOp::Not,
                        Box::new(sub_expr),
                        Type::Bool,
                        loc,
                    ))
                }
            }
            _ => Err(new_type_error(
                "invalid operand type for logical negation".to_string(),
                loc,
            )),
        },
        UnaryOp::Hash => {
            if let TypeCheckedExpr::Const(Value::Int(ui), _, loc) = sub_expr {
                Ok(TypeCheckedExpr::Const(
                    Value::Int(ui.avm_hash()),
                    Type::Bytes32,
                    loc,
                ))
            } else {
                Ok(TypeCheckedExpr::UnaryOp(
                    UnaryOp::Hash,
                    Box::new(sub_expr),
                    Type::Bytes32,
                    loc,
                ))
            }
        }
        UnaryOp::Len => match tc_type {
            Type::Tuple(tv) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_usize(tv.len())),
                Type::Uint,
                loc,
            )),
            Type::FixedArray(_, sz) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_usize(sz)),
                Type::Uint,
                loc,
            )),
            Type::Array(_) => Ok(TypeCheckedExpr::UnaryOp(
                UnaryOp::Len,
                Box::new(sub_expr),
                Type::Uint,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid operand type for len".to_string(),
                loc,
            )),
        },
        UnaryOp::ToUint => {
            if let TypeCheckedExpr::Const(val, _, loc) = sub_expr {
                Ok(TypeCheckedExpr::Const(val, Type::Uint, loc))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExpr::UnaryOp(
                            UnaryOp::ToUint,
                            Box::new(sub_expr),
                            Type::Uint,
                            loc,
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
            if let TypeCheckedExpr::Const(val, _, loc) = sub_expr {
                Ok(TypeCheckedExpr::Const(val, Type::Int, loc))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExpr::UnaryOp(
                            UnaryOp::ToInt,
                            Box::new(sub_expr),
                            Type::Int,
                            loc,
                        ))
                    }
                    _ => Err(new_type_error(
                        "invalid operand type for int()".to_string(),
                        loc,
                    )),
                }
            }
        }
        UnaryOp::ToBytes32 => {
            if let TypeCheckedExpr::Const(val, _, loc) = sub_expr {
                Ok(TypeCheckedExpr::Const(val, Type::Bytes32, loc))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExpr::UnaryOp(
                            UnaryOp::ToBytes32,
                            Box::new(sub_expr),
                            Type::Bytes32,
                            loc,
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
            if let TypeCheckedExpr::Const(val, _, loc) = sub_expr {
                Ok(TypeCheckedExpr::Const(val, Type::EthAddress, loc))
            } else {
                match tc_type {
                    Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                        Ok(TypeCheckedExpr::UnaryOp(
                            UnaryOp::ToAddress,
                            Box::new(sub_expr),
                            Type::EthAddress,
                            loc,
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
) -> Result<TypeCheckedExpr, TypeError> {
    if let TypeCheckedExpr::Const(Value::Int(val2), t2, _) = tcs2.clone() {
        if let TypeCheckedExpr::Const(Value::Int(val1), t1, _) = tcs1.clone() {
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
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to binary op".to_string(),
                loc,
            )),
        },
        BinaryOp::Div => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                BinaryOp::Sdiv,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to divide".to_string(),
                loc,
            )),
        },
        BinaryOp::Mod => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                BinaryOp::Smod,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Int,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to mod".to_string(),
                loc,
            )),
        },
        BinaryOp::LessThan => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                BinaryOp::SLessThan,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to <".to_string(),
                loc,
            )),
        },
        BinaryOp::GreaterThan => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                BinaryOp::SGreaterThan,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to >".to_string(),
                loc,
            )),
        },
        BinaryOp::LessEq => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                BinaryOp::SLessEq,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to <=".to_string(),
                loc,
            )),
        },
        BinaryOp::GreaterEq => match (subtype1, subtype2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                BinaryOp::SGreaterEq,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to >=".to_string(),
                loc,
            )),
        },
        BinaryOp::Equal | BinaryOp::NotEqual => {
            if (subtype1 == Type::Any) || (subtype2 == Type::Any) || (subtype1 == subtype2) {
                Ok(TypeCheckedExpr::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Bool,
                    loc,
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
                (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Uint,
                    loc,
                )),
                (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Int,
                    loc,
                )),
                (Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExpr::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Bytes32,
                    loc,
                )),
                _ => Err(new_type_error(
                    "invalid argument types to binary bitwise operator".to_string(),
                    loc,
                )),
            }
        }
        BinaryOp::_LogicalAnd | BinaryOp::LogicalOr => match (subtype1, subtype2) {
            (Type::Bool, Type::Bool) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to binary logical operator".to_string(),
                loc,
            )),
        },
        BinaryOp::Hash => match (subtype1, subtype2) {
            (Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExpr::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bytes32,
                loc,
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
) -> Result<TypeCheckedExpr, TypeError> {
    match op {
        BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times => match (&t1, &t2) {
            (Type::Uint, Type::Uint) | (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Const(
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
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to binary op".to_string(),
                loc,
            )),
        },
        BinaryOp::Div => match (&t1, &t2) {
            (Type::Uint, Type::Uint) => match val1.div(&val2) {
                Some(v) => Ok(TypeCheckedExpr::Const(Value::Int(v), t1, loc)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            (Type::Int, Type::Int) => match val1.sdiv(&val2) {
                Some(v) => Ok(TypeCheckedExpr::Const(Value::Int(v), t1, loc)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            _ => Err(new_type_error(
                "invalid argument types to divide".to_string(),
                loc,
            )),
        },
        BinaryOp::Mod => match (&t1, &t2) {
            (Type::Uint, Type::Uint) => match val1.modulo(&val2) {
                Some(v) => Ok(TypeCheckedExpr::Const(Value::Int(v), t1, loc)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            (Type::Int, Type::Int) => match val1.smodulo(&val2) {
                Some(v) => Ok(TypeCheckedExpr::Const(Value::Int(v), t1, loc)),
                None => Err(new_type_error("divide by constant zero".to_string(), loc)),
            },
            _ => Err(new_type_error(
                "invalid argument types to mod".to_string(),
                loc,
            )),
        },
        BinaryOp::LessThan => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(val1 < val2)),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(val1.s_less_than(&val2))),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to <".to_string(),
                loc,
            )),
        },
        BinaryOp::GreaterThan => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(val1 > val2)),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(val2.s_less_than(&val1))),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to >".to_string(),
                loc,
            )),
        },
        BinaryOp::LessEq => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(val1 <= val2)),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(!val2.s_less_than(&val1))),
                Type::Bool,
                loc,
            )),
            _ => Err(new_type_error(
                "invalid argument types to <=".to_string(),
                loc,
            )),
        },
        BinaryOp::GreaterEq => match (t1, t2) {
            (Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(val1 >= val2)),
                Type::Bool,
                loc,
            )),
            (Type::Int, Type::Int) => Ok(TypeCheckedExpr::Const(
                Value::Int(Uint256::from_bool(!val1.s_less_than(&val2))),
                Type::Bool,
                loc,
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
                Ok(TypeCheckedExpr::Const(
                    Value::Int(match op {
                        BinaryOp::Equal => Uint256::from_bool(val1 == val2),
                        BinaryOp::NotEqual => Uint256::from_bool(val1 != val2),
                        BinaryOp::BitwiseAnd => val1.bitwise_and(&val2),
                        BinaryOp::BitwiseOr => val1.bitwise_or(&val2),
                        BinaryOp::BitwiseXor => val1.bitwise_xor(&val2),
                        BinaryOp::Hash => {
                            if let Type::Bytes32 = t1 {
                                return Ok(TypeCheckedExpr::Const(
                                    Value::avm_hash2(&Value::Int(val1), &Value::Int(val2)),
                                    Type::Bool,
                                    loc,
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
                    loc,
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
                Ok(TypeCheckedExpr::Const(
                    Value::Int(Uint256::from_bool(!val1.is_zero() && !val2.is_zero())),
                    Type::Bool,
                    loc,
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
                Ok(TypeCheckedExpr::Const(
                    Value::Int(Uint256::from_bool(!val1.is_zero() || !val2.is_zero())),
                    Type::Bool,
                    loc,
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
