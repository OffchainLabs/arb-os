/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Converts non-type checked ast nodes to type checked versions, and other related utilities.

use super::ast::{
    BinaryOp, CodeBlock, Constant, DebugInfo, Expr, ExprKind, Func, FuncDeclKind, GlobalVarDecl,
    MatchPattern, MatchPatternKind, Statement, StatementKind, StructField, TopLevelDecl, TrinaryOp,
    Type, TypeTree, UnaryOp,
};
use crate::compile::ast::FieldInitializer;
use crate::link::{ExportedFunc, Import, ImportedFunc};
use crate::mavm::{Instruction, Label, Value};
use crate::pos::Location;
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

type TypeTable = HashMap<usize, Type>;

///Trait for all nodes in the AST, currently only implemented for type checked versions.
pub trait AbstractSyntaxTree {
    ///Returns a list of direct children of `self`
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        vec![]
    }
    ///Applies `func` to `self` recursively, stopping when `func` returns `false`.  The `state` and
    ///`mut_state` arguments are accessible to all nodes called by this method, and `mut_state` can
    ///be modified by `func`.  The modifications will only be visible to the child nodes.
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
    fn is_pure(&mut self) -> bool;
}

///Represents a mutable reference to any AST node.
#[derive(Debug)]
pub enum TypeCheckedNode<'a> {
    Statement(&'a mut TypeCheckedStatement),
    Expression(&'a mut TypeCheckedExpr),
    StructField(&'a mut TypeCheckedFieldInitializer),
    Type(&'a mut Type),
}

impl<'a> AbstractSyntaxTree for TypeCheckedNode<'a> {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            TypeCheckedNode::Statement(stat) => stat.child_nodes(),
            TypeCheckedNode::Expression(exp) => exp.child_nodes(),
            TypeCheckedNode::StructField(field) => field.child_nodes(),
            TypeCheckedNode::Type(tipe) => tipe.child_nodes(),
        }
    }
    fn is_pure(&mut self) -> bool {
        match self {
            TypeCheckedNode::Statement(stat) => stat.is_pure(),
            TypeCheckedNode::Expression(exp) => exp.is_pure(),
            TypeCheckedNode::StructField(field) => field.is_pure(),
            TypeCheckedNode::Type(_) => true,
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
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct PropertiesList {
    pub pure: bool,
}

pub type TypeCheckedFunc = Func<TypeCheckedStatement>;

impl AbstractSyntaxTree for TypeCheckedFunc {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        self.code
            .iter_mut()
            .map(|stat| TypeCheckedNode::Statement(stat))
            .collect()
    }
    fn is_pure(&mut self) -> bool {
        self.code.iter_mut().all(|statement| statement.is_pure())
    }
}

///Used by inlining to replace early returns with break statements
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

///Used to inline an AST node
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
                kind: TypeCheckedExprKind::FuncRef(id, _),
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
                                TypeCheckedMatchPattern::new_simple(
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
                exp.kind = TypeCheckedExprKind::CodeBlock(TypeCheckedCodeBlock::new(
                    code,
                    block_exp,
                    Some("_inline".to_string()),
                ));
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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeCheckedStatement {
    pub kind: TypeCheckedStatementKind,
    pub debug_info: DebugInfo,
}

///A mini statement that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeCheckedStatementKind {
    Noop(),
    ReturnVoid(),
    Return(TypeCheckedExpr),
    Break(Option<TypeCheckedExpr>, String),
    Expression(TypeCheckedExpr),
    Let(TypeCheckedMatchPattern, TypeCheckedExpr),
    AssignLocal(StringId, TypeCheckedExpr),
    AssignGlobal(usize, TypeCheckedExpr),
    While(TypeCheckedExpr, Vec<TypeCheckedStatement>),
    Asm(Vec<Instruction>, Vec<TypeCheckedExpr>),
    DebugPrint(TypeCheckedExpr),
}

impl AbstractSyntaxTree for TypeCheckedStatement {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match &mut self.kind {
            TypeCheckedStatementKind::Noop() | TypeCheckedStatementKind::ReturnVoid() => vec![],
            TypeCheckedStatementKind::Return(exp)
            | TypeCheckedStatementKind::Expression(exp)
            | TypeCheckedStatementKind::Let(_, exp)
            | TypeCheckedStatementKind::AssignLocal(_, exp)
            | TypeCheckedStatementKind::AssignGlobal(_, exp)
            | TypeCheckedStatementKind::DebugPrint(exp) => vec![TypeCheckedNode::Expression(exp)],
            TypeCheckedStatementKind::While(exp, stats) => vec![TypeCheckedNode::Expression(exp)]
                .into_iter()
                .chain(
                    stats
                        .iter_mut()
                        .map(|stat| TypeCheckedNode::Statement(stat)),
                )
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
    fn is_pure(&mut self) -> bool {
        if let TypeCheckedStatementKind::Noop() | TypeCheckedStatementKind::ReturnVoid() = self.kind
        {
            true
        } else if let TypeCheckedStatementKind::AssignGlobal(_, _) = self.kind {
            false
        } else if let TypeCheckedStatementKind::Asm(vec, _) = &self.kind {
            vec.iter().all(|insn| insn.is_pure())
                && self.child_nodes().iter_mut().all(|node| node.is_pure())
        } else {
            self.child_nodes().iter_mut().all(|node| node.is_pure())
        }
    }
}

pub type TypeCheckedMatchPattern = MatchPattern<Type>;

///A mini expression with associated `DebugInfo` that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeCheckedExpr {
    pub kind: TypeCheckedExprKind,
    pub debug_info: DebugInfo,
}

///A mini expression that has been type checked.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeCheckedExprKind {
    NewBuffer,
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
    CodeBlock(TypeCheckedCodeBlock),
    StructInitializer(Vec<TypeCheckedFieldInitializer>, Type),
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
    Panic,
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
    Loop(Vec<TypeCheckedStatement>),
}

impl AbstractSyntaxTree for TypeCheckedExpr {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match &mut self.kind {
            TypeCheckedExprKind::LocalVariableRef(_, _)
            | TypeCheckedExprKind::GlobalVariableRef(_, _)
            | TypeCheckedExprKind::FuncRef(_, _)
            | TypeCheckedExprKind::Const(_, _)
            | TypeCheckedExprKind::NewBuffer
            | TypeCheckedExprKind::NewMap(_)
            | TypeCheckedExprKind::Panic => vec![],
            TypeCheckedExprKind::UnaryOp(_, exp, _)
            | TypeCheckedExprKind::Variant(exp)
            | TypeCheckedExprKind::TupleRef(exp, _, _)
            | TypeCheckedExprKind::DotRef(exp, _, _, _)
            | TypeCheckedExprKind::NewArray(exp, _, _)
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
            TypeCheckedExprKind::CodeBlock(block) => block.child_nodes(),
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
            TypeCheckedExprKind::Loop(stats) => stats
                .iter_mut()
                .map(|stat| TypeCheckedNode::Statement(stat))
                .collect(),
        }
    }
    fn is_pure(&mut self) -> bool {
        if let TypeCheckedExprKind::GlobalVariableRef(_, _) = self.kind {
            false
        } else if let TypeCheckedExprKind::FuncRef(_, tipe) = &self.kind {
            if let Type::Func(impure, _, _) = tipe {
                !*impure
            } else {
                panic!("Internal error: func ref has non function type")
            }
        } else if let TypeCheckedExprKind::Asm(_, instrs, args) = &mut self.kind {
            instrs.iter().all(|inst| inst.is_pure()) && args.iter_mut().all(|expr| expr.is_pure())
        } else {
            self.child_nodes().iter_mut().all(|node| node.is_pure())
        }
    }
}

impl TypeCheckedExpr {
    ///Extracts the type returned from the expression.
    pub fn get_type(&self) -> Type {
        match &self.kind {
            TypeCheckedExprKind::NewBuffer => Type::Buffer,
            TypeCheckedExprKind::Panic => Type::Every,
            TypeCheckedExprKind::UnaryOp(_, _, t) => t.clone(),
            TypeCheckedExprKind::Binary(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::Trinary(_, _, _, _, t) => t.clone(),
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
            TypeCheckedExprKind::CodeBlock(block) => block.get_type(),
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
            TypeCheckedExprKind::If(_, _, _, t) => t.clone(),
            TypeCheckedExprKind::IfLet(_, _, _, _, t) => t.clone(),
            TypeCheckedExprKind::Loop(_) => Type::Every,
        }
    }
}

type TypeCheckedFieldInitializer = FieldInitializer<TypeCheckedExpr>;

impl AbstractSyntaxTree for TypeCheckedFieldInitializer {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        self.value.child_nodes()
    }
    fn is_pure(&mut self) -> bool {
        self.value.is_pure()
    }
}

///Returns a vector of `ImportFuncDecl`s corresponding to the builtins as defined by string_table,
/// if they are not defined in string_table, they are inserted.
fn builtin_func_decls() -> Vec<Import> {
    vec![
        Import::new(
            vec!["core".to_string(), "array".to_string()],
            "builtin_arrayNew".to_string(),
        ),
        Import::new(
            vec!["core".to_string(), "array".to_string()],
            "builtin_arrayGet".to_string(),
        ),
        Import::new(
            vec!["core".to_string(), "array".to_string()],
            "builtin_arraySet".to_string(),
        ),
        Import::new(
            vec!["core".to_string(), "kvs".to_string()],
            "builtin_kvsNew".to_string(),
        ),
        Import::new(
            vec!["core".to_string(), "kvs".to_string()],
            "builtin_kvsGet".to_string(),
        ),
        Import::new(
            vec!["core".to_string(), "kvs".to_string()],
            "builtin_kvsSet".to_string(),
        ),
    ]
}

///Sorts the `TopLevelDecl`s into collections based on their type
pub fn sort_top_level_decls(
    decls: &[TopLevelDecl],
) -> (
    Vec<Import>,
    Vec<Func>,
    HashMap<usize, Type>,
    Vec<GlobalVarDecl>,
    HashMap<usize, Type>,
) {
    let mut imports = builtin_func_decls();
    let mut funcs = Vec::new();
    let mut named_types = HashMap::new();
    let mut func_table = HashMap::new();
    let mut global_vars = Vec::new();

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
            TopLevelDecl::UseDecl(path, filename) => {
                imports.push(Import::new(path.clone(), filename.clone()));
            }
        }
    }
    (imports, funcs, named_types, global_vars, func_table)
}

///Performs typechecking various top level declarations, including `ImportedFunc`s, `FuncDecl`s,
/// named `Type`s, and global variables.
pub fn typecheck_top_level_decls(
    funcs: Vec<Func>,
    named_types: HashMap<usize, Type>,
    global_vars: Vec<GlobalVarDecl>,
    string_table: StringTable,
    func_map: HashMap<usize, Type>,
    checked_funcs: &mut Vec<TypeCheckedFunc>,
    type_tree: &TypeTree,
) -> Result<(Vec<ExportedFunc>, Vec<GlobalVarDecl>, StringTable), TypeError> {
    let global_vars_map = global_vars
        .iter()
        .enumerate()
        .map(|(idx, var)| (var.name_id, (var.tipe.clone(), idx)))
        .collect::<HashMap<_, _>>();
    let mut exported_funcs = Vec::new();

    let type_table: HashMap<_, _> = named_types.clone().into_iter().collect();

    let mut resolved_global_vars_map = HashMap::new();
    for (name, (tipe, slot_num)) in global_vars_map {
        resolved_global_vars_map.insert(name, (tipe, slot_num));
    }

    let func_table: HashMap<_, _> = func_map.clone().into_iter().collect();

    for func in funcs.iter() {
        let f = typecheck_function(
            &func,
            &type_table,
            &resolved_global_vars_map,
            &func_table,
            type_tree,
            &string_table,
        )?;
        match func.kind {
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
        }
    }

    let mut res_global_vars = Vec::new();
    for global_var in global_vars {
        res_global_vars.push(global_var);
    }

    Ok((exported_funcs, res_global_vars, string_table))
}

///If successful, produces a `TypeCheckedFunc` from `FuncDecl` reference fd, according to global
/// state defined by type_table, global_vars, and func_table.
///
/// If not successful the function returns a `TypeError`.
pub fn typecheck_function(
    fd: &Func,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &TypeTable,
    type_tree: &TypeTree,
    string_table: &StringTable,
) -> Result<TypeCheckedFunc, TypeError> {
    let mut hm = HashMap::new();
    if fd.ret_type != Type::Void {
        if let Some(stat) = fd.code.last() {
            match &stat.kind {
                StatementKind::Return(_) => {}
                _ => {
                    return Err(new_type_error(
                        format!("Last statement of function must be return"),
                        stat.debug_info.location,
                    ))
                }
            }
        }
    }
    for arg in fd.args.iter() {
        arg.tipe.get_representation(type_tree).map_err(|_| {
            new_type_error(
                format!(
                    "Unknown type for function argument \"{}\"",
                    string_table.name_from_id(arg.name)
                ),
                arg.debug_info.location,
            )
        })?;
        hm.insert(arg.name, arg.tipe.clone());
    }
    let mut inner_type_table = type_table.clone();
    inner_type_table.extend(hm);
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
        kind: fd.kind,
        debug_info: DebugInfo::from(fd.debug_info),
        properties: fd.properties.clone(),
    })
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
fn typecheck_statement_sequence(
    statements: &[Statement],
    return_type: &Type,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &TypeTable,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
    typecheck_statement_sequence_with_bindings(
        &statements,
        return_type,
        type_table,
        global_vars,
        func_table,
        &[],
        type_tree,
        scopes,
    )
}

///Operates identically to `typecheck_statement_sequence`, except that the pairs in bindings are
/// added to type_table.
fn typecheck_statement_sequence_with_bindings<'a>(
    statements: &'a [Statement],
    return_type: &Type,
    type_table: &'a TypeTable,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &TypeTable,
    bindings: &[(StringId, Type)],
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
    let mut inner_type_table = type_table.clone();
    for (sid, tipe) in bindings {
        inner_type_table.insert(*sid, tipe.clone());
    }
    let mut output = vec![];
    for stat in statements {
        let (tcs, bindings) = typecheck_statement(
            stat,
            return_type,
            &inner_type_table,
            global_vars,
            func_table,
            type_tree,
            scopes,
        )?;
        output.push(tcs);
        for (sid, bind) in bindings {
            inner_type_table.insert(sid, bind);
        }
    }
    Ok(output)
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
    type_table: &'a TypeTable,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &TypeTable,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<(TypeCheckedStatement, Vec<(StringId, Type)>), TypeError> {
    let kind = &statement.kind;
    let debug_info = statement.debug_info;
    let (stat, binds) = match kind {
        StatementKind::Noop() => Ok((TypeCheckedStatementKind::Noop(), vec![])),
        StatementKind::ReturnVoid() => {
            if Type::Void.assignable(return_type, type_tree, HashSet::new()) {
                Ok((TypeCheckedStatementKind::ReturnVoid(), vec![]))
            } else {
                Err(new_type_error(
                    format!(
                        "Tried to return without type in function that returns {}",
                        return_type.display()
                    ),
                    debug_info.location,
                ))
            }
        }
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
            if return_type.assignable(&tc_expr.get_type(), type_tree, HashSet::new()) {
                Ok((TypeCheckedStatementKind::Return(tc_expr), vec![]))
            } else {
                Err(new_type_error(
                    format!(
                        "return statement has wrong type, expected: \"{}\", got: \"{}\"",
                        return_type.get_representation(type_tree)?.display(),
                        tc_expr.get_type().get_representation(type_tree)?.display()
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
            if tce_type == Type::Void {
                return Err(new_type_error(
                    format!("Assignment of void value to local variable"),
                    debug_info.location,
                ));
            }
            match &pat.kind {
                MatchPatternKind::Simple(name) => Ok((
                    TypeCheckedStatementKind::Let(
                        TypeCheckedMatchPattern::new_simple(*name, tce_type.clone()),
                        tc_expr,
                    ),
                    vec![(*name, tce_type)],
                )),
                MatchPatternKind::Tuple(pats) => {
                    let (tc_pats, bindings) =
                        typecheck_patvec(tce_type.clone(), pats.to_vec(), debug_info.location)?;
                    Ok((
                        TypeCheckedStatementKind::Let(
                            TypeCheckedMatchPattern::new_tuple(tc_pats, tce_type),
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
            match type_table.get(name) {
                Some(var_type) => {
                    if var_type.assignable(&tc_expr.get_type(), type_tree, HashSet::new()) {
                        Ok((
                            TypeCheckedStatementKind::AssignLocal(*name, tc_expr),
                            vec![],
                        ))
                    } else {
                        Err(new_type_error(
                            format!(
                                "mismatched types in assignment statement expected {}, got {}",
                                var_type.get_representation(type_tree)?.display(),
                                tc_expr.get_type().get_representation(type_tree)?.display()
                            ),
                            debug_info.location,
                        ))
                    }
                }
                None => {
                    match global_vars.get(&*name) {
                        Some((var_type, idx)) => {
                            if var_type.assignable(&tc_expr.get_type(), type_tree, HashSet::new()) {
                                Ok((
                                    TypeCheckedStatementKind::AssignGlobal(*idx, tc_expr),
                                    vec![],
                                ))
                            } else {
                                Err(new_type_error(
                                format!("mismatched types in assignment statement expected {}, got {}", var_type.get_representation(type_tree)?.display(), tc_expr.get_type().get_representation(type_tree)?.display()),
                                debug_info.location,
                            ))
                            }
                        }
                        None => Err(new_type_error(
                            "assignment to non-existent variable".to_string(),
                            debug_info.location,
                        )),
                    }
                }
            }
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
                    format!(
                        "while condition must be bool, found {}",
                        tc_cond.get_type().display()
                    ),
                    debug_info.location,
                )),
            }
        }
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
                if *rhs_type == Type::Void {
                    return Err(new_type_error("attempted to assign void in tuple binding".to_string(), location));
                }
                let pat = &patterns[i];
                match &pat.kind {
                    MatchPatternKind::Simple(name) => {
                        tc_pats.push(TypeCheckedMatchPattern::new_simple(*name, rhs_type.clone()));
                        bindings.push((*name, rhs_type.clone()));
                    }
                    MatchPatternKind::Tuple(_) => {
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
            format!(
                "tuple-match let must receive tuple value, found \"{}\"",
                rhs_type.display()
            ),
            location,
        ))
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
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &TypeTable,
    return_type: &Type,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<TypeCheckedExpr, TypeError> {
    let debug_info = expr.debug_info;
    let loc = debug_info.location;
    Ok(TypeCheckedExpr {
        kind: match &expr.kind {
            ExprKind::NewBuffer => Ok(TypeCheckedExprKind::NewBuffer),
            ExprKind::Panic => Ok(TypeCheckedExprKind::Panic),
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
            ExprKind::Trinary(op, sub1, sub2, sub3) => {
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
                let tc_sub3 = typecheck_expr(
                    sub3,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
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
                if (tc_sub1.get_type(), tc_sub2.get_type()) != (Type::Bool, Type::Bool) {
                    return Err(new_type_error(
                        format!(
                            "operands to logical or must be boolean, got \"{}\" and \"{}\"",
                            tc_sub1.get_type().display(),
                            tc_sub2.get_type().display(),
                        ),
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
                if (tc_sub1.get_type(), tc_sub2.get_type()) != (Type::Bool, Type::Bool) {
                    return Err(new_type_error(
                        format!(
                            "operands to logical and must be boolean, got \"{}\" and \"{}\"",
                            tc_sub1.get_type().display(),
                            tc_sub2.get_type().display()
                        ),
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
            ExprKind::VariableRef(name) => match func_table.get(name) {
                Some(t) => Ok(TypeCheckedExprKind::FuncRef(*name, (*t).clone())),
                None => match type_table.get(name) {
                    Some(t) => Ok(TypeCheckedExprKind::LocalVariableRef(*name, (*t).clone())),
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
                        format!(
                            "tuple field access to non-tuple value of type \"{}\"",
                            tc_sub.get_type().display()
                        ),
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
                        format!(
                            "struct field access to non-struct value of type \"{}\"",
                            tc_sub.get_type().display()
                        ),
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
                                let resolved_arg_type = arg_types[i].clone();
                                if !resolved_arg_type.assignable(
                                    &tc_args[i].get_type().get_representation(type_tree)?,
                                    type_tree,
                                    HashSet::new(),
                                ) {
                                    return Err(new_type_error(
                                        format!("wrong argument type in function call, expected \"{}\", got \"{}\"", resolved_arg_type.get_representation(type_tree)?.display(), tc_args[i].get_type().get_representation(type_tree)?.display()),
                                        loc,
                                    ));
                                }
                            }
                            Ok(TypeCheckedExprKind::FunctionCall(
                                Box::new(tc_fexpr),
                                tc_args,
                                *ret_type,
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
                        format!(
                            "function call to non-function value of type \"{}\"",
                            tc_fexpr.get_type().get_representation(type_tree)?.display()
                        ),
                        loc,
                    )),
                }
            }
            ExprKind::CodeBlock(block) => Ok(TypeCheckedExprKind::CodeBlock(typecheck_codeblock(
                block,
                &type_table,
                global_vars,
                func_table,
                return_type,
                type_tree,
                scopes,
            )?)),
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
                            Err(new_type_error(
                                format!(
                                    "array index must be Uint, found \"{}\"",
                                    tc_idx.get_type().display()
                                ),
                                loc,
                            ))
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
                                format!(
                                    "fixedarray index must be uint, found \"{}\"",
                                    tc_idx.get_type().display()
                                ),
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
                                format!(
                                    "invalid key value in map lookup, expected \"{}\", got \"{}\"",
                                    kt.display(),
                                    tc_idx.get_type().display()
                                ),
                                loc,
                            ))
                        }
                    }
                    _ => Err(new_type_error(
                        format!(
                            "fixedarray lookup in non-array type \"{}\"",
                            tc_arr.get_type().get_representation(type_tree)?.display()
                        ),
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
                    tc_fields.push(TypeCheckedFieldInitializer::new(
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
                        if t.assignable(&tc_val.get_type(), type_tree, HashSet::new()) {
                            if tc_index.get_type() != Type::Uint {
                                Err(new_type_error(
                                    format!(
                                        "array modifier requires uint index, found \"{}\"",
                                        tc_index.get_type().display()
                                    ),
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
                                format!("mismatched types in array modifier, expected \"{}\", got \"{}\"", t.display(), tc_val.get_type().display()),
                                loc,
                            ))
                        }
                    }
                    Type::FixedArray(t, sz) => {
                        if tc_index.get_type() != Type::Uint {
                            Err(new_type_error(
                                format!(
                                    "array modifier requires uint index, found \"{}\"",
                                    tc_index.get_type().display()
                                ),
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
                            if vt.assignable(&tc_val.get_type(), type_tree, HashSet::new()) {
                                Ok(TypeCheckedExprKind::MapMod(
                                    Box::new(tc_arr),
                                    Box::new(tc_index),
                                    Box::new(tc_val),
                                    Type::Map(kt, vt),
                                ))
                            } else {
                                Err(new_type_error(
                                    format!("invalid value type for map modifier, expected \"{}\", got \"{}\"", vt.display(), tc_val.get_type().display()),
                                    loc,
                                ))
                            }
                        } else {
                            Err(new_type_error(
                                format!("invalid key type for map modifier, expected \"{}\", got \"{}\"", kt.display(), tc_index.get_type().display()),
                                loc,
                            ))
                        }
                    }
                    other => Err(new_type_error(
                        format!(
                            "[] modifier must operate on array or block, found \"{}\"",
                            other.display()
                        ),
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
                            if fields[index].tipe.assignable(
                                &tc_val.get_type(),
                                type_tree,
                                HashSet::new(),
                            ) {
                                Ok(TypeCheckedExprKind::StructMod(
                                    Box::new(tc_struc),
                                    index,
                                    Box::new(tc_val),
                                    tcs_type,
                                ))
                            } else {
                                Err(new_type_error(
                                    format!("incorrect value type in struct modifier, expected \"{}\", found \"{}\"", fields[index].tipe.display(), tc_val.get_type().display()),
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
                        format!(
                            "struct modifier must operate on a struct, found \"{}\"",
                            tcs_type.display()
                        ),
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
                    ret => {
                        return Err(new_type_error(
                            format!("Can only use \"?\" operator in functions that can return option, found \"{}\"", ret.display()),
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
                        format!(
                            "Try expression requires option type, found \"{}\"",
                            other.display()
                        ),
                        loc,
                    )),
                }
            }
            ExprKind::If(cond, block, else_block) => {
                let cond_expr = typecheck_expr(
                    cond,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let block = typecheck_codeblock(
                    block,
                    type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
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
                            return_type,
                            type_tree,
                            scopes,
                        )
                    })
                    .transpose()?;
                if cond_expr.get_type() != Type::Bool {
                    Err(new_type_error(
                        format!(
                            "Condition of if expression must be bool: found \"{}\"",
                            cond_expr.get_type().display()
                        ),
                        debug_info.location,
                    ))
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
                        return Err(new_type_error(
                            format!(
                                "Mismatch of if and else types found: \"{}\" and \"{}\"",
                                block_type.display(),
                                else_type.display()
                            ),
                            debug_info.location,
                        ));
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
                    return_type,
                    type_tree,
                    scopes,
                )?;
                let tct = match tcr.get_type() {
                    Type::Option(t) => *t,
                    unexpected => {
                        return Err(new_type_error(
                            format!("Expected option type got: \"{}\"", unexpected.display()),
                            debug_info.location,
                        ))
                    }
                };
                let mut inner_type_table = type_table.clone();
                inner_type_table.insert(*l, tct);
                let checked_block = typecheck_codeblock(
                    if_block,
                    &inner_type_table,
                    global_vars,
                    func_table,
                    return_type,
                    type_tree,
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
                            return_type,
                            type_tree,
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
                    return Err(new_type_error(
                        format!(
                            "Mismatch of if and else types found: \"{}\" and \"{}\"",
                            block_type.display(),
                            else_type.display()
                        ),
                        debug_info.location,
                    ));
                };
                Ok(TypeCheckedExprKind::IfLet(
                    *l,
                    Box::new(tcr),
                    checked_block,
                    checked_else,
                    if_let_type,
                ))
            }
            ExprKind::Loop(stats) => Ok(TypeCheckedExprKind::Loop(typecheck_statement_sequence(
                stats,
                return_type,
                type_table,
                global_vars,
                func_table,
                type_tree,
                scopes,
            )?)),
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
            other => Err(new_type_error(
                format!(
                    "invalid operand type \"{}\" for unary minus",
                    other.display()
                ),
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
                    other => Err(new_type_error(
                        format!(
                            "invalid operand type \"{}\" for bitwise negation",
                            other.display()
                        ),
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
                    other => Err(new_type_error(
                        format!(
                            "invalid operand type \"{}\" for bitwise negation",
                            other.display()
                        ),
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
            other => Err(new_type_error(
                format!(
                    "invalid operand type \"{}\" for logical negation",
                    other.display()
                ),
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
            other => Err(new_type_error(
                format!("invalid operand type \"{}\" for len", other.display()),
                loc,
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
                    other => Err(new_type_error(
                        format!("invalid operand type \"{}\" for uint()", other.display()),
                        loc,
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
                    other => Err(new_type_error(
                        format!("invalid operand type \"{}\" for int()", other.display()),
                        loc,
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
                    other => Err(new_type_error(
                        format!("invalid operand type \"{}\" for bytes32()", other.display()),
                        loc,
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
                    other => Err(new_type_error(
                        format!(
                            "invalid operand type \"{}\" for address cast",
                            other.display()
                        ),
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
            match op {
                BinaryOp::GetBuffer256 | BinaryOp::GetBuffer64 | BinaryOp::GetBuffer8 => {}
                _ => {
                    return typecheck_binary_op_const(op, val1, t1, val2, t2, loc);
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to binary op: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to divide: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
                loc,
            )),
        },
        BinaryOp::GetBuffer8 => match (subtype1, subtype2) {
            (Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to getbuffer8: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
                loc,
            )),
        },
        BinaryOp::GetBuffer64 => match (subtype1, subtype2) {
            (Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to getbuffer64: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
                loc,
            )),
        },
        BinaryOp::GetBuffer256 => match (subtype1, subtype2) {
            (Type::Uint, Type::Buffer) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Uint,
            )),
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to getbuffer256: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to mod: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to <: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to >: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to <=: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to >=: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
                loc,
            )),
        },
        BinaryOp::Equal | BinaryOp::NotEqual => {
            if subtype1 != Type::Void
                && subtype2 != Type::Void
                && ((subtype1 == Type::Any) || (subtype2 == Type::Any) || (subtype1 == subtype2))
            {
                Ok(TypeCheckedExprKind::Binary(
                    op,
                    Box::new(tcs1),
                    Box::new(tcs2),
                    Type::Bool,
                ))
            } else {
                Err(new_type_error(
                    format!(
                        "invalid argument types to equality comparison: \"{}\" and \"{}\"",
                        subtype1.display(),
                        subtype2.display()
                    ),
                    loc,
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to binary bitwise operator: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
                loc,
            )),
        },
        BinaryOp::_LogicalAnd | BinaryOp::LogicalOr => match (subtype1, subtype2) {
            (Type::Bool, Type::Bool) => Ok(TypeCheckedExprKind::Binary(
                op,
                Box::new(tcs1),
                Box::new(tcs2),
                Type::Bool,
            )),
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to binary logical operator: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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
            (subtype1, subtype2) => Err(new_type_error(
                format!(
                    "invalid argument types to binary hash operator: \"{}\" and \"{}\"",
                    subtype1.display(),
                    subtype2.display()
                ),
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

fn typecheck_trinary_op(
    op: TrinaryOp,
    tcs1: TypeCheckedExpr,
    tcs2: TypeCheckedExpr,
    tcs3: TypeCheckedExpr,
    type_tree: &TypeTree,
    loc: Option<Location>,
) -> Result<TypeCheckedExprKind, TypeError> {
    let subtype1 = tcs1.get_type().get_representation(type_tree)?;
    let subtype2 = tcs2.get_type().get_representation(type_tree)?;
    let subtype3 = tcs3.get_type().get_representation(type_tree)?;
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
                (t1, t2, t3) => Err(new_type_error(
                    format!(
                        "invalid argument types to 3-ary op: \"{}\", \"{}\" and \"{}\"",
                        t1.display(),
                        t2.display(),
                        t3.display()
                    ),
                    loc,
                )),
            }
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
                format!(
                    "invalid argument types to binary op: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
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
                format!(
                    "invalid argument types to divide: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
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
                format!(
                    "invalid argument types to mod: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
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
            (t1, t2) => Err(new_type_error(
                format!(
                    "invalid argument types to <: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
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
            (t1, t2) => Err(new_type_error(
                format!(
                    "invalid argument types to >: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
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
            (t1, t2) => Err(new_type_error(
                format!(
                    "invalid argument types to <=: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
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
            (t1, t2) => Err(new_type_error(
                format!(
                    "invalid argument types to >=: \"{}\" and \"{}\"",
                    t1.display(),
                    t2.display()
                ),
                loc,
            )),
        },
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::ShiftLeft
        | BinaryOp::ShiftRight
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
                                    format!(
                                        "invalid argument types to binary op: \"{}\" and \"{}\"",
                                        t1.display(),
                                        t2.display()
                                    ),
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
                    format!(
                        "invalid argument types to binary op: \"{}\" and \"{}\"",
                        t1.display(),
                        t2.display()
                    ),
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
                    format!(
                        "invalid argument types to logical and: \"{}\" and \"{}\"",
                        t1.display(),
                        t2.display()
                    ),
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
                    format!(
                        "invalid argument types to logical or: \"{}\" and \"{}\"",
                        t1.display(),
                        t2.display()
                    ),
                    loc,
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
        self.ret_expr
            .iter_mut()
            .map(|exp| TypeCheckedNode::Expression(exp))
            .chain(
                self.body
                    .iter_mut()
                    .map(|stat| TypeCheckedNode::Statement(stat)),
            )
            .collect()
    }
    fn is_pure(&mut self) -> bool {
        self.body.iter_mut().all(|statement| statement.is_pure())
            && self
                .ret_expr
                .as_mut()
                .map(|expr| expr.is_pure())
                .unwrap_or(true)
    }
}

fn typecheck_codeblock(
    block: &CodeBlock,
    type_table: &TypeTable,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &TypeTable,
    return_type: &Type,
    type_tree: &TypeTree,
    scopes: &mut Vec<(String, Option<Type>)>,
) -> Result<TypeCheckedCodeBlock, TypeError> {
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
                    return_type,
                    type_tree,
                    scopes,
                )
            })
            .transpose()?
            .map(Box::new),
        scope: None,
    })
}
