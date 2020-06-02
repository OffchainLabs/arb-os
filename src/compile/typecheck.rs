/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use super::symtable::SymTable;
use crate::compile::ast::{
    BinaryOp, Constant, Expr, FuncArg, FuncDecl, FuncDeclKind, GlobalVarDecl, IfArm,
    ImportFuncDecl, MatchPattern, Statement, StructField, TopLevelDecl, Type, UnaryOp,
};
use crate::link::{ExportedFunc, ImportedFunc};
use crate::mavm::{Instruction, Label, Value};
use crate::pos::Location;
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::HashMap;

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

#[derive(Debug)]
pub struct TypeCheckedFunc {
    pub name: StringId,
    pub args: Vec<FuncArg>,
    pub ret_type: Type,
    pub code: Vec<TypeCheckedStatement>,
    pub tipe: Type,
    pub imported: bool,
    pub location: Option<Location>,
}

#[derive(Debug, Clone)]
pub enum TypeCheckedStatement {
    Noop(Option<Location>),
    Panic(Option<Location>),
    ReturnVoid(Option<Location>),
    Return(TypeCheckedExpr, Option<Location>),
    FunctionCall(TypeCheckedExpr, Vec<TypeCheckedExpr>, Option<Location>),
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
        Option<Location>,
    ),
    Asm(Vec<Instruction>, Vec<TypeCheckedExpr>, Option<Location>),
    DebugPrint(TypeCheckedExpr, Option<Location>),
}

#[derive(Debug, Clone)]
pub enum TypeCheckedMatchPattern {
    Simple(StringId, Type),
    Tuple(Vec<TypeCheckedMatchPattern>, Type),
}

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
    FuncRef(usize, Type, Option<Location>),
    TupleRef(Box<TypeCheckedExpr>, Uint256, Type, Option<Location>),
    DotRef(Box<TypeCheckedExpr>, StringId, Type, Option<Location>),
    Const(Value, Type, Option<Location>),
    FunctionCall(
        Box<TypeCheckedExpr>,
        Vec<TypeCheckedExpr>,
        Type,
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
}

impl<'a> TypeCheckedExpr {
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
            TypeCheckedExpr::DotRef(_, _, t, _) => t.clone(),
            TypeCheckedExpr::Const(_, t, _) => t.clone(),
            TypeCheckedExpr::FunctionCall(_, _, t, _) => t.clone(),
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckedStructField {
    pub name: StringId,
    pub value: TypeCheckedExpr,
}

impl<'a> TypeCheckedStructField {
    pub fn new(name: StringId, value: TypeCheckedExpr) -> Self {
        TypeCheckedStructField { name, value }
    }
}

fn builtin_func_decls(mut string_table: StringTable) -> (Vec<ImportFuncDecl>, StringTable) {
    let imps = vec![
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayNew"),
            false,
            vec![Type::Uint, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayGet"),
            false,
            vec![Type::Any, Type::Uint],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arraySet"),
            false,
            vec![Type::Any, Type::Uint, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(string_table.get("builtin_kvsNew"), false, vec![], Type::Any),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsHasKey"),
            false,
            vec![Type::Any, Type::Any],
            Type::Bool,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsGet"),
            false,
            vec![Type::Any, Type::Any],
            Type::Tuple(vec![Type::Any, Type::Bool]),
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsSet"),
            false,
            vec![Type::Any, Type::Any, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsDelete"),
            false,
            vec![Type::Any, Type::Any],
            Type::Any,
        ),
    ];
    (imps, string_table)
}

pub fn typecheck_top_level_decls<'a>(
    decls: &[TopLevelDecl],
    checked_funcs: &mut Vec<TypeCheckedFunc>,
    string_table_in: StringTable<'a>,
) -> Result<
    (
        Vec<ExportedFunc>,
        Vec<ImportedFunc>,
        Vec<GlobalVarDecl>,
        StringTable<'a>,
    ),
    TypeError,
> {
    let mut exported_funcs = Vec::new();
    let mut imported_funcs = Vec::new();
    let mut funcs = Vec::new();
    let mut named_types = HashMap::new();
    let mut hm = HashMap::new();
    let mut global_vars = Vec::new();
    let mut global_vars_map = HashMap::new();

    let (builtin_fds, string_table) = builtin_func_decls(string_table_in);
    for fd in builtin_fds.iter() {
        hm.insert(fd.name, &fd.tipe);
        imported_funcs.push(ImportedFunc::new(
            imported_funcs.len(),
            fd.name,
            &string_table,
        ));
    }
    for decl in decls.iter() {
        match decl {
            TopLevelDecl::TypeDecl(td) => {
                named_types.insert(td.name, &td.tipe);
            }
            TopLevelDecl::FuncDecl(fd) => {
                funcs.push(fd);
                hm.insert(fd.name, &fd.tipe);
            }
            TopLevelDecl::VarDecl(vd) => {
                let slot_num = global_vars.len();
                global_vars.push(vd);
                global_vars_map.insert(vd.name, (vd.tipe.clone(), slot_num));
            }
            TopLevelDecl::ImpFuncDecl(fd) => {
                hm.insert(fd.name, &fd.tipe);
                imported_funcs.push(ImportedFunc::new(
                    imported_funcs.len(),
                    fd.name,
                    &string_table,
                ));
            }
            TopLevelDecl::ImpTypeDecl(itd) => {
                named_types.insert(itd.name, &itd.tipe);
            }
        }
    }

    let type_table = SymTable::<Type>::new();
    let type_table = type_table.push_multi(named_types);
    let type_table = type_table.push_multi(hm.clone());

    let mut resolved_global_vars_map = HashMap::new();
    for (name, (tipe, slot_num)) in global_vars_map {
        resolved_global_vars_map.insert(name, (tipe.resolve_types(&type_table, None)?, slot_num));
    }

    let func_table = SymTable::<Type>::new();
    let func_table = func_table.push_multi(hm);

    for func in funcs.iter() {
        match func.resolve_types(&type_table, func.location) {
            Ok(f) => {
                match typecheck_function(&f, &type_table, &resolved_global_vars_map, &func_table) {
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

pub fn typecheck_function<'a>(
    fd: &'a FuncDecl,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &'a SymTable<'a, Type>,
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
            )?;
            Ok(TypeCheckedFunc {
                name: fd.name,
                args: fd.args.clone(),
                ret_type: fd.ret_type.clone(),
                code: tc_stats,
                tipe: fd.tipe.clone(),
                imported: false,
                location: fd.location,
            })
        }
    }
}

fn typecheck_statement_sequence<'a>(
    statements: &'a [Statement],
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
    if statements.is_empty() {
        return Ok(Vec::new());
    }
    let first_stat = &statements[0];
    let rest_of_stats = &statements[1..];

    let (tcs, bindings) =
        typecheck_statement(first_stat, return_type, type_table, global_vars, func_table)?;
    let mut rest_result = typecheck_statement_sequence_with_bindings(
        rest_of_stats,
        return_type,
        type_table,
        global_vars,
        func_table,
        &bindings,
    )?;
    rest_result.insert(0, tcs);
    Ok(rest_result)
}

fn typecheck_statement_sequence_with_bindings<'a>(
    statements: &'a [Statement],
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
    bindings: &[(StringId, Type)],
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
    if bindings.is_empty() {
        typecheck_statement_sequence(statements, return_type, type_table, global_vars, func_table)
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
        )
    }
}

fn typecheck_statement<'a>(
    statement: &'a Statement,
    return_type: &Type,
    type_table: &'a SymTable<'a, Type>,
    global_vars: &'a HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
) -> Result<(TypeCheckedStatement, Vec<(StringId, Type)>), TypeError> {
    match statement {
        Statement::Noop(loc) => Ok((TypeCheckedStatement::Noop(*loc), vec![])),
        Statement::Panic(loc) => Ok((TypeCheckedStatement::Panic(*loc), vec![])),
        Statement::ReturnVoid(loc) => Ok((TypeCheckedStatement::ReturnVoid(*loc), vec![])),
        Statement::Return(expr, loc) => {
            let tc_expr = typecheck_expr(expr, type_table, global_vars, func_table)?;
            if return_type.assignable(&tc_expr.get_type()) {
                Ok((TypeCheckedStatement::Return(tc_expr, *loc), vec![]))
            } else {
                Err(new_type_error(
                    format!(
                        "return statement has wrong type, expected: \"{:?}\", got: \"{:?}\"",
                        return_type,
                        tc_expr.get_type()
                    ),
                    *loc,
                ))
            }
        }
        Statement::FunctionCall(fexpr, args, loc) => {
            let tc_fexpr = typecheck_expr(fexpr, type_table, global_vars, func_table)?;
            if let Type::Func(_, arg_types, ret_type) = tc_fexpr.get_type() {
                if *ret_type != Type::Void {
                    return Err(new_type_error(
                        "function call statement to non-void function".to_string(),
                        *loc,
                    ));
                }
                if args.len() == arg_types.len() {
                    let mut tc_args = Vec::new();
                    for i in 0..args.len() {
                        let tc_arg = typecheck_expr(&args[i], type_table, global_vars, func_table)?;
                        tc_args.push(tc_arg);
                        let resolved_arg_type = arg_types[i].resolve_types(&type_table, *loc)?;
                        if !resolved_arg_type.assignable(&tc_args[i].get_type()) {
                            return Err(new_type_error(
                                "wrong argument type in function call".to_string(),
                                *loc,
                            ));
                        }
                    }
                    Ok((
                        TypeCheckedStatement::FunctionCall(tc_fexpr, tc_args, *loc),
                        vec![],
                    ))
                } else {
                    Err(new_type_error(
                        "wrong number of args passed to function".to_string(),
                        *loc,
                    ))
                }
            } else {
                Err(new_type_error(
                    "function call to non-function object".to_string(),
                    *loc,
                ))
            }
        }
        Statement::Let(pat, expr, loc) => {
            let tc_expr = typecheck_expr(expr, type_table, global_vars, func_table)?;
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
        Statement::Assign(name, expr, loc) => {
            let tc_expr = typecheck_expr(expr, type_table, global_vars, func_table)?;
            match type_table.get(*name) {
                Some(var_type) => {
                    if var_type.assignable(&tc_expr.get_type()) {
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
                        if var_type.assignable(&tc_expr.get_type()) {
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
        Statement::Loop(body, loc) => {
            let tc_body = typecheck_statement_sequence(
                body,
                return_type,
                type_table,
                global_vars,
                func_table,
            )?;
            Ok((TypeCheckedStatement::Loop(tc_body, *loc), vec![]))
        }
        Statement::While(cond, body, loc) => {
            let tc_cond = typecheck_expr(cond, type_table, global_vars, func_table)?;
            match tc_cond.get_type() {
                Type::Bool => {
                    let tc_body = typecheck_statement_sequence(
                        body,
                        return_type,
                        type_table,
                        global_vars,
                        func_table,
                    )?;
                    Ok((TypeCheckedStatement::While(tc_cond, tc_body, *loc), vec![]))
                }
                _ => Err(new_type_error(
                    "while condition is not bool".to_string(),
                    *loc,
                )),
            }
        }
        Statement::If(arm) => Ok((
            TypeCheckedStatement::If(typecheck_if_arm(
                arm,
                return_type,
                type_table,
                global_vars,
                func_table,
            )?),
            vec![],
        )),
        Statement::Asm(insns, args, loc) => {
            let mut tc_args = Vec::new();
            for arg in args {
                tc_args.push(typecheck_expr(arg, type_table, global_vars, func_table)?);
            }
            Ok((
                TypeCheckedStatement::Asm(insns.to_vec(), tc_args, *loc),
                vec![],
            ))
        }
        Statement::DebugPrint(e, loc) => {
            let tce = typecheck_expr(e, type_table, global_vars, func_table)?;
            Ok((TypeCheckedStatement::DebugPrint(tce, *loc), vec![]))
        }
        Statement::IfLet(l, r, s, loc) => {
            let tcr = typecheck_expr(r, type_table, global_vars, func_table)?;
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
                        s,
                        return_type,
                        type_table,
                        global_vars,
                        func_table,
                        &vec![(*l, tct.clone())],
                    )?,
                    *loc,
                ),
                vec![(*l, tct)],
            ))
        }
    }
}

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

fn typecheck_if_arm(
    arm: &IfArm,
    return_type: &Type,
    type_table: &SymTable<Type>,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
) -> Result<TypeCheckedIfArm, TypeError> {
    match arm {
        IfArm::Cond(cond, body, orest, loc) => {
            let tc_cond = typecheck_expr(cond, type_table, global_vars, func_table)?;
            match tc_cond.get_type() {
                Type::Bool => Ok(TypeCheckedIfArm::Cond(
                    tc_cond,
                    typecheck_statement_sequence(
                        body,
                        return_type,
                        type_table,
                        global_vars,
                        func_table,
                    )?,
                    match orest {
                        Some(rest) => Some(Box::new(typecheck_if_arm(
                            rest,
                            return_type,
                            type_table,
                            global_vars,
                            func_table,
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
            typecheck_statement_sequence(body, return_type, type_table, global_vars, func_table)?,
            *loc,
        )),
    }
}

fn typecheck_expr(
    expr: &Expr,
    type_table: &SymTable<Type>,
    global_vars: &HashMap<StringId, (Type, usize)>,
    func_table: &SymTable<Type>,
) -> Result<TypeCheckedExpr, TypeError> {
    match expr {
        Expr::UnaryOp(op, subexpr, loc) => {
            let tc_sub = typecheck_expr(subexpr, type_table, global_vars, func_table)?;
            typecheck_unary_op(*op, tc_sub, *loc)
        }
        Expr::Binary(op, sub1, sub2, loc) => {
            let tc_sub1 = typecheck_expr(sub1, type_table, global_vars, func_table)?;
            let tc_sub2 = typecheck_expr(sub2, type_table, global_vars, func_table)?;
            typecheck_binary_op(*op, tc_sub1, tc_sub2, *loc)
        }
        Expr::ShortcutOr(sub1, sub2, loc) => {
            let tc_sub1 = typecheck_expr(sub1, type_table, global_vars, func_table)?;
            let tc_sub2 = typecheck_expr(sub2, type_table, global_vars, func_table)?;
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
            let tc_sub1 = typecheck_expr(sub1, type_table, global_vars, func_table)?;
            let tc_sub2 = typecheck_expr(sub2, type_table, global_vars, func_table)?;
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
            let tc_sub = typecheck_expr(&*tref, type_table, global_vars, func_table)?;
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
            let tc_sub = typecheck_expr(&*sref, type_table, global_vars, func_table)?;
            if let Type::Struct(v) = tc_sub.get_type() {
                for sf in v.iter() {
                    if *name == sf.name {
                        return Ok(TypeCheckedExpr::DotRef(
                            Box::new(tc_sub),
                            *name,
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
            let tc_fexpr = typecheck_expr(fexpr, type_table, global_vars, func_table)?;
            match tc_fexpr.get_type() {
                Type::Func(_, arg_types, ret_type) => {
                    let ret_type = ret_type.resolve_types(type_table, *loc)?;
                    if args.len() == arg_types.len() {
                        let mut tc_args = Vec::new();
                        for i in 0..args.len() {
                            let tc_arg =
                                typecheck_expr(&args[i], type_table, global_vars, func_table)?;
                            tc_args.push(tc_arg);
                            let resolved_arg_type =
                                arg_types[i].resolve_types(&type_table, *loc)?;
                            if !resolved_arg_type.assignable(&tc_args[i].get_type()) {
                                println!("expected {:?}", resolved_arg_type);
                                println!("actual   {:?}", tc_args[i].get_type());
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
        Expr::ArrayOrMapRef(array, index, loc) => {
            let tc_arr = typecheck_expr(&*array, type_table, global_vars, func_table)?;
            let tc_idx = typecheck_expr(&*index, type_table, global_vars, func_table)?;
            match tc_arr.get_type() {
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
                            Type::Tuple(vec![*vt, Type::Bool]),
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
            )?),
            tipe.clone(),
            Type::Array(Box::new(tipe.clone())),
            *loc,
        )),
        Expr::NewFixedArray(size, maybe_expr, loc) => match maybe_expr {
            Some(expr) => {
                let tc_expr = typecheck_expr(expr, type_table, global_vars, func_table)?;
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
                let tc_expr = typecheck_expr(&field.value, type_table, global_vars, func_table)?;
                tc_fields.push(TypeCheckedStructField::new(field.name, tc_expr.clone()));
                tc_fieldtypes.push(StructField::new(field.name, tc_expr.get_type()));
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
                let tc_field = typecheck_expr(field, type_table, global_vars, func_table)?;
                types.push(tc_field.get_type().clone());
                tc_fields.push(tc_field);
            }
            Ok(TypeCheckedExpr::Tuple(tc_fields, Type::Tuple(types), *loc))
        }
        Expr::ArrayOrMapMod(arr, index, val, loc) => {
            let tc_arr = typecheck_expr(arr, type_table, global_vars, func_table)?;
            let tc_index = typecheck_expr(index, type_table, global_vars, func_table)?;
            let tc_val = typecheck_expr(val, type_table, global_vars, func_table)?;
            match tc_arr.get_type() {
                Type::Array(t) => {
                    if t.assignable(&tc_val.get_type()) {
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
                        if vt.assignable(&tc_val.get_type()) {
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
            let tc_struc = typecheck_expr(struc, type_table, global_vars, func_table)?;
            let tc_val = typecheck_expr(val, type_table, global_vars, func_table)?;
            let tcs_type = tc_struc.get_type();
            if let Type::Struct(fields) = &tcs_type {
                match tcs_type.get_struct_slot_by_name(*name) {
                    Some(index) => {
                        if fields[index].tipe.assignable(&tc_val.get_type()) {
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
            Box::new(typecheck_expr(expr, type_table, global_vars, func_table)?),
            t.clone(),
            *loc,
        )),
        Expr::Asm(ret_type, insns, args, loc) => {
            if ret_type.is_void() {
                return Err(new_type_error(
                    "asm expression cannot return void".to_string(),
                    *loc,
                ));
            }
            let mut tc_args = Vec::new();
            for arg in args {
                tc_args.push(typecheck_expr(arg, type_table, global_vars, func_table)?);
            }
            Ok(TypeCheckedExpr::Asm(
                ret_type.clone(),
                insns.to_vec(),
                tc_args,
                *loc,
            ))
        }
    }
}

fn typecheck_unary_op(
    op: UnaryOp,
    sub_expr: TypeCheckedExpr,
    loc: Option<Location>,
) -> Result<TypeCheckedExpr, TypeError> {
    let tc_type = sub_expr.get_type();
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

fn typecheck_binary_op(
    mut op: BinaryOp,
    mut tcs1: TypeCheckedExpr,
    mut tcs2: TypeCheckedExpr,
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
    let subtype1 = tcs1.get_type();
    let subtype2 = tcs2.get_type();
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
                    "invalid argument types to equality comparison".to_string(),
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
        BinaryOp::LogicalAnd | BinaryOp::LogicalOr => match (subtype1, subtype2) {
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
        BinaryOp::LogicalAnd => {
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
