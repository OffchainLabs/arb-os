/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Contains utilities for generating instructions from AST structures.

use super::ast::{BinaryOp, FuncProperties, GlobalVar, TrinaryOp, Type, UnaryOp};
use super::typecheck::{
    TypeCheckedExpr, TypeCheckedFunc, TypeCheckedMatchPattern, TypeCheckedStatement,
};
use crate::compile::ast::{DebugInfo, MatchPatternKind};
use crate::compile::typecheck::{
    TypeCheckedCodeBlock, TypeCheckedExprKind, TypeCheckedStatementKind,
};
use crate::compile::CompileError;
use crate::console::Color;
use crate::link::{TupleTree, TUPLE_SIZE};
use crate::mavm::{AVMOpcode, Buffer, Instruction, Label, LabelGenerator, Opcode, Value};
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::HashSet;
use std::{cmp::max, collections::HashMap};

/// This generates code for individual mini functions.
///
/// Here func represents the function to be codegened, string_table is used to get builtins,
/// and globals lists the globals available to the func.
///
/// Each func gets a unique, hashed label id, after which local labels are assigned. This ensures
/// two labels are the same iff they point to the same destination.
pub fn mavm_codegen_func(
    func: TypeCheckedFunc,
    string_table: &StringTable,
    globals: &HashMap<StringId, GlobalVar>,
    func_labels: &HashMap<StringId, Label>,
    issues: &mut Vec<CompileError>,
    release_build: bool,
) -> Result<(Vec<Instruction>, LabelGenerator), CompileError> {
    let mut code = vec![];
    let debug_info = func.debug_info;

    let unique_id = match func.unique_id {
        Some(id) => id,
        None => {
            return Err(CompileError::new_codegen_error(
                format!("Func {} has no id", Color::red(&func.name)),
                func.debug_info.location,
            ))
        }
    };

    let label = match func.properties.closure {
        true => Label::Closure(unique_id),
        false => Label::Func(unique_id),
    };

    code.push(Instruction::from_opcode(Opcode::Label(label), debug_info));

    let num_args = func.args.len();
    let mut locals = HashMap::new();

    let make_frame_slot = code.len();
    code.push(Instruction::from_opcode(
        Opcode::AVMOpcode(AVMOpcode::Noop),
        debug_info,
    )); // placeholder; will replace this later

    for arg in func.args {
        let next_slot = locals.len();
        locals.insert(arg.name, next_slot);
    }
    for capture in &func.captures {
        let next_slot = locals.len();
        locals.insert(*capture, next_slot);
    }

    let mut label_gen = LabelGenerator::new(unique_id + 1);

    let (mut space_for_locals, _slot_map) = mavm_codegen_statements(
        func.code,
        &mut code,
        locals.len(),
        &locals,
        &mut label_gen,
        string_table,
        func_labels,
        globals,
        0,
        &mut vec![],
        issues,
        release_build,
    )?;

    match func.tipe {
        Type::Func(_prop, _, ret) => {
            // put makeframe Instruction at beginning of function, to build the frame (replacing placeholder)

            let prebuilt = !func.captures.is_empty(); // whether caller will pass in the frame

            if !func.captures.is_empty() {
                space_for_locals = func.frame_size;
            }

            code[make_frame_slot] = Instruction::from_opcode(
                Opcode::MakeFrame(num_args, space_for_locals, prebuilt, &*ret != &Type::Every),
                debug_info,
            );
        }
        wrong => {
            return Err(CompileError::new_codegen_error(
                format!(
                    "type checking bug: func with non-func type {}",
                    Color::red(wrong.display())
                ),
                debug_info.location,
            ))
        }
    }

    Ok((code, label_gen))
}

fn mavm_codegen_code_block(
    block: &TypeCheckedCodeBlock,
    code: &mut Vec<Instruction>,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    label_gen: &mut LabelGenerator,
    string_table: &StringTable,
    func_labels: &HashMap<StringId, Label>,
    global_vars: &HashMap<StringId, GlobalVar>,
    prepushed_vals: usize,
    scopes: &mut Vec<(String, Label, Option<Type>)>,
    issues: &mut Vec<CompileError>,
    debug_info: DebugInfo,
    release_build: bool,
) -> Result<usize, CompileError> {
    let bottom_label = label_gen.next();
    scopes.push((
        block.scope.clone().unwrap_or("_".to_string()),
        bottom_label,
        None,
    ));
    let (nl, block_locals) = mavm_codegen_statements(
        block.body.clone(),
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        func_labels,
        global_vars,
        prepushed_vals,
        scopes,
        issues,
        release_build,
    )?;
    if let Some(ret_expr) = &block.ret_expr {
        let mut new_locals = locals.clone();
        new_locals.extend(block_locals);
        let prepushed_vals_expr = mavm_codegen_expr(
            ret_expr,
            code,
            nl,
            &new_locals,
            label_gen,
            string_table,
            func_labels,
            global_vars,
            prepushed_vals,
            scopes,
            issues,
            release_build,
        )
        .map(|exp_locals| (max(num_locals, max(exp_locals, nl))))?;
        code.push(Instruction::from_opcode(
            Opcode::Label(bottom_label),
            debug_info,
        ));
        let _scope = scopes.pop();
        Ok(prepushed_vals_expr)
    } else {
        code.push(Instruction::from_opcode(
            Opcode::Label(bottom_label),
            debug_info,
        ));
        Ok(max(num_locals, nl))
    }
}

/// Generates code for the provided statements with index 0 generated first. code represents the
/// code generated previously, num_locals the maximum number of locals used at any point in the call
/// frame so far, locals is a map of local variables, label_gen points to the next available locals
/// slot, string_table is used to get builtins, func_labels associates each imported function
/// with a label, and globals maps global variable IDs to their slot number.
///
/// If successful the function returns a tuple containing the maximum number of locals used
/// so far by this call frame and a map of locals available at the end of the statement sequence.
fn mavm_codegen_statements(
    statements: Vec<TypeCheckedStatement>, // statements to codegen
    code: &mut Vec<Instruction>,           // accumulates the code as it's generated
    mut num_locals: usize,                 // num locals that have been allocated
    locals: &HashMap<usize, usize>,        // lookup local variable slot number by name
    label_gen: &mut LabelGenerator,
    string_table: &StringTable,
    func_labels: &HashMap<StringId, Label>,
    global_vars: &HashMap<StringId, GlobalVar>,
    prepushed_vals: usize,
    scopes: &mut Vec<(String, Label, Option<Type>)>,
    issues: &mut Vec<CompileError>,
    release_build: bool,
) -> Result<(usize, HashMap<StringId, usize>), CompileError> {
    let mut bindings = HashMap::new();
    for statement in statements {
        let mut new_locals = locals.clone();
        new_locals.extend(bindings.clone());
        let (statement_locals, statement_bindings) = mavm_codegen_statement(
            statement,
            code,
            num_locals,
            &new_locals,
            label_gen,
            string_table,
            func_labels,
            global_vars,
            prepushed_vals,
            scopes,
            issues,
            release_build,
        )?;
        num_locals = max(statement_locals, num_locals);
        for (id, bind) in statement_bindings {
            bindings.insert(id, bind);
        }
    }
    Ok((num_locals, bindings))
}

/// Generates code for the provided statement. code represents the code generated previously,
/// num_locals the maximum number of locals used at any point in the call frame so far, locals is a
/// map of local variables, string_table is used to get builtins, func_labels associates each
/// imported function with a label, and globals maps global variable IDs to their slot number.
///
/// If successful the function returns the number of locals slots used by this statement
/// and a map of locals generated by this statement.
fn mavm_codegen_statement(
    statement: TypeCheckedStatement, // statement to codegen
    code: &mut Vec<Instruction>,     // accumulates the code as it's generated
    mut num_locals: usize,           // num locals that have been allocated
    locals: &HashMap<usize, usize>,  // lookup local variable slot number by name
    label_gen: &mut LabelGenerator,
    string_table: &StringTable,
    func_labels: &HashMap<StringId, Label>,
    globals: &HashMap<StringId, GlobalVar>,
    prepushed_vals: usize,
    scopes: &mut Vec<(String, Label, Option<Type>)>,
    issues: &mut Vec<CompileError>,
    release_build: bool,
) -> Result<(usize, HashMap<StringId, usize>), CompileError> {
    macro_rules! expr {
        ($expr:expr) => {
            mavm_codegen_expr(
                &$expr,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals,
                scopes,
                issues,
                release_build,
            )?
        };
    }

    let debug = statement.debug_info;
    let loc = statement.debug_info.location;

    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
        };
        ($opcode:ident, $immediate:expr) => {
            Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::$opcode), $immediate, debug)
        };
    }

    match &statement.kind {
        TypeCheckedStatementKind::ReturnVoid() => {
            code.push(Instruction::from_opcode(Opcode::Return, debug));
            Ok((0, HashMap::new()))
        }
        TypeCheckedStatementKind::Return(expr) => {
            let exp_locals = expr!(expr);
            if prepushed_vals > 0 {
                code.push(opcode!(AuxPush));
                for _ in 0..prepushed_vals {
                    code.push(opcode!(Pop));
                }
                code.push(opcode!(AuxPop));
            }
            code.push(Instruction::from_opcode(Opcode::Return, debug));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::Break(oexpr, scope_id) => {
            let mut inner_scopes = (*scopes).clone();
            let (_scope_name, lab, t) = scopes
                .iter_mut()
                .rev()
                .find(|(s, _, _)| scope_id == s)
                .ok_or_else(|| {
                    CompileError::new_codegen_error(
                        format!("could not find scope {}", scope_id),
                        loc,
                    )
                })?;
            if let Some(tipe) = t {
                if *tipe
                    != oexpr
                        .clone()
                        .map(|exp| exp.get_type())
                        .unwrap_or(Type::Tuple(vec![]))
                {
                    //Uncomment for info on break statement type mismatches
                    /*println!(
                        "Types did not match in break statement expected {:?} got {:?}",
                        tipe,
                        oexpr
                            .clone()
                            .map(|exp| exp.get_type())
                            .unwrap_or(Type::Tuple(vec![]))
                    );*/
                }
            } else {
                *t = Some(
                    oexpr
                        .clone()
                        .map(|exp| exp.get_type())
                        .unwrap_or(Type::Tuple(vec![])),
                );
            }
            let num_locals = if let Some(expr) = oexpr {
                mavm_codegen_expr(
                    expr,
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    func_labels,
                    globals,
                    prepushed_vals,
                    &mut inner_scopes,
                    issues,
                    release_build,
                )?
            } else {
                prepushed_vals
            };
            code.push(opcode!(Jump, Value::Label(*lab)));
            Ok((num_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::Expression(expr) => {
            let exp_locals = expr!(expr);
            if !(expr.get_type() == Type::Void || expr.get_type() == Type::Every) {
                code.push(opcode!(Pop));
                if expr.get_type() != Type::Tuple(vec![]) {
                    issues.push(CompileError::new_warning(
                        String::from("Compile warning"),
                        format!(
                            "expression statement returns value of type {:?}, which is discarded",
                            expr.get_type()
                        ),
                        loc.into_iter().collect(),
                    ));
                }
            }
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::Let(pat, expr) => {
            let exp_locals = expr!(expr);
            let (new_locals, bindings, _assignments) = mavm_codegen_tuple_pattern(
                code,
                &pat,
                num_locals,
                locals,
                globals,
                string_table,
                debug,
            )?;
            num_locals += new_locals;
            num_locals = max(num_locals, exp_locals);
            Ok((num_locals, bindings))
        }
        TypeCheckedStatementKind::AssignLocal(name, expr) => {
            let slot_num = match locals.get(name) {
                Some(slot) => slot,
                None => {
                    return Err(CompileError::new_codegen_error(
                        "assigned to non-existent variable".to_string(),
                        loc,
                    ))
                }
            };
            let exp_locals = expr!(expr);
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                Value::Int(Uint256::from_usize(*slot_num)),
                debug,
            ));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::AssignGlobal(id, expr) => {
            let exp_locals = expr!(expr);
            let global = globals.get(id).expect("No global exists for stringID");
            let offset = global.offset.unwrap();
            code.push(Instruction::from_opcode(
                Opcode::SetGlobalVar(offset),
                debug,
            ));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::While(cond, body) => {
            let slot_num = Value::Int(Uint256::from_usize(num_locals));
            num_locals += 1;
            let top_label = label_gen.next();
            let cond_label = label_gen.next();
            code.push(opcode!(Noop, Value::Label(top_label)));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                slot_num.clone(),
                debug,
            ));
            code.push(opcode!(Jump, Value::Label(cond_label)));
            code.push(Instruction::from_opcode(Opcode::Label(top_label), debug));
            let (nl, _) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals,
                scopes,
                issues,
                release_build,
            )?;
            num_locals = nl;
            code.push(Instruction::from_opcode(Opcode::Label(cond_label), debug));
            let cond_locals = expr!(cond);
            code.push(Instruction::from_opcode_imm(
                Opcode::GetLocal,
                slot_num,
                debug,
            ));
            code.push(opcode!(Cjump));
            Ok((max(cond_locals, num_locals), HashMap::new()))
        }
        TypeCheckedStatementKind::Asm(insns, args) => {
            let n_args = args.len();
            let mut exp_locals = 0;
            for i in 0..n_args {
                let e_locals = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    func_labels,
                    globals,
                    prepushed_vals + i,
                    scopes,
                    issues,
                    release_build,
                )?;
                exp_locals = max(exp_locals, e_locals);
            }
            for insn in insns {
                code.push(insn.clone());
            }
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::DebugPrint(expr) => {
            let exp_locals = expr!(expr);
            code.push(opcode!(DebugPrint));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::Assert(expr) => {
            if release_build {
                // Release builds don't include asserts
                return Ok((0, HashMap::new()));
            }

            let call_type = Type::Func(
                FuncProperties::pure(1, 0),
                vec![Type::Tuple(vec![Type::Bool, Type::Any])],
                Box::new(Type::Void),
            );

            let assert_call = TypeCheckedExpr {
                kind: TypeCheckedExprKind::FunctionCall(
                    Box::new(TypeCheckedExpr {
                        kind: TypeCheckedExprKind::FuncRef(
                            string_table.get_if_exists("builtin_assert").unwrap(),
                            call_type.clone(),
                        ),
                        debug_info: DebugInfo::from(loc),
                    }),
                    vec![expr.clone()],
                    call_type,
                    FuncProperties::pure(1, 0),
                ),
                debug_info: DebugInfo::from(loc),
            };

            let exp_locals = expr!(assert_call);
            Ok((exp_locals, HashMap::new()))
        }
    }
}

/// Generates code for assigning the contents of a tuple on the top of the stack to a sequential set
/// of locals. code represents previously generated code, pattern is a slice of match patterns
/// corresponding to the structure of the tuple, local_slot_num_base is the slot of the first local
/// being assigned to, and loc is the location the operation originates from in the source code.
fn mavm_codegen_tuple_pattern(
    code: &mut Vec<Instruction>,
    pattern: &TypeCheckedMatchPattern,
    local_slot_num_base: usize,
    locals: &HashMap<usize, usize>,
    globals: &HashMap<StringId, GlobalVar>,
    string_table: &StringTable,
    debug_info: DebugInfo,
) -> Result<(usize, HashMap<usize, usize>, HashSet<usize>), CompileError> {
    match &pattern.kind {
        MatchPatternKind::Bind(name) => {
            let mut bindings = HashMap::new();
            bindings.insert(*name, local_slot_num_base);
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                Value::Int(Uint256::from_usize(local_slot_num_base)),
                debug_info,
            ));
            Ok((1, bindings, HashSet::new()))
        }
        MatchPatternKind::Assign(id) => {
            if let Some(val) = locals.get(id) {
                code.push(Instruction::from_opcode_imm(
                    Opcode::SetLocal,
                    Value::Int(Uint256::from_usize(*val)),
                    debug_info,
                ))
            } else {
                code.push(Instruction::from_opcode(
                    Opcode::SetGlobalVar(
                        globals
                            .get(id)
                            .ok_or_else(|| {
                                CompileError::new_codegen_error(
                                    "assigned to non-existent variable in mixed let".to_string(),
                                    debug_info.location,
                                )
                            })?
                            .offset
                            .unwrap(),
                    ),
                    debug_info,
                ))
            }
            let mut assignments = HashSet::new();
            assignments.insert(*id);
            Ok((0, HashMap::new(), assignments))
        }
        MatchPatternKind::Tuple(sub_pats) => {
            let mut num_bindings = 0;
            let mut bindings = HashMap::new();
            let mut assignments = HashSet::new();
            let pat_size = sub_pats.len();
            for (i, pat) in sub_pats.iter().enumerate() {
                if i < pat_size - 1 {
                    code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Dup0),
                        debug_info,
                    ));
                }
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleGet(pat_size),
                    Value::Int(Uint256::from_usize(i)),
                    debug_info,
                ));
                let (num_new_bindings, new_bindings, new_assignments) = mavm_codegen_tuple_pattern(
                    code,
                    pat,
                    local_slot_num_base + num_bindings,
                    locals,
                    globals,
                    string_table,
                    debug_info,
                )?;
                num_bindings += num_new_bindings;
                bindings.extend(new_bindings);
                for name in new_assignments {
                    if !assignments.insert(name) {
                        return Err(CompileError::new_codegen_error(
                            format!(
                                "assigned to variable {} in mixed let multiple times",
                                string_table.name_from_id(name)
                            ),
                            debug_info.location,
                        ));
                    }
                }
            }
            Ok((num_bindings, bindings, assignments))
        }
    }
}

/// Generates code for the expression expr.
///
/// code represents the previously generated code, num_locals is the maximum number of locals used
/// at any previous point in the callframe, locals is the table of local variable names to slot
/// numbers, string_table is used to get builtins, func_labels maps `stringId`s for imported func
/// to their associated labels, globals maps `stringID`s to their associated slot numbers,
/// and prepushed_vals indicates the number of items on the stack at the start of the call,
/// which is needed for early returns.
///
/// If successful this function returns a tuple containing a mutable reference to the generated code,
/// and a usize containing the number of locals used by the expression.
fn mavm_codegen_expr(
    expr: &TypeCheckedExpr,
    code: &mut Vec<Instruction>,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    label_gen: &mut LabelGenerator,
    string_table: &StringTable,
    func_labels: &HashMap<StringId, Label>,
    globals: &HashMap<StringId, GlobalVar>,
    prepushed_vals: usize,
    scopes: &mut Vec<(String, Label, Option<Type>)>,
    issues: &mut Vec<CompileError>,
    release_build: bool,
) -> Result<usize, CompileError> {
    macro_rules! expr {
        ($expr:expr) => {
            expr!($expr, 0)
        };
        ($expr:expr, $prepushed:expr) => {
            mavm_codegen_expr(
                $expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals + $prepushed,
                scopes,
                issues,
                release_build,
            )
        };
    }

    let debug = expr.debug_info;
    let loc = expr.debug_info.location;

    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
        };
        ($opcode:ident, $immediate:expr) => {
            Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::$opcode), $immediate, debug)
        };
    }

    match &expr.kind {
        TypeCheckedExprKind::NewBuffer => {
            code.push(opcode!(NewBuffer));
            Ok(num_locals)
        }
        TypeCheckedExprKind::Quote(bytes) => {
            code.push(opcode!(
                Noop,
                Value::new_tuple(vec![
                    Value::Int(Uint256::from_usize(2 * bytes.len())),
                    Value::Buffer(Buffer::from_bytes(bytes.clone())),
                ])
            ));
            Ok(num_locals)
        }
        TypeCheckedExprKind::Error => {
            code.push(opcode!(Error));
            Ok(num_locals)
        }
        TypeCheckedExprKind::GetGas => {
            code.push(opcode!(PushGas));
            Ok(num_locals)
        }
        TypeCheckedExprKind::SetGas(tce) => {
            let exp_locals = expr!(tce, 0)?;
            code.push(opcode!(SetGas));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::UnaryOp(op, tce, _) => {
            let exp_locals = expr!(tce, 0)?;
            code.push(match op {
                UnaryOp::BitwiseNeg => opcode!(BitwiseNeg),
                UnaryOp::Not => opcode!(IsZero),
                UnaryOp::Hash => opcode!(Hash),
                UnaryOp::Len => Instruction::from_opcode_imm(
                    Opcode::TupleGet(3),
                    Value::Int(Uint256::zero()),
                    debug,
                ),
                UnaryOp::ToAddress => {
                    let mask = Uint256::from_usize(2)
                        .exp(&Uint256::from_usize(160))
                        .sub(&Uint256::one())
                        .ok_or_else(|| {
                            CompileError::new_codegen_error(
                                "Underflow on subtraction".to_string(),
                                loc,
                            )
                        })?;
                    opcode!(BitwiseAnd, Value::Int(mask))
                }
                UnaryOp::Minus => {
                    opcode!(Sub, Value::from(0))
                }
                UnaryOp::ToUint | UnaryOp::ToInt | UnaryOp::ToBytes32 => opcode!(Noop),
            });
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::Variant(inner) => {
            let exp_locals = expr!(inner, 0)?;
            code.push(opcode!(
                Noop,
                Value::new_tuple(vec![Value::Int(Uint256::from_usize(1)), Value::none()])
            ));
            code.push(opcode!(Tset, Value::Int(Uint256::from_u64(1))));
            Ok((max(num_locals, exp_locals)))
        }
        TypeCheckedExprKind::Binary(op, tce1, tce2, _) => {
            let left_locals = expr!(tce2, 0)?;
            let right_locals = expr!(tce1, 1)?;
            let opcode = match op {
                BinaryOp::GetBuffer8 => Opcode::AVMOpcode(AVMOpcode::GetBuffer8),
                BinaryOp::GetBuffer64 => Opcode::AVMOpcode(AVMOpcode::GetBuffer64),
                BinaryOp::GetBuffer256 => Opcode::AVMOpcode(AVMOpcode::GetBuffer256),
                BinaryOp::Plus => Opcode::AVMOpcode(AVMOpcode::Add),
                BinaryOp::Minus => Opcode::AVMOpcode(AVMOpcode::Sub),
                BinaryOp::Times => Opcode::AVMOpcode(AVMOpcode::Mul),
                BinaryOp::Div => Opcode::AVMOpcode(AVMOpcode::Div),
                BinaryOp::Mod => Opcode::AVMOpcode(AVMOpcode::Mod),
                BinaryOp::Sdiv => Opcode::AVMOpcode(AVMOpcode::Sdiv),
                BinaryOp::Smod => Opcode::AVMOpcode(AVMOpcode::Smod),
                BinaryOp::LessThan => Opcode::AVMOpcode(AVMOpcode::LessThan),
                BinaryOp::GreaterThan => Opcode::AVMOpcode(AVMOpcode::GreaterThan),
                BinaryOp::LessEq => Opcode::AVMOpcode(AVMOpcode::GreaterThan), // will negate
                BinaryOp::GreaterEq => Opcode::AVMOpcode(AVMOpcode::LessThan), // will negate
                BinaryOp::SLessThan => Opcode::AVMOpcode(AVMOpcode::SLessThan),
                BinaryOp::SGreaterThan => Opcode::AVMOpcode(AVMOpcode::SGreaterThan),
                BinaryOp::SLessEq => Opcode::AVMOpcode(AVMOpcode::SGreaterThan), // will negate
                BinaryOp::SGreaterEq => Opcode::AVMOpcode(AVMOpcode::SLessThan), // will negate
                BinaryOp::Equal => Opcode::AVMOpcode(AVMOpcode::Equal),
                BinaryOp::NotEqual => Opcode::AVMOpcode(AVMOpcode::Equal), // will negate
                BinaryOp::BitwiseAnd => Opcode::AVMOpcode(AVMOpcode::BitwiseAnd),
                BinaryOp::BitwiseOr => Opcode::AVMOpcode(AVMOpcode::BitwiseOr),
                BinaryOp::ShiftLeft => Opcode::AVMOpcode(AVMOpcode::ShiftLeft),
                BinaryOp::ShiftRight => Opcode::AVMOpcode(AVMOpcode::ShiftRight),
                BinaryOp::BitwiseXor => Opcode::AVMOpcode(AVMOpcode::BitwiseXor),
                BinaryOp::Hash => Opcode::AVMOpcode(AVMOpcode::EthHash2),
            };
            code.push(Instruction::from_opcode(opcode, debug));
            match op {
                BinaryOp::NotEqual
                | BinaryOp::LessEq
                | BinaryOp::GreaterEq
                | BinaryOp::SLessEq
                | BinaryOp::SGreaterEq => code.push(opcode!(IsZero)),
                _ => {}
            }
            Ok(max(num_locals, max(left_locals, right_locals)))
        }
        TypeCheckedExprKind::Trinary(op, tce1, tce2, tce3, _) => {
            let locals3 = expr!(tce3, 0)?;
            let locals2 = expr!(tce2, 1)?;
            let locals1 = expr!(tce1, 2)?;
            let opcode = match op {
                TrinaryOp::SetBuffer8 => Opcode::AVMOpcode(AVMOpcode::SetBuffer8),
                TrinaryOp::SetBuffer64 => Opcode::AVMOpcode(AVMOpcode::SetBuffer64),
                TrinaryOp::SetBuffer256 => Opcode::AVMOpcode(AVMOpcode::SetBuffer256),
            };
            code.push(Instruction::from_opcode(opcode, debug));
            Ok(max(num_locals, max(locals1, max(locals2, locals3))))
        }
        TypeCheckedExprKind::ShortcutOr(tce1, tce2) => {
            let left_locals = expr!(tce1, 0)?;
            let lab = label_gen.next();
            code.push(opcode!(Dup0));
            code.push(opcode!(Cjump, Value::Label(lab)));
            code.push(opcode!(Pop));
            let right_locals = expr!(tce2, 0)?;
            code.push(Instruction::from_opcode(Opcode::Label(lab), debug));
            Ok((max(num_locals, max(left_locals, right_locals))))
        }
        TypeCheckedExprKind::ShortcutAnd(tce1, tce2) => {
            let left_locals = expr!(tce1, 0)?;
            let lab = label_gen.next();
            code.push(opcode!(Dup0));
            code.push(opcode!(IsZero));
            code.push(opcode!(Cjump, Value::Label(lab)));
            code.push(opcode!(Pop));
            let right_locals = expr!(tce2, 0)?;
            code.push(Instruction::from_opcode(Opcode::Label(lab), debug));
            Ok((max(num_locals, max(left_locals, right_locals))))
        }
        TypeCheckedExprKind::LocalVariableRef(name, _) => match locals.get(name) {
            Some(n) => {
                code.push(Instruction::from_opcode_imm(
                    Opcode::GetLocal,
                    Value::Int(Uint256::from_usize(*n)),
                    debug,
                ));
                Ok(num_locals)
            }
            None => {
                println!("local: {:?}", *name);
                Err(CompileError::new_codegen_error(
                    "tried to access non-existent local variable".to_string(),
                    loc,
                ))
            }
        },
        TypeCheckedExprKind::ClosureLoad(id, nargs, local_space, captures, _) => {
            // The closure ABI is based around the idea that a closure pointer is essentially an updatable
            // function frame passed around and copied for each of its invocations. This means space is reserved
            // for the args, which are dynamically written to at the time of a call. We'll call these "blanks",
            // since as the closure is passed around, the values are not yet known. This yeilds the format:
            //
            //            ( codepoint, ( blank_arg1, blank_arg2, ..., blank_argN ) )
            //
            // Closures may also have local variables. Space for these is reserved after the args. These are
            // read and written to during the execution of the closure's code.
            //
            //            ( codepoint, ( <blank_args>, local1, local2, ... localN ) )
            //
            // Closures may also have captures. These are placed after the args and are written to exactly
            // once at the time of creation. They are effectively read-only, and by being at the end
            // remove any type distinction between closures of different captures.
            //
            //            ( codepoint, ( <blank_args>, capture1, capture2, ..., captureN, <locals> ) )
            //
            // For efficiency, when a closure has no captures, we actually express it like a func pointer
            // and then use code at runtime that checks the type.
            //
            //              codepoint
            //
            // In this manner, a function pointer is equivalent to a closure with 0 captures, which is
            // why they are said to be the same type. Lastly, in the rare case a single item is present
            // in the tuple, we de-tuplify for efficiency.
            //
            //            ( codepoint, ( item ) )   === becomes ===>   ( codepoint, item )
            //

            let label = Value::Label(*func_labels.get(id).unwrap());

            if captures.is_empty() {
                code.push(opcode!(Noop, label)); // Equivalent to a function call
            } else {
                for capture in captures {
                    match locals.get(capture) {
                        Some(slot) => code.push(Instruction::from_opcode_imm(
                            Opcode::GetLocal,
                            Value::Int(Uint256::from_usize(*slot)),
                            debug,
                        )),
                        None => {
                            return Err(CompileError::new_codegen_error(
                                "capture doesn't exist".to_string(),
                                loc,
                            ))
                        }
                    }
                }

                let tree = TupleTree::new(*local_space, false);
                code.push(opcode!(Noop, TupleTree::make_empty(&tree)));

                for (index, capture) in captures.iter().enumerate().rev() {
                    if let Some(_) = locals.get(capture) {
                        code.push(Instruction::from_opcode_imm(
                            Opcode::TupleSet(*local_space),
                            Value::from(index + nargs),
                            debug,
                        ))
                    }
                }

                let container = Value::new_tuple(vec![label, Value::none()]);

                code.push(opcode!(Noop, container));
                code.push(opcode!(Tset, Value::from(1)));
            }

            Ok(num_locals)
        }
        TypeCheckedExprKind::GlobalVariableRef(id, _) => {
            let global = match globals.get(id) {
                Some(global) => global,
                None => {
                    return Err(CompileError::new(
                        format!("Internal Error"),
                        format!("StringID {} doesn't exist in {:?}", Color::red(id), globals),
                        loc.into_iter().collect(),
                    ))
                }
            };
            let offset = global.offset.unwrap();
            code.push(Instruction::from_opcode(
                Opcode::GetGlobalVar(offset),
                debug,
            ));
            Ok(num_locals)
        }
        TypeCheckedExprKind::FuncRef(name, _) => {
            let the_label = match func_labels.get(name) {
                Some(label) => *label,
                None => {
                    return Err(CompileError::new_codegen_error(
                        format!("No label for func ref {}", Color::red(name)),
                        debug.location,
                    ))
                }
            };
            code.push(opcode!(Noop, Value::Label(the_label)));
            Ok(num_locals)
        }
        TypeCheckedExprKind::TupleRef(tce, idx, _) => {
            let tce_type = tce.get_type();
            let tuple_size = if let Type::Tuple(fields) = tce_type {
                fields.len()
            } else {
                return Err(CompileError::new_codegen_error(
                    format!(
                        "type-checking bug: tuple lookup in non-tuple type {}",
                        Color::red(tce_type.display())
                    ),
                    loc,
                ));
            };
            let exp_locals = expr!(tce, 0)?;
            code.push(Instruction::from_opcode_imm(
                Opcode::TupleGet(tuple_size),
                Value::Int(idx.clone()),
                debug,
            ));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::DotRef(tce, slot_num, s_size, _) => {
            let exp_locals = expr!(tce, 0)?;
            code.push(Instruction::from_opcode_imm(
                Opcode::TupleGet(*s_size),
                Value::Int(Uint256::from_usize(*slot_num)),
                debug,
            ));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::Const(val, _) => {
            code.push(opcode!(Noop, val.clone()));
            Ok(num_locals)
        }
        TypeCheckedExprKind::FunctionCall(fexpr, args, func_type, _) => {
            let n_args = args.len();
            let ret_label = label_gen.next();
            let mut args_locals = 0;
            for i in 0..n_args {
                let arg_locals = expr!(&args[n_args - 1 - i], i)?;
                args_locals = max(args_locals, arg_locals);
            }
            // this is the thing that pushes the address to the stack
            if &Type::Every != func_type {
                code.push(opcode!(Noop, Value::Label(ret_label)));
            }
            let fexpr_locals = expr!(fexpr, n_args + 1)?;

            // could be a closure, so let's check for the right interface
            //   func vs (closure, frame)

            let codepoint_call = label_gen.next();

            // check whether we're calling on a codepoint or closure tuple
            code.push(opcode!(Dup0));
            code.push(opcode!(Type));
            code.push(opcode!(Equal, Value::from(1))); // 1 for codepoint
            code.push(opcode!(Cjump, Value::Label(codepoint_call)));

            // not a codepoint, let's unpack
            code.push(opcode!(Dup0)); // return (closure, frame) (closure, frame)
            code.push(opcode!(Tget, Value::from(1))); // return (closure, frame) frame
            code.push(opcode!(Swap2)); // frame (closure, frame) return
            code.push(opcode!(Swap1)); // frame return (closure, frame)
            code.push(opcode!(Tget, Value::from(0))); // frame return closure

            code.push(Instruction::from_opcode(
                Opcode::Label(codepoint_call),
                debug,
            ));
            code.push(opcode!(Jump));
            code.push(Instruction::from_opcode(Opcode::Label(ret_label), debug));
            Ok((max(num_locals, max(fexpr_locals, args_locals))))
        }
        TypeCheckedExprKind::CodeBlock(block) => mavm_codegen_code_block(
            block,
            code,
            num_locals,
            locals,
            label_gen,
            string_table,
            func_labels,
            globals,
            prepushed_vals,
            scopes,
            issues,
            debug,
            release_build,
        ),
        TypeCheckedExprKind::StructInitializer(fields, _) => {
            let fields_len = fields.len();
            let mut struct_locals = 0;
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let field_locals = expr!(&field.value, i)?;
                struct_locals = max(struct_locals, field_locals);
            }
            let empty_vec = TupleTree::new(fields_len, false).make_empty();
            code.push(opcode!(Noop, empty_vec));
            for i in 0..fields_len {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(fields_len),
                    Value::Int(Uint256::from_usize(i)),
                    debug,
                ));
            }
            Ok(max(num_locals, struct_locals))
        }
        TypeCheckedExprKind::Tuple(fields, _) => {
            let fields_len = fields.len();
            let mut tuple_locals = 0;
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let field_locals = expr!(&field, i)?;
                tuple_locals = max(field_locals, tuple_locals);
            }
            let empty_vec = vec![Value::none(); fields_len];
            code.push(opcode!(Noop, Value::new_tuple(empty_vec)));
            for i in 0..fields_len {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(fields_len),
                    Value::Int(Uint256::from_usize(i)),
                    debug,
                ));
            }
            Ok(max(num_locals, tuple_locals))
        }
        TypeCheckedExprKind::ArrayRef(expr1, expr2, t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_arrayGet",
                vec![expr1, expr2],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
        }
        TypeCheckedExprKind::FixedArrayRef(expr1, expr2, size, _) => {
            let exp1_locals = expr!(expr1)?;
            let exp2_locals = expr!(expr2, 1)?;
            if *size != 8 {
                //TODO: also skip check if size is larger power of 8
                let cont_label = label_gen.next();
                code.push(opcode!(Dup0));
                code.push(opcode!(GreaterThan, Value::Int(Uint256::from_usize(*size))));
                code.push(opcode!(Cjump, Value::Label(cont_label)));
                code.push(opcode!(Error));
                code.push(Instruction::from_opcode(Opcode::Label(cont_label), debug));
            }
            code.push(Instruction::from_opcode(
                Opcode::UncheckedFixedArrayGet(*size),
                debug,
            ));
            Ok(max(num_locals, max(exp1_locals, exp2_locals)))
        }
        TypeCheckedExprKind::MapRef(map_expr, key_expr, t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_kvsGet",
                vec![map_expr, key_expr],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
        }
        TypeCheckedExprKind::NewArray(sz_expr, base_type, array_type) => {
            let call_type = Type::Func(
                FuncProperties::pure(2, 1),
                vec![Type::Uint, Type::Any],
                Box::new(array_type.clone()),
            );
            let (default_val, _is_safe) = base_type.default_value();
            let the_expr = TypeCheckedExpr {
                kind: TypeCheckedExprKind::FunctionCall(
                    Box::new(TypeCheckedExpr::new(
                        TypeCheckedExprKind::FuncRef(
                            string_table.get_if_exists("builtin_arrayNew").unwrap(),
                            call_type.clone(),
                        ),
                        DebugInfo::from(loc),
                    )),
                    vec![
                        *sz_expr.clone(),
                        TypeCheckedExpr::new(
                            TypeCheckedExprKind::Const(default_val, Type::Any),
                            DebugInfo::from(loc),
                        ),
                    ],
                    call_type,
                    FuncProperties::pure(2, 1),
                ),
                debug_info: DebugInfo::from(loc),
            };
            expr!(&the_expr, 0)
        }
        TypeCheckedExprKind::NewFixedArray(sz, bo_expr, _) => {
            let mut expr_locals = 0;
            match bo_expr {
                Some(expr) => {
                    let some_locals = expr!(expr, 0)?;
                    expr_locals = some_locals;
                    for _i in 0..7 {
                        code.push(opcode!(Dup0));
                    }
                    let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                    code.push(opcode!(Noop, Value::new_tuple(empty_tuple)));
                    for i in 0..8 {
                        code.push(opcode!(Tset, Value::Int(Uint256::from_usize(i))));
                    }
                }
                None => {
                    let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                    code.push(opcode!(Noop, Value::new_tuple(empty_tuple)));
                }
            }
            let mut tuple_size: usize = 8;
            while tuple_size < *sz {
                for _i in 0..7 {
                    code.push(opcode!(Dup0));
                }
                let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                code.push(opcode!(Noop, Value::new_tuple(empty_tuple)));
                for i in 0..8 {
                    code.push(opcode!(Tset, Value::Int(Uint256::from_usize(i))));
                }
                tuple_size *= 8;
            }
            Ok(max(num_locals, expr_locals))
        }
        TypeCheckedExprKind::NewMap(t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_kvsNew",
                vec![],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
        }
        TypeCheckedExprKind::ArrayMod(arr, index, val, t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_arraySet",
                vec![arr, index, val],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
        }
        TypeCheckedExprKind::FixedArrayMod(arr, index, val, size, _) => codegen_fixed_array_mod(
            arr,
            index,
            val,
            *size,
            code,
            num_locals,
            locals,
            label_gen,
            string_table,
            func_labels,
            globals,
            debug,
            prepushed_vals,
            scopes,
            issues,
            release_build,
        ),
        TypeCheckedExprKind::MapMod(map, key, val, t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_kvsSet",
                vec![map, key, val],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
        }
        TypeCheckedExprKind::StructMod(struc, index, val, t) => {
            let val_locals = expr!(val, 0)?;
            let struc_locals = expr!(struc, 1)?;
            if let Type::Struct(v) = t {
                let struct_len = v.len();
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(struct_len),
                    Value::Int(Uint256::from_usize(*index)),
                    debug,
                ));
            } else {
                panic!("impossible value in TypeCheckedExpr::StructMod");
            }
            Ok(max(num_locals, max(val_locals, struc_locals)))
        }
        TypeCheckedExprKind::Cast(expr, _) => expr!(expr, 0),
        TypeCheckedExprKind::Asm(_, insns, args) => {
            let n_args = args.len();
            let mut args_locals = 0;
            for i in 0..n_args {
                let arg_locals = expr!(&args[n_args - 1 - i], i)?;
                args_locals = max(args_locals, arg_locals);
            }
            for insn in insns {
                code.push(insn.clone());
            }
            Ok(max(num_locals, args_locals))
        }
        TypeCheckedExprKind::Try(exp, _) => {
            let exp_locals = expr!(exp, 0)?;
            let extract = label_gen.next();
            code.push(opcode!(Dup0));
            code.push(opcode!(Tget, Value::Int(Uint256::zero())));
            code.push(opcode!(Cjump, Value::Label(extract)));
            // We use the auxstack here to temporarily store the return value while we clear the temp values on the stack
            if prepushed_vals > 0 {
                code.push(opcode!(AuxPush));
                for _ in 0..prepushed_vals {
                    code.push(opcode!(Pop));
                }
                code.push(opcode!(AuxPop));
            }
            code.push(Instruction::from_opcode(Opcode::Return, debug));
            code.push(Instruction::from_opcode(Opcode::Label(extract), debug));
            code.push(opcode!(Tget, Value::Int(Uint256::one())));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::If(cond, block, else_block, _) => {
            let cond_locals = expr!(cond, 0)?;
            let after_label = label_gen.next();
            let end_label = label_gen.next();
            code.push(opcode!(IsZero));
            code.push(opcode!(Cjump, Value::Label(after_label)));
            let block_locals = mavm_codegen_code_block(
                block,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals,
                scopes,
                issues,
                debug,
                release_build,
            )?;
            if else_block.is_some() {
                code.push(opcode!(Jump, Value::Label(end_label)));
            }
            code.push(Instruction::from_opcode(Opcode::Label(after_label), debug));
            let (code, else_locals) = if let Some(block) = else_block {
                let else_locals = mavm_codegen_code_block(
                    block,
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    func_labels,
                    globals,
                    prepushed_vals,
                    scopes,
                    issues,
                    debug,
                    release_build,
                )?;
                (code, else_locals)
            } else {
                (code, 0)
            };
            code.push(Instruction::from_opcode(Opcode::Label(end_label), debug));
            Ok(max(cond_locals, max(block_locals, else_locals)))
        }
        TypeCheckedExprKind::IfLet(name, expr, block, else_block, _) => {
            let after_label = label_gen.next();
            let slot_num = num_locals;
            let mut new_locals = locals.clone();
            new_locals.insert(*name, slot_num);
            let exp_locals = mavm_codegen_expr(
                expr,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals,
                scopes,
                issues,
                release_build,
            )?;
            code.push(opcode!(Dup0));
            code.push(opcode!(Tget, Value::Int(Uint256::from_usize(0))));
            code.push(opcode!(IsZero));
            code.push(opcode!(Cjump, Value::Label(after_label)));
            code.push(opcode!(Tget, Value::Int(Uint256::from_usize(1))));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                Value::Int(Uint256::from_usize(slot_num)),
                debug,
            ));
            let mut total_locals = mavm_codegen_code_block(
                &block,
                code,
                num_locals + 1,
                &new_locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals,
                scopes,
                issues,
                debug,
                release_build,
            )?;
            total_locals = max(total_locals, exp_locals);

            let outside_label = label_gen.next();
            code.push(opcode!(Jump, Value::Label(outside_label)));
            code.push(Instruction::from_opcode(Opcode::Label(after_label), debug));
            code.push(opcode!(Pop));
            if let Some(else_block) = else_block {
                let else_locals = mavm_codegen_code_block(
                    &else_block,
                    code,
                    num_locals,
                    &locals,
                    label_gen,
                    string_table,
                    func_labels,
                    globals,
                    prepushed_vals,
                    scopes,
                    issues,
                    debug,
                    release_build,
                )?;
                total_locals = max(total_locals, else_locals);
            }
            code.push(Instruction::from_opcode(
                Opcode::Label(outside_label),
                debug,
            ));
            Ok(total_locals)
        }
        TypeCheckedExprKind::Loop(body) => {
            let slot_num = Value::Int(Uint256::from_usize(num_locals));
            let top_label = label_gen.next();
            let bottom_label = label_gen.next();
            scopes.push(("_".to_string(), bottom_label, Some(Type::Tuple(vec![]))));
            code.push(opcode!(Noop, Value::Label(top_label)));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                slot_num.clone(),
                debug,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(top_label), debug));
            let (nl, _) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals + 1,
                locals,
                label_gen,
                string_table,
                func_labels,
                globals,
                prepushed_vals,
                scopes,
                issues,
                release_build,
            )?;
            scopes.pop();
            code.push(Instruction::from_opcode_imm(
                Opcode::GetLocal,
                slot_num,
                debug,
            ));
            code.push(opcode!(Jump));
            code.push(Instruction::from_opcode(Opcode::Label(bottom_label), debug));
            Ok(max(num_locals + 1, nl))
        }
    }
}

/// Used to codegen the FixedArrayMod variant of TypeCheckedExpr.
fn codegen_fixed_array_mod(
    arr_expr: &TypeCheckedExpr,
    idx_expr: &TypeCheckedExpr,
    val_expr: &TypeCheckedExpr,
    size: usize,
    code: &mut Vec<Instruction>,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    label_gen: &mut LabelGenerator,
    string_table: &StringTable,
    func_labels: &HashMap<StringId, Label>,
    globals: &HashMap<StringId, GlobalVar>,
    debug_info: DebugInfo,
    prepushed_vals: usize,
    scopes: &mut Vec<(String, Label, Option<Type>)>,
    issues: &mut Vec<CompileError>,
    release_build: bool,
) -> Result<usize, CompileError> {
    let val_locals = mavm_codegen_expr(
        val_expr,
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        func_labels,
        globals,
        prepushed_vals,
        scopes,
        issues,
        release_build,
    )?;
    let arr_locals = mavm_codegen_expr(
        arr_expr,
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        func_labels,
        globals,
        prepushed_vals + 1,
        scopes,
        issues,
        release_build,
    )?;
    let idx_locals = mavm_codegen_expr(
        idx_expr,
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        func_labels,
        globals,
        prepushed_vals + 2,
        scopes,
        issues,
        release_build,
    )?;

    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug_info)
        };
        ($opcode:ident, $immediate:expr) => {
            Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::$opcode),
                $immediate,
                debug_info,
            )
        };
    }

    if size != 8 {
        // TODO: safe for if-condition to say size does not equal any power of 8
        let ok_label = label_gen.next();
        code.push(opcode!(Dup0));
        code.push(opcode!(GreaterThan, Value::Int(Uint256::from_usize(size))));
        code.push(opcode!(Cjump, Value::Label(ok_label)));
        code.push(opcode!(Error));
        code.push(Instruction::from_opcode(
            Opcode::Label(ok_label),
            debug_info,
        ));
    }
    let exp_locals = max(val_locals, max(arr_locals, idx_locals));
    codegen_fixed_array_mod_2(
        val_expr,
        size,
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        func_labels,
        globals,
        debug_info,
    )
    .map(|num_locals| max(num_locals, exp_locals))
}

/// Used by codegen_fixed_array_mod, you should not call this directly.
fn codegen_fixed_array_mod_2(
    val_expr: &TypeCheckedExpr,
    size: usize,
    code: &mut Vec<Instruction>,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    label_gen: &mut LabelGenerator,
    string_table: &StringTable,
    func_labels: &HashMap<StringId, Label>,
    globals: &HashMap<StringId, GlobalVar>,
    debug_info: DebugInfo,
) -> Result<usize, CompileError> {
    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug_info)
        };
        ($opcode:ident, $immediate:expr) => {
            Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::$opcode),
                $immediate,
                debug_info,
            )
        };
    }

    if size <= 8 {
        // stack: idx tuple val
        code.push(opcode!(Tset));
        Ok(num_locals)
    } else {
        let tuple_size = Value::Int(Uint256::from_usize(TUPLE_SIZE));
        // stack: idx tupletree val
        code.push(opcode!(Dup2, tuple_size.clone()));
        code.push(opcode!(AuxPush));
        code.push(opcode!(Dup1));

        // stack: idx TUPLE_SIZE idx tupletree val; aux: tupletree
        code.push(opcode!(Mod));
        code.push(opcode!(Dup0));
        code.push(opcode!(AuxPush));

        // stack: slot idx tupletree val; aux: slot tupletree
        code.push(opcode!(Swap1));
        code.push(opcode!(Swap1, tuple_size));
        code.push(opcode!(Div));

        // stack: subidx slot tupletree val; aux: slot tupletree
        code.push(opcode!(Swap2));
        code.push(opcode!(Swap1));

        // stack: slot tupletree subidx val; aux: slot tupletree
        code.push(opcode!(Tget));
        code.push(opcode!(Swap1));

        // stack: subidx subtupletree val; aux: slot tupletree

        let inner_locals = codegen_fixed_array_mod_2(
            val_expr,
            (size + (TUPLE_SIZE - 1)) / TUPLE_SIZE,
            code,
            num_locals,
            locals,
            label_gen,
            string_table,
            func_labels,
            globals,
            debug_info,
        )?;

        // stack: newsubtupletree; aux: slot tupletree
        code.push(opcode!(AuxPop));
        code.push(opcode!(AuxPop));
        code.push(opcode!(Swap1));

        // stack: slot tupletree newsubtupletree
        code.push(opcode!(Tset));

        Ok(max(num_locals, inner_locals))
    }
}
