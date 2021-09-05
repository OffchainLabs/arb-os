/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Contains utilities for generating instructions from AST structures.

use super::ast::{BinaryOp, FuncProperties, GlobalVar, TrinaryOp, Type, UnaryOp};
use super::typecheck::{
    TypeCheckedExpr, TypeCheckedFunc, TypeCheckedMatchPattern, TypeCheckedStatement,
};
use crate::compile::ast::{DebugInfo, MatchPatternKind, TypeTree};
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

struct Codegen<'a> {
    code: &'a mut Vec<Instruction>,
    label_gen: &'a mut LabelGenerator,
    string_table: &'a StringTable,
    func_labels: &'a HashMap<StringId, Label>,
    globals: &'a HashMap<StringId, GlobalVar>,
    issues: &'a mut Vec<CompileError>,
    release_build: bool,
}

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

    let mut codegen = Codegen {
        code: &mut code,
        label_gen: &mut label_gen,
        string_table,
        func_labels,
        globals,
        issues,
        release_build,
    };

    let (mut space_for_locals, _slot_map) =
        mavm_codegen_statements(func.code, &mut codegen, locals.len(), &locals, 0)?;

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
    cgen: &mut Codegen,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    prepushed_vals: usize,
    _debug_info: DebugInfo,
) -> Result<usize, CompileError> {
    let (nl, block_locals) =
        mavm_codegen_statements(block.body.clone(), cgen, num_locals, locals, prepushed_vals)?;
    if let Some(ret_expr) = &block.ret_expr {
        let mut new_locals = locals.clone();
        new_locals.extend(block_locals);
        let prepushed_vals_expr =
            mavm_codegen_expr(ret_expr, cgen, nl, &new_locals, prepushed_vals)
                .map(|exp_locals| (max(num_locals, max(exp_locals, nl))))?;
        Ok(prepushed_vals_expr)
    } else {
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
    cgen: &mut Codegen,
    mut num_locals: usize,          // num locals that have been allocated
    locals: &HashMap<usize, usize>, // lookup local variable slot number by name
    prepushed_vals: usize,
) -> Result<(usize, HashMap<StringId, usize>), CompileError> {
    let mut bindings = HashMap::new();
    for statement in statements {
        let mut new_locals = locals.clone();
        new_locals.extend(bindings.clone());
        let (statement_locals, statement_bindings) =
            mavm_codegen_statement(statement, cgen, num_locals, &new_locals, prepushed_vals)?;
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
    cgen: &mut Codegen,
    mut num_locals: usize,          // num locals that have been allocated
    locals: &HashMap<usize, usize>, // lookup local variable slot number by name
    prepushed_vals: usize,
) -> Result<(usize, HashMap<StringId, usize>), CompileError> {
    macro_rules! expr {
        ($expr:expr) => {
            mavm_codegen_expr(&$expr, cgen, num_locals, &locals, prepushed_vals)?
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
            cgen.code
                .push(Instruction::from_opcode(Opcode::Return, debug));
            Ok((0, HashMap::new()))
        }
        TypeCheckedStatementKind::Return(expr) => {
            let exp_locals = expr!(expr);
            if prepushed_vals > 0 {
                cgen.code.push(opcode!(AuxPush));
                for _ in 0..prepushed_vals {
                    cgen.code.push(opcode!(Pop));
                }
                cgen.code.push(opcode!(AuxPop));
            }
            cgen.code
                .push(Instruction::from_opcode(Opcode::Return, debug));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::Break(..) => {
            panic!("Encountered a break node")
        }
        TypeCheckedStatementKind::Expression(expr) => {
            let exp_locals = expr!(expr);
            if !(expr.get_type() == Type::Void || expr.get_type() == Type::Every) {
                cgen.code.push(opcode!(Pop));
                if expr.get_type() != Type::Tuple(vec![]) {
                    cgen.issues.push(CompileError::new_warning(
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
            let (new_locals, bindings, _assignments) =
                mavm_codegen_tuple_pattern(cgen, &pat, num_locals, locals, debug)?;
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
            cgen.code
                .push(Instruction::from_opcode(Opcode::SetLocal(*slot_num), debug));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::AssignGlobal(id, expr) => {
            let exp_locals = expr!(expr);
            let global = cgen.globals.get(id).expect("No global exists for stringID");
            let offset = global.offset.unwrap();
            cgen.code.push(Instruction::from_opcode(
                Opcode::SetGlobalVar(offset),
                debug,
            ));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::While(cond, body) => {
            let slot_num = num_locals;
            num_locals += 1;
            let top_label = cgen.label_gen.next();
            let cond_label = cgen.label_gen.next();
            cgen.code.push(opcode!(Noop, Value::Label(top_label)));
            cgen.code
                .push(Instruction::from_opcode(Opcode::SetLocal(slot_num), debug));
            cgen.code.push(opcode!(Jump, Value::Label(cond_label)));
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(top_label), debug));
            let (nl, _) =
                mavm_codegen_statements(body.to_vec(), cgen, num_locals, locals, prepushed_vals)?;
            num_locals = nl;
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(cond_label), debug));
            let cond_locals = expr!(cond);
            cgen.code
                .push(Instruction::from_opcode(Opcode::GetLocal(slot_num), debug));
            cgen.code.push(opcode!(Cjump));
            Ok((max(cond_locals, num_locals), HashMap::new()))
        }
        TypeCheckedStatementKind::Asm(insns, args) => {
            let n_args = args.len();
            let mut exp_locals = 0;
            for i in 0..n_args {
                let e_locals = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    cgen,
                    num_locals,
                    locals,
                    prepushed_vals + i,
                )?;
                exp_locals = max(exp_locals, e_locals);
            }
            for insn in insns {
                cgen.code.push(insn.clone());
            }
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::DebugPrint(expr) => {
            let exp_locals = expr!(expr);
            cgen.code.push(opcode!(DebugPrint));
            Ok((exp_locals, HashMap::new()))
        }
        TypeCheckedStatementKind::Assert(expr) => {
            if cgen.release_build {
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
                            cgen.string_table.get_if_exists("builtin_assert").unwrap(),
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
    cgen: &mut Codegen,
    pattern: &TypeCheckedMatchPattern,
    local_slot_num_base: usize,
    locals: &HashMap<usize, usize>,
    debug_info: DebugInfo,
) -> Result<(usize, HashMap<usize, usize>, HashSet<usize>), CompileError> {
    match &pattern.kind {
        MatchPatternKind::Bind(name) => {
            let mut bindings = HashMap::new();
            bindings.insert(*name, local_slot_num_base);
            cgen.code.push(Instruction::from_opcode(
                Opcode::SetLocal(local_slot_num_base),
                debug_info,
            ));
            Ok((1, bindings, HashSet::new()))
        }
        MatchPatternKind::Assign(id) => {
            if let Some(val) = locals.get(id) {
                cgen.code
                    .push(Instruction::from_opcode(Opcode::SetLocal(*val), debug_info))
            } else {
                cgen.code.push(Instruction::from_opcode(
                    Opcode::SetGlobalVar(
                        cgen.globals
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
                    cgen.code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Dup0),
                        debug_info,
                    ));
                }
                cgen.code.push(Instruction::from_opcode_imm(
                    Opcode::TupleGet(pat_size),
                    Value::Int(Uint256::from_usize(i)),
                    debug_info,
                ));
                let (num_new_bindings, new_bindings, new_assignments) = mavm_codegen_tuple_pattern(
                    cgen,
                    pat,
                    local_slot_num_base + num_bindings,
                    locals,
                    debug_info,
                )?;
                num_bindings += num_new_bindings;
                bindings.extend(new_bindings);
                for name in new_assignments {
                    if !assignments.insert(name) {
                        return Err(CompileError::new_codegen_error(
                            format!(
                                "assigned to variable {} in mixed let multiple times",
                                cgen.string_table.name_from_id(name)
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
    cgen: &mut Codegen,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    prepushed_vals: usize,
) -> Result<usize, CompileError> {
    macro_rules! expr {
        ($expr:expr) => {
            expr!($expr, 0)
        };
        ($expr:expr, $prepushed:expr) => {
            mavm_codegen_expr($expr, cgen, num_locals, locals, prepushed_vals + $prepushed)
        };
    }

    let debug = expr.debug_info;
    let loc = expr.debug_info.location;

    let string_table = cgen.string_table;

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
            cgen.code.push(opcode!(NewBuffer));
            Ok(num_locals)
        }
        TypeCheckedExprKind::Quote(bytes) => {
            cgen.code.push(opcode!(
                Noop,
                Value::new_tuple(vec![
                    Value::Int(Uint256::from_usize(bytes.len())),
                    Value::Buffer(Buffer::from_bytes(bytes.clone())),
                ])
            ));
            Ok(num_locals)
        }
        TypeCheckedExprKind::Error => {
            cgen.code.push(opcode!(Error));
            Ok(num_locals)
        }
        TypeCheckedExprKind::GetGas => {
            cgen.code.push(opcode!(PushGas));
            Ok(num_locals)
        }
        TypeCheckedExprKind::SetGas(tce) => {
            let exp_locals = expr!(tce, 0)?;
            cgen.code.push(opcode!(SetGas));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::UnaryOp(op, tce, _) => {
            let exp_locals = expr!(tce, 0)?;
            cgen.code.push(match op {
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
            cgen.code.push(opcode!(
                Noop,
                Value::new_tuple(vec![Value::Int(Uint256::from_usize(1)), Value::none()])
            ));
            cgen.code
                .push(opcode!(Tset, Value::Int(Uint256::from_u64(1))));
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
            cgen.code.push(Instruction::from_opcode(opcode, debug));
            match op {
                BinaryOp::NotEqual
                | BinaryOp::LessEq
                | BinaryOp::GreaterEq
                | BinaryOp::SLessEq
                | BinaryOp::SGreaterEq => cgen.code.push(opcode!(IsZero)),
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
            cgen.code.push(Instruction::from_opcode(opcode, debug));
            Ok(max(num_locals, max(locals1, max(locals2, locals3))))
        }
        TypeCheckedExprKind::ShortcutOr(tce1, tce2) => {
            let left_locals = expr!(tce1, 0)?;
            let lab = cgen.label_gen.next();
            cgen.code.push(opcode!(Dup0));
            cgen.code.push(opcode!(Cjump, Value::Label(lab)));
            cgen.code.push(opcode!(Pop));
            let right_locals = expr!(tce2, 0)?;
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(lab), debug));
            Ok((max(num_locals, max(left_locals, right_locals))))
        }
        TypeCheckedExprKind::ShortcutAnd(tce1, tce2) => {
            let left_locals = expr!(tce1, 0)?;
            let lab = cgen.label_gen.next();
            cgen.code.push(opcode!(Dup0));
            cgen.code.push(opcode!(IsZero));
            cgen.code.push(opcode!(Cjump, Value::Label(lab)));
            cgen.code.push(opcode!(Pop));
            let right_locals = expr!(tce2, 0)?;
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(lab), debug));
            Ok((max(num_locals, max(left_locals, right_locals))))
        }
        TypeCheckedExprKind::LocalVariableRef(name, _) => match locals.get(name) {
            Some(n) => {
                cgen.code
                    .push(Instruction::from_opcode(Opcode::GetLocal(*n), debug));
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

            let label = Value::Label(*cgen.func_labels.get(id).unwrap());

            if captures.is_empty() {
                cgen.code.push(opcode!(Noop, label)); // Equivalent to a function call
            } else {
                for capture in captures {
                    match locals.get(capture) {
                        Some(slot) => cgen
                            .code
                            .push(Instruction::from_opcode(Opcode::GetLocal(*slot), debug)),
                        None => {
                            return Err(CompileError::new_codegen_error(
                                "capture doesn't exist".to_string(),
                                loc,
                            ))
                        }
                    }
                }

                let tree = TupleTree::new(*local_space, false);
                cgen.code.push(opcode!(Noop, TupleTree::make_empty(&tree)));

                for (index, capture) in captures.iter().enumerate().rev() {
                    if let Some(_) = locals.get(capture) {
                        cgen.code.push(Instruction::from_opcode_imm(
                            Opcode::TupleSet(*local_space),
                            Value::from(index + nargs),
                            debug,
                        ))
                    }
                }

                let container = Value::new_tuple(vec![label, Value::none()]);

                cgen.code.push(opcode!(Noop, container));
                cgen.code.push(opcode!(Tset, Value::from(1)));
            }

            Ok(num_locals)
        }
        TypeCheckedExprKind::GlobalVariableRef(id, _) => {
            let global = match cgen.globals.get(id) {
                Some(global) => global,
                None => {
                    return Err(CompileError::new(
                        format!("Internal Error"),
                        format!(
                            "StringID {} doesn't exist in {:?}",
                            Color::red(id),
                            cgen.globals
                        ),
                        loc.into_iter().collect(),
                    ))
                }
            };
            let offset = global.offset.unwrap();
            cgen.code.push(Instruction::from_opcode(
                Opcode::GetGlobalVar(offset),
                debug,
            ));
            Ok(num_locals)
        }
        TypeCheckedExprKind::FuncRef(name, _) => {
            let the_label = match cgen.func_labels.get(name) {
                Some(label) => *label,
                None => {
                    return Err(CompileError::new_codegen_error(
                        format!("No label for func ref {}", Color::red(name)),
                        debug.location,
                    ))
                }
            };
            cgen.code.push(opcode!(Noop, Value::Label(the_label)));
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
            cgen.code.push(Instruction::from_opcode_imm(
                Opcode::TupleGet(tuple_size),
                Value::Int(idx.clone()),
                debug,
            ));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::DotRef(tce, slot_num, s_size, _) => {
            let exp_locals = expr!(tce, 0)?;
            cgen.code.push(Instruction::from_opcode_imm(
                Opcode::TupleGet(*s_size),
                Value::Int(Uint256::from_usize(*slot_num)),
                debug,
            ));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::Const(val, _) => {
            cgen.code.push(opcode!(Noop, val.clone()));
            Ok(num_locals)
        }
        TypeCheckedExprKind::FunctionCall(fexpr, args, _, prop) => {
            let nargs = args.len();
            let mut args_locals = 0;
            for i in 0..nargs {
                let arg_locals = expr!(&args[nargs - 1 - i], i)?;
                args_locals = max(args_locals, arg_locals);
            }
            let fexpr_locals = expr!(fexpr, nargs + 1)?;
            cgen.code
                .push(Instruction::from_opcode(Opcode::FuncCall(*prop), debug));
            Ok((max(num_locals, max(fexpr_locals, args_locals))))
        }
        TypeCheckedExprKind::CodeBlock(block) => {
            mavm_codegen_code_block(block, cgen, num_locals, locals, prepushed_vals, debug)
        }
        TypeCheckedExprKind::StructInitializer(fields, _) => {
            let fields_len = fields.len();
            let mut struct_locals = 0;
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let field_locals = expr!(&field.value, i)?;
                struct_locals = max(struct_locals, field_locals);
            }
            let empty_vec = TupleTree::new(fields_len, false).make_empty();
            cgen.code.push(opcode!(Noop, empty_vec));
            for i in 0..fields_len {
                cgen.code.push(Instruction::from_opcode_imm(
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
            cgen.code.push(opcode!(Noop, Value::new_tuple(empty_vec)));
            for i in 0..fields_len {
                cgen.code.push(Instruction::from_opcode_imm(
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
                let cont_label = cgen.label_gen.next();
                cgen.code.push(opcode!(Dup0));
                cgen.code
                    .push(opcode!(GreaterThan, Value::Int(Uint256::from_usize(*size))));
                cgen.code.push(opcode!(Cjump, Value::Label(cont_label)));
                cgen.code.push(opcode!(Error));
                cgen.code
                    .push(Instruction::from_opcode(Opcode::Label(cont_label), debug));
            }
            cgen.code.push(Instruction::from_opcode(
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
            let default_val = base_type.default_value(&TypeTree::default());
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
                        cgen.code.push(opcode!(Dup0));
                    }
                    let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                    cgen.code.push(opcode!(Noop, Value::new_tuple(empty_tuple)));
                    for i in 0..8 {
                        cgen.code
                            .push(opcode!(Tset, Value::Int(Uint256::from_usize(i))));
                    }
                }
                None => {
                    let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                    cgen.code.push(opcode!(Noop, Value::new_tuple(empty_tuple)));
                }
            }
            let mut tuple_size: usize = 8;
            while tuple_size < *sz {
                for _i in 0..7 {
                    cgen.code.push(opcode!(Dup0));
                }
                let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                cgen.code.push(opcode!(Noop, Value::new_tuple(empty_tuple)));
                for i in 0..8 {
                    cgen.code
                        .push(opcode!(Tset, Value::Int(Uint256::from_usize(i))));
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
            cgen,
            num_locals,
            locals,
            debug,
            prepushed_vals,
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
                cgen.code.push(Instruction::from_opcode_imm(
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
                cgen.code.push(insn.clone());
            }
            Ok(max(num_locals, args_locals))
        }
        TypeCheckedExprKind::Try(exp, _) => {
            let exp_locals = expr!(exp, 0)?;
            let extract = cgen.label_gen.next();
            cgen.code.push(opcode!(Dup0));
            cgen.code.push(opcode!(Tget, Value::Int(Uint256::zero())));
            cgen.code.push(opcode!(Cjump, Value::Label(extract)));
            // We use the auxstack here to temporarily store the return value while we clear the temp values on the stack
            if prepushed_vals > 0 {
                cgen.code.push(opcode!(AuxPush));
                for _ in 0..prepushed_vals {
                    cgen.code.push(opcode!(Pop));
                }
                cgen.code.push(opcode!(AuxPop));
            }
            cgen.code
                .push(Instruction::from_opcode(Opcode::Return, debug));
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(extract), debug));
            cgen.code.push(opcode!(Tget, Value::Int(Uint256::one())));
            Ok(max(num_locals, exp_locals))
        }
        TypeCheckedExprKind::If(cond, block, else_block, _) => {
            let cond_locals = expr!(cond, 0)?;
            let after_label = cgen.label_gen.next();
            let end_label = cgen.label_gen.next();
            cgen.code.push(opcode!(IsZero));
            cgen.code.push(opcode!(Cjump, Value::Label(after_label)));
            let block_locals =
                mavm_codegen_code_block(block, cgen, num_locals, locals, prepushed_vals, debug)?;
            if else_block.is_some() {
                cgen.code.push(opcode!(Jump, Value::Label(end_label)));
            }
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(after_label), debug));
            let else_locals = if let Some(block) = else_block {
                let else_locals = mavm_codegen_code_block(
                    block,
                    cgen,
                    num_locals,
                    locals,
                    prepushed_vals,
                    debug,
                )?;
                else_locals
            } else {
                0
            };
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(end_label), debug));
            Ok(max(cond_locals, max(block_locals, else_locals)))
        }
        TypeCheckedExprKind::IfLet(name, expr, block, else_block, _) => {
            let after_label = cgen.label_gen.next();
            let slot_num = num_locals;
            let mut new_locals = locals.clone();
            new_locals.insert(*name, slot_num);
            let exp_locals = mavm_codegen_expr(expr, cgen, num_locals, &locals, prepushed_vals)?;
            cgen.code.push(opcode!(Dup0));
            cgen.code
                .push(opcode!(Tget, Value::Int(Uint256::from_usize(0))));
            cgen.code.push(opcode!(IsZero));
            cgen.code.push(opcode!(Cjump, Value::Label(after_label)));
            cgen.code
                .push(opcode!(Tget, Value::Int(Uint256::from_usize(1))));
            cgen.code
                .push(Instruction::from_opcode(Opcode::SetLocal(slot_num), debug));
            let mut total_locals = mavm_codegen_code_block(
                &block,
                cgen,
                num_locals + 1,
                &new_locals,
                prepushed_vals,
                debug,
            )?;
            total_locals = max(total_locals, exp_locals);

            let outside_label = cgen.label_gen.next();
            cgen.code.push(opcode!(Jump, Value::Label(outside_label)));
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(after_label), debug));
            cgen.code.push(opcode!(Pop));
            if let Some(else_block) = else_block {
                let else_locals = mavm_codegen_code_block(
                    &else_block,
                    cgen,
                    num_locals,
                    &locals,
                    prepushed_vals,
                    debug,
                )?;
                total_locals = max(total_locals, else_locals);
            }
            cgen.code.push(Instruction::from_opcode(
                Opcode::Label(outside_label),
                debug,
            ));
            Ok(total_locals)
        }
        TypeCheckedExprKind::Loop(body) => {
            let slot_num = num_locals;
            let top_label = cgen.label_gen.next();
            let bottom_label = cgen.label_gen.next();
            cgen.code.push(opcode!(Noop, Value::Label(top_label)));
            cgen.code
                .push(Instruction::from_opcode(Opcode::SetLocal(slot_num), debug));
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(top_label), debug));
            let (nl, _) = mavm_codegen_statements(
                body.to_vec(),
                cgen,
                num_locals + 1,
                locals,
                prepushed_vals,
            )?;
            cgen.code
                .push(Instruction::from_opcode(Opcode::GetLocal(slot_num), debug));
            cgen.code.push(opcode!(Jump));
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(bottom_label), debug));
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
    cgen: &mut Codegen,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
    debug_info: DebugInfo,
    prepushed_vals: usize,
) -> Result<usize, CompileError> {
    let val_locals = mavm_codegen_expr(val_expr, cgen, num_locals, locals, prepushed_vals)?;
    let arr_locals = mavm_codegen_expr(arr_expr, cgen, num_locals, locals, prepushed_vals + 1)?;
    let idx_locals = mavm_codegen_expr(idx_expr, cgen, num_locals, locals, prepushed_vals + 2)?;

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
        let ok_label = cgen.label_gen.next();
        cgen.code.push(opcode!(Dup0));
        cgen.code
            .push(opcode!(GreaterThan, Value::Int(Uint256::from_usize(size))));
        cgen.code.push(opcode!(Cjump, Value::Label(ok_label)));
        cgen.code.push(opcode!(Error));
        cgen.code.push(Instruction::from_opcode(
            Opcode::Label(ok_label),
            debug_info,
        ));
    }
    let exp_locals = max(val_locals, max(arr_locals, idx_locals));
    codegen_fixed_array_mod_2(val_expr, cgen, size, num_locals, locals, debug_info)
        .map(|num_locals| max(num_locals, exp_locals))
}

/// Used by codegen_fixed_array_mod, you should not call this directly.
fn codegen_fixed_array_mod_2(
    val_expr: &TypeCheckedExpr,
    cgen: &mut Codegen,
    size: usize,
    num_locals: usize,
    locals: &HashMap<usize, usize>,
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
        cgen.code.push(opcode!(Tset));
        Ok(num_locals)
    } else {
        let tuple_size = Value::Int(Uint256::from_usize(TUPLE_SIZE));
        // stack: idx tupletree val
        cgen.code.push(opcode!(Dup2, tuple_size.clone()));
        cgen.code.push(opcode!(AuxPush));
        cgen.code.push(opcode!(Dup1));

        // stack: idx TUPLE_SIZE idx tupletree val; aux: tupletree
        cgen.code.push(opcode!(Mod));
        cgen.code.push(opcode!(Dup0));
        cgen.code.push(opcode!(AuxPush));

        // stack: slot idx tupletree val; aux: slot tupletree
        cgen.code.push(opcode!(Swap1));
        cgen.code.push(opcode!(Swap1, tuple_size));
        cgen.code.push(opcode!(Div));

        // stack: subidx slot tupletree val; aux: slot tupletree
        cgen.code.push(opcode!(Swap2));
        cgen.code.push(opcode!(Swap1));

        // stack: slot tupletree subidx val; aux: slot tupletree
        cgen.code.push(opcode!(Tget));
        cgen.code.push(opcode!(Swap1));

        // stack: subidx subtupletree val; aux: slot tupletree

        let inner_locals = codegen_fixed_array_mod_2(
            val_expr,
            cgen,
            (size + (TUPLE_SIZE - 1)) / TUPLE_SIZE,
            num_locals,
            locals,
            debug_info,
        )?;

        // stack: newsubtupletree; aux: slot tupletree
        cgen.code.push(opcode!(AuxPop));
        cgen.code.push(opcode!(AuxPop));
        cgen.code.push(opcode!(Swap1));

        // stack: slot tupletree newsubtupletree
        cgen.code.push(opcode!(Tset));

        Ok(max(num_locals, inner_locals))
    }
}
