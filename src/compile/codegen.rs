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

use super::ast::{BinaryOp, FuncArg, GlobalVarDecl, Type, UnaryOp};
use super::symtable::CopyingSymTable;
use super::typecheck::{
    TypeCheckedExpr, TypeCheckedFunc, TypeCheckedIfArm, TypeCheckedMatchPattern,
    TypeCheckedStatement,
};
use crate::compile::ast::OptionConst;
use crate::link::ImportedFunc;
use crate::link::TUPLE_SIZE;
use crate::mavm::{Instruction, Label, LabelGenerator, Opcode, Value};
use crate::pos::Location;
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::HashMap;

#[derive(Debug)]
pub struct CodegenError {
    pub reason: &'static str,
    pub location: Option<Location>,
}

pub fn new_codegen_error(reason: &'static str, location: Option<Location>) -> CodegenError {
    CodegenError { reason, location }
}

pub fn mavm_codegen<'a>(
    funcs: Vec<TypeCheckedFunc>,
    code_in: &'a mut Vec<Instruction>,
    string_table: &'a StringTable,
    imported_funcs: &[ImportedFunc],
    global_vars: &[GlobalVarDecl],
) -> Result<&'a mut Vec<Instruction>, CodegenError> {
    let mut import_func_map = HashMap::new();
    for imp_func in imported_funcs {
        import_func_map.insert(imp_func.name_id, Label::External(imp_func.slot_num));
    }

    let mut global_var_map = HashMap::new();
    for (idx, gv) in global_vars.iter().enumerate() {
        global_var_map.insert(gv.name, idx);
    }

    let mut label_gen = LabelGenerator::new();
    let mut code = code_in;
    for func in funcs {
        if !func.imported {
            let (lg, c) = mavm_codegen_func(
                func,
                code,
                label_gen,
                string_table,
                &import_func_map,
                &global_var_map,
            )?;
            label_gen = lg;
            code = c;
        }
    }
    Ok(code)
}

fn mavm_codegen_func<'a>(
    func: TypeCheckedFunc,
    code: &'a mut Vec<Instruction>,
    mut label_gen: LabelGenerator,
    string_table: &'a StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
    let location = func.location;
    code.push(Instruction::from_opcode(
        Opcode::Label(Label::Func(func.name)),
        location,
    ));

    let num_args = func.args.len();
    let locals = CopyingSymTable::<usize>::new();

    let make_frame_slot = code.len();
    code.push(Instruction::from_opcode(Opcode::Noop, location)); // placeholder; will replace this later

    let (lg, max_num_locals, maybe_continue) = add_args_to_locals_table(
        locals,
        &func.args,
        0,
        func.code,
        code,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
    )?;
    label_gen = lg;

    if maybe_continue {
        match func.ret_type {
            Type::Void => {
                code.push(Instruction::from_opcode(Opcode::Return, location));
            }
            _ => {
                return Err(new_codegen_error(
                    "apparent path to end of function without return",
                    location,
                ));
            }
        }
    }

    // put makeframe Instruction at beginning of function, to build the frame (replacing placeholder)
    code[make_frame_slot] =
        Instruction::from_opcode(Opcode::MakeFrame(num_args, max_num_locals), location);

    Ok((label_gen, code))
}

fn add_args_to_locals_table<'a>(
    locals: CopyingSymTable<'a, usize>,
    args: &'a [FuncArg],
    num_locals: usize,
    statements: Vec<TypeCheckedStatement>,
    code: &'a mut Vec<Instruction>,
    label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
    if args.is_empty() {
        mavm_codegen_statements(
            statements,
            code,
            num_locals,
            &locals,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
        )
    } else {
        let new_locals = locals.push_one(args[0].name, num_locals);
        add_args_to_locals_table(
            new_locals,
            &args[1..],
            num_locals + 1,
            statements,
            code,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
        )
    }
}

fn mavm_codegen_statements<'a>(
    statements: Vec<TypeCheckedStatement>,  // statements to codegen
    mut code: &'a mut Vec<Instruction>,     // accumulates the code as it's generated
    mut num_locals: usize,                  // num locals that have been allocated
    locals: &'a CopyingSymTable<'a, usize>, // lookup local variable slot number by name
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
    // (label_gen, num_labels, execution_might_continue)
    if statements.is_empty() {
        return Ok((label_gen, num_locals, true));
    }
    let rest_of_statements = &statements[1..];
    match &statements[0] {
        TypeCheckedStatement::Noop(_) => mavm_codegen_statements(
            rest_of_statements.to_vec(),
            code,
            num_locals,
            locals,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
        ),
        TypeCheckedStatement::Panic(loc) => {
            code.push(Instruction::from_opcode(Opcode::Panic, *loc));
            Ok((label_gen, num_locals, false))
            // no need to append the rest of the statements; they'll never be executed
        }
        TypeCheckedStatement::ReturnVoid(loc) => {
            code.push(Instruction::from_opcode(Opcode::Return, *loc));
            Ok((label_gen, num_locals, false))
            // no need to append the rest of the statements; they'll never be executed
        }
        TypeCheckedStatement::Return(expr, loc) => {
            let (lg, c) = mavm_codegen_expr(
                expr,
                code,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode(Opcode::Return, *loc));
            Ok((label_gen, num_locals, false))
            // no need to append the rest of the statements; they'll never be executed
        }
        TypeCheckedStatement::FunctionCall(fexpr, args, loc) => {
            let n_args = args.len();
            let (ret_label, lg) = label_gen.next();
            label_gen = lg;
            for i in 0..n_args {
                let (lg, c) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
            }
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Label(ret_label),
                *loc,
            ));
            let (lg, c) = mavm_codegen_expr(
                fexpr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            c.push(Instruction::from_opcode(Opcode::Jump, *loc));
            c.push(Instruction::from_opcode(Opcode::Label(ret_label), *loc));
            mavm_codegen_statements(
                rest_of_statements.to_vec(),
                c,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedStatement::Let(pat, expr, loc) => match pat {
            TypeCheckedMatchPattern::Simple(name, _) => {
                let slot_num = num_locals;
                let new_locals = locals.push_one(*name, slot_num);
                num_locals += 1;
                let (lg, c) = mavm_codegen_expr(
                    expr,
                    code,
                    &new_locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
                code.push(Instruction::from_opcode_imm(
                    Opcode::SetLocal,
                    Value::Int(Uint256::from_usize(slot_num)),
                    *loc,
                ));
                mavm_codegen_statements(
                    rest_of_statements.to_vec(),
                    code,
                    num_locals,
                    &new_locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )
            }
            TypeCheckedMatchPattern::Tuple(pattern, _) => {
                let (lg, c) = mavm_codegen_expr(
                    expr,
                    code,
                    &locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
                let local_slot_num_base = num_locals;
                let mut pairs = HashMap::new();
                for (i, sub_pat) in pattern.clone().iter().enumerate() {
                    match sub_pat {
                        TypeCheckedMatchPattern::Simple(name, _) => {
                            pairs.insert(*name, local_slot_num_base + i);
                        }
                        TypeCheckedMatchPattern::Tuple(_, _) => {
                            return Err(new_codegen_error(
                                "nested pattern not supported in pattern-match let",
                                *loc,
                            ));
                        }
                    }
                }
                num_locals += pattern.len();
                mavm_codegen_tuple_pattern(code, pattern, local_slot_num_base, *loc);
                mavm_codegen_statements(
                    rest_of_statements.to_vec(),
                    code,
                    num_locals,
                    &locals.push_multi(pairs),
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )
            }
        },
        TypeCheckedStatement::AssignLocal(name, expr, loc) => {
            let slot_num = match locals.get(*name) {
                Some(slot) => slot,
                None => return Err(new_codegen_error("assigned to non-existent variable", *loc)),
            };
            let (lg, c) = mavm_codegen_expr(
                expr,
                code,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                Value::Int(Uint256::from_usize(slot_num)),
                *loc,
            ));
            mavm_codegen_statements(
                rest_of_statements.to_vec(),
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedStatement::AssignGlobal(idx, expr, loc) => {
            let (lg, c) = mavm_codegen_expr(
                expr,
                code,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            c.push(Instruction::from_opcode(Opcode::SetGlobalVar(*idx), *loc));
            mavm_codegen_statements(
                rest_of_statements.to_vec(),
                c,
                num_locals,
                &locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedStatement::Loop(body, loc) => {
            let slot_num = Value::Int(Uint256::from_usize(num_locals));
            num_locals += 1;
            let (top_label, lg) = label_gen.next();
            label_gen = lg;
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Label(top_label),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                slot_num.clone(),
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(top_label), *loc));
            let (lg, nl, _) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            num_locals = nl;
            code.push(Instruction::from_opcode_imm(
                Opcode::GetLocal,
                slot_num,
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Jump, *loc));
            Ok((label_gen, num_locals, false))
            // no need to append the rest of the statements; they'll never be executed
        }
        TypeCheckedStatement::While(cond, body, loc) => {
            let num_locals_at_start = num_locals;
            let slot_num = Value::Int(Uint256::from_usize(num_locals));
            num_locals += 1;
            let (top_label, lg) = label_gen.next();
            let (cond_label, lg) = lg.next();
            label_gen = lg;
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Label(top_label),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                slot_num,
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::Jump,
                Value::Label(cond_label),
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(top_label), *loc));
            let (lg, nl, _) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            num_locals = nl;
            code.push(Instruction::from_opcode(Opcode::Label(cond_label), *loc));
            let (lg, c) = mavm_codegen_expr(
                cond,
                code,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode_imm(
                Opcode::Cjump,
                Value::Label(top_label),
                *loc,
            ));
            let (lg, nl, more) = mavm_codegen_statements(
                rest_of_statements.to_vec(),
                code,
                num_locals_at_start,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            Ok((lg, if nl > num_locals { nl } else { num_locals }, more))
        }
        TypeCheckedStatement::If(arm) => {
            let (end_label, lg) = label_gen.next();
            let (lg, nl1, might_continue) = mavm_codegen_if_arm(
                arm,
                end_label,
                code,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            if might_continue {
                let (lg, nl2, more) = mavm_codegen_statements(
                    rest_of_statements.to_vec(),
                    code,
                    num_locals,
                    locals,
                    lg,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                Ok((lg, if nl1 > nl2 { nl1 } else { nl2 }, more))
            } else {
                Ok((lg, nl1, false))
            }
        }
        TypeCheckedStatement::Asm(insns, args, _loc) => {
            let n_args = args.len();
            for i in 0..n_args {
                let (lg, c) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
            }
            for insn in insns {
                code.push(insn.clone());
            }
            mavm_codegen_statements(
                rest_of_statements.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedStatement::DebugPrint(e, loc) => {
            let (lg, c) = mavm_codegen_expr(
                e,
                code,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode(Opcode::DebugPrint, *loc));
            mavm_codegen_statements(
                rest_of_statements.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
    }
}

fn mavm_codegen_tuple_pattern<'a>(
    code: &'a mut Vec<Instruction>,
    pattern: &[TypeCheckedMatchPattern],
    local_slot_num_base: usize,
    loc: Option<Location>,
) {
    let pat_size = pattern.len();
    for (i, pat) in pattern.iter().enumerate() {
        if i < pat_size - 1 {
            code.push(Instruction::from_opcode(Opcode::Dup0, loc));
        }
        match pat {
            TypeCheckedMatchPattern::Simple(_, _) => {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleGet(pat_size),
                    Value::Int(Uint256::from_usize(i)),
                    loc,
                ));
                code.push(Instruction::from_opcode_imm(
                    Opcode::SetLocal,
                    Value::Int(Uint256::from_usize(local_slot_num_base + i)),
                    loc,
                ));
            }
            TypeCheckedMatchPattern::Tuple(_, _) => {
                panic!("Can't yet generate code for pattern-match let with nested tuples");
            }
        }
    }
}

fn mavm_codegen_if_arm<'a>(
    arm: &TypeCheckedIfArm,
    end_label: Label,
    mut code: &'a mut Vec<Instruction>, // accumulates the code as it's generated
    num_locals: usize,                  // num locals that have been allocated
    locals: &CopyingSymTable<usize>,    // lookup local variable slot number by name
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
    // (label_gen, num_labels, execution_might_continue)
    match arm {
        TypeCheckedIfArm::Cond(cond, body, orest, loc) => {
            let (after_label, lg) = label_gen.next();
            let (lg, c) = mavm_codegen_expr(
                cond,
                code,
                &locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode(Opcode::Not, *loc));
            code.push(Instruction::from_opcode_imm(
                Opcode::Cjump,
                Value::Label(after_label),
                *loc,
            ));
            let (lg, nl1, might_continue_here) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            if might_continue_here {
                code.push(Instruction::from_opcode_imm(
                    Opcode::Jump,
                    Value::Label(end_label),
                    *loc,
                ));
            }
            code.push(Instruction::from_opcode(Opcode::Label(after_label), *loc));
            match orest {
                Some(inner_arm) => {
                    let (lg, nl2, inner_might_continue) = mavm_codegen_if_arm(
                        inner_arm,
                        end_label,
                        code,
                        num_locals,
                        locals,
                        label_gen,
                        string_table,
                        import_func_map,
                        global_var_map,
                    )?;
                    Ok((
                        lg,
                        if nl1 > nl2 { nl1 } else { nl2 },
                        inner_might_continue || might_continue_here,
                    ))
                }
                None => {
                    code.push(Instruction::from_opcode(Opcode::Label(end_label), *loc));
                    Ok((label_gen, nl1, true))
                }
            }
        }
        TypeCheckedIfArm::Catchall(body, loc) => {
            let (lg, nl, might_continue) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            code.push(Instruction::from_opcode(Opcode::Label(end_label), *loc));
            Ok((lg, nl, might_continue))
        }
    }
}

fn mavm_codegen_expr<'a>(
    expr: &TypeCheckedExpr,
    mut code: &'a mut Vec<Instruction>,
    locals: &CopyingSymTable<usize>,
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
    match expr {
        TypeCheckedExpr::UnaryOp(op, tce, _, loc) => {
            let (lg, c) = mavm_codegen_expr(
                tce,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            let (maybe_opcode, maybe_imm) = match op {
                UnaryOp::Minus => (Some(Opcode::UnaryMinus), None),
                UnaryOp::BitwiseNeg => (Some(Opcode::BitwiseNeg), None),
                UnaryOp::Not => (Some(Opcode::Not), None),
                UnaryOp::Hash => (Some(Opcode::Hash), None),
                UnaryOp::ToUint => (None, None),
                UnaryOp::ToInt => (None, None),
                UnaryOp::ToBytes32 => (None, None),
                UnaryOp::ToAddress => {
                    let mask = Uint256::from_usize(2)
                        .exp(&Uint256::from_usize(20))
                        .sub(&Uint256::one())
                        .ok_or(new_codegen_error("Underflow on substraction", *loc))?;
                    (Some(Opcode::BitwiseAnd), Some(Value::Int(mask)))
                }
                UnaryOp::Len => (Some(Opcode::TupleGet(3)), Some(Value::Int(Uint256::zero()))),
            };
            if let Some(opcode) = maybe_opcode {
                code.push(Instruction::new(opcode, maybe_imm, *loc));
            }
            Ok((label_gen, code))
        }
        TypeCheckedExpr::Binary(op, tce1, tce2, _, loc) => {
            let (lg, c) = mavm_codegen_expr(
                tce2,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            let (lg, c) = mavm_codegen_expr(
                tce1,
                c,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            let opcode = match op {
                BinaryOp::Plus => Opcode::Plus,
                BinaryOp::Minus => Opcode::Minus,
                BinaryOp::Times => Opcode::Mul,
                BinaryOp::Div => Opcode::Div,
                BinaryOp::Mod => Opcode::Mod,
                BinaryOp::Sdiv => Opcode::Sdiv,
                BinaryOp::Smod => Opcode::Smod,
                BinaryOp::LessThan => Opcode::LessThan,
                BinaryOp::GreaterThan => Opcode::GreaterThan,
                BinaryOp::LessEq => Opcode::GreaterThan, // will negate
                BinaryOp::GreaterEq => Opcode::SLessThan, // will negate
                BinaryOp::SLessThan => Opcode::SLessThan,
                BinaryOp::SGreaterThan => Opcode::SGreaterThan,
                BinaryOp::SLessEq => Opcode::SGreaterThan, // will negate
                BinaryOp::SGreaterEq => Opcode::SLessThan, // will negate
                BinaryOp::Equal => Opcode::Equal,
                BinaryOp::NotEqual => Opcode::NotEqual,
                BinaryOp::BitwiseAnd => Opcode::BitwiseAnd,
                BinaryOp::BitwiseOr => Opcode::BitwiseOr,
                BinaryOp::BitwiseXor => Opcode::BitwiseXor,
                BinaryOp::LogicalAnd => Opcode::LogicalAnd,
                BinaryOp::LogicalOr => Opcode::LogicalOr,
                BinaryOp::Hash => Opcode::Hash2,
            };
            code.push(Instruction::from_opcode(opcode, *loc));
            match op {
                BinaryOp::LessEq
                | BinaryOp::GreaterEq
                | BinaryOp::SLessEq
                | BinaryOp::SGreaterEq => {
                    code.push(Instruction::from_opcode(Opcode::Not, *loc));
                }
                _ => {}
            }
            Ok((label_gen, code))
        }
        TypeCheckedExpr::ShortcutOr(tce1, tce2, loc) => {
            let (lg, c) = mavm_codegen_expr(
                tce1,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            let (lab, lg) = lg.next();
            c.push(Instruction::from_opcode(Opcode::Dup0, *loc));
            c.push(Instruction::from_opcode_imm(
                Opcode::Cjump,
                Value::Label(lab),
                *loc,
            ));
            c.push(Instruction::from_opcode(Opcode::Pop, *loc));
            let (lg, c) = mavm_codegen_expr(
                tce2,
                c,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            c.push(Instruction::from_opcode(Opcode::Label(lab), *loc));
            Ok((lg, c))
        }
        TypeCheckedExpr::ShortcutAnd(tce1, tce2, loc) => {
            let (lg, c) = mavm_codegen_expr(
                tce1,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            let (lab, lg) = lg.next();
            c.push(Instruction::from_opcode(Opcode::Dup0, *loc));
            c.push(Instruction::from_opcode(Opcode::Not, *loc));
            c.push(Instruction::from_opcode_imm(
                Opcode::Cjump,
                Value::Label(lab),
                *loc,
            ));
            c.push(Instruction::from_opcode(Opcode::Pop, *loc));
            let (lg, c) = mavm_codegen_expr(
                tce2,
                c,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            c.push(Instruction::from_opcode(Opcode::Label(lab), *loc));
            Ok((lg, c))
        }
        TypeCheckedExpr::LocalVariableRef(name, _, loc) => match locals.get(*name) {
            Some(n) => {
                code.push(Instruction::from_opcode_imm(
                    Opcode::GetLocal,
                    Value::Int(Uint256::from_usize(n)),
                    *loc,
                ));
                Ok((label_gen, code))
            }
            None => {
                println!("local: {:?}", *name);
                Err(new_codegen_error(
                    "tried to access non-existent local variable",
                    *loc,
                ))
            }
        },
        TypeCheckedExpr::GlobalVariableRef(idx, _, loc) => {
            code.push(Instruction::from_opcode(Opcode::GetGlobalVar(*idx), *loc));
            Ok((label_gen, code))
        }
        TypeCheckedExpr::FuncRef(name, _, loc) => {
            let the_label = match import_func_map.get(name) {
                Some(label) => *label,
                None => Label::Func(*name),
            };
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Label(the_label),
                *loc,
            ));
            Ok((label_gen, code))
        }
        TypeCheckedExpr::TupleRef(tce, idx, _, loc) => {
            let tce_type = tce.get_type();
            let tuple_size = if let Type::Tuple(fields) = tce_type {
                fields.len()
            } else {
                panic!("type-checking bug: tuple lookup in non-tuple type");
            };
            let (lg, c) = mavm_codegen_expr(
                tce,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            c.push(Instruction::from_opcode_imm(
                Opcode::TupleGet(tuple_size),
                Value::Int(idx.clone()),
                *loc,
            ));
            Ok((lg, c))
        }
        TypeCheckedExpr::DotRef(tce, name, _, loc) => {
            let tce_type = tce.get_type();
            let struct_size = match tce_type.clone() {
                Type::Struct(fields) => fields.len(),
                _ => panic!("type-checking bug: struct lookup in non-struct type"),
            };
            let (lg, c) = mavm_codegen_expr(
                tce,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            match tce_type.get_struct_slot_by_name(*name) {
                Some(slot_num) => {
                    code.push(Instruction::from_opcode_imm(
                        Opcode::TupleGet(struct_size),
                        Value::Int(Uint256::from_usize(slot_num)),
                        *loc,
                    ));
                    Ok((label_gen, code))
                }
                None => Err(new_codegen_error(
                    "tried to get non-existent struct field",
                    *loc,
                )),
            }
        }
        TypeCheckedExpr::ConstOption(optconst) => {
            let val = match optconst {
                OptionConst::Some(t) => (*t).value(),
                OptionConst::None(_) => Value::Int(Uint256::zero()),
            };
            code.push(Instruction::from_opcode_imm(Opcode::Noop, val, None));
            Ok((label_gen, code))
        }
        TypeCheckedExpr::Const(val, _, loc) => {
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                val.clone(),
                *loc,
            ));
            Ok((label_gen, code))
        }
        TypeCheckedExpr::FunctionCall(fexpr, args, _, loc) => {
            let n_args = args.len();
            let (ret_label, lg) = label_gen.next();
            label_gen = lg;
            for i in 0..n_args {
                let (lg, c) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
            }
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Label(ret_label),
                *loc,
            ));
            let (lg, c) = mavm_codegen_expr(
                fexpr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            c.push(Instruction::from_opcode(Opcode::Jump, *loc));
            c.push(Instruction::from_opcode(Opcode::Label(ret_label), *loc));
            Ok((lg, c))
        }
        TypeCheckedExpr::StructInitializer(fields, _, loc) => {
            let fields_len = fields.len();
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let (lg, c) = mavm_codegen_expr(
                    &field.value,
                    code,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
            }
            let empty_vec = vec![Value::none(); fields_len];
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Tuple(empty_vec),
                *loc,
            ));
            for i in 0..fields_len {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(fields_len),
                    Value::Int(Uint256::from_usize(i)),
                    *loc,
                ));
            }
            Ok((label_gen, code))
        }
        TypeCheckedExpr::Tuple(fields, _, loc) => {
            let fields_len = fields.len();
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let (lg, c) = mavm_codegen_expr(
                    &field,
                    code,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
            }
            let empty_vec = vec![Value::none(); fields_len];
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Tuple(empty_vec),
                *loc,
            ));
            for i in 0..fields_len {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(fields_len),
                    Value::Int(Uint256::from_usize(i)),
                    *loc,
                ));
            }
            Ok((label_gen, code))
        }
        TypeCheckedExpr::ArrayRef(expr1, expr2, t, loc) => {
            let call_type = Type::Func(
                false,
                vec![Type::Array(Box::new(Type::Any)), Type::Uint],
                Box::new(t.clone()),
            );
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_arrayGet").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![*expr1.clone(), *expr2.clone()],
                call_type,
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedExpr::FixedArrayRef(expr1, expr2, size, _, loc) => {
            let (lg, c) = mavm_codegen_expr(
                expr1,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            let (lg, c) = mavm_codegen_expr(
                expr2,
                c,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            if *size != 8 {
                //TODO: also skip check if size is larger power of 8
                let (cont_label, lg) = label_gen.next();
                label_gen = lg;
                code.push(Instruction::from_opcode(Opcode::Dup0, *loc));
                code.push(Instruction::from_opcode_imm(
                    Opcode::GreaterThan,
                    Value::Int(Uint256::from_usize(*size)),
                    *loc,
                ));
                code.push(Instruction::from_opcode_imm(
                    Opcode::Cjump,
                    Value::Label(cont_label),
                    *loc,
                ));
                code.push(Instruction::from_opcode(Opcode::Panic, *loc));
                code.push(Instruction::from_opcode(Opcode::Label(cont_label), *loc));
            }
            code.push(Instruction::from_opcode(
                Opcode::UncheckedFixedArrayGet(*size),
                *loc,
            ));
            Ok((label_gen, code))
        }
        TypeCheckedExpr::MapRef(map_expr, key_expr, _, loc) => {
            let call_type = Type::Func(
                false,
                vec![Type::Any, Type::Any],
                Box::new(Type::Tuple(vec![Type::Bool, Type::Any])),
            );
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_kvsGet").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![*map_expr.clone(), *key_expr.clone()],
                call_type,
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedExpr::NewArray(sz_expr, base_type, array_type, loc) => {
            let call_type = Type::Func(
                false,
                vec![Type::Uint, Type::Any],
                Box::new(array_type.clone()),
            );
            let default_val = base_type.default_value();
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_arrayNew").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![
                    *sz_expr.clone(),
                    TypeCheckedExpr::Const(default_val, Type::Any, *loc),
                ],
                call_type,
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedExpr::NewFixedArray(sz, bo_expr, _, loc) => {
            match bo_expr {
                Some(expr) => {
                    let (lg, c) = mavm_codegen_expr(
                        expr,
                        code,
                        locals,
                        label_gen,
                        string_table,
                        import_func_map,
                        global_var_map,
                    )?;
                    label_gen = lg;
                    code = c;
                    for _i in 0..7 {
                        code.push(Instruction::from_opcode(Opcode::Dup0, *loc));
                    }
                    let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
                    code.push(Instruction::from_opcode_imm(
                        Opcode::Noop,
                        Value::Tuple(empty_tuple),
                        *loc,
                    ));
                    for i in 0..8 {
                        code.push(Instruction::from_opcode_imm(
                            Opcode::Tset,
                            Value::Int(Uint256::from_usize(i)),
                            *loc,
                        ));
                    }
                }
                None => {
                    let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
                    code.push(Instruction::from_opcode_imm(
                        Opcode::Noop,
                        Value::Tuple(empty_tuple),
                        *loc,
                    ));
                }
            }
            let mut tuple_size: usize = 8;
            while tuple_size < *sz {
                for _i in 0..7 {
                    code.push(Instruction::from_opcode(Opcode::Dup0, *loc));
                }
                let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
                code.push(Instruction::from_opcode_imm(
                    Opcode::Noop,
                    Value::Tuple(empty_tuple),
                    *loc,
                ));
                for i in 0..8 {
                    code.push(Instruction::from_opcode_imm(
                        Opcode::Tset,
                        Value::Int(Uint256::from_usize(i)),
                        *loc,
                    ));
                }
                tuple_size *= 8;
            }
            Ok((label_gen, code))
        }
        TypeCheckedExpr::NewMap(_, loc) => {
            let call_type = Type::Func(false, vec![], Box::new(Type::Any));
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_kvsNew").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![],
                call_type,
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedExpr::ArrayMod(arr, index, val, _, loc) => {
            let call_type = Type::Func(
                false,
                vec![arr.get_type(), index.get_type(), val.get_type()],
                Box::new(arr.get_type()),
            );
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_arraySet").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![*arr.clone(), *index.clone(), *val.clone()],
                call_type,
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedExpr::FixedArrayMod(arr, index, val, size, _, loc) => codegen_fixed_array_mod(
            arr,
            index,
            val,
            *size,
            code,
            locals,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
            *loc,
        ),
        TypeCheckedExpr::MapMod(map_expr, key_expr, val_expr, _, loc) => {
            let call_type = Type::Func(
                false,
                vec![
                    map_expr.get_type(),
                    key_expr.get_type(),
                    val_expr.get_type(),
                ],
                Box::new(map_expr.get_type()),
            );
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_kvsSet").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![*map_expr.clone(), *key_expr.clone(), *val_expr.clone()],
                call_type,
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )
        }
        TypeCheckedExpr::StructMod(struc, index, val, t, loc) => {
            let (lg, c) = mavm_codegen_expr(
                val,
                code,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            let (lg, c) = mavm_codegen_expr(
                struc,
                c,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            label_gen = lg;
            code = c;
            if let Type::Struct(v) = t {
                let struct_len = v.len();
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(struct_len),
                    Value::Int(Uint256::from_usize(*index)),
                    *loc,
                ));
            } else {
                panic!("impossible value in TypeCheckedExpr::StructMod");
            }
            Ok((label_gen, code))
        }
        TypeCheckedExpr::Cast(expr, _, _) => mavm_codegen_expr(
            expr,
            code,
            locals,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
        ),
        TypeCheckedExpr::Asm(_, insns, args, _) => {
            let n_args = args.len();
            for i in 0..n_args {
                let (lg, c) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                label_gen = lg;
                code = c;
            }
            for insn in insns {
                code.push(insn.clone());
            }
            Ok((label_gen, code))
        }
    }
}

fn codegen_fixed_array_mod<'a>(
    arr_expr: &TypeCheckedExpr,
    idx_expr: &TypeCheckedExpr,
    val_expr: &TypeCheckedExpr,
    size: usize,
    code_in: &'a mut Vec<Instruction>,
    locals: &CopyingSymTable<usize>,
    label_gen_in: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
    location: Option<Location>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
    let (label_gen, code) = mavm_codegen_expr(
        val_expr,
        code_in,
        locals,
        label_gen_in,
        string_table,
        import_func_map,
        global_var_map,
    )?;
    let (label_gen, code) = mavm_codegen_expr(
        arr_expr,
        code,
        locals,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
    )?;
    let (mut label_gen, code) = mavm_codegen_expr(
        idx_expr,
        code,
        locals,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
    )?;
    if size != 8 {
        // TODO: safe for if-condition to say size does not equal any power of 8
        let (ok_label, lg) = label_gen.next();
        label_gen = lg;
        code.push(Instruction::from_opcode(Opcode::Dup0, location));
        code.push(Instruction::from_opcode_imm(
            Opcode::GreaterThan,
            Value::Int(Uint256::from_usize(size)),
            location,
        ));
        code.push(Instruction::from_opcode_imm(
            Opcode::Cjump,
            Value::Label(ok_label),
            location,
        ));
        code.push(Instruction::from_opcode(Opcode::Panic, location));
        code.push(Instruction::from_opcode(Opcode::Label(ok_label), location));
    }
    codegen_fixed_array_mod_2(
        val_expr,
        size,
        code,
        locals,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
        location,
    )
}

fn codegen_fixed_array_mod_2<'a>(
    val_expr: &TypeCheckedExpr,
    size: usize,
    code_in: &'a mut Vec<Instruction>,
    locals: &CopyingSymTable<usize>,
    label_gen_in: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
    location: Option<Location>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
    if size <= 8 {
        // stack: idx tuple val
        code_in.push(Instruction::from_opcode(Opcode::Tset, location));
        Ok((label_gen_in, code_in))
    } else {
        let tuple_size = Value::Int(Uint256::from_usize(TUPLE_SIZE));
        // stack: idx tupletree val
        code_in.push(Instruction::from_opcode_imm(
            Opcode::Dup2,
            tuple_size.clone(),
            location,
        ));
        code_in.push(Instruction::from_opcode(Opcode::AuxPush, location));
        code_in.push(Instruction::from_opcode(Opcode::Dup1, location));
        // stack: idx TUPLE_SIZE idx tupletree val; aux: tupletree
        code_in.push(Instruction::from_opcode(Opcode::Mod, location));
        code_in.push(Instruction::from_opcode(Opcode::Dup0, location));
        code_in.push(Instruction::from_opcode(Opcode::AuxPush, location));
        // stack: slot idx tupletree val; aux: slot tupletree
        code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
        code_in.push(Instruction::from_opcode_imm(
            Opcode::Swap1,
            tuple_size,
            location,
        ));
        code_in.push(Instruction::from_opcode(Opcode::Div, location));
        // stack: subidx slot tupletree val; aux: slot tupletree
        code_in.push(Instruction::from_opcode(Opcode::Swap2, location));
        code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
        // stack: slot tupletree subidx val; aux: slot tupletree
        code_in.push(Instruction::from_opcode(Opcode::Tget, location));
        code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
        // stack: subidx subtupletree val; aux: slot tupletree

        let (label_gen, code) = codegen_fixed_array_mod_2(
            val_expr,
            (size + (TUPLE_SIZE - 1)) / TUPLE_SIZE,
            code_in,
            locals,
            label_gen_in,
            string_table,
            import_func_map,
            global_var_map,
            location,
        )?;

        // stack: newsubtupletree; aux: slot tupletree
        code.push(Instruction::from_opcode(Opcode::AuxPop, location));
        code.push(Instruction::from_opcode(Opcode::AuxPop, location));
        code.push(Instruction::from_opcode(Opcode::Swap1, location));
        // stack: slot tupletree newsubtupletree
        code.push(Instruction::from_opcode(Opcode::Tset, location));

        Ok((label_gen, code))
    }
}
