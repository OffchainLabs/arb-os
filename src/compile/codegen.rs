/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Contains utilities for generating instructions from AST structures.

use super::ast::{BinaryOp, FuncArg, GlobalVarDecl, Type, UnaryOp};
use super::symtable::CopyingSymTable;
use super::typecheck::{
    PropertiesList, TypeCheckedExpr, TypeCheckedFunc, TypeCheckedIfArm, TypeCheckedMatchPattern,
    TypeCheckedStatement,
};
use crate::link::{ImportedFunc, TupleTree, TUPLE_SIZE};
use crate::mavm::{AVMOpcode, Instruction, Label, LabelGenerator, Opcode, Value};
use crate::pos::Location;
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::BTreeMap;
use std::{cmp::max, collections::HashMap};

///Represents any encountered during codegen
#[derive(Debug)]
pub struct CodegenError {
    pub reason: &'static str,
    pub location: Option<Location>,
}

pub fn new_codegen_error(reason: &'static str, location: Option<Location>) -> CodegenError {
    CodegenError { reason, location }
}

///Top level function for code generation, generates code for modules.
///
///In this function, funcs represents a list of functions in scope, string_table is used to get
/// builtins, imported_funcs is a list of functions imported from other modules, and global_vars
/// lists the globals available in the module.
///
/// The function returns a vector of instructions representing the generated code if it is
/// successful, otherwise it returns a CodegenError.
pub fn mavm_codegen(
    funcs: Vec<TypeCheckedFunc>,
    string_table: &StringTable,
    imported_funcs: &[ImportedFunc],
    global_vars: &[GlobalVarDecl],
) -> Result<Vec<Instruction>, CodegenError> {
    let mut import_func_map = HashMap::new();
    for imp_func in imported_funcs {
        import_func_map.insert(imp_func.name_id, Label::External(imp_func.slot_num));
    }

    let mut global_var_map = HashMap::new();
    for (idx, gv) in global_vars.iter().enumerate() {
        global_var_map.insert(gv.name, idx);
    }

    let mut label_gen = LabelGenerator::new();
    let mut funcs_code = BTreeMap::new();
    for func in funcs {
        if !func.imported {
            let id = func.name;
            let (lg, function_code) = mavm_codegen_func(
                func,
                label_gen,
                string_table,
                &import_func_map,
                &global_var_map,
            )?;
            label_gen = lg;
            funcs_code.insert(id, function_code);
        }
    }
    let mut code = Vec::new();
    for (_id, mut func) in funcs_code {
        code.append(&mut func)
    }
    Ok(code)
}

///This generates code for individual mini functions.
///
///In this function, func represents the function to be codegened, label_gen should point to the
/// next available label ID, string_table is used to get builtins, imported_func_map is a list of
/// functions imported from other modules, and global_var_map lists the globals available in the
/// module.
///
/// If successful the function returns a tuple containing the state of the label generator after
/// codegen, and a vector of the generated code, otherwise it returns a CodegenError.
fn mavm_codegen_func(
    func: TypeCheckedFunc,
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, Vec<Instruction>), CodegenError> {
    let mut code = vec![];
    let location = func.location;
    code.push(Instruction::from_opcode(
        Opcode::Label(Label::Func(func.name)),
        location,
    ));

    let num_args = func.args.len();
    let locals = CopyingSymTable::<usize>::new();

    let make_frame_slot = code.len();
    code.push(Instruction::from_opcode(
        Opcode::AVMOpcode(AVMOpcode::Noop),
        location,
    )); // placeholder; will replace this later

    let (lg, max_num_locals, maybe_continue) = add_args_to_locals_table(
        locals,
        &func.args,
        0,
        func.code,
        &mut code,
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

///This adds args to locals, and then codegens the statements in statements, using the updated
/// locals and other function arguments as parameters.
///
/// If successful the function returns a tuple containing the updated label generator, maximum
/// number of locals used so far by this call frame, and a bool that is set to true when the
/// function may continue past the end of the generated code, otherwise the function returns a
/// CodegenError.
fn add_args_to_locals_table(
    locals: CopyingSymTable<'_, usize>,
    args: &[FuncArg],
    num_locals: usize,
    statements: Vec<TypeCheckedStatement>,
    code: &mut Vec<Instruction>,
    label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
    let mut locals_map = HashMap::new();
    for (index, arg) in args.iter().enumerate() {
        locals_map.insert(arg.name, num_locals + index);
    }
    let new_locals = locals.push_multi(locals_map);
    mavm_codegen_statements(
        statements,
        code,
        num_locals + args.len(),
        &new_locals,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
    )
    .map(|(a, b, c, _)| (a, b, c))
}

///Generates code for the provided statements with index 0 generated first. code represents the
/// code generated previously, num_locals the maximum number of locals used at any point in the call
/// frame so far, locals is a map of local variables, label_gen points to the next available locals
/// slot, string_table is used to get builtins, import_func_map associates each imported function
/// with a label, and global_var_map maps global variable IDs to their slot number.
///
/// If successful the function returns a tuple containing the updated label generator, maximum
/// number of locals used so far by this call frame, a bool that is set to true when the function
/// may continue past the end of the generated code, and a map of locals available at the end of the
/// statement sequence, otherwise the function returns a CodegenError.
fn mavm_codegen_statements(
    statements: Vec<TypeCheckedStatement>, // statements to codegen
    code: &mut Vec<Instruction>,           // accumulates the code as it's generated
    mut num_locals: usize,                 // num locals that have been allocated
    locals: &CopyingSymTable<'_, usize>,   // lookup local variable slot number by name
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool, HashMap<StringId, usize>), CodegenError> {
    let mut bindings = HashMap::new();
    for statement in statements {
        let new_locals = locals.push_multi(bindings.clone());
        let (lg, statement_locals, returns, statement_bindings) = mavm_codegen_statement(
            statement,
            code,
            num_locals,
            &new_locals,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
        )?;
        label_gen = lg;
        num_locals = max(statement_locals, num_locals);
        for (id, bind) in statement_bindings {
            bindings.insert(id, bind);
        }
        if returns {
            return Ok((label_gen, num_locals, false, bindings));
        }
    }
    Ok((label_gen, num_locals, true, bindings))
}

///Generates code for the provided statement. code represents the code generated previously,
/// num_locals the maximum number of locals used at any point in the call frame so far, locals is a
/// map of local variables, label_gen points to the next available locals slot, string_table is used
/// to get builtins, import_func_map associates each imported function with a label, and
/// global_var_map maps global variable IDs to their slot number.
///
/// If successful the function returns a tuple containing the updated label generator, number of
/// locals slots used by this statement, a bool that is set to true if execution can not continue
/// past this statement, and a map of locals generated by this statement, otherwise the function
/// returns a CodegenError.
fn mavm_codegen_statement(
    statement: TypeCheckedStatement,     // statement to codegen
    mut code: &mut Vec<Instruction>,     // accumulates the code as it's generated
    mut num_locals: usize,               // num locals that have been allocated
    locals: &CopyingSymTable<'_, usize>, // lookup local variable slot number by name
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool, HashMap<StringId, usize>), CodegenError> {
    match &statement {
        TypeCheckedStatement::Noop(_) => Ok((label_gen, 0, false, HashMap::new())),
        TypeCheckedStatement::Panic(loc) => {
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Panic),
                *loc,
            ));
            Ok((label_gen, 0, true, HashMap::new()))
        }
        TypeCheckedStatement::ReturnVoid(loc) => {
            code.push(Instruction::from_opcode(Opcode::Return, *loc));
            Ok((label_gen, 0, true, HashMap::new()))
        }
        TypeCheckedStatement::Return(expr, loc) => {
            let (lg, c, exp_locals) = mavm_codegen_expr(
                expr,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            c.push(Instruction::from_opcode(Opcode::Return, *loc));
            Ok((lg, exp_locals, true, HashMap::new()))
        }
        TypeCheckedStatement::Expression(expr, loc) => {
            let (lg, c, exp_locals) = mavm_codegen_expr(
                expr,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            if expr.get_type() != Type::Void {
                c.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Pop),
                    *loc,
                ));
                if expr.get_type() != Type::Tuple(vec![]) {
                    println!(
                        "Warning: expression statement at {} returns value of type {:?}, which is discarded",
                        if let Some(loc) = loc {
                            format!("line: {} column: {}", loc.line, loc.column)
                        } else {
                            "unknown location".to_string()
                        },
                        expr.get_type()
                    );
                }
            }
            Ok((lg, exp_locals, false, HashMap::new()))
        }
        TypeCheckedStatement::Let(pat, expr, loc) => match pat {
            TypeCheckedMatchPattern::Simple(name, _) => {
                let slot_num = num_locals;
                let (lg, c, exp_locals) = mavm_codegen_expr(
                    expr,
                    code,
                    num_locals,
                    &locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    0,
                )?;
                let mut bindings = HashMap::new();
                bindings.insert(*name, slot_num);
                num_locals += 1;
                num_locals = max(num_locals, exp_locals);
                label_gen = lg;
                code = c;
                code.push(Instruction::from_opcode_imm(
                    Opcode::SetLocal,
                    Value::Int(Uint256::from_usize(slot_num)),
                    *loc,
                ));
                Ok((label_gen, num_locals, false, bindings))
            }
            TypeCheckedMatchPattern::Tuple(pattern, _) => {
                let (lg, c, exp_locals) = mavm_codegen_expr(
                    expr,
                    code,
                    num_locals,
                    &locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    0,
                )?;
                label_gen = lg;
                code = c;
                let mut pairs = HashMap::new();
                let mut binding_types = Vec::new();
                for (i, sub_pat) in pattern.clone().iter().enumerate() {
                    match sub_pat {
                        TypeCheckedMatchPattern::Simple(name, _) => {
                            pairs.insert(*name, num_locals + i);
                            binding_types.push((*name, num_locals + i));
                        }
                        TypeCheckedMatchPattern::Tuple(_, _) => {
                            return Err(new_codegen_error(
                                "nested pattern not supported in pattern-match let",
                                *loc,
                            ));
                        }
                    }
                }
                mavm_codegen_tuple_pattern(code, pattern, num_locals, *loc);
                num_locals += pattern.len();
                num_locals = max(num_locals, exp_locals);
                Ok((label_gen, num_locals, false, pairs))
            }
        },
        TypeCheckedStatement::AssignLocal(name, expr, loc) => {
            let slot_num = match locals.get(*name) {
                Some(slot) => slot,
                None => return Err(new_codegen_error("assigned to non-existent variable", *loc)),
            };
            let (lg, c, exp_locals) = mavm_codegen_expr(
                expr,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                Value::Int(Uint256::from_usize(slot_num)),
                *loc,
            ));
            Ok((label_gen, exp_locals, false, HashMap::new()))
        }
        TypeCheckedStatement::AssignGlobal(idx, expr, loc) => {
            let (lg, c, exp_locals) = mavm_codegen_expr(
                expr,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            c.push(Instruction::from_opcode(Opcode::SetGlobalVar(*idx), *loc));
            Ok((lg, exp_locals, false, HashMap::new()))
        }
        TypeCheckedStatement::Loop(body, loc) => {
            let slot_num = Value::Int(Uint256::from_usize(num_locals));
            num_locals += 1;
            let (top_label, lg) = label_gen.next();
            label_gen = lg;
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                Value::Label(top_label),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                slot_num.clone(),
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(top_label), *loc));
            let (lg, nl, _, hm) = mavm_codegen_statements(
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
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Jump),
                *loc,
            ));
            Ok((label_gen, num_locals, true, hm))
        }
        TypeCheckedStatement::While(cond, body, loc) => {
            let slot_num = Value::Int(Uint256::from_usize(num_locals));
            num_locals += 1;
            let (top_label, lg) = label_gen.next();
            let (cond_label, lg) = lg.next();
            label_gen = lg;
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                Value::Label(top_label),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                slot_num.clone(),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Jump),
                Value::Label(cond_label),
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(top_label), *loc));
            let (lg, nl, _, _) = mavm_codegen_statements(
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
            let (lg, c, cond_locals) = mavm_codegen_expr(
                cond,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode_imm(
                Opcode::GetLocal,
                slot_num,
                *loc,
            ));
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Cjump),
                *loc,
            ));
            Ok((
                label_gen,
                max(cond_locals, num_locals),
                false,
                HashMap::new(),
            ))
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
            Ok((lg, nl1, !might_continue, HashMap::new()))
        }
        TypeCheckedStatement::Asm(insns, args, _loc) => {
            let n_args = args.len();
            let mut exp_locals = 0;
            for i in 0..n_args {
                let (lg, c, e_locals) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    i,
                )?;
                exp_locals = max(exp_locals, e_locals);
                label_gen = lg;
                code = c;
            }
            for insn in insns {
                code.push(insn.clone());
            }
            Ok((label_gen, exp_locals, false, HashMap::new()))
        }
        TypeCheckedStatement::DebugPrint(e, loc) => {
            let (lg, c, exp_locals) = mavm_codegen_expr(
                e,
                code,
                num_locals,
                &locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::DebugPrint),
                *loc,
            ));
            Ok((label_gen, exp_locals, false, HashMap::new()))
        }
        TypeCheckedStatement::IfLet(name, expr, block, else_block, loc) => {
            let (after_label, lgg) = label_gen.next();
            let slot_num = num_locals;
            let new_locals = locals.push_one(*name, slot_num);
            let (lg, c, exp_locals) = mavm_codegen_expr(
                expr,
                code,
                num_locals,
                &locals,
                lgg,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Dup0),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Tget),
                Value::Int(Uint256::from_usize(0)),
                *loc,
            ));
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::IsZero),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Cjump),
                Value::Label(after_label),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Tget),
                Value::Int(Uint256::from_usize(1)),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::SetLocal,
                Value::Int(Uint256::from_usize(slot_num)),
                *loc,
            ));
            let (lg, mut total_locals, mut can_continue, _) = mavm_codegen_statements(
                block.clone(),
                code,
                num_locals + 1,
                &new_locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            total_locals = max(total_locals, exp_locals);

            let (outside_label, lg2) = lg.next();
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Jump),
                Value::Label(outside_label),
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(after_label), *loc));
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Pop),
                *loc,
            ));
            if let Some(else_block) = else_block {
                let (lg3, else_locals, else_can_continue, _) = mavm_codegen_statements(
                    else_block.clone(),
                    code,
                    num_locals,
                    &locals,
                    lg2,
                    string_table,
                    import_func_map,
                    global_var_map,
                )?;
                can_continue |= else_can_continue;
                total_locals = max(total_locals, else_locals);
                label_gen = lg3;
            } else {
                can_continue = true;
                label_gen = lg2;
            };
            code.push(Instruction::from_opcode(Opcode::Label(outside_label), *loc));
            Ok((label_gen, total_locals, !can_continue, HashMap::new()))
        }
    }
}

///Generates code for assigning the contents of a tuple on the top of the stack to a sequential set
/// of locals.  code represents previously generated code, pattern is a slice of match patterns
/// corresponding to the structure of the tuple, local_slot_num_base is the slot of the first local
/// being assigned to, and loc is the location the operation originates from in the source code.
///
/// Nothing is returned directly, and the generated code can be accessed through the code reference.
fn mavm_codegen_tuple_pattern(
    code: &mut Vec<Instruction>,
    pattern: &[TypeCheckedMatchPattern],
    local_slot_num_base: usize,
    loc: Option<Location>,
) {
    let pat_size = pattern.len();
    for (i, pat) in pattern.iter().enumerate() {
        if i < pat_size - 1 {
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Dup0),
                loc,
            ));
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

///Generates code for if arm specified by arm, end_label is the label jumped to when exiting the if
/// else chain, code is the previously generated code, num_locals is the maximum number of locals
/// used at any point previously in this call frame, locals is a table of names to variable slot
/// numbers, label_gen points to the next available local slot, string_table is used to look up
/// builtins, import_func_map maps IDs to the associated label, and global_var_map maps IDs to
/// global variables.
fn mavm_codegen_if_arm(
    arm: &TypeCheckedIfArm,
    end_label: Label,
    mut code: &mut Vec<Instruction>, // accumulates the code as it's generated
    num_locals: usize,               // num locals that have been allocated
    locals: &CopyingSymTable<usize>, // lookup local variable slot number by name
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
    // (label_gen, num_labels, execution_might_continue)
    match arm {
        TypeCheckedIfArm::Cond(cond, body, orest, loc) => {
            let (after_label, lg) = label_gen.next();
            let (lg, c, cond_locals) = mavm_codegen_expr(
                cond,
                code,
                num_locals,
                &locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
                0,
            )?;
            label_gen = lg;
            code = c;
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::IsZero),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Cjump),
                Value::Label(after_label),
                *loc,
            ));
            let (lg, nl1, might_continue_here, _) = mavm_codegen_statements(
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
                    Opcode::AVMOpcode(AVMOpcode::Jump),
                    Value::Label(end_label),
                    *loc,
                ));
            }
            code.push(Instruction::from_opcode(Opcode::Label(after_label), *loc));
            let nl1 = max(nl1, cond_locals);
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
            let (lg, nl, might_continue, _) = mavm_codegen_statements(
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

///Generates code for the expression expr.
///
/// code represents the previously generated code, num_locals is the maximum number of locals used
/// at any previous point in the callframe, locals is the table of local variable names to slot
/// numbers, label_gen points to the first available locals slot, string_table is used to get
/// builtins, import_func_map maps stringIDs for imported function to their associated labels,
/// global_var_map maps stringIDs to their associated slot numbers, and prepushed_vals indicates the
/// number of items on the stack at the start of the call, this is needed for early returns.
///
/// If successful this function returns a tuple containing the updated label_gen, a mutable
/// reference to the generated code, and a usize containing the number of locals used by the
/// expression, otherwise it returns a CodegenError.
fn mavm_codegen_expr<'a>(
    expr: &TypeCheckedExpr,
    mut code: &'a mut Vec<Instruction>,
    num_locals: usize,
    locals: &CopyingSymTable<usize>,
    mut label_gen: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
    prepushed_vals: usize,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>, usize), CodegenError> {
    match expr {
        TypeCheckedExpr::UnaryOp(op, tce, _, loc) => {
            let (lg, c, exp_locals) = mavm_codegen_expr(
                tce,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            label_gen = lg;
            code = c;
            let (maybe_opcode, maybe_imm) = match op {
                UnaryOp::Minus => (Some(Opcode::UnaryMinus), None),
                UnaryOp::BitwiseNeg => (Some(Opcode::AVMOpcode(AVMOpcode::BitwiseNeg)), None),
                UnaryOp::Not => (Some(Opcode::AVMOpcode(AVMOpcode::IsZero)), None),
                UnaryOp::Hash => (Some(Opcode::AVMOpcode(AVMOpcode::Hash)), None),
                UnaryOp::ToUint => (None, None),
                UnaryOp::ToInt => (None, None),
                UnaryOp::ToBytes32 => (None, None),
                UnaryOp::ToAddress => {
                    let mask = Uint256::from_usize(2)
                        .exp(&Uint256::from_usize(160))
                        .sub(&Uint256::one())
                        .ok_or_else(|| new_codegen_error("Underflow on substraction", *loc))?;
                    (
                        Some(Opcode::AVMOpcode(AVMOpcode::BitwiseAnd)),
                        Some(Value::Int(mask)),
                    )
                }
                UnaryOp::Len => (Some(Opcode::TupleGet(3)), Some(Value::Int(Uint256::zero()))),
            };
            if let Some(opcode) = maybe_opcode {
                code.push(Instruction::new(opcode, maybe_imm, *loc));
            }
            Ok((label_gen, code, max(num_locals, exp_locals)))
        }
        TypeCheckedExpr::Variant(inner, loc) => {
            let (lg, c, exp_locals) = mavm_codegen_expr(
                inner,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            c.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                Value::new_tuple(vec![Value::Int(Uint256::from_usize(1)), Value::none()]),
                *loc,
            ));
            c.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Tset),
                Value::Int(Uint256::from_u64(1)),
                *loc,
            ));
            Ok((lg, c, max(num_locals, exp_locals)))
        }
        TypeCheckedExpr::Binary(op, tce1, tce2, _, loc) => {
            let (lg, c, left_locals) = mavm_codegen_expr(
                tce2,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            let (lg, c, right_locals) = mavm_codegen_expr(
                tce1,
                c,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals + 1,
            )?;
            label_gen = lg;
            code = c;
            let opcode = match op {
                BinaryOp::Plus => Opcode::AVMOpcode(AVMOpcode::Plus),
                BinaryOp::Minus => Opcode::AVMOpcode(AVMOpcode::Minus),
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
                BinaryOp::BitwiseXor => Opcode::AVMOpcode(AVMOpcode::BitwiseXor),
                BinaryOp::_LogicalAnd => Opcode::LogicalAnd,
                BinaryOp::LogicalOr => Opcode::LogicalOr,
                BinaryOp::Hash => Opcode::AVMOpcode(AVMOpcode::Hash2),
            };
            code.push(Instruction::from_opcode(opcode, *loc));
            match op {
                BinaryOp::NotEqual
                | BinaryOp::LessEq
                | BinaryOp::GreaterEq
                | BinaryOp::SLessEq
                | BinaryOp::SGreaterEq => {
                    code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::IsZero),
                        *loc,
                    ));
                }
                _ => {}
            }
            Ok((
                label_gen,
                code,
                max(num_locals, max(left_locals, right_locals)),
            ))
        }
        TypeCheckedExpr::ShortcutOr(tce1, tce2, loc) => {
            let (lg, c, left_locals) = mavm_codegen_expr(
                tce1,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            let (lab, lg) = lg.next();
            c.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Dup0),
                *loc,
            ));
            c.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Cjump),
                Value::Label(lab),
                *loc,
            ));
            c.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Pop),
                *loc,
            ));
            let (lg, c, right_locals) = mavm_codegen_expr(
                tce2,
                c,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            c.push(Instruction::from_opcode(Opcode::Label(lab), *loc));
            Ok((lg, c, max(num_locals, max(left_locals, right_locals))))
        }
        TypeCheckedExpr::ShortcutAnd(tce1, tce2, loc) => {
            let (lg, c, left_locals) = mavm_codegen_expr(
                tce1,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            let (lab, lg) = lg.next();
            c.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Dup0),
                *loc,
            ));
            c.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::IsZero),
                *loc,
            ));
            c.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Cjump),
                Value::Label(lab),
                *loc,
            ));
            c.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Pop),
                *loc,
            ));
            let (lg, c, right_locals) = mavm_codegen_expr(
                tce2,
                c,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            c.push(Instruction::from_opcode(Opcode::Label(lab), *loc));
            Ok((lg, c, max(num_locals, max(left_locals, right_locals))))
        }
        TypeCheckedExpr::LocalVariableRef(name, _, loc) => match locals.get(*name) {
            Some(n) => {
                code.push(Instruction::from_opcode_imm(
                    Opcode::GetLocal,
                    Value::Int(Uint256::from_usize(n)),
                    *loc,
                ));
                Ok((label_gen, code, num_locals))
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
            Ok((label_gen, code, num_locals))
        }
        TypeCheckedExpr::FuncRef(name, _, loc) => {
            let the_label = match import_func_map.get(name) {
                Some(label) => *label,
                None => Label::Func(*name),
            };
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                Value::Label(the_label),
                *loc,
            ));
            Ok((label_gen, code, num_locals))
        }
        TypeCheckedExpr::TupleRef(tce, idx, _, loc) => {
            let tce_type = tce.get_type();
            let tuple_size = if let Type::Tuple(fields) = tce_type {
                fields.len()
            } else {
                panic!("type-checking bug: tuple lookup in non-tuple type");
            };
            let (lg, c, exp_locals) = mavm_codegen_expr(
                tce,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            c.push(Instruction::from_opcode_imm(
                Opcode::TupleGet(tuple_size),
                Value::Int(idx.clone()),
                *loc,
            ));
            Ok((lg, c, max(num_locals, exp_locals)))
        }
        TypeCheckedExpr::DotRef(tce, name, _, loc) => {
            let tce_type = tce.get_type();
            let struct_size = match tce_type.clone() {
                Type::Struct(fields) => fields.len(),
                _ => panic!("type-checking bug: struct lookup in non-struct type"),
            };
            let (lg, c, exp_locals) = mavm_codegen_expr(
                tce,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
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
                    Ok((label_gen, code, max(num_locals, exp_locals)))
                }
                None => Err(new_codegen_error(
                    "tried to get non-existent struct field",
                    *loc,
                )),
            }
        }
        TypeCheckedExpr::Const(val, _, loc) => {
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                val.clone(),
                *loc,
            ));
            Ok((label_gen, code, num_locals))
        }
        TypeCheckedExpr::FunctionCall(fexpr, args, _, _, loc) => {
            let n_args = args.len();
            let (ret_label, lg) = label_gen.next();
            label_gen = lg;
            let mut args_locals = 0;
            for i in 0..n_args {
                let (lg, c, arg_locals) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    prepushed_vals + i,
                )?;
                args_locals = max(args_locals, arg_locals);
                label_gen = lg;
                code = c;
            }
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                Value::Label(ret_label),
                *loc,
            ));
            let (lg, c, fexpr_locals) = mavm_codegen_expr(
                fexpr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals + n_args + 1,
            )?;
            c.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Jump),
                *loc,
            ));
            c.push(Instruction::from_opcode(Opcode::Label(ret_label), *loc));
            Ok((lg, c, max(num_locals, max(fexpr_locals, args_locals))))
        }
        TypeCheckedExpr::CodeBlock(body, ret_expr, loc) => {
            let (lab_gen, nl, _cont, block_locals) = mavm_codegen_statements(
                body.to_vec(),
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
            )?;
            if let Some(ret_expr) = ret_expr {
                let new_locals = locals.push_multi(block_locals);
                mavm_codegen_expr(
                    ret_expr,
                    code,
                    num_locals,
                    &new_locals,
                    lab_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    prepushed_vals,
                )
                .map(|(lg, code, exp_locals)| (lg, code, max(num_locals, max(exp_locals, nl))))
            } else {
                code.push(Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::Noop),
                    Value::new_tuple(vec![]),
                    *loc,
                ));
                Ok((lab_gen, code, max(num_locals, nl)))
            }
        }
        TypeCheckedExpr::StructInitializer(fields, _, loc) => {
            let fields_len = fields.len();
            let mut struct_locals = 0;
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let (lg, c, field_locals) = mavm_codegen_expr(
                    &field.value,
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    prepushed_vals + i,
                )?;
                struct_locals = max(struct_locals, field_locals);
                label_gen = lg;
                code = c;
            }
            let empty_vec = TupleTree::new(fields_len, false).make_empty();
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                empty_vec,
                *loc,
            ));
            for i in 0..fields_len {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(fields_len),
                    Value::Int(Uint256::from_usize(i)),
                    *loc,
                ));
            }
            Ok((label_gen, code, max(num_locals, struct_locals)))
        }
        TypeCheckedExpr::Tuple(fields, _, loc) => {
            let fields_len = fields.len();
            let mut tuple_locals = 0;
            for i in 0..fields_len {
                let field = &fields[fields_len - 1 - i];
                let (lg, c, field_locals) = mavm_codegen_expr(
                    &field,
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    prepushed_vals + i,
                )?;
                tuple_locals = max(field_locals, tuple_locals);
                label_gen = lg;
                code = c;
            }
            let empty_vec = vec![Value::none(); fields_len];
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Noop),
                Value::new_tuple(empty_vec),
                *loc,
            ));
            for i in 0..fields_len {
                code.push(Instruction::from_opcode_imm(
                    Opcode::TupleSet(fields_len),
                    Value::Int(Uint256::from_usize(i)),
                    *loc,
                ));
            }
            Ok((label_gen, code, max(num_locals, tuple_locals)))
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
                PropertiesList { pure: true },
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )
        }
        TypeCheckedExpr::FixedArrayRef(expr1, expr2, size, _, loc) => {
            let (lg, c, exp1_locals) = mavm_codegen_expr(
                expr1,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            let (lg, c, exp2_locals) = mavm_codegen_expr(
                expr2,
                c,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals + 1,
            )?;
            label_gen = lg;
            code = c;
            if *size != 8 {
                //TODO: also skip check if size is larger power of 8
                let (cont_label, lg) = label_gen.next();
                label_gen = lg;
                code.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Dup0),
                    *loc,
                ));
                code.push(Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::GreaterThan),
                    Value::Int(Uint256::from_usize(*size)),
                    *loc,
                ));
                code.push(Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::Cjump),
                    Value::Label(cont_label),
                    *loc,
                ));
                code.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Panic),
                    *loc,
                ));
                code.push(Instruction::from_opcode(Opcode::Label(cont_label), *loc));
            }
            code.push(Instruction::from_opcode(
                Opcode::UncheckedFixedArrayGet(*size),
                *loc,
            ));
            Ok((
                label_gen,
                code,
                max(num_locals, max(exp1_locals, exp2_locals)),
            ))
        }
        TypeCheckedExpr::MapRef(map_expr, key_expr, _, loc) => {
            let call_type = Type::Func(
                false,
                vec![Type::Any, Type::Any],
                Box::new(Type::Option(Box::new(Type::Any))),
            );
            let the_expr = TypeCheckedExpr::FunctionCall(
                Box::new(TypeCheckedExpr::FuncRef(
                    *string_table.get_if_exists("builtin_kvsGet").unwrap(),
                    call_type.clone(),
                    *loc,
                )),
                vec![*map_expr.clone(), *key_expr.clone()],
                call_type,
                PropertiesList { pure: true },
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
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
                PropertiesList { pure: true },
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )
        }
        TypeCheckedExpr::NewFixedArray(sz, bo_expr, _, loc) => {
            let mut expr_locals = 0;
            match bo_expr {
                Some(expr) => {
                    let (lg, c, some_locals) = mavm_codegen_expr(
                        expr,
                        code,
                        num_locals,
                        locals,
                        label_gen,
                        string_table,
                        import_func_map,
                        global_var_map,
                        prepushed_vals,
                    )?;
                    expr_locals = some_locals;
                    label_gen = lg;
                    code = c;
                    for _i in 0..7 {
                        code.push(Instruction::from_opcode(
                            Opcode::AVMOpcode(AVMOpcode::Dup0),
                            *loc,
                        ));
                    }
                    let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                    code.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Noop),
                        Value::new_tuple(empty_tuple),
                        *loc,
                    ));
                    for i in 0..8 {
                        code.push(Instruction::from_opcode_imm(
                            Opcode::AVMOpcode(AVMOpcode::Tset),
                            Value::Int(Uint256::from_usize(i)),
                            *loc,
                        ));
                    }
                }
                None => {
                    let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                    code.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Noop),
                        Value::new_tuple(empty_tuple),
                        *loc,
                    ));
                }
            }
            let mut tuple_size: usize = 8;
            while tuple_size < *sz {
                for _i in 0..7 {
                    code.push(Instruction::from_opcode(
                        Opcode::AVMOpcode(AVMOpcode::Dup0),
                        *loc,
                    ));
                }
                let empty_tuple = vec![Value::new_tuple(Vec::new()); 8];
                code.push(Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::Noop),
                    Value::new_tuple(empty_tuple),
                    *loc,
                ));
                for i in 0..8 {
                    code.push(Instruction::from_opcode_imm(
                        Opcode::AVMOpcode(AVMOpcode::Tset),
                        Value::Int(Uint256::from_usize(i)),
                        *loc,
                    ));
                }
                tuple_size *= 8;
            }
            Ok((label_gen, code, max(num_locals, expr_locals)))
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
                PropertiesList { pure: true },
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
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
                PropertiesList { pure: true },
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )
        }
        TypeCheckedExpr::FixedArrayMod(arr, index, val, size, _, loc) => codegen_fixed_array_mod(
            arr,
            index,
            val,
            *size,
            code,
            num_locals,
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
                PropertiesList { pure: true },
                *loc,
            );
            mavm_codegen_expr(
                &the_expr,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )
        }
        TypeCheckedExpr::StructMod(struc, index, val, t, loc) => {
            let (lg, c, val_locals) = mavm_codegen_expr(
                val,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            let (lg, c, struc_locals) = mavm_codegen_expr(
                struc,
                c,
                num_locals,
                locals,
                lg,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals + 1,
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
            Ok((
                label_gen,
                code,
                max(num_locals, max(val_locals, struc_locals)),
            ))
        }
        TypeCheckedExpr::Cast(expr, _, _) => mavm_codegen_expr(
            expr,
            code,
            num_locals,
            locals,
            label_gen,
            string_table,
            import_func_map,
            global_var_map,
            prepushed_vals,
        ),
        TypeCheckedExpr::Asm(_, insns, args, _) => {
            let n_args = args.len();
            let mut args_locals = 0;
            for i in 0..n_args {
                let (lg, c, arg_locals) = mavm_codegen_expr(
                    &args[n_args - 1 - i],
                    code,
                    num_locals,
                    locals,
                    label_gen,
                    string_table,
                    import_func_map,
                    global_var_map,
                    prepushed_vals + i,
                )?;
                args_locals = max(args_locals, arg_locals);
                label_gen = lg;
                code = c;
            }
            for insn in insns {
                code.push(insn.clone());
            }
            Ok((label_gen, code, max(num_locals, args_locals)))
        }
        TypeCheckedExpr::Try(exp, _, loc) => {
            let (label_gen, code, exp_locals) = mavm_codegen_expr(
                exp,
                code,
                num_locals,
                locals,
                label_gen,
                string_table,
                import_func_map,
                global_var_map,
                prepushed_vals,
            )?;
            let (extract, label_gen) = label_gen.next();
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::Dup0),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Tget),
                Value::Int(Uint256::zero()),
                *loc,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Cjump),
                Value::Label(extract),
                *loc,
            ));
            // We use the auxstack here to temporarily store the return value while we clear the temp values on the stack
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::AuxPush),
                *loc,
            ));
            for _ in 0..prepushed_vals {
                code.push(Instruction::from_opcode(
                    Opcode::AVMOpcode(AVMOpcode::Pop),
                    *loc,
                ));
            }
            code.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::AuxPop),
                *loc,
            ));
            code.push(Instruction::from_opcode(Opcode::Return, *loc));
            code.push(Instruction::from_opcode(Opcode::Label(extract), *loc));
            code.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::Tget),
                Value::Int(Uint256::one()),
                *loc,
            ));
            Ok((label_gen, code, max(num_locals, exp_locals)))
        }
    }
}

///Used to codegen the FixedArrayMod variant of TypeCheckedExpr.
fn codegen_fixed_array_mod<'a>(
    arr_expr: &TypeCheckedExpr,
    idx_expr: &TypeCheckedExpr,
    val_expr: &TypeCheckedExpr,
    size: usize,
    code_in: &'a mut Vec<Instruction>,
    num_locals: usize,
    locals: &CopyingSymTable<usize>,
    label_gen_in: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
    location: Option<Location>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>, usize), CodegenError> {
    let (label_gen, code, val_locals) = mavm_codegen_expr(
        val_expr,
        code_in,
        num_locals,
        locals,
        label_gen_in,
        string_table,
        import_func_map,
        global_var_map,
        0,
    )?;
    let (label_gen, code, arr_locals) = mavm_codegen_expr(
        arr_expr,
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
        1,
    )?;
    let (mut label_gen, code, idx_locals) = mavm_codegen_expr(
        idx_expr,
        code,
        num_locals,
        locals,
        label_gen,
        string_table,
        import_func_map,
        global_var_map,
        2,
    )?;
    if size != 8 {
        // TODO: safe for if-condition to say size does not equal any power of 8
        let (ok_label, lg) = label_gen.next();
        label_gen = lg;
        code.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Dup0),
            location,
        ));
        code.push(Instruction::from_opcode_imm(
            Opcode::AVMOpcode(AVMOpcode::GreaterThan),
            Value::Int(Uint256::from_usize(size)),
            location,
        ));
        code.push(Instruction::from_opcode_imm(
            Opcode::AVMOpcode(AVMOpcode::Cjump),
            Value::Label(ok_label),
            location,
        ));
        code.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Panic),
            location,
        ));
        code.push(Instruction::from_opcode(Opcode::Label(ok_label), location));
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
        import_func_map,
        global_var_map,
        location,
    )
    .map(|(lg, code, num_locals)| (lg, code, max(num_locals, exp_locals)))
}

///Used by codegen_fixed_array_mod, you should not call this directly.
fn codegen_fixed_array_mod_2<'a>(
    val_expr: &TypeCheckedExpr,
    size: usize,
    code_in: &'a mut Vec<Instruction>,
    num_locals: usize,
    locals: &CopyingSymTable<usize>,
    label_gen_in: LabelGenerator,
    string_table: &StringTable,
    import_func_map: &HashMap<StringId, Label>,
    global_var_map: &HashMap<StringId, usize>,
    location: Option<Location>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>, usize), CodegenError> {
    if size <= 8 {
        // stack: idx tuple val
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Tset),
            location,
        ));
        Ok((label_gen_in, code_in, num_locals))
    } else {
        let tuple_size = Value::Int(Uint256::from_usize(TUPLE_SIZE));
        // stack: idx tupletree val
        code_in.push(Instruction::from_opcode_imm(
            Opcode::AVMOpcode(AVMOpcode::Dup2),
            tuple_size.clone(),
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::AuxPush),
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Dup1),
            location,
        ));
        // stack: idx TUPLE_SIZE idx tupletree val; aux: tupletree
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Mod),
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Dup0),
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::AuxPush),
            location,
        ));
        // stack: slot idx tupletree val; aux: slot tupletree
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Swap1),
            location,
        ));
        code_in.push(Instruction::from_opcode_imm(
            Opcode::AVMOpcode(AVMOpcode::Swap1),
            tuple_size,
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Div),
            location,
        ));
        // stack: subidx slot tupletree val; aux: slot tupletree
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Swap2),
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Swap1),
            location,
        ));
        // stack: slot tupletree subidx val; aux: slot tupletree
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Tget),
            location,
        ));
        code_in.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Swap1),
            location,
        ));
        // stack: subidx subtupletree val; aux: slot tupletree

        let (label_gen, code, inner_locals) = codegen_fixed_array_mod_2(
            val_expr,
            (size + (TUPLE_SIZE - 1)) / TUPLE_SIZE,
            code_in,
            num_locals,
            locals,
            label_gen_in,
            string_table,
            import_func_map,
            global_var_map,
            location,
        )?;

        // stack: newsubtupletree; aux: slot tupletree
        code.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::AuxPop),
            location,
        ));
        code.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::AuxPop),
            location,
        ));
        code.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Swap1),
            location,
        ));
        // stack: slot tupletree newsubtupletree
        code.push(Instruction::from_opcode(
            Opcode::AVMOpcode(AVMOpcode::Tset),
            location,
        ));

        Ok((label_gen, code, max(num_locals, inner_locals)))
    }
}
