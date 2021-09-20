/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Contains utilities for generating instructions from AST structures.

use super::ast::{BinaryOp, DebugInfo, FuncProperties, GlobalVar, TrinaryOp, Type, UnaryOp};
use super::typecheck::{TypeCheckedExpr, TypeCheckedFunc, TypeCheckedNode, TypeCheckedStatement};
use crate::compile::typecheck::{
    AbstractSyntaxTree, TypeCheckedCodeBlock, TypeCheckedExprKind, TypeCheckedStatementKind,
};
use crate::compile::CompileError;
use crate::console::Color;
use crate::link::{TupleTree, TUPLE_SIZE};
use crate::mavm::{AVMOpcode, Buffer, Instruction, Label, LabelGenerator, Opcode, Value};
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::{HashMap, HashSet};

/// Represents a slot number in a locals tuple
type SlotNum = usize;

/// Represents the code generation process for a function.
struct Codegen<'a> {
    /// The code being generated.
    code: &'a mut Vec<Instruction>,
    /// A source of unique labels.
    label_gen: &'a mut LabelGenerator,
    /// Maps `StringId`s to parsed names
    string_table: &'a StringTable,
    /// Associates each imported function with a unique label
    func_labels: &'a HashMap<StringId, Label>,
    /// List of globals this func has access to.
    globals: &'a HashMap<StringId, GlobalVar>,
    /// Compiler issues discovered during codegen that needn't halt compilation.
    issues: &'a mut Vec<CompileError>,
    /// Whether to elide debug-only constructs like assert().
    release_build: bool,

    locals: HashMap<StringId, SlotNum>,
    aliases: HashMap<SlotNum, Vec<SlotNum>>,
    next_slot: SlotNum,
}

impl Codegen<'_> {
    fn next_slot(&mut self) -> SlotNum {
        let next = self.next_slot;
        self.next_slot += 1;
        next
    }
}

/// This generates code for individual mini functions.
///
/// Here func represents the function to be codegened, string_table is used to get builtins,
/// and globals lists the globals available to the func.
///
/// Each func gets a unique, hashed label id, after which local labels are assigned. This ensures
/// two labels are the same iff they point to the same destination.
pub fn mavm_codegen_func(
    mut func: TypeCheckedFunc, // only mutable because of child_nodes()
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

    for arg in func.args.clone() {
        let next_slot = locals.len();
        locals.insert(arg.name, next_slot);
    }
    for capture in func.captures.clone() {
        let next_slot = locals.len();
        locals.insert(capture, next_slot);
    }

    let mut label_gen = LabelGenerator::new(unique_id + 1);

    let mut cgen = Codegen {
        code: &mut code,
        label_gen: &mut label_gen,
        string_table,
        func_labels,
        globals,
        issues,
        release_build,
        locals: locals.clone(),
        aliases: HashMap::new(),
        next_slot: locals.len(),
    };

    //let _ = codegen_statements(func.code, &mut cgen, &locals, 0)?;

    codegen(func.child_nodes(), &mut cgen, 0, true)?;

    let mut space_for_locals = cgen.next_slot + 128;

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

fn codegen(
    nodes: Vec<TypeCheckedNode>,
    cgen: &mut Codegen,
    stack_items: usize,
    scope: bool,
) -> Result<(), CompileError> {
    // The *current* assignment for each local variable when entering the scope.
    let saved_locals = cgen.locals.clone();

    macro_rules! expr {
        ($expr:expr, $push:expr) => {
            codegen(
                vec![TypeCheckedNode::Expression($expr)],
                cgen,
                stack_items + $push,
                false,
            )?
        };
        ($expr:expr) => {
            expr!($expr, 0)
        };
    }

    for node in nodes {
        let debug = match &node {
            TypeCheckedNode::Statement(stat) => stat.debug_info,
            TypeCheckedNode::Expression(expr) => expr.debug_info,
            TypeCheckedNode::Type(tipe) => panic!("Found type node {}", tipe.display()),
        };

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

        macro_rules! clear_stack {
            () => {
                for _ in 0..stack_items {
                    cgen.code.push(opcode!(Pop));
                }
            };
        }

        macro_rules! error {
            ($text:expr $(,$args:expr)* $(,)?) => {
                return Err(CompileError::new("Internal error", format!($text, $(Color::red($args),)*), debug.locs()));
            };
            (@$text:expr, $debug:expr) => {
                return Err(CompileError::new("Internal error", format!($text), $debug.locs()));
            };
        }

        macro_rules! block {
            ($expr:expr) => {{
                codegen(
                    vec![TypeCheckedNode::Expression(&mut TypeCheckedExpr::new(
                        TypeCheckedExprKind::CodeBlock($expr.clone()),
                        debug,
                    ))],
                    cgen,
                    stack_items,
                    false,
                )?;
                // TODO: Alias
            }};
        }

        match node {
            TypeCheckedNode::Statement(stat) => {
                match &mut stat.kind {
                    TypeCheckedStatementKind::SetLocals(assigned, expr) => {
                        expr!(expr);
                        let count = assigned.len();
                        for _ in 0..(count - 1) {
                            cgen.code.push(opcode!(Dup0));
                        }

                        for (index, local) in assigned.into_iter().enumerate() {
                            let slot = if local.shadow {
                                cgen.next_slot()
                            } else {
                                match cgen.locals.get(&local.id) {
                                    Some(slot) => *slot,
                                    None => {
                                        error!(@"No slot has been assigned", local.debug_info)
                                    }
                                }
                            };
                            cgen.locals.insert(local.id, slot);

                            if count > 1 {
                                cgen.code.push(Instruction::from_opcode(
                                    Opcode::TupleGet(index, count),
                                    debug,
                                ));
                            }

                            cgen.code.push(opcode!(@SetLocal(slot)));
                        }
                    }
                    TypeCheckedStatementKind::AssignGlobal(id, expr) => {
                        expr!(expr);
                        let global = cgen.globals.get(id).expect("No global exists for stringID");
                        let offset = global.offset.unwrap();
                        cgen.code.push(opcode!(@SetGlobalVar(offset)));
                    }
                    TypeCheckedStatementKind::ReturnVoid() => {
                        clear_stack!();
                        cgen.code.push(opcode!(@Return));
                    }
                    TypeCheckedStatementKind::Return(expr) => {
                        clear_stack!();
                        expr!(expr);
                        cgen.code.push(opcode!(@Return));
                    }
                    TypeCheckedStatementKind::Expression(expr) => {
                        // TODO: Add typechecking of dropped expressions
                        expr!(expr);
                        if !matches!(expr.get_type(), Type::Void | Type::Every) {
                            cgen.code.push(opcode!(Pop));
                        }
                    }
                    TypeCheckedStatementKind::Asm(payload, args) => {
                        let nargs = args.len();
                        for i in 0..nargs {
                            expr!(&mut args[nargs - 1 - i], i);
                        }
                        for insn in payload {
                            cgen.code.push(insn.clone());
                        }
                    }
                    TypeCheckedStatementKind::DebugPrint(expr) => {
                        expr!(expr);
                        cgen.code.push(opcode!(DebugPrint));
                    }
                    TypeCheckedStatementKind::Assert(expr) => {
                        if cgen.release_build {
                            // Release builds don't include asserts
                            continue;
                        }
                    }
                    TypeCheckedStatementKind::While(cond, body) => {
                        let slot_num = cgen.next_slot();
                        let top_label = cgen.label_gen.next();
                        let cond_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(Noop, Value::Label(top_label)));
                        cgen.code.push(opcode!(@SetLocal(slot_num)));
                        cgen.code.push(opcode!(Jump, Value::Label(cond_label)));
                        cgen.code.push(opcode!(@Label(top_label)));
                        block!(body);
                        cgen.code.push(opcode!(@Label(cond_label)));
                        expr!(cond);
                        cgen.code.push(opcode!(@GetLocal(slot_num)));
                        cgen.code.push(opcode!(Cjump));
                    }
                    TypeCheckedStatementKind::Break(..) => {
                        panic!("Encountered a break node");
                    }
                }
            }
            TypeCheckedNode::Expression(expr) => {
                match &mut expr.kind {
                    TypeCheckedExprKind::CodeBlock(block) => {
                        codegen(block.child_nodes(), cgen, stack_items, true)?;
                    }
                    TypeCheckedExprKind::If(cond, block, else_block, _) => {
                        expr!(cond);
                        let end_label = cgen.label_gen.next();
                        let else_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(IsZero));
                        cgen.code.push(opcode!(Cjump, Value::Label(else_label)));
                        block!(block);
                        // TODO: alias

                        cgen.code.push(opcode!(@Label(else_label)));
                        if let Some(else_block) = else_block {
                            block!(else_block);
                        }
                        // TODO: alias
                        cgen.code.push(opcode!(@Label(end_label)));
                    }
                    TypeCheckedExprKind::IfLet(id, right, block, else_block, _) => {
                        expr!(right);
                        let end_label = cgen.label_gen.next();
                        let else_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(Tget, Value::from(0)));
                        cgen.code.push(opcode!(IsZero));
                        cgen.code.push(opcode!(Cjump, Value::Label(else_label)));

                        // Some(_) case
                        let slot = cgen.next_slot();
                        cgen.locals.insert(*id, slot);
                        cgen.code.push(opcode!(Tget, Value::from(1)));
                        cgen.code.push(opcode!(@SetLocal(slot)));
                        block!(block);
                        cgen.code.push(opcode!(Jump, Value::Label(end_label)));

                        // None case
                        cgen.code.push(opcode!(@Label(else_label)));
                        cgen.code.push(opcode!(Pop));
                        if let Some(else_block) = else_block {
                            block!(else_block);
                        }
                        cgen.code.push(opcode!(@Label(end_label)));
                    }
                    TypeCheckedExprKind::Loop(body, _) => {
                        let slot = cgen.next_slot();
                        let top = cgen.label_gen.next();
                        cgen.code.push(opcode!(Noop, Value::Label(top)));
                        cgen.code.push(opcode!(@SetLocal(slot)));
                        cgen.code.push(opcode!(@Label(top)));
                        block!(body);
                        cgen.code.push(opcode!(@GetLocal(slot)));
                        cgen.code.push(opcode!(Jump));
                    }
                    TypeCheckedExprKind::Cast(expr, _) => expr!(expr),
                    TypeCheckedExprKind::Error => cgen.code.push(opcode!(Error)),
                    TypeCheckedExprKind::NewBuffer => cgen.code.push(opcode!(NewBuffer)),
                    TypeCheckedExprKind::GetGas => cgen.code.push(opcode!(PushGas)),
                    TypeCheckedExprKind::SetGas(amount) => {
                        expr!(amount);
                        cgen.code.push(opcode!(SetGas));
                    }
                    TypeCheckedExprKind::Const(val, _) => {
                        cgen.code.push(opcode!(Noop, val.clone()))
                    }
                    TypeCheckedExprKind::Quote(bytes) => {
                        cgen.code.push(opcode!(
                            Noop,
                            Value::new_tuple(vec![
                                Value::from(bytes.len()),
                                Value::Buffer(Buffer::from_bytes(bytes.clone())),
                            ])
                        ));
                    }
                    TypeCheckedExprKind::LocalVariableRef(id, _) => match cgen.locals.get(id) {
                        Some(slot) => {
                            cgen.code.push(opcode!(@GetLocal(*slot)));
                        }
                        None => {
                            error!(
                                "Variable {} doesn't have an assigned slot",
                                cgen.string_table.name_from_id(*id)
                            )
                        }
                    },
                    TypeCheckedExprKind::FuncRef(id, _) => {
                        let func_label = match cgen.func_labels.get(id) {
                            Some(label) => *label,
                            None => {
                                error!("No label for func ref {}", id)
                            }
                        };
                        cgen.code.push(opcode!(Noop, Value::Label(func_label)));
                    }
                    TypeCheckedExprKind::GlobalVariableRef(id, _) => {
                        let offset = match cgen.globals.get(id) {
                            Some(global) => global.offset.unwrap(),
                            None => {
                                let globals = format!("{:?}", cgen.globals);
                                error!("StringId {} doesn't exist in {}", id, globals);
                            }
                        };
                        cgen.code.push(opcode!(@GetGlobalVar(offset)));
                    }
                    TypeCheckedExprKind::FunctionCall(fexpr, args, _, prop) => {
                        let nargs = args.len();
                        for i in 0..nargs {
                            expr!(&mut args[nargs - 1 - i], i);
                        }
                        expr!(fexpr, nargs + 1);
                        cgen.code.push(opcode!(@FuncCall(*prop)));
                    }
                    TypeCheckedExprKind::Tuple(fields, _) => {
                        let nfields = fields.len();
                        for i in 0..nfields {
                            expr!(&mut fields[nfields - 1 - i], i);
                        }
                        let container = vec![Value::none(); nfields];
                        cgen.code.push(opcode!(Noop, Value::new_tuple(container)));
                        for i in 0..nfields {
                            cgen.code.push(opcode!(@TupleSet(i, nfields)));
                        }
                    }
                    TypeCheckedExprKind::TupleRef(expr, offset, width, _) => {
                        expr!(expr);
                        cgen.code.push(opcode!(@TupleGet(*offset, *width)));
                    }
                    TypeCheckedExprKind::ArrayRef(expr1, expr2, t) => {}
                    TypeCheckedExprKind::NewFixedArray(size, fill, _) => {
                        expr!(fill);
                        for _ in 0..7 {
                            cgen.code.push(opcode!(Dup0));
                        }
                        let container = vec![Value::new_tuple(Vec::new()); 8];
                        cgen.code.push(opcode!(Noop, Value::new_tuple(container)));
                        for i in 0..8 {
                            cgen.code.push(opcode!(Tset, Value::from(i)));
                        }

                        let mut tuple_size: usize = 8;
                        while tuple_size < *size {
                            for _ in 0..7 {
                                cgen.code.push(opcode!(Dup0));
                            }
                            let container = vec![Value::new_tuple(Vec::new()); 8];
                            cgen.code.push(opcode!(Noop, Value::new_tuple(container)));
                            for i in 0..8 {
                                cgen.code.push(opcode!(Tset, Value::from(i)));
                            }
                            tuple_size *= 8;
                        }
                    }
                    TypeCheckedExprKind::FixedArrayRef(expr1, expr2, size, _) => {
                        expr!(expr1, 0);
                        expr!(expr2, 1);
                        if *size != 8 {
                            //TODO: also skip check if size is larger power of 8
                            let cont_label = cgen.label_gen.next();
                            cgen.code.push(opcode!(Dup0));
                            cgen.code.push(opcode!(GreaterThan, Value::from(*size)));
                            cgen.code.push(opcode!(Cjump, Value::Label(cont_label)));
                            cgen.code.push(opcode!(Error));
                            cgen.code.push(opcode!(@Label(cont_label)));
                        }
                        cgen.code.push(opcode!(@UncheckedFixedArrayGet(*size)));
                    }
                    TypeCheckedExprKind::FixedArrayMod(arr, key, val, size, _) => {
                        expr!(val, 0);
                        expr!(arr, 1);
                        expr!(key, 2);

                        if *size != 8 {
                            // TODO: safe for if-condition to say size does not equal any power of 8
                            let ok_label = cgen.label_gen.next();
                            cgen.code.push(opcode!(Dup0));
                            cgen.code.push(opcode!(GreaterThan, Value::from(*size)));
                            cgen.code.push(opcode!(Cjump, Value::Label(ok_label)));
                            cgen.code.push(opcode!(Error));
                            cgen.code.push(opcode!(@Label(ok_label)));
                        }

                        fn fixed_array_step(cgen: &mut Codegen, size: usize, debug: DebugInfo) {
                            macro_rules! opcode {
                                ($opcode:ident) => {
                                    Instruction::from_opcode(
                                        Opcode::AVMOpcode(AVMOpcode::$opcode),
                                        debug,
                                    )
                                };
                                ($opcode:ident, $immediate:expr) => {
                                    Instruction::from_opcode_imm(
                                        Opcode::AVMOpcode(AVMOpcode::$opcode),
                                        $immediate,
                                        debug,
                                    )
                                };
                            }

                            if size <= 8 {
                                // stack: idx tuple val
                                cgen.code.push(opcode!(Tset));
                                return;
                            }

                            // stack: idx tupletree val
                            cgen.code.push(opcode!(Dup2, Value::from(TUPLE_SIZE)));
                            cgen.code.push(opcode!(AuxPush));
                            cgen.code.push(opcode!(Dup1));

                            // stack: idx TUPLE_SIZE idx tupletree val; aux: tupletree
                            cgen.code.push(opcode!(Mod));
                            cgen.code.push(opcode!(Dup0));
                            cgen.code.push(opcode!(AuxPush));

                            // stack: slot idx tupletree val; aux: slot tupletree
                            cgen.code.push(opcode!(Swap1));
                            cgen.code.push(opcode!(Swap1, Value::from(TUPLE_SIZE)));
                            cgen.code.push(opcode!(Div));

                            // stack: subidx slot tupletree val; aux: slot tupletree
                            cgen.code.push(opcode!(Swap2));
                            cgen.code.push(opcode!(Swap1));

                            // stack: slot tupletree subidx val; aux: slot tupletree
                            cgen.code.push(opcode!(Tget));
                            cgen.code.push(opcode!(Swap1));

                            fixed_array_step(cgen, (size + (TUPLE_SIZE - 1)) / TUPLE_SIZE, debug);

                            // stack: newsubtupletree; aux: slot tupletree
                            cgen.code.push(opcode!(AuxPop));
                            cgen.code.push(opcode!(AuxPop));
                            cgen.code.push(opcode!(Swap1));

                            // stack: slot tupletree newsubtupletree
                            cgen.code.push(opcode!(Tset));
                        }

                        fixed_array_step(cgen, *size, debug);
                    }
                    TypeCheckedExprKind::NewMap(t) => {}
                    TypeCheckedExprKind::MapRef(map_expr, key_expr, t) => {}
                    TypeCheckedExprKind::StructMod(structure, slot, size, item, _) => {
                        expr!(item, 0);
                        expr!(structure, 1);
                        cgen.code.push(opcode!(@TupleSet(*slot, *size)));
                    }
                    TypeCheckedExprKind::UnaryOp(op, expr1, _) => {
                        expr!(expr1);
                        cgen.code.push(match op {
                            UnaryOp::BitwiseNeg => opcode!(BitwiseNeg),
                            UnaryOp::Not => opcode!(IsZero),
                            UnaryOp::Hash => opcode!(Hash),
                            UnaryOp::Len => opcode!(Tlen),
                            UnaryOp::ToAddress => {
                                let mask = Uint256::from_usize(2)
                                    .exp(&Uint256::from_usize(160))
                                    .sub(&Uint256::one())
                                    .expect("mask is incorrect");
                                opcode!(BitwiseAnd, Value::Int(mask))
                            }
                            UnaryOp::Minus => {
                                opcode!(Sub, Value::from(0))
                            }
                            UnaryOp::ToUint | UnaryOp::ToInt | UnaryOp::ToBytes32 => opcode!(Noop),
                        });
                    }
                    TypeCheckedExprKind::Binary(op, expr1, expr2, _) => {
                        expr!(expr2, 0);
                        expr!(expr1, 1);
                        let opcode = Opcode::AVMOpcode(match op {
                            BinaryOp::GetBuffer8 => AVMOpcode::GetBuffer8,
                            BinaryOp::GetBuffer64 => AVMOpcode::GetBuffer64,
                            BinaryOp::GetBuffer256 => AVMOpcode::GetBuffer256,
                            BinaryOp::Plus => AVMOpcode::Add,
                            BinaryOp::Minus => AVMOpcode::Sub,
                            BinaryOp::Times => AVMOpcode::Mul,
                            BinaryOp::Div => AVMOpcode::Div,
                            BinaryOp::Mod => AVMOpcode::Mod,
                            BinaryOp::Sdiv => AVMOpcode::Sdiv,
                            BinaryOp::Smod => AVMOpcode::Smod,
                            BinaryOp::BitwiseAnd => AVMOpcode::BitwiseAnd,
                            BinaryOp::BitwiseOr => AVMOpcode::BitwiseOr,
                            BinaryOp::ShiftLeft => AVMOpcode::ShiftLeft,
                            BinaryOp::ShiftRight => AVMOpcode::ShiftRight,
                            BinaryOp::BitwiseXor => AVMOpcode::BitwiseXor,
                            BinaryOp::Hash => AVMOpcode::EthHash2,
                            BinaryOp::Equal | BinaryOp::NotEqual => AVMOpcode::Equal,
                            BinaryOp::GreaterThan | BinaryOp::LessEq => AVMOpcode::GreaterThan,
                            BinaryOp::SGreaterThan | BinaryOp::SLessEq => AVMOpcode::SGreaterThan,
                            BinaryOp::LessThan | BinaryOp::GreaterEq => AVMOpcode::LessThan,
                            BinaryOp::SLessThan | BinaryOp::SGreaterEq => AVMOpcode::SLessThan,
                        });
                        cgen.code.push(Instruction::from_opcode(opcode, debug));
                        match op {
                            BinaryOp::NotEqual
                            | BinaryOp::LessEq
                            | BinaryOp::GreaterEq
                            | BinaryOp::SLessEq
                            | BinaryOp::SGreaterEq => {
                                // negate these to flip the comparisons
                                cgen.code.push(opcode!(IsZero));
                            }
                            _ => {}
                        }
                    }
                    TypeCheckedExprKind::Trinary(op, expr1, expr2, expr3, _) => {
                        expr!(expr3, 0);
                        expr!(expr2, 1);
                        expr!(expr1, 2);
                        let opcode = match op {
                            TrinaryOp::SetBuffer8 => Opcode::AVMOpcode(AVMOpcode::SetBuffer8),
                            TrinaryOp::SetBuffer64 => Opcode::AVMOpcode(AVMOpcode::SetBuffer64),
                            TrinaryOp::SetBuffer256 => Opcode::AVMOpcode(AVMOpcode::SetBuffer256),
                        };
                        cgen.code.push(Instruction::from_opcode(opcode, debug));
                    }
                    TypeCheckedExprKind::ShortcutOr(left, right) => {
                        expr!(left);
                        let short = cgen.label_gen.next();
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(Cjump, Value::Label(short)));
                        cgen.code.push(opcode!(Pop));
                        expr!(right);
                        cgen.code.push(opcode!(@Label(short)));
                    }
                    TypeCheckedExprKind::ShortcutAnd(left, right) => {
                        expr!(left);
                        let short = cgen.label_gen.next();
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(IsZero));
                        cgen.code.push(opcode!(Cjump, Value::Label(short)));
                        cgen.code.push(opcode!(Pop));
                        expr!(right);
                        cgen.code.push(opcode!(@Label(short)));
                    }
                    TypeCheckedExprKind::FixedArrayMod(..) => {}
                    TypeCheckedExprKind::Asm(_, payload, args) => {
                        let nargs = args.len();
                        for i in 0..nargs {
                            expr!(&mut args[nargs - 1 - i], i);
                        }
                        for insn in payload {
                            cgen.code.push(insn.clone());
                        }
                    }
                    TypeCheckedExprKind::Variant(inner) => {
                        expr!(inner);
                        let option = Value::new_tuple(vec![Value::from(1), Value::none()]);
                        cgen.code.push(opcode!(Noop, option));
                        cgen.code.push(opcode!(Tset, Value::from(1)));
                    }
                    TypeCheckedExprKind::Try(variant, _) => {
                        expr!(variant);
                        let success = cgen.label_gen.next();
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(Tget, Value::from(0)));
                        cgen.code.push(opcode!(Cjump, Value::Label(success)));
                        cgen.code.push(opcode!(@Return));
                        clear_stack!();
                        cgen.code.push(opcode!(@Label(success)));
                        cgen.code.push(opcode!(Tget, Value::from(1)));
                    }
                    TypeCheckedExprKind::ClosureLoad(id, nargs, local_space, captures, _) => {
                        unimplemented!("Needs to happen after register coloring");
                    }
                }
            }
            _ => {}
        }
    }

    if scope {
        for (id, slot) in saved_locals {
            if let Some(alias) = cgen.locals.get(&id) {
                cgen.aliases.entry(id).or_insert(vec![]).push(*alias);
                cgen.locals.insert(id, slot);
            }
        }
    }

    Ok(())
}

/// Generates code for the provided statements with index 0 generated first. code represents the
/// code generated previously, num_locals the maximum number of locals used at any point in the call
/// frame so far, locals is a map of local variables, label_gen points to the next available locals
/// slot, string_table is used to get builtins, func_labels associates each imported function
/// with a label, and globals maps global variable IDs to their slot number.
///
/// If successful the function returns a tuple containing the maximum number of locals used
/// so far by this call frame and a map of locals available at the end of the statement sequence.
fn codegen_statements(
    statements: Vec<TypeCheckedStatement>, // statements to codegen
    cgen: &mut Codegen,
    locals: &HashMap<usize, usize>, // lookup local variable slot number by name
    stack_items: usize,
) -> Result<HashMap<StringId, usize>, CompileError> {
    let mut bindings = HashMap::new();
    for statement in statements {
        let mut new_locals = locals.clone();
        new_locals.extend(bindings.clone());
        let statement_bindings = codegen_statement(statement, cgen, &new_locals, stack_items)?;
        for (id, bind) in statement_bindings {
            bindings.insert(id, bind);
        }
    }
    Ok(bindings)
}

/// Generates code for the provided statement. code represents the code generated previously,
/// num_locals the maximum number of locals used at any point in the call frame so far, locals is a
/// map of local variables, string_table is used to get builtins, func_labels associates each
/// imported function with a label, and globals maps global variable IDs to their slot number.
///
/// If successful the function returns the number of locals slots used by this statement
/// and a map of locals generated by this statement.
fn codegen_statement(
    statement: TypeCheckedStatement, // statement to codegen
    cgen: &mut Codegen,
    locals: &HashMap<usize, usize>, // lookup local variable slot number by name
    stack_items: usize,
) -> Result<HashMap<StringId, usize>, CompileError> {
    macro_rules! expr {
        ($expr:expr) => {
            codegen_expr(&$expr, cgen, &locals, stack_items)?
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
        TypeCheckedStatementKind::SetLocals(assigned, expr) => {
            expr!(expr);
            let mut bindings = HashMap::new();

            let count = assigned.len();

            for _ in 0..(count - 1) {
                cgen.code.push(opcode!(Dup0));
            }

            for (index, local) in assigned.into_iter().enumerate() {
                let slot = if local.shadow {
                    cgen.next_slot()
                } else {
                    match locals.get(&local.id) {
                        Some(slot) => *slot,
                        None => Err(CompileError::new(
                            "Internal Error",
                            "No slot has been assigned",
                            local.debug_info.locs(),
                        ))?,
                    }
                };

                bindings.insert(local.id, slot);

                if count > 1 {
                    cgen.code.push(Instruction::from_opcode(
                        Opcode::TupleGet(index, count),
                        debug,
                    ));
                }

                cgen.code
                    .push(Instruction::from_opcode(Opcode::SetLocal(slot), debug));
            }
            Ok(bindings)
        }
        TypeCheckedStatementKind::AssignGlobal(id, expr) => {
            expr!(expr);
            let global = cgen.globals.get(id).expect("No global exists for stringID");
            let offset = global.offset.unwrap();
            cgen.code.push(Instruction::from_opcode(
                Opcode::SetGlobalVar(offset),
                debug,
            ));
            Ok(HashMap::new())
        }
        TypeCheckedStatementKind::Assert(expr) => {
            if cgen.release_build {
                // Release builds don't include asserts
                return Ok(HashMap::new());
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

            expr!(assert_call);
            Ok(HashMap::new())
        }
        _ => panic!("removed"),
    }
}

fn codegen_code_block(
    block: &TypeCheckedCodeBlock,
    cgen: &mut Codegen,
    locals: &HashMap<usize, usize>,
    stack_items: usize,
) -> Result<(), CompileError> {
    let block_locals = codegen_statements(block.body.clone(), cgen, locals, stack_items)?;
    if let Some(ret_expr) = &block.ret_expr {
        let mut new_locals = locals.clone();
        new_locals.extend(block_locals);
        codegen_expr(ret_expr, cgen, &new_locals, stack_items)?;
    }
    Ok(())
}

/// Generates code for the expression expr.
///
/// code represents the previously generated code, num_locals is the maximum number of locals used
/// at any previous point in the callframe, locals is the table of local variable names to slot
/// numbers, string_table is used to get builtins, func_labels maps `stringId`s for imported func
/// to their associated labels, globals maps `stringID`s to their associated slot numbers,
/// and stack_items indicates the number of items on the stack at the start of the call,
/// which is needed for early returns.
///
/// If successful this function returns a tuple containing a mutable reference to the generated code,
/// and a usize containing the number of locals used by the expression.
fn codegen_expr(
    expr: &TypeCheckedExpr,
    cgen: &mut Codegen,
    locals: &HashMap<usize, usize>,
    stack_items: usize,
) -> Result<(), CompileError> {
    macro_rules! expr {
        ($expr:expr) => {
            expr!($expr, 0)
        };
        ($expr:expr, $prepushed:expr) => {
            codegen_expr($expr, cgen, locals, stack_items + $prepushed)
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
                        cgen.code.push(Instruction::from_opcode(
                            Opcode::TupleSet(index + nargs, *local_space),
                            debug,
                        ))
                    }
                }

                let container = Value::new_tuple(vec![label, Value::none()]);

                cgen.code.push(opcode!(Noop, container));
                cgen.code.push(opcode!(Tset, Value::from(1)));
            }

            Ok(())
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
            Ok(())
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
        TypeCheckedExprKind::MapRef(map_expr, key_expr, t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_kvsGet",
                vec![map_expr, key_expr],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
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
        /*        TypeCheckedExprKind::MapMod(map, key, val, t) => {
            expr!(&TypeCheckedExpr::builtin(
                "builtin_kvsSet",
                vec![map, key, val],
                t,
                string_table,
                DebugInfo::from(loc)
            ))
        }*/
        TypeCheckedExprKind::Asm(_, insns, args) => {
            let n_args = args.len();
            for i in 0..n_args {
                expr!(&args[n_args - 1 - i], i)?;
            }
            for insn in insns {
                cgen.code.push(insn.clone());
            }
            Ok(())
        }
        TypeCheckedExprKind::Loop(body, _) => {
            let slot_num = cgen.next_slot();
            let top_label = cgen.label_gen.next();
            cgen.code.push(opcode!(Noop, Value::Label(top_label)));
            cgen.code
                .push(Instruction::from_opcode(Opcode::SetLocal(slot_num), debug));
            cgen.code
                .push(Instruction::from_opcode(Opcode::Label(top_label), debug));
            //let _ = codegen_statements(body.to_vec(), cgen, locals, stack_items)?;
            cgen.code
                .push(Instruction::from_opcode(Opcode::GetLocal(slot_num), debug));
            cgen.code.push(opcode!(Jump));
            Ok(())
        }
        _ => panic!("removed"),
    }
}
