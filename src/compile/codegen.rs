/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Contains utilities for generating instructions from AST structures.

use super::ast::{BinaryOp, DebugInfo, GlobalVar, TrinaryOp, Type, UnaryOp};
use super::typecheck::{TypeCheckedFunc, TypeCheckedNode};
use crate::compile::typecheck::{
    AbstractSyntaxTree, TypeCheckedExprKind, TypeCheckedStatementKind,
};
use crate::compile::CompileError;
use crate::console::Color;
use crate::link::TUPLE_SIZE;
use crate::mavm::{AVMOpcode, Buffer, Instruction, Label, LabelGenerator, Opcode, Value};
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use std::collections::{BTreeMap, HashMap};

/// Represents a slot number in a locals tuple
pub type FrameSize = u32;
pub type SlotNum = u32;

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
    /// Whether to elide debug-only constructs like assert().
    release_build: bool,
    /// The open set of scopes
    scopes: Vec<Scope>,
    /// The next slot available for assignment
    next_assignable_slot: SlotNum,
}

/// Represents a mini scope and the values it has access to.
#[derive(Clone, Debug, Default)]
struct Scope {
    /// A variable's current slot
    locals: BTreeMap<StringId, SlotNum>,
    /// A variable's slot just before its first shadow
    shadows: HashMap<StringId, SlotNum>,
}

impl Codegen<'_> {
    /// Get the next available slot.
    fn next_slot(&mut self) -> SlotNum {
        let next = self.next_assignable_slot;
        self.next_assignable_slot += 1;
        next
    }

    /// Create a new scope, inheriting the locals of the one prior.
    fn open_scope(&mut self) {
        let mut scope = match self.scopes.last() {
            Some(scope) => scope.clone(),
            None => Scope::default(),
        };
        scope.shadows = HashMap::new();
        self.scopes.push(scope);
    }

    /// Create a new assignment for a local variable.
    fn set_local(&mut self, local: StringId, slot: SlotNum) {
        let last = self.scopes.last_mut().expect("no scope");
        last.locals.insert(local, slot);
    }

    /// Shadow a variable, saving the last unshadowed assignment for phi-ing if needed.
    fn shadow(&mut self, local: StringId, slot: SlotNum) {
        let last = self.scopes.last_mut().expect("no scope");

        // for the first time shadowing, we save the old value if it exists
        if let Some(old) = last.locals.get(&local) {
            let old = *old;
            if !last.shadows.contains_key(&local) {
                last.shadows.insert(local, old);
            }
        }
        last.locals.insert(local, slot);
    }

    /// Get the currently accessible slot assignment for a variable in scope.
    fn get_local(&mut self, local: &StringId) -> Option<SlotNum> {
        let last = self.scopes.last_mut().expect("no scope");
        last.locals.get(local).cloned()
    }

    /// Debug print the open scope's current assignments.
    fn _print_locals(&self, title: &str, depth: usize) {
        let len = self.scopes.len();
        let scope = &self.scopes[len - 1];

        let spacing = " ".repeat(8 * depth);

        println!("{}{}", spacing, Color::grey(title));
        println!("{}{}", spacing, Color::grey("  locals"));
        for (local, slot) in &scope.locals {
            println!(
                "{}    {} {} {}",
                spacing,
                Color::grey(local),
                self.string_table.name_from_id(*local),
                slot
            );
        }
        if scope.shadows.len() > 0 {
            println!("{}{}", spacing, Color::grey("  shadows"));
            for (local, slot) in &scope.shadows {
                println!(
                    "{}    {} {} {}",
                    spacing,
                    Color::grey(local),
                    self.string_table.name_from_id(*local),
                    slot
                );
            }
        }
        println!();
    }
}

/// This generates code for individual mini functions.
///
/// Here func represents the function to be codegened, string_table is used to get builtins,
/// and globals lists the globals available to the func.
///
/// Each func gets a unique, hashed label id, after which local labels are assigned. This ensures
/// two labels are the same iff they point to the same destination. func_labels maps local `StringId`s
/// to these globally consistent labels.
pub fn mavm_codegen_func(
    mut func: TypeCheckedFunc, // only mutable because of child_nodes()
    string_table: &StringTable,
    globals: &HashMap<StringId, GlobalVar>,
    func_labels: &HashMap<StringId, Label>,
    release_build: bool,
) -> Result<(Vec<Instruction>, LabelGenerator, u32), CompileError> {
    let mut code = vec![];
    let debug = func.debug_info;

    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
        };
        (@$($opcode:tt)+) => {
            Instruction::from_opcode(Opcode::$($opcode)+, debug)
        };
    }

    let unique_id = match func.unique_id {
        Some(id) => id,
        None => {
            return Err(CompileError::new_codegen_error(
                format!("Func {} has no id", Color::red(&func.name)),
                debug.location,
            ))
        }
    };

    let label = match func.properties.closure {
        true => Label::Closure(unique_id),
        false => Label::Func(unique_id),
    };

    code.push(opcode!(@Label(label)));

    // Codegen the initial function frame

    let prop = match &func.tipe {
        Type::Func(prop, ..) => prop,
        _ => panic!("not a func"),
    };

    let prebuilt = !func.captures.is_empty();
    let returns = prop.returns;
    let make_frame_offset = code.len();
    code.push(opcode!(@MakeFrame(0, returns, prebuilt)));

    let nargs = func.args.len();
    for i in 0..nargs {
        code.push(opcode!(@SetLocal(i as SlotNum)));
    }
    for (index, id) in func.captures.iter().enumerate() {
        let slot = nargs + index;
        code.push(opcode!(@ReserveCapture(slot as SlotNum, *id)));
    }

    let mut label_gen = LabelGenerator::new(unique_id + 1);

    let mut cgen = Codegen {
        code: &mut code,
        label_gen: &mut label_gen,
        string_table,
        func_labels,
        globals,
        release_build,
        scopes: vec![Scope::default()],
        next_assignable_slot: 0,
    };

    let mut declare = vec![];
    declare.extend(func.args.clone().into_iter().map(|x| x.name));
    declare.extend(func.captures.clone().into_iter().map(|x| x));

    codegen(func.child_nodes(), &mut cgen, 0, declare)?;

    let space_for_locals = cgen.next_assignable_slot;

    code[make_frame_offset] = opcode!(@MakeFrame(space_for_locals, returns, prebuilt));

    Ok((code, label_gen, space_for_locals))
}

/// Codegen a scope of typechecked nodes.
///
/// stack_items counts the number of items that need be popped for an early return.
/// declare carries a list of variables to immediately shadow upon opening the scope,
/// which is useful for function arguments and if-let.
fn codegen(
    nodes: Vec<TypeCheckedNode>,
    cgen: &mut Codegen,
    stack_items: usize,
    declare: Vec<StringId>,
) -> Result<(), CompileError> {
    cgen.open_scope();

    for id in declare {
        let slot = cgen.next_slot();
        cgen.shadow(id, slot);
    }

    macro_rules! expr {
        ($expr:expr, $push:expr) => {
            codegen(
                vec![TypeCheckedNode::Expression($expr)],
                cgen,
                stack_items + $push,
                vec![],
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

        macro_rules! local {
            ($id:expr) => {
                match cgen.get_local($id) {
                    Some(slot) => slot,
                    None => error!(
                        "no slot assigned for {} {}",
                        cgen.string_table.name_from_id(*$id),
                        $id
                    ),
                }
            };
        }

        macro_rules! block {
            ($block:expr, $declare:expr) => {
                codegen($block.child_nodes(), cgen, stack_items, $declare)?
            };
            ($block:expr) => {
                block!($block, vec![])
            };
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
                            let slot = cgen.next_slot();

                            match local.shadow {
                                true => cgen.shadow(local.id, slot),
                                false => cgen.set_local(local.id, slot),
                            }

                            if count > 1 {
                                cgen.code.push(opcode!(@TupleGet(index, count)));
                            }

                            cgen.code.push(opcode!(@SetLocal(slot)));
                        }
                    }
                    TypeCheckedStatementKind::AssignGlobal(id, expr) => {
                        expr!(expr);
                        let global = cgen.globals.get(id).expect("No global exists for stringId");
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
                        expr!(expr);
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
                        expr!(expr);

                        let ok_label = cgen.label_gen.next();

                        // Test the condition
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(@TupleGet(0, 2)));
                        cgen.code.push(opcode!(Cjump, Value::Label(ok_label)));

                        // failure state
                        let line = debug.location.expect("no location").line;
                        let text = format!("assert on line {} failed with", line);
                        let tuple =
                            Value::new_tuple(vec![Value::from(text.as_ref()), Value::none()]);
                        cgen.code.push(opcode!(@TupleGet(1, 2)));
                        cgen.code.push(opcode!(Noop, tuple));
                        cgen.code.push(opcode!(@TupleSet(1, 2)));
                        cgen.code.push(opcode!(DebugPrint));
                        cgen.code.push(opcode!(Error));

                        // passing state
                        cgen.code.push(opcode!(@Label(ok_label)));
                        cgen.code.push(opcode!(Pop));
                    }
                    TypeCheckedStatementKind::While(cond, body) => {
                        let loop_slot = cgen.next_slot();
                        let top_label = cgen.label_gen.next();
                        let cond_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(Noop, Value::Label(top_label)));
                        cgen.code.push(opcode!(@SetLocal(loop_slot)));
                        cgen.code.push(opcode!(Jump, Value::Label(cond_label)));
                        cgen.code.push(opcode!(@Label(top_label)));
                        block!(body);
                        cgen.code.push(opcode!(@Label(cond_label)));
                        expr!(cond);
                        cgen.code.push(opcode!(@GetLocal(loop_slot)));
                        cgen.code.push(opcode!(@CjumpTo(top_label)));
                    }
                }
            }
            TypeCheckedNode::Expression(expr) => {
                match &mut expr.kind {
                    TypeCheckedExprKind::CodeBlock(block) => {
                        codegen(block.child_nodes(), cgen, stack_items, vec![])?;
                    }
                    TypeCheckedExprKind::If(cond, block, else_block, _) => {
                        expr!(cond);
                        let end_label = cgen.label_gen.next();
                        let else_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(IsZero));
                        cgen.code.push(opcode!(Cjump, Value::Label(else_label)));
                        block!(block);
                        cgen.code.push(opcode!(Jump, Value::Label(end_label)));
                        cgen.code.push(opcode!(@Label(else_label)));
                        if let Some(else_block) = else_block {
                            block!(else_block);
                        }
                        cgen.code.push(opcode!(@Label(end_label)));
                    }
                    TypeCheckedExprKind::IfLet(id, right, block, else_block, _) => {
                        expr!(right);
                        let end_label = cgen.label_gen.next();
                        let else_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(@TupleGet(0, 2)));
                        cgen.code.push(opcode!(IsZero));
                        cgen.code.push(opcode!(Cjump, Value::Label(else_label)));

                        // Some(_) case
                        cgen.code.push(opcode!(@TupleGet(1, 2)));

                        // if-let is tricky since the local variable isn't defined in the same scope.
                        // To work around this, we get the next slot without advancing. This means
                        // not actually *calling* next_slot().
                        let slot = cgen.next_assignable_slot;
                        cgen.code.push(opcode!(@SetLocal(slot)));
                        block!(block, vec![*id]);
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
                        let loop_slot = cgen.next_slot();
                        let top_label = cgen.label_gen.next();
                        cgen.code.push(opcode!(Noop, Value::Label(top_label)));
                        cgen.code.push(opcode!(@SetLocal(loop_slot)));
                        cgen.code.push(opcode!(@Label(top_label)));
                        block!(body);
                        cgen.code.push(opcode!(@GetLocal(loop_slot)));
                        cgen.code.push(opcode!(@JumpTo(top_label)));
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
                    TypeCheckedExprKind::LocalVariableRef(id, _) => {
                        let slot = local!(id);
                        cgen.code.push(opcode!(@GetLocal(slot)));
                    }
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

                        if prop.sensitive {
                            // a sensitive func must be in its own basic block
                            // since it does something that violates the func call ABI
                            let prior = cgen.label_gen.next();
                            let after = cgen.label_gen.next();
                            cgen.code.push(opcode!(@Label(prior)));
                            cgen.code.push(opcode!(@FuncCall(*prop)));
                            cgen.code.push(opcode!(@Label(after)));
                        } else {
                            cgen.code.push(opcode!(@FuncCall(*prop)));
                        }
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
                    TypeCheckedExprKind::NewFixedArray(size, fill, _) => {
                        expr!(fill);
                        for _ in 0..7 {
                            cgen.code.push(opcode!(Dup0));
                        }
                        let container = vec![Value::new_tuple(Vec::new()); 8];
                        cgen.code.push(opcode!(Noop, Value::new_tuple(container)));
                        for i in 0..8 {
                            cgen.code.push(opcode!(@TupleSet(i, 8)));
                        }

                        let mut tuple_size: usize = 8;
                        while tuple_size < *size {
                            for _ in 0..7 {
                                cgen.code.push(opcode!(Dup0));
                            }
                            let container = vec![Value::new_tuple(Vec::new()); 8];
                            cgen.code.push(opcode!(Noop, Value::new_tuple(container)));
                            for i in 0..8 {
                                cgen.code.push(opcode!(@TupleSet(i, 8)));
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
                            UnaryOp::Len => {
                                // The op-version of len() only applies to arrays, for which
                                // we need the first tuple item
                                opcode!(@TupleGet(0, 3))
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
                        cgen.code.push(opcode!(@TupleSet(1, 2)));
                    }
                    TypeCheckedExprKind::Try(variant, _) => {
                        expr!(variant);
                        let success = cgen.label_gen.next();
                        cgen.code.push(opcode!(Dup0));
                        cgen.code.push(opcode!(@TupleGet(0, 2)));
                        cgen.code.push(opcode!(Cjump, Value::Label(success)));
                        cgen.code.push(opcode!(@Return));
                        clear_stack!();
                        cgen.code.push(opcode!(@Label(success)));
                        cgen.code.push(opcode!(@TupleGet(1, 2)));
                    }
                    TypeCheckedExprKind::ClosureLoad(id, captures, _) => {
                        // The closure ABI is based around the idea that a closure pointer is essentially
                        // an updatable function frame passed around and copied for each of its invocations.
                        // This means space is reserved for the args, which are dynamically written to at the
                        // time of a call. We'll call these "blanks", since as the closure is passed around,
                        // the values are not yet known. This yeilds the format:
                        //
                        //            ( codepoint, ( blank_arg1, blank_arg2, ..., blank_argN ) )
                        //
                        // Closures may also have local variables, for which space must be preserved. It's
                        // up to the optimizer to determine their place within the frame.
                        //
                        //            ( codepoint, ( arg_or_local_1, arg_or_local_2, ..., arg_or_local_N ) )
                        //
                        // Closures may also have captures, which are written to exactly once at the time of
                        // creation. It's up to the optimizer to determine their place within the frame.
                        //
                        //            ( codepoint, { <blank_args>, <locals>, <captures> } )
                        //
                        // For efficiency, when a closure has no captures, we express it like a func pointer
                        // and then use code at runtime (or compile time when the optimizer can tell) that
                        // checks the type. In this manner, a function pointer is equivalent to a closure
                        // with 0 captures, which is why they are said to be the same type.
                        //
                        //              codepoint
                        //
                        // Lastly, in the rare case a single item is present in the tuple, we de-tuplify.
                        //
                        //             ( codepoint, ( item ) )   === becomes ===>   ( codepoint, item )
                        //

                        let (closure_id, closure) = match cgen.func_labels.get(id) {
                            Some(label) => (label.get_id(), Value::Label(*label)),
                            None => {
                                error!("No label for closure ref {}", id)
                            }
                        };

                        for capture in captures.iter().rev() {
                            let slot = local!(capture);
                            cgen.code.push(opcode!(@GetLocal(slot)));
                        }

                        if captures.is_empty() {
                            // Having no captures makes this equivalent to a function call
                            cgen.code.push(opcode!(Noop, closure));
                        } else {
                            // We'll pack this closure later when we actually know its size and colors
                            cgen.code.push(opcode!(@MakeClosure(closure_id)));
                            for &capture in captures.iter() {
                                cgen.code.push(opcode!(@Capture(closure_id, capture)));
                            }

                            let container = Value::new_tuple(vec![closure, Value::none()]);
                            cgen.code.push(opcode!(Noop, container));
                            cgen.code.push(opcode!(@TupleSet(1, 2)));
                        }
                    }
                }
            }
            _ => {}
        }
    }

    let debug = cgen.code.last().unwrap().debug_info;
    let scope = cgen.scopes.pop().expect("No scope");

    for (local, mut slot) in scope.locals {
        // We're closing a scope, so any final assignments need to be phi'd

        if let Some(alias) = scope.shadows.get(&local) {
            slot = *alias;
        }

        if let Some(old) = cgen.get_local(&local) {
            if old != slot {
                cgen.code.push(Instruction::from_opcode(
                    Opcode::MoveLocal(old, slot),
                    debug,
                ));
            }
        }
    }

    Ok(())
}
