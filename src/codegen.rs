use std::collections::HashMap;
use crate::mavm::{Label, LabelGenerator, Instruction, Opcode, Value};
use crate::uint256::Uint256;
use crate::typecheck::{TypeCheckedFunc, TypeCheckedStatement, TypeCheckedIfArm, TypeCheckedExpr};
use crate::symtable::CopyingSymTable;
use crate::ast::{UnaryOp, BinaryOp, FuncArg, Type};
use crate::stringtable::{StringId, StringTable};
use crate::link::ImportedFunc;
use crate::xformcode::TUPLE_SIZE;
use crate::pos::Location;


#[derive(Debug)]
pub struct CodegenError {
	pub reason: &'static str,
	pub location: Option<Location>,
}

pub fn new_codegen_error(reason: &'static str, location: Option<Location>) -> CodegenError {
	CodegenError{ reason, location }
}

pub fn mavm_codegen<'a>(
	funcs: Vec<TypeCheckedFunc>, 
	code_in: &'a mut Vec<Instruction>,
	string_table: &'a StringTable,
	imported_funcs: &[ImportedFunc],
) -> Result<&'a mut Vec<Instruction>, CodegenError> {
	let mut import_func_map = HashMap::new();
	for imp_func in imported_funcs {
		import_func_map.insert(imp_func.name_id, Label::External(imp_func.slot_num));
	}

	let mut label_gen = LabelGenerator::new();
	let mut code = code_in;
	for func in funcs {
		if ! func.imported {
			let (lg, c) = mavm_codegen_func(func, code, label_gen, string_table, &import_func_map)?;
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
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
	let location = func.location;
	code.push(Instruction::from_opcode(Opcode::Label(Label::Func(func.name)), location));

	let num_args = func.args.len();
	let locals = CopyingSymTable::<usize>::new();

	let make_frame_slot = code.len();
	code.push(Instruction::from_opcode(Opcode::Noop, location));  // placeholder; will replace this later

	let (lg, num_locals, maybe_continue) = add_args_to_locals_table(
		locals, 
		&func.args, 
		0, 
		func.code, 
		code, 
		label_gen, 
		string_table,
		import_func_map
	)?;
	label_gen = lg;

	if maybe_continue {
		match func.ret_type {
			Type::Void => {
				code.push(Instruction::from_opcode(Opcode::Return, location));
			} 
			_ => {
				return Err(new_codegen_error("apparent path to end of function without return", location));
			}
		}
	}

	// put makeframe Instruction at beginning of function, to build the frame
	code[make_frame_slot] = Instruction::from_opcode(Opcode::MakeFrame(num_args, num_locals), location);

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
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
	if args.is_empty() {
		mavm_codegen_statements(statements, code, num_locals, &locals, label_gen, string_table, import_func_map)
	} else {
		let new_locals = locals.push_one(args[0].name, num_locals);
		add_args_to_locals_table(
			new_locals, 
			&args[1..], 
			num_locals+1, 
			statements, 
			code, 
			label_gen, 
			string_table,
			import_func_map
		)
	}
}

fn mavm_codegen_statements<'a>(
	statements: Vec<TypeCheckedStatement>,     // statements to codegen
	mut code: &'a mut Vec<Instruction>,        // accumulates the code as it's generated
	mut num_locals: usize,                         // num locals that have been allocated
	locals: &CopyingSymTable<usize>,               // lookup local variable slot number by name
	mut label_gen: LabelGenerator,   
	string_table: &StringTable,
	import_func_map: &HashMap<StringId, Label>,       
) -> Result<(LabelGenerator, usize, bool), CodegenError> { // (label_gen, num_labels, execution_might_continue)
	if statements.is_empty() {
		return Ok((label_gen, num_locals, true));
	}
	let rest_of_statements = &statements[1..];
	match &statements[0] {
		TypeCheckedStatement::Noop(_) => 
			mavm_codegen_statements(
				rest_of_statements.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			),
		TypeCheckedStatement::Panic(loc) => {
			code.push(Instruction::from_opcode(Opcode::Panic, loc.clone()));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::ReturnVoid(loc) => {
			code.push(Instruction::from_opcode(Opcode::Return, loc.clone()));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::Return(expr, loc) => {
			let (lg, c) = mavm_codegen_expr(expr, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Return, loc.clone()));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::Let(name, expr, loc) => {
			let slot_num = num_locals;
			let new_locals = locals.push_one(*name, slot_num);
			num_locals += 1;
			let (lg, c) = mavm_codegen_expr(expr, code, &new_locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, Value::Int(Uint256::from_usize(slot_num)), loc.clone()));   
			mavm_codegen_statements(
				rest_of_statements.to_vec(), 
				code, 
				num_locals, 
				&new_locals, 
				label_gen, 
				string_table,
				import_func_map
			)
		}
		TypeCheckedStatement::Assign(name, expr, loc) => {
			let slot_num = match locals.get(*name) {
				Some(slot) => slot,
				None => { return Err(new_codegen_error("assigned to non-existent variable", *loc)) }
			};
			let (lg, c) = mavm_codegen_expr(expr, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, Value::Int(Uint256::from_usize(slot_num)), loc.clone()));   
			mavm_codegen_statements(
				rest_of_statements.to_vec(), 
				code, 
				num_locals, 
				&locals, 
				label_gen, 
				string_table,
				import_func_map
			)
		}
		TypeCheckedStatement::Loop(body, loc) => {
			let slot_num = Value::Int(Uint256::from_usize(num_locals));
			num_locals += 1;
			let (top_label, lg) = label_gen.next();
			label_gen = lg;
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(top_label), loc.clone()));
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, slot_num.clone(), loc.clone()));
			let (lg, nl, _) = mavm_codegen_statements(
				body.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			)?;
			label_gen = lg;
			num_locals = nl;
			code.push(Instruction::from_opcode_imm(Opcode::GetLocal, slot_num, loc.clone()));
			code.push(Instruction::from_opcode(Opcode::Jump, loc.clone()));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::While(cond, body, loc) => {
			let slot_num = Value::Int(Uint256::from_usize(num_locals));
			num_locals += 1;
			let (top_label, lg) = label_gen.next();
			let (end_label, lg) = lg.next();
			label_gen = lg;
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(top_label), loc.clone()));
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, slot_num.clone(), loc.clone()));
			code.push(Instruction::from_opcode(Opcode::Label(top_label), loc.clone()));
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Not, loc.clone()));
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(end_label), loc.clone()));
			let (lg, nl, _) = mavm_codegen_statements(
				body.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			)?;
			label_gen = lg;
			num_locals = nl;
			code.push(Instruction::from_opcode_imm(Opcode::GetLocal, slot_num, loc.clone()));
			code.push(Instruction::from_opcode(Opcode::Jump, loc.clone()));
			code.push(Instruction::from_opcode(Opcode::Label(end_label), loc.clone()));
			mavm_codegen_statements(
				rest_of_statements.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			)
		}
		TypeCheckedStatement::If(arms, loc) => {
			let mut might_ever_continue = false;
			let mut have_catchall = false;
			let (end_label, lg) = label_gen.next();
			label_gen = lg;
			for arm in arms {
				match arm {
					TypeCheckedIfArm::Cond(cond, body) => {
						let (after_label, lg) = label_gen.next();
						let (lg, c) = mavm_codegen_expr(cond, code, &locals, lg, string_table, import_func_map)?;
						label_gen = lg;
						code = c;
						code.push(Instruction::from_opcode(Opcode::Not, *loc));
						code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(after_label), loc.clone()));
						let (lg, nl, might_continue_here) = mavm_codegen_statements(
							body.to_vec(), 
							code, 
							num_locals, 
							locals, 
							label_gen, 
							string_table,
							import_func_map
						)?;
						label_gen = lg;
						num_locals = nl;
						if might_continue_here {
							might_ever_continue = true;
							code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), loc.clone()));
						}
						code.push(Instruction::from_opcode(Opcode::Label(after_label), loc.clone()));
					}
					TypeCheckedIfArm::Catchall(body) => {
						have_catchall = true;
						let (lg, nl, might_continue_here) = mavm_codegen_statements(
							body.to_vec(), 
							code, 
							num_locals, 
							locals, 
							label_gen, 
							string_table,
							import_func_map
						)?;
						label_gen = lg;
						num_locals = nl;
						if might_continue_here {
							might_ever_continue = true;
							code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), loc.clone()));
						}
					}
				}
			}
			code.push(Instruction::from_opcode(Opcode::Label(end_label), loc.clone()));
			if might_ever_continue || !have_catchall {
				code.push(Instruction::from_opcode(Opcode::Label(end_label), loc.clone()));
				mavm_codegen_statements(
					rest_of_statements.to_vec(), 
					code, 
					num_locals, 
					locals, 
					label_gen, 
					string_table,
					import_func_map
				)
			} else {
				Ok((label_gen, num_locals, false))
			}
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
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
	match expr {
		TypeCheckedExpr::UnaryOp(op, tce, _, loc) => {
			let (lg, c) = mavm_codegen_expr(tce, code, locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			let maybe_opcode = match op {
				UnaryOp::Minus => Some(Opcode::UnaryMinus),
				UnaryOp::BitwiseNeg => Some(Opcode::BitwiseNeg),
				UnaryOp::Not => Some(Opcode::Not),
				UnaryOp::Hash => Some(Opcode::Hash),
				UnaryOp::Len => Some(Opcode::Len),
				UnaryOp::ToUint => None,
				UnaryOp::ToInt => None,
				UnaryOp::ToBytes32 => None
			};
			if let Some(opcode) = maybe_opcode {
				code.push(Instruction::from_opcode(opcode, loc.clone()));
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Binary(op, tce1, tce2, _, loc) => {
			let (lg, c) = mavm_codegen_expr(tce2, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(tce1, c, locals, lg, string_table, import_func_map)?;
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
				BinaryOp::LessEq => Opcode::GreaterThan,   // will negate
				BinaryOp::GreaterEq => Opcode::SLessThan,  // will negate
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
			code.push(Instruction::from_opcode(opcode, loc.clone()));
			match op {
				BinaryOp::LessEq |
				BinaryOp::GreaterEq |
				BinaryOp::SLessEq |
				BinaryOp::SGreaterEq => { code.push(Instruction::from_opcode(Opcode::Not, loc.clone())); }
				_ => {}
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ShortcutOr(tce1, tce2, loc) => {
			let (lg, c) = mavm_codegen_expr(tce1, code, locals, label_gen, string_table, import_func_map)?;
			let (lab, lg) = lg.next();
			c.push(Instruction::from_opcode(Opcode::Dup0, loc.clone()));
			c.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(lab), loc.clone()));
			c.push(Instruction::from_opcode(Opcode::Pop, loc.clone()));
			let (lg, c) = mavm_codegen_expr(tce2, c, locals, lg, string_table, import_func_map)?;
			c.push(Instruction::from_opcode(Opcode::Label(lab), loc.clone()));
			Ok((lg, c))
		}
		TypeCheckedExpr::ShortcutAnd(tce1, tce2, loc) => {
			let (lg, c) = mavm_codegen_expr(tce1, code, locals, label_gen, string_table, import_func_map)?;
			let (lab, lg) = lg.next();
			c.push(Instruction::from_opcode(Opcode::Dup0, loc.clone()));
			c.push(Instruction::from_opcode(Opcode::Not, loc.clone()));
			c.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(lab), loc.clone()));
			c.push(Instruction::from_opcode(Opcode::Pop, loc.clone()));
			let (lg, c) = mavm_codegen_expr(tce2, c, locals, lg, string_table, import_func_map)?;
			c.push(Instruction::from_opcode(Opcode::Label(lab), loc.clone()));
			Ok((lg, c))
		}
		TypeCheckedExpr::VariableRef(name, _, loc) => {
			match locals.get(*name) {
				Some(n) => {
					code.push(Instruction::from_opcode_imm(Opcode::GetLocal, Value::Int(Uint256::from_usize(n)), loc.clone())); 
					Ok((label_gen, code))
				}
				None => {
					println!("local: {:?}", *name);
					Err(new_codegen_error("tried to access non-existent local variable", *loc))
				}
			}
		}
		TypeCheckedExpr::TupleRef(tce, idx, _, loc) => {
			let tce_type = tce.get_type();
			let tuple_size = if let Type::Tuple(fields) = tce_type {
				fields.len()
			} else {
				panic!("type-checking bug: tuple lookup in non-tuple type");
			};
			let (lg, c) = mavm_codegen_expr(tce, code, locals, label_gen, string_table, import_func_map)?;
			c.push(Instruction::from_opcode_imm(
				Opcode::TupleGet(tuple_size), 
				Value::Int(idx.clone()),
				loc.clone(),
			));
			Ok((lg, c))
		}
		TypeCheckedExpr::DotRef(tce, name, _, loc) => {
			let tce_type = tce.get_type();
			let struct_size = match tce_type.clone() {
				Type::Struct(fields) => fields.len(),
				_ => { panic!("type-checking bug: struct lookup in non-struct type") }
			};
			let (lg, c) = mavm_codegen_expr(tce, code, locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			match tce_type.get_struct_slot_by_name(*name) {
				Some(slot_num) => {
					code.push(Instruction::from_opcode_imm(
						Opcode::TupleGet(struct_size), 
						Value::Int(Uint256::from_usize(slot_num)),
						loc.clone()
					));
					Ok((label_gen, code))
				}
				None => Err(new_codegen_error("tried to get non-existent struct field", *loc))
			}
		}
		TypeCheckedExpr::ConstUint(ui, loc) => {
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(ui.clone()), loc.clone()));
			Ok((label_gen, code))			
		}
		TypeCheckedExpr::ConstInt(ui, loc) => {
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(ui.clone()), loc.clone()));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::FunctionCall(name, args, _, loc) => {
			let n_args = args.len();
			let (ret_label, lg) = label_gen.next();
			label_gen = lg;
			for i in 0..n_args {
				let (lg, c) = mavm_codegen_expr(
					&args[n_args-1-i], 
					code, 
					locals, 
					label_gen, 
					string_table, 
					import_func_map
				)?;
				label_gen = lg;
				code = c;
			}
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(ret_label), loc.clone()));
			let func_label = match import_func_map.get(name) {
				Some(lab) => *lab,
				None => Label::Func(*name),
			};
			code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(func_label), loc.clone()));
			code.push(Instruction::from_opcode(Opcode::Label(ret_label), loc.clone()));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::StructInitializer(fields, _, loc) => {
			let fields_len = fields.len();
			for i in 0..fields_len {
				let field = &fields[fields_len-1-i];
				let (lg, c) = mavm_codegen_expr(&field.value, code, locals, label_gen, string_table, import_func_map)?;
				label_gen = lg;
				code = c;
			}
			let empty_vec = vec![Value::none(); fields_len];
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Tuple(empty_vec), loc.clone()));
			for i in 0..fields_len {
				code.push(Instruction::from_opcode_imm(Opcode::TupleSet(fields_len), Value::Int(Uint256::from_usize(i)), loc.clone()));
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Tuple(fields, _, loc) => {
			let fields_len = fields.len();
			for i in 0..fields_len {
				let field = &fields[fields_len-1-i];
				let (lg, c) = mavm_codegen_expr(&field, code, locals, label_gen, string_table, import_func_map)?;
				label_gen = lg;
				code = c;
			}
			let empty_vec = vec![Value::none(); fields_len];
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Tuple(empty_vec), loc.clone()));
			for i in 0..fields_len {
				code.push(Instruction::from_opcode_imm(Opcode::TupleSet(fields_len), Value::Int(Uint256::from_usize(i)), loc.clone()));
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ArrayRef(expr1, expr2, t, loc) => {
			let the_expr = TypeCheckedExpr::FunctionCall(
				*string_table.get_if_exists("builtin_arrayGet").unwrap(),
				vec![
					*expr1.clone(), 
					*expr2.clone(),
				],
				Type::Func(
					vec![Type::Array(Box::new(Type::Any)), Type::Uint],
					Box::new(t.clone()),
				),
				loc.clone()
			);
			mavm_codegen_expr(
				&the_expr,
				code, 
				locals,
				label_gen,
				string_table,
				import_func_map,
			)
		}
		TypeCheckedExpr::FixedArrayRef(expr1, expr2, size, _, loc) => {
			let (lg, c) = mavm_codegen_expr(expr1, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(expr2, c, locals, lg, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			if *size != 8 {  //TODO: also skip check if size is larger power of 8
				let (cont_label, lg) = label_gen.next();
				label_gen = lg;
				code.push(Instruction::from_opcode(Opcode::Dup0, loc.clone()));
				code.push(Instruction::from_opcode_imm(
					Opcode::GreaterThan, 
					Value::Int(Uint256::from_usize(*size)),
					loc.clone(),
				));
				code.push(Instruction::from_opcode_imm(
					Opcode::Cjump,
					Value::Label(cont_label),
					loc.clone(),
				));
				code.push(Instruction::from_opcode(Opcode::Panic, loc.clone()));
				code.push(Instruction::from_opcode(Opcode::Label(cont_label), loc.clone()));
			}
			code.push(Instruction::from_opcode(Opcode::UncheckedFixedArrayGet(*size), loc.clone()));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::NewArray(sz_expr, base_type, array_type, loc) => {
			let default_val = base_type.default_value();
			let the_expr = TypeCheckedExpr::FunctionCall(
				*string_table.get_if_exists("builtin_arrayNew").unwrap(),
				vec![
					*sz_expr.clone(), 
					TypeCheckedExpr::RawValue(default_val, Type::Any, loc.clone()),
				],
				Type::Func(
					vec![Type::Uint, Type::Any],
					Box::new(array_type.clone()),
				),
				loc.clone(),
			);
			mavm_codegen_expr(
				&the_expr,
				code, 
				locals,
				label_gen,
				string_table,
				import_func_map,
			)
		}
		TypeCheckedExpr::NewFixedArray(sz, bo_expr, _, loc) => {
			match bo_expr {
				Some(expr) => {
					let (lg, c) = mavm_codegen_expr(expr, code, locals, label_gen, string_table, import_func_map)?;
					label_gen = lg;
					code = c;
					for _i in 0..7 {
						code.push(Instruction::from_opcode(Opcode::Dup0, loc.clone()));
					}
					let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
					code.push(Instruction::from_opcode_imm(Opcode::Noop,Value::Tuple(empty_tuple), loc.clone()));
					for i in 0..8 {
						code.push(Instruction::from_opcode_imm(Opcode::Tset, Value::Int(Uint256::from_usize(i)), loc.clone()));
					}
				}
				None => {
					let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
					code.push(Instruction::from_opcode_imm(Opcode::Noop,Value::Tuple(empty_tuple), loc.clone()));
				}
			}
			let mut tuple_size: usize = 8;
			while tuple_size < *sz {
				for _i in 0..7 {
					code.push(Instruction::from_opcode(Opcode::Dup0, loc.clone()));
				}
				let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
				code.push(Instruction::from_opcode_imm(Opcode::Noop,Value::Tuple(empty_tuple), loc.clone()));
				for i in 0..8 {
					code.push(Instruction::from_opcode_imm(Opcode::Tset, Value::Int(Uint256::from_usize(i)), loc.clone()));
				}				
				tuple_size *= 8;
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ArrayMod(arr, index, val, _, loc) => {
			let the_expr = TypeCheckedExpr::FunctionCall(
				*string_table.get_if_exists("builtin_arraySet").unwrap(),
				vec![*arr.clone(), *index.clone(), *val.clone()],
				Type::Func(
					vec![arr.get_type(), index.get_type(), val.get_type()],
					Box::new(arr.get_type()),
				),
				loc.clone(),
			);
			mavm_codegen_expr(
				&the_expr,
				code,
				locals,
				label_gen,
				string_table,
				import_func_map,
			)
		}
		TypeCheckedExpr::FixedArrayMod(arr, index, val, size, _, loc) => codegen_fixed_array_mod(
			arr, index, val, *size,
			code,
			locals,
			label_gen,
			string_table,
			import_func_map,
			*loc,
		),
		TypeCheckedExpr::StructMod(struc, index, val, t, loc) => {
			let (lg, c) = mavm_codegen_expr(val, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(struc, c, locals, lg, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			if let Type::Struct(v) = t {
				let struct_len = v.len();
				code.push(Instruction::from_opcode_imm(Opcode::TupleSet(struct_len), Value::Int(Uint256::from_usize(*index)), loc.clone()));
			} else {
				panic!("impossible value in TypeCheckedExpr::StructMod");
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Cast(expr, _, _) => 
			mavm_codegen_expr(expr, code, locals, label_gen, string_table, import_func_map),
		TypeCheckedExpr::RawValue(val, _, loc) => {
			code.push(Instruction::from_opcode_imm(Opcode::Noop, val.clone(), loc.clone()));
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
	location: Option<Location>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
	let (label_gen, code) = mavm_codegen_expr(arr_expr, code_in, locals, label_gen_in, string_table, import_func_map)?;
	let (mut label_gen, code) = mavm_codegen_expr(idx_expr, code, locals, label_gen, string_table, import_func_map)?;
	if size != 8 {  // TODO: safe for if-condition to say size does not equal any power of 8
		let (ok_label, lg) = label_gen.next();
		label_gen = lg;
		code.push(Instruction::from_opcode(Opcode::Dup0, location));
		code.push(Instruction::from_opcode_imm(Opcode::GreaterThan, Value::Int(Uint256::from_usize(size)), location));
		code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(ok_label), location));
		code.push(Instruction::from_opcode(Opcode::Panic, location));
		code.push(Instruction::from_opcode(Opcode::Label(ok_label), location));
	}
	codegen_fixed_array_mod_2(val_expr, size, code, locals, label_gen, string_table, import_func_map, location)
}

fn codegen_fixed_array_mod_2<'a>(
	val_expr: &TypeCheckedExpr,
	size: usize,
	code_in: &'a mut Vec<Instruction>,
	locals: &CopyingSymTable<usize>,
	label_gen_in: LabelGenerator,
	string_table: &StringTable,
	import_func_map: &HashMap<StringId, Label>,
	location: Option<Location>,
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
	if size <= 8 {
		// stack: idx tuple
		code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
		let (label_gen, code) = mavm_codegen_expr(val_expr, code_in, locals, label_gen_in, string_table, import_func_map)?;
		code.push(Instruction::from_opcode(Opcode::Swap2, location));
		// stack: idx tuple value
		code.push(Instruction::from_opcode(Opcode::Tset, location));
		Ok((label_gen, code))
	} else {
		let tuple_size = Value::Int(Uint256::from_usize(TUPLE_SIZE));
		// stack: idx tupletree
		code_in.push(Instruction::from_opcode_imm(Opcode::Dup2, tuple_size.clone(), location));
		code_in.push(Instruction::from_opcode(Opcode::AuxPush, location));
		code_in.push(Instruction::from_opcode(Opcode::Dup0, location));
		// stack: idx TUPLE_SIZE idx tupletree; aux: tupletree
		code_in.push(Instruction::from_opcode(Opcode::Div, location));
		code_in.push(Instruction::from_opcode(Opcode::Dup0, location));
		code_in.push(Instruction::from_opcode(Opcode::AuxPush, location));
		// stack: slot idx tupletree; aux: slot tupletree
		code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
		code_in.push(Instruction::from_opcode_imm(Opcode::Swap1, tuple_size, location));
		// stack: subidx slot tupletree; aux: slot tupletree
		code_in.push(Instruction::from_opcode(Opcode::Swap2, location));
		code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
		code_in.push(Instruction::from_opcode(Opcode::Tget, location));
		code_in.push(Instruction::from_opcode(Opcode::Swap1, location));
		// stack: subidx subtupletree; aux: slot tupletree

		let (label_gen, code) = codegen_fixed_array_mod_2(
			val_expr,
			(size+(TUPLE_SIZE-1))/TUPLE_SIZE,
			code_in,
			locals,
			label_gen_in,
			string_table,
			import_func_map,
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
