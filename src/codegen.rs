use std::collections::HashMap;
use crate::mavm::{Label, LabelGenerator, Instruction, Opcode, Value};
use crate::uint256::Uint256;
use crate::typecheck::{TypeCheckedFunc, TypeCheckedStatement, TypeCheckedExpr};
use crate::symtable::CopyingSymTable;
use crate::ast::{UnaryOp, BinaryOp, FuncArg, Type};
use crate::stringtable::{StringId, StringTable};
use crate::linker::ImportedFunc;


#[derive(Debug)]
pub struct CodegenError {
	pub reason: &'static str
}

pub fn new_codegen_error(reason: &'static str) -> CodegenError {
	CodegenError{ reason }
}

pub fn mavm_codegen<'a>(
	funcs: Vec<TypeCheckedFunc>, 
	code_in: &'a mut Vec<Instruction>,
	string_table: &'a StringTable,
	imported_funcs: &Vec<ImportedFunc>,
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
	code.push(Instruction::from_opcode(Opcode::Label(Label::Func(func.name))));

	let num_args = func.args.len();
	let locals = CopyingSymTable::<usize>::new();

	let make_frame_slot = code.len();
	code.push(Instruction::from_opcode(Opcode::Noop));  // placeholder; will replace this later

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
				code.push(Instruction::from_opcode(Opcode::Return));
			} 
			_ => {
				return Err(new_codegen_error("apparent path to end of function without return"));
			}
		}
	}

	// put makeframe instruction at beginning of function, to build the frame
	code[make_frame_slot] = Instruction::from_opcode(Opcode::MakeFrame(num_args, num_locals));

	Ok((label_gen, code))
}

fn add_args_to_locals_table<'a>(
	locals: CopyingSymTable<'a, usize>, 
	args: &'a [FuncArg], 
	num_locals: usize,
	statements: Vec<TypeCheckedStatement>,
	code: &mut Vec<Instruction>,
	label_gen: LabelGenerator,
	string_table: &StringTable,
	import_func_map: &HashMap<StringId, Label>,
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
	if args.len() == 0 {
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

fn mavm_codegen_statements(
	statements: Vec<TypeCheckedStatement>,  // statements to codegen
	mut code: &mut Vec<Instruction>,        // accumulates the code as it's generator
	mut num_locals: usize,                  // num locals that have been allocated
	locals: &CopyingSymTable<usize>,        // lookup local variable slot number by name
	mut label_gen: LabelGenerator,   
	string_table: &StringTable,
	import_func_map: &HashMap<StringId, Label>,       
) -> Result<(LabelGenerator, usize, bool), CodegenError> { // (label_gen, num_labels, execution_might_continue)
	if statements.len() == 0 {
		return Ok((label_gen, num_locals, true));
	}
	let rest_of_statements = &statements[1..];
	match &statements[0] {
		TypeCheckedStatement::Noop => 
			mavm_codegen_statements(
				rest_of_statements.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			),
		TypeCheckedStatement::Panic => {
			code.push(Instruction::from_opcode(Opcode::Panic));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::ReturnVoid => {
			code.push(Instruction::from_opcode(Opcode::Return));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::Return(expr) => {
			let (lg, c) = mavm_codegen_expr(expr, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Return));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::Let(name, expr) => {
			let slot_num = num_locals;
			let new_locals = locals.push_one(*name, slot_num);
			num_locals = num_locals+1;
			let (lg, c) = mavm_codegen_expr(expr, code, &new_locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, Value::Int(Uint256::from_usize(slot_num))));   
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
		TypeCheckedStatement::Assign(name, expr) => {
			let slot_num = match locals.get(*name) {
				Some(slot) => slot,
				None => { return Err(new_codegen_error("assigned to non-existent variable")) }
			};
			let (lg, c) = mavm_codegen_expr(expr, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, Value::Int(Uint256::from_usize(slot_num))));   
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
		TypeCheckedStatement::Loop(body) => {
			let slot_num = Value::Int(Uint256::from_usize(num_locals));
			num_locals = num_locals+1;
			let (top_label, lg) = label_gen.next();
			label_gen = lg;
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(top_label)));
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, slot_num.clone()));
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
			code.push(Instruction::from_opcode_imm(Opcode::GetLocal, slot_num));
			code.push(Instruction::from_opcode(Opcode::Jump));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::While(cond, body) => {
			let slot_num = Value::Int(Uint256::from_usize(num_locals));
			num_locals = num_locals+1;
			let (end_label, lg) = label_gen.next();
			label_gen = lg;
			code.push(Instruction::from_opcode(Opcode::GetPC));
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, slot_num.clone()));
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Not));
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(end_label.clone())));
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
			code.push(Instruction::from_opcode_imm(Opcode::GetLocal, slot_num));
			code.push(Instruction::from_opcode(Opcode::Jump));
			code.push(Instruction::from_opcode(Opcode::Label(end_label)));
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
		TypeCheckedStatement::If(cond, tbody, None) => {
			let (end_label, lg) = label_gen.next();
			label_gen = lg;
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Not));
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(end_label.clone())));
			let (lg, nl, _) = mavm_codegen_statements(
				tbody.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table, import_func_map
			)?;
			label_gen = lg;
			num_locals = nl;
			code.push(Instruction::from_opcode(Opcode::Label(end_label)));
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
		TypeCheckedStatement::If(cond, tbody, Some(fbody)) => {
			let (true_label, lg) = label_gen.next();
			let (end_label, lg) = lg.next();
			label_gen = lg;
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(true_label.clone())));
			let (lg, nl, might_continue_1) = mavm_codegen_statements(
				fbody.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			)?;
			label_gen = lg;
			num_locals = nl;
			if might_continue_1 {
				code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label.clone())));
			}
			code.push(Instruction::from_opcode(Opcode::Label(true_label)));
			let (lg, nl, might_continue_2) = mavm_codegen_statements(
				tbody.to_vec(), 
				code, 
				num_locals, 
				locals, 
				label_gen, 
				string_table,
				import_func_map
			)?;
			label_gen = lg;
			if num_locals < nl {
				num_locals = nl;
			}
			code.push(Instruction::from_opcode(Opcode::Label(end_label)));
			if might_continue_1 || might_continue_2 {
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
		TypeCheckedExpr::UnaryOp(op, tce, _) => {
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
				code.push(Instruction::from_opcode(opcode));
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Binary(op, tce1, tce2, _) => {
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
			code.push(Instruction::from_opcode(opcode));
			match op {
				BinaryOp::LessEq |
				BinaryOp::GreaterEq |
				BinaryOp::SLessEq |
				BinaryOp::SGreaterEq => { code.push(Instruction::from_opcode(Opcode::Not)); }
				_ => {}
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::VariableRef(name, _) => {
			match locals.get(*name) {
				Some(n) => {
					code.push(Instruction::from_opcode_imm(Opcode::GetLocal, Value::Int(Uint256::from_usize(n)))); 
					Ok((label_gen, code))
				}
				None => {
					println!("local: {:?}", *name);
					Err(new_codegen_error("tried to access non-existent local variable"))
				}
			}
		}
		TypeCheckedExpr::DotRef(tce, name, _) => {
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
						Value::Int(Uint256::from_usize(slot_num))
					));
					Ok((label_gen, code))
				}
				None => Err(new_codegen_error("tried to get non-existent struct field"))
			}
		}
		TypeCheckedExpr::ConstUint(str_id) => {
			let s = string_table.name_from_id(*str_id);
			let val = match Uint256::from_string(s) {
				Some(v) => Value::Int(v),
				None => {
					return Err(new_codegen_error("invalid numeric constant"));
				}
			};
			code.push(Instruction::from_opcode_imm(Opcode::Noop, val));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ConstInt(str_id) => {
			let s = string_table.name_from_id(*str_id);
			let val = match Uint256::from_signed_string(s) {
				Some(v) => Value::Int(v),
				None => {
					return Err(new_codegen_error("invalid numeric constant"));
				}
			};
			code.push(Instruction::from_opcode_imm(Opcode::Noop, val));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::FunctionCall(name, args, _) => {
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
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(ret_label.clone())));
			let func_label = match import_func_map.get(name) {
				Some(lab) => *lab,
				None => Label::Func(*name),
			};
			code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(func_label)));
			code.push(Instruction::from_opcode(Opcode::Label(ret_label)));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::StructInitializer(fields, _) => {
			let fields_len = fields.len();
			for i in 0..fields_len {
				let field = &fields[fields_len-1-i];
				let (lg, c) = mavm_codegen_expr(&field.value, code, locals, label_gen, string_table, import_func_map)?;
				label_gen = lg;
				code = c;
			}
			let empty_vec = vec![Value::none(); fields_len];
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Tuple(empty_vec)));
			for i in 0..fields_len {
				code.push(Instruction::from_opcode_imm(Opcode::TupleSet(fields_len), Value::Int(Uint256::from_usize(i))));
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Tuple(fields, _) => {
			let fields_len = fields.len();
			for i in 0..fields_len {
				let field = &fields[fields_len-1-i];
				let (lg, c) = mavm_codegen_expr(&field, code, locals, label_gen, string_table, import_func_map)?;
				label_gen = lg;
				code = c;
			}
			let empty_vec = vec![Value::none(); fields_len];
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Tuple(empty_vec)));
			for i in 0..fields_len {
				code.push(Instruction::from_opcode_imm(Opcode::TupleSet(fields_len), Value::Int(Uint256::from_usize(i))));
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ArrayRef(expr1, expr2, _) => {
			let (lg, c) = mavm_codegen_expr(expr2, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(expr1, c, locals, lg, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::ArrayGet));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::NewBlock(bo_expr) => match bo_expr {
			Some(expr) => {
				let (lg, c) = mavm_codegen_expr(expr, code, locals, label_gen, string_table, import_func_map)?;
				label_gen = lg;
				code = c;
				for _i in 0..7 {
					code.push(Instruction::from_opcode(Opcode::Dup0));
				}
				let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
				code.push(Instruction::from_opcode_imm(Opcode::Noop,Value::Tuple(empty_tuple)));
				for i in 0..8 {
					code.push(Instruction::from_opcode_imm(Opcode::Tset, Value::Int(Uint256::from_usize(i))));
				}
				Ok((label_gen, code))
			}
			None => {
				let empty_tuple = vec![Value::Tuple(Vec::new()); 8];
				code.push(Instruction::from_opcode_imm(Opcode::Noop,Value::Tuple(empty_tuple)));
				Ok((label_gen, code))
			}
		}
		TypeCheckedExpr::BlockRef(expr1, expr2) => {
			let (lg, c) = mavm_codegen_expr(expr1, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(expr2, c, locals, lg, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Tget));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ArrayMod(_arr, _index, _val, _) => Err(new_codegen_error("ArrayMod not yet implemented")),
		TypeCheckedExpr::StructMod(struc, index, val, t) => {
			let (lg, c) = mavm_codegen_expr(val, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(struc, c, locals, lg, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			if let Type::Struct(v) = t {
				let struct_len = v.len();
				code.push(Instruction::from_opcode_imm(Opcode::TupleSet(struct_len), Value::Int(Uint256::from_usize(*index))));
			} else {
				panic!("impossible value in TypeCheckedExpr::StructMod");
			}
			Ok((label_gen, code))
		}
		TypeCheckedExpr::BlockMod(block, index, val, _) => {
			let (lg, c) = mavm_codegen_expr(val, code, locals, label_gen, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(block, c, locals, lg, string_table, import_func_map)?;
			let (lg, c) = mavm_codegen_expr(index, c, locals, lg, string_table, import_func_map)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Tset));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Cast(expr, _) => 
			mavm_codegen_expr(expr, code, locals, label_gen, string_table, import_func_map),
	}
}
