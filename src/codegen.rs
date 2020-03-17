use std::collections::HashMap;
use crate::mavm::{Label, LabelGenerator, Instruction, Opcode, Value, Uint256};
use crate::typecheck::{TypeCheckedFunc, TypeCheckedStatement, TypeCheckedExpr};
use crate::stringtable::StringId;
use crate::symtable::CopyingSymTable;
use crate::ast::{UnaryOp, BinaryOp, FuncArg, Type};
use crate::stringtable::StringTable;


#[derive(Debug)]
pub struct CodegenError {
	msg: &'static str
}

pub fn new_codegen_error(msg: &'static str) -> CodegenError {
	CodegenError{ msg }
}

pub fn mavm_codegen<'a>(
	funcs: Vec<TypeCheckedFunc>, 
	code_in: &'a mut Vec<Instruction>,
	string_table: &'a StringTable
) -> Result<&'a mut Vec<Instruction>, CodegenError> {
	let mut label_gen = LabelGenerator::new();
	let mut code = code_in;
	for func in funcs {
		let (lg, c) = mavm_codegen_func(func, code, label_gen, string_table)?;
		label_gen = lg;
		code = c;
	}
	Ok(code)
}

fn mavm_codegen_func<'a>(
	func: TypeCheckedFunc, 
	code: &'a mut Vec<Instruction>, 
	mut label_gen: LabelGenerator,
	string_table: &'a StringTable
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
	code.push(Instruction::from_opcode(Opcode::Label(Label::Func(func.name))));

	let num_args = func.args.len();
	let locals = CopyingSymTable::<usize>::new();

	let make_frame_slot = code.len();
	code.push(Instruction::from_opcode(Opcode::Noop));  // placeholder; will replace this later

	let (lg, num_locals, _) = add_args_to_locals_table(locals, &func.args, 0, func.code, code, label_gen, string_table)?;
	label_gen = lg;

	// put makeframe instruction at beginning of function, to build the frame
	code[make_frame_slot] = Instruction::from_opcode(Opcode::MakeFrame(num_args, num_locals));

	return Ok((label_gen, code));
}

fn add_args_to_locals_table<'a>(
	locals: CopyingSymTable<'a, usize>, 
	args: &'a [FuncArg], 
	num_locals: usize,
	statements: Vec<TypeCheckedStatement>,
	code: &mut Vec<Instruction>,
	label_gen: LabelGenerator,
	string_table: &StringTable
) -> Result<(LabelGenerator, usize, bool), CodegenError> {
	if args.len() == 0 {
		mavm_codegen_statements(statements, code, num_locals, &locals, label_gen, string_table)
	} else {
		let new_locals = locals.push_one(args[0].name, num_locals);
		add_args_to_locals_table(new_locals, &args[1..], num_locals+1, statements, code, label_gen, string_table)
	}
}

fn mavm_codegen_statements(
	statements: Vec<TypeCheckedStatement>,  // statements to codegen
	mut code: &mut Vec<Instruction>,        // accumulates the code as it's generator
	mut num_locals: usize,                  // num locals that have been allocated
	locals: &CopyingSymTable<usize>,        // lookup local variable slot number by name
	mut label_gen: LabelGenerator,   
	string_table: &StringTable       
) -> Result<(LabelGenerator, usize, bool), CodegenError> { // (label_gen, num_labels, execution_might_continue)
	if statements.len() == 0 {
		return Ok((label_gen, num_locals, true));
	}
	let rest_of_statements = &statements[1..];
	match &statements[0] {
		TypeCheckedStatement::Noop => 
			mavm_codegen_statements(rest_of_statements.to_vec(), code, num_locals, locals, label_gen, string_table),
		TypeCheckedStatement::ReturnVoid => {
			code.push(Instruction::from_opcode(Opcode::Return));
			Ok((label_gen, num_locals, false))
			// no need to append the rest of the statements; they'll never be executed
		}
		TypeCheckedStatement::Return(expr) => {
			let (lg, c) = mavm_codegen_expr(expr, code, &locals, label_gen, string_table)?;
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
			let (lg, c) = mavm_codegen_expr(expr, code, &new_locals, label_gen, string_table)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, Value::Int(Uint256::from_usize(slot_num))));   
			mavm_codegen_statements(rest_of_statements.to_vec(), code, num_locals, &new_locals, label_gen, string_table)
		}
		TypeCheckedStatement::Loop(body) => {
			let slot_num = Value::Int(Uint256::from_usize(num_locals));
			num_locals = num_locals+1;
			code.push(Instruction::from_opcode(Opcode::GetPC));
			code.push(Instruction::from_opcode_imm(Opcode::SetLocal, slot_num.clone()));
			let (lg, nl, _) = mavm_codegen_statements(body.to_vec(), code, num_locals, locals, label_gen, string_table)?;
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
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Not));
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(end_label.clone())));
			let (lg, nl, _) = mavm_codegen_statements(body.to_vec(), code, num_locals, locals, label_gen, string_table)?;
			label_gen = lg;
			num_locals = nl;
			code.push(Instruction::from_opcode_imm(Opcode::GetLocal, slot_num));
			code.push(Instruction::from_opcode(Opcode::Jump));
			code.push(Instruction::from_opcode(Opcode::Label(end_label)));
			mavm_codegen_statements(rest_of_statements.to_vec(), code, num_locals, locals, label_gen, string_table)
		}
		TypeCheckedStatement::If(cond, tbody, None) => {
			let (end_label, lg) = label_gen.next();
			label_gen = lg;
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::Not));
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(end_label.clone())));
			let (lg, nl, _) = mavm_codegen_statements(tbody.to_vec(), code, num_locals, locals, label_gen, string_table)?;
			label_gen = lg;
			num_locals = nl;
			code.push(Instruction::from_opcode(Opcode::Label(end_label)));
			mavm_codegen_statements(rest_of_statements.to_vec(), code, num_locals, locals, label_gen, string_table)
		}
		TypeCheckedStatement::If(cond, tbody, Some(fbody)) => {
			let (true_label, lg) = label_gen.next();
			let (end_label, lg) = lg.next();
			label_gen = lg;
			let (lg, c) = mavm_codegen_expr(cond, code, &locals, label_gen, string_table)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(true_label.clone())));
			let (lg, nl, might_continue_1) = mavm_codegen_statements(fbody.to_vec(), code, num_locals, locals, label_gen, string_table)?;
			label_gen = lg;
			num_locals = nl;
			if might_continue_1 {
				code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label.clone())));
			}
			code.push(Instruction::from_opcode(Opcode::Label(true_label)));
			let (lg, nl, might_continue_2) = mavm_codegen_statements(tbody.to_vec(), code, num_locals, locals, label_gen, string_table)?;
			label_gen = lg;
			num_locals = nl;
			code.push(Instruction::from_opcode(Opcode::Label(end_label)));
			if might_continue_1 || might_continue_2 {
				mavm_codegen_statements(rest_of_statements.to_vec(), code, num_locals, locals, label_gen, string_table)
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
	string_table: &StringTable
) -> Result<(LabelGenerator, &'a mut Vec<Instruction>), CodegenError> {
	match expr {
		TypeCheckedExpr::UnaryOp(op, tce, _) => {
			let (lg, c) = mavm_codegen_expr(tce, code, locals, label_gen, string_table)?;
			label_gen = lg;
			code = c;
			let opcode = match op {
				UnaryOp::Minus => Opcode::UnaryMinus,
				UnaryOp::BitwiseNeg => Opcode::BitwiseNeg,
				UnaryOp::Not => Opcode::Not,
				UnaryOp::Hash => Opcode::Hash,
				UnaryOp::Len => Opcode::Len,
			};
			code.push(Instruction::from_opcode(opcode));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::Binary(op, tce1, tce2, tipe) => {
			let (lg, c) = mavm_codegen_expr(tce2, code, locals, label_gen, string_table)?;
			let (lg, c) = mavm_codegen_expr(tce1, c, locals, lg, string_table)?;
			label_gen = lg;
			code = c;
			let opcode = match op {     //TODO: distinguish signed from unsigned ops, where needed
				BinaryOp::Plus => Opcode::Plus,
				BinaryOp::Minus => Opcode::Minus,
				BinaryOp::Times => Opcode::Mul,
				BinaryOp::Div => Opcode::Div,
				BinaryOp::Mod => Opcode::Mod,
				BinaryOp::LessThan => Opcode::LessThan,
				BinaryOp::GreaterThan => Opcode::GreaterThan,
				BinaryOp::LessEq => Opcode::LessEq,
				BinaryOp::GreaterEq => Opcode::GreaterEq,
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
			Ok((label_gen, code))
		}
		TypeCheckedExpr::VariableRef(name, _) => {
			match locals.get(*name) {
				Some(n) => {
					code.push(Instruction::from_opcode_imm(Opcode::GetLocal, Value::Int(Uint256::from_usize(n)))); 
					Ok((label_gen, code))
				}
				None => {
					print!("local: {:?}\n", *name);
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
			let (lg, c) = mavm_codegen_expr(tce, code, locals, label_gen, string_table)?;
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
			let val = Value::Int(Uint256::from_string(s));
			code.push(Instruction::from_opcode_imm(Opcode::Noop, val));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::FunctionCall(name, args, _) => {
			let n_args = args.len();
			let (ret_label, lg) = label_gen.next();
			label_gen = lg;
			for i in 0..n_args {
				let (lg, c) = mavm_codegen_expr(&args[n_args-1-i], code, locals, label_gen, string_table)?;
				label_gen = lg;
				code = c;
			}
			code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(ret_label.clone())));
			code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(Label::Func(*name))));
			code.push(Instruction::from_opcode(Opcode::Label(ret_label)));
			Ok((label_gen, code))
		}
		TypeCheckedExpr::ArrayRef(expr1, expr2, _) => {
			let (lg, c) = mavm_codegen_expr(expr2, code, locals, label_gen, string_table)?;
			let (lg, c) = mavm_codegen_expr(expr1, c, locals, lg, string_table)?;
			label_gen = lg;
			code = c;
			code.push(Instruction::from_opcode(Opcode::ArrayGet));
			Ok((label_gen, code))
		}
	}
}

struct LocalsTable {
	next: usize,
	max_next: usize,
	index: HashMap<StringId, usize>,
	push_points: Vec<usize>
}

impl LocalsTable {
	fn new() -> Self {
		LocalsTable{ next: 0, max_next: 0, index: HashMap::new(), push_points: Vec::new() }
	}

	fn push(&mut self) {
		self.push_points.push(self.next);
	}

	fn alloc(&mut self, name: StringId) -> usize {
		let ret = self.next;
		self.index.insert(name, self.next);
		self.next = 1 + self.next;
		if self.max_next < self.next {
			self.max_next = self.next;
		}
		ret
	}

	fn get(&self, name: StringId) -> Option<usize> {
		match self.index.get(&name) {
			Some(ret) => {
				if ret < &self.next {
					Some(*ret)
				} else {
					None
				}
			},
			None => None
		}
	}
}