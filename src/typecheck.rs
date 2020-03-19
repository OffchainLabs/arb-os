use std::collections::HashMap;
use crate::ast::{Type, TopLevelDecl, FuncDecl, FuncArg, Statement, Expr, StructField, UnaryOp, BinaryOp};
use crate::symtable::SymTable;
use crate::stringtable::StringId;

#[derive(Debug)]
pub struct TypeError {
	reason: &'static str,
}

pub fn new_type_error(msg: &'static str) -> TypeError {
	return TypeError{ reason: msg };
}

#[derive(Debug)]
pub struct TypeCheckedFunc {
	pub name: StringId,
	pub args: Vec<FuncArg>,
	pub ret_type: Type,
	pub code: Vec<TypeCheckedStatement>,
	pub tipe: Type,
}

#[derive(Debug)]
#[derive(Clone)]
pub enum TypeCheckedStatement {
	Noop,
	Panic,
	ReturnVoid,
	Return(TypeCheckedExpr),
	Let(StringId, TypeCheckedExpr),
	Loop(Vec<TypeCheckedStatement>),
	While(TypeCheckedExpr, Vec<TypeCheckedStatement>),
	If(TypeCheckedExpr, Vec<TypeCheckedStatement>, Option<Vec<TypeCheckedStatement>>),
}

#[derive(Debug)]
#[derive(Clone)]
pub enum TypeCheckedExpr {
	UnaryOp(UnaryOp, Box<TypeCheckedExpr>, Type),
	Binary(BinaryOp, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
	VariableRef(StringId, Type),
	DotRef(Box<TypeCheckedExpr>, StringId, Type),
	ConstUint(StringId),
	FunctionCall(StringId, Vec<TypeCheckedExpr>, Type),
	StructInitializer(Vec<TypeCheckedStructField>, Type),
	ArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
	BlockRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>),
	Tuple(Vec<TypeCheckedExpr>, Type),
	NewBlock(Option<Box<TypeCheckedExpr>>),
	Cast(Box<TypeCheckedExpr>, Type),
}

impl TypeCheckedExpr {
	pub fn get_type(&self) -> Type {
		match self {
			TypeCheckedExpr::UnaryOp(_, _, t) => t.clone(),
			TypeCheckedExpr::Binary(_, _, _, t) => t.clone(),
			TypeCheckedExpr::VariableRef(_, t) => t.clone(),
			TypeCheckedExpr::DotRef(_, _, t) => t.clone(),
			TypeCheckedExpr::ConstUint(_) => Type::Uint,
			TypeCheckedExpr::FunctionCall(_, _, t) => t.clone(),
			TypeCheckedExpr::StructInitializer(_, t) => t.clone(),
			TypeCheckedExpr::ArrayRef(_, _, t) => t.clone(),
			TypeCheckedExpr::BlockRef(_, _) => Type::Any,
			TypeCheckedExpr::Tuple(_, t) => t.clone(),
			TypeCheckedExpr::NewBlock(_) => Type::Block,
			TypeCheckedExpr::Cast(_, t) => t.clone(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct TypeCheckedStructField {
	pub name: StringId,
	pub value: TypeCheckedExpr,
}

impl TypeCheckedStructField {
	pub fn new(name: StringId, value: TypeCheckedExpr) -> Self {
		TypeCheckedStructField{ name, value }
	}
}

pub fn typecheck_top_level_decls(
	decls: &Vec<TopLevelDecl>, 
	checked_funcs: &mut Vec<TypeCheckedFunc>,
) -> Option<TypeError> {
	let mut funcs = Vec::new();
	let mut named_types = HashMap::new();
	let mut hm = HashMap::new();
	for decl in decls.iter() {
		match decl {
			TopLevelDecl::TypeDecl(td) => { named_types.insert(td.name, &td.tipe); }
			TopLevelDecl::FuncDecl(fd) => { 
				funcs.push(fd); 
				hm.insert(fd.name, &fd.tipe);
			}
		}
	}
	let type_table = SymTable::<Type>::new();
	let type_table = type_table.push_multi(named_types);
	let type_table = type_table.push_multi(hm);

	for func in funcs.iter() {
		match func.resolve_types(&type_table) {
			Ok(f) => match typecheck_function(&f, &type_table) {
				Ok(f) => { checked_funcs.push(f); }
				Err(e) => { return Some(e); }
			}
			Err(e) => { return Some(e); }
		}
	}

	return None;
}

pub fn typecheck_function<'a>(
	fd: &'a FuncDecl, 
	type_table: &'a SymTable<'a, Type>
) -> Result<TypeCheckedFunc, TypeError> {
	let mut hm = HashMap::new();
	for arg in fd.args.iter() {
		hm.insert(arg.name, &arg.tipe);
	}
	let inner_type_table = type_table.push_multi(hm);
	let tc_stats = typecheck_statement_sequence(&fd.code, &fd.ret_type, &inner_type_table)?;
	Ok(TypeCheckedFunc{ 
		name: fd.name, 
		args: fd.args.clone(), 
		ret_type: fd.ret_type.clone(), 
		code: tc_stats, 
		tipe: fd.tipe.clone()
	})
}

fn typecheck_statement_sequence<'a>(
	statements: &[Statement],
	return_type: &Type,
	type_table: &'a SymTable<'a, Type>
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
	if statements.len() == 0 {
		return Ok(Vec::new());
	}
	let first_stat = &statements[0];
	let rest_of_stats = &statements[1..];

	match typecheck_statement(first_stat, return_type, type_table)? {
		(tcs, None) => {
			let mut rest_result = typecheck_statement_sequence(rest_of_stats, return_type, type_table)?;
			rest_result.insert(0, tcs);
			Ok(rest_result)
		},
		(tcs, Some((sid, tipe))) => {
			let inner_type_table = type_table.push_one(sid, &tipe);
			let mut rest_result = typecheck_statement_sequence(rest_of_stats, return_type, &inner_type_table)?;
			rest_result.insert(0, tcs);
			Ok(rest_result)
		}
	}
}

fn typecheck_statement<'a>(
	statement: &Statement,
	return_type: &Type,
	type_table: &'a SymTable<'a, Type>
) -> Result<(TypeCheckedStatement, Option<(StringId, Type)>), TypeError> {
	match statement {
		Statement::Noop => Ok((TypeCheckedStatement::Noop, None)),
		Statement::Panic => Ok((TypeCheckedStatement::Panic, None)),
		Statement::ReturnVoid => Ok((TypeCheckedStatement::ReturnVoid, None)),
		Statement::Return(expr) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			if return_type.assignable(&tc_expr.get_type()) {
				Ok((TypeCheckedStatement::Return(tc_expr), None))
			} else {
				Err(new_type_error("return statement has wrong type"))
			}
		},
		Statement::Let(name, expr) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			let tce_type = tc_expr.get_type();
			Ok((TypeCheckedStatement::Let(*name, tc_expr), Some((*name, tce_type))))
		},
		Statement::Loop(body) => {
			let tc_body = typecheck_statement_sequence(body, return_type, type_table)?;
			Ok((TypeCheckedStatement::Loop(tc_body), None))
		}
		Statement::While(cond, body) => {
			let tc_cond = typecheck_expr(cond, type_table)?;
			match tc_cond.get_type() {
				Type::Bool => {
					let tc_body = typecheck_statement_sequence(body, return_type, type_table)?;
					Ok((TypeCheckedStatement::While(tc_cond, tc_body), None))
				},
				_ => Err(new_type_error("while condition is not bool")),
			}
		}
		Statement::If(cond, tbody, ofbody) => {
			let tc_cond = typecheck_expr(cond, type_table)?;
			match tc_cond.get_type() {
				Type::Bool => {
					let tc_tbody = typecheck_statement_sequence(tbody, return_type, type_table)?;
					let o_tc_fbody = match ofbody {
						None => None,
						Some(fbody) => {
							let tc_fbody = typecheck_statement_sequence(fbody, return_type, type_table)?;
							Some(tc_fbody)
						}
					};
					Ok((TypeCheckedStatement::If(tc_cond, tc_tbody, o_tc_fbody), None))
				},
				_ => Err(new_type_error("if condition is not bool")),
			}
		}
	}
}

fn typecheck_expr(
	expr: &Expr,
	type_table: &SymTable<Type>
) -> Result<TypeCheckedExpr, TypeError> {
	match expr {
		Expr::UnaryOp(op, subexpr) => {
			let tc_sub = typecheck_expr(subexpr, type_table)?;
			typecheck_unary_op(*op, tc_sub)
		},
		Expr::Binary(op, sub1, sub2) => {
			let tc_sub1 = typecheck_expr(sub1, type_table)?;
			let tc_sub2 = typecheck_expr(sub2, type_table)?;
			typecheck_binary_op(*op, tc_sub1, tc_sub2)
		},
		Expr::VariableRef(name) => match type_table.get(*name) {
			None => {
				Err(new_type_error("referenced non-existent variable"))
			},
			Some(t) => Ok(TypeCheckedExpr::VariableRef(*name, t.clone()))
		}
		Expr::DotRef(sref, name) => {
			let tc_sub = typecheck_expr(&*sref, type_table)?;
			if let Type::Struct(v) = tc_sub.get_type() {
				for sf in v.iter() {
					if *name==sf.name {
						return Ok(TypeCheckedExpr::DotRef(Box::new(tc_sub), *name, sf.tipe.clone()));
					}
				}
			} 
			return Err(new_type_error("reference to non-existent struct field"));
		}
		Expr::ConstUint(n) => Ok(TypeCheckedExpr::ConstUint(*n)),
		Expr::FunctionCall(name, args) => {
			match type_table.get(*name) {
				Some(Type::Func(arg_types, ret_type)) => {
					if args.len() == arg_types.len() {
						let mut tc_args = Vec::new();
						for i in 0..args.len() {
							let tc_arg = typecheck_expr(&args[i], type_table)?;
							tc_args.push(tc_arg);
							if &tc_args[i].get_type() != &arg_types[i] {
								return Err(new_type_error("wrong argument type in function call"))
							}
						};
						return Ok(TypeCheckedExpr::FunctionCall(*name, tc_args, *ret_type.clone()));
					} else {
						return Err(new_type_error("wrong number of args passed to function"));
					}
				},
				_ => { return Err(new_type_error("call to non-existent function")); }
			}
		},
		Expr::ArrayRef(array, index) => {
			let tc_arr = typecheck_expr(&*array, type_table)?;
			let tc_idx = typecheck_expr(&*index, type_table)?;
			match tc_arr.get_type() {
				Type::Array(t) => {
					if &tc_idx.get_type() == &Type::Uint {
						Ok(TypeCheckedExpr::ArrayRef(Box::new(tc_arr), Box::new(tc_idx), *t))
					} else {
						Err(new_type_error("array index must be Uint"))
					}
				}
				Type::Block => {
					if &tc_idx.get_type() == &Type::Uint {
						Ok(TypeCheckedExpr::BlockRef(Box::new(tc_arr), Box::new(tc_idx)))
					} else {
						Err(new_type_error("block index must be Uint"))
					}
				}
				_ => Err(new_type_error("array lookup in non-array type"))
			}
		}
		Expr::StructInitializer(fieldvec) => {
			let mut tc_fields = Vec::new();
			let mut tc_fieldtypes = Vec::new();
			for field in fieldvec {
				let tc_expr = typecheck_expr(&field.value, type_table)?;
				tc_fields.push(TypeCheckedStructField::new(field.name, tc_expr.clone()));
				tc_fieldtypes.push(StructField::new(field.name, tc_expr.get_type()));
			}
			Ok(TypeCheckedExpr::StructInitializer(tc_fields, Type::Struct(tc_fieldtypes)))
		}
		Expr::Tuple(fields) => {
			let mut tc_fields = Vec::new();
			let mut types = Vec::new();
			for field in fields {
				let tc_field = typecheck_expr(field, type_table)?;
				types.push(tc_field.get_type().clone());
				tc_fields.push(tc_field);
			}
			Ok(TypeCheckedExpr::Tuple(tc_fields, Type::Tuple(types)))
		}
		Expr::NewBlock(bo_expr) => match &*bo_expr {
			Some(expr) => Ok(TypeCheckedExpr::NewBlock(Some(Box::new(typecheck_expr(&expr, type_table)?)))),
			None => Ok(TypeCheckedExpr::NewBlock(None)),
		}
		Expr::UnsafeCast(expr, t) => Ok(TypeCheckedExpr::Cast(Box::new(typecheck_expr(expr, type_table)?), t.clone())),
	}
}

fn typecheck_unary_op(
	op: UnaryOp,
	sub_expr: TypeCheckedExpr,
) -> Result<TypeCheckedExpr, TypeError> {
	let tc_type = sub_expr.get_type();
	match op {
		UnaryOp::Minus => match tc_type {
			Type::Int => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Minus, Box::new(sub_expr), Type::Int)),
			_ => Err(new_type_error("invalid operand type for unary minus"))
		},
		UnaryOp::BitwiseNeg => match tc_type {
			Type::Uint => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::BitwiseNeg, Box::new(sub_expr), Type::Uint)),
			Type::Int => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::BitwiseNeg, Box::new(sub_expr), Type::Int)),
			_ => Err(new_type_error("invalid operand type for bitwise negation"))
		},
		UnaryOp::Not => match tc_type {
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Not, Box::new(sub_expr), Type::Bool)),
			_ => Err(new_type_error("invalid operand type for logical negation"))
		},
		UnaryOp::Hash => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Hash, Box::new(sub_expr), Type::Bytes32)),
		UnaryOp::Len => match tc_type {
			Type::Tuple(_) => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Len, Box::new(sub_expr), Type::Uint)),
			Type::Array(_) => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Len, Box::new(sub_expr), Type::Uint)),
			_ => Err(new_type_error("invalid operand type for len"))
		},
	}
}

fn typecheck_binary_op(
	op: BinaryOp,
	tcs1: TypeCheckedExpr,
	tcs2: TypeCheckedExpr
) -> Result<TypeCheckedExpr, TypeError> {
	let subtype1 = tcs1.get_type();
	let subtype2 = tcs2.get_type();
	match op {
		BinaryOp::Plus | 
		BinaryOp::Minus |
		BinaryOp::Times | 
		BinaryOp::Div |
		BinaryOp::Mod => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Int)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		},
		BinaryOp::LessThan |
		BinaryOp::GreaterThan | 
		BinaryOp::LessEq |
		BinaryOp::GreaterEq => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) |
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		},
		BinaryOp::Equal |
		BinaryOp::NotEqual => if &subtype1 == &subtype2 {
				Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool))
			} else {
				Err(new_type_error("invalid argument types to binary op"))
			},
		BinaryOp::BitwiseAnd |
		BinaryOp::BitwiseOr |
		BinaryOp::BitwiseXor => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Int)),
			(Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bytes32)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		},
		BinaryOp::LogicalAnd | 
		BinaryOp::LogicalOr => match (subtype1, subtype2) {
			(Type::Bool, Type::Bool) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		},
		BinaryOp::Hash => match (subtype1, subtype2) {
			(Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bytes32)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		}
	}
}