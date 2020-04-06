use std::collections::HashMap;
use crate::ast::{Type, TopLevelDecl, FuncDecl, FuncDeclKind, FuncArg, Statement, Expr, StructField, UnaryOp, BinaryOp};
use crate::symtable::SymTable;
use crate::stringtable::{StringId, StringTable};
use crate::link::{ExportedFunc, ImportedFunc};
use crate::mavm::{Label, Value};
use crate::uint256::Uint256;
use crate::builtins::builtin_func_decls;

#[derive(Debug)]
pub struct TypeError {
	pub reason: &'static str,
}

pub fn new_type_error(msg: &'static str) -> TypeError {
	TypeError{ reason: msg }
}

#[derive(Debug)]
pub struct TypeCheckedFunc {
	pub name: StringId,
	pub args: Vec<FuncArg>,
	pub ret_type: Type,
	pub code: Vec<TypeCheckedStatement>,
	pub tipe: Type,
	pub imported: bool,
}

#[derive(Debug)]
#[derive(Clone)]
pub enum TypeCheckedStatement {
	Noop,
	Panic,
	ReturnVoid,
	Return(TypeCheckedExpr),
	Let(StringId, TypeCheckedExpr),
	Assign(StringId, TypeCheckedExpr),
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
	TupleRef(Box<TypeCheckedExpr>, Uint256, Type),
	DotRef(Box<TypeCheckedExpr>, StringId, Type),
	ConstUint(Uint256),
	ConstInt(Uint256),
	FunctionCall(StringId, Vec<TypeCheckedExpr>, Type),
	StructInitializer(Vec<TypeCheckedStructField>, Type),
	ArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
	FixedArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, usize, Type),
	Tuple(Vec<TypeCheckedExpr>, Type),
	NewArray(Box<TypeCheckedExpr>, Type, Type),
	NewFixedArray(usize, Option<Box<TypeCheckedExpr>>, Type),
	ArrayMod(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type),
	FixedArrayMod(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, usize, Type),
	StructMod(Box<TypeCheckedExpr>, usize, Box<TypeCheckedExpr>, Type),
	Cast(Box<TypeCheckedExpr>, Type),
	RawValue(Value, Type),
}

impl TypeCheckedExpr {
	pub fn get_type(&self) -> Type {
		match self {
			TypeCheckedExpr::UnaryOp(_, _, t) => t.clone(),
			TypeCheckedExpr::Binary(_, _, _, t) => t.clone(),
			TypeCheckedExpr::VariableRef(_, t) => t.clone(),
			TypeCheckedExpr::TupleRef(_, _, t) => t.clone(),
			TypeCheckedExpr::DotRef(_, _, t) => t.clone(),
			TypeCheckedExpr::ConstUint(_) => Type::Uint,
			TypeCheckedExpr::ConstInt(_) => Type::Int,
			TypeCheckedExpr::FunctionCall(_, _, t) => t.clone(),
			TypeCheckedExpr::StructInitializer(_, t) => t.clone(),
			TypeCheckedExpr::ArrayRef(_, _, t) => t.clone(),
			TypeCheckedExpr::FixedArrayRef(_, _, _, t) => t.clone(),
			TypeCheckedExpr::Tuple(_, t) => t.clone(),
			TypeCheckedExpr::NewArray(_, _, t) => t.clone(),
			TypeCheckedExpr::NewFixedArray(_, _, t) => t.clone(),
			TypeCheckedExpr::ArrayMod(_, _, _, t) => t.clone(),
			TypeCheckedExpr::FixedArrayMod(_, _, _, _, t) => t.clone(),
			TypeCheckedExpr::StructMod(_, _, _, t) => t.clone(),
			TypeCheckedExpr::Cast(_, t) => t.clone(),
			TypeCheckedExpr::RawValue(_, t) => t.clone(),
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

pub fn typecheck_top_level_decls<'a>(
	decls: &[TopLevelDecl], 
	checked_funcs: &mut Vec<TypeCheckedFunc>,
	string_table_in: StringTable<'a>,
) -> Result<(Vec<ExportedFunc>, Vec<ImportedFunc>, StringTable<'a>), TypeError> {
	let mut exported_funcs = Vec::new();
	let mut imported_funcs = Vec::new();
	let mut funcs = Vec::new();
	let mut named_types = HashMap::new();
	let mut hm = HashMap::new();
	let (builtin_fds, string_table) = builtin_func_decls(string_table_in);
	for fd in builtin_fds.iter() {
		hm.insert(fd.name, &fd.tipe);
		imported_funcs.push(ImportedFunc::new(imported_funcs.len(), fd.name, &string_table));
	}
	for decl in decls.iter() {
		match decl {
			TopLevelDecl::TypeDecl(td) => { named_types.insert(td.name, &td.tipe); }
			TopLevelDecl::FuncDecl(fd) => { 
				funcs.push(fd); 
				hm.insert(fd.name, &fd.tipe);
			}
			TopLevelDecl::ImpFuncDecl(fd) => {
				hm.insert(fd.name, &fd.tipe);
				imported_funcs.push(ImportedFunc::new(imported_funcs.len(), fd.name, &string_table));
			}
		}
	}
	let type_table = SymTable::<Type>::new();
	let type_table = type_table.push_multi(named_types);
	let type_table = type_table.push_multi(hm);

	for func in funcs.iter() {
		match func.resolve_types(&type_table) {
			Ok(f) => match typecheck_function(&f, &type_table) {
				Ok(f) => { 
					match func.kind {
						FuncDeclKind::Public => {
							exported_funcs.push(
								ExportedFunc::new(
									f.name, 
									Label::Func(f.name), 
									f.tipe.clone(),
									&string_table,
								)
							);
							checked_funcs.push(f); 
						}
						FuncDeclKind::Private => {
							checked_funcs.push(f);
						}
					}

				}
				Err(e) => { return Err(e); }
			}
			Err(e) => { return Err(e); }
		}
	}

	Ok((exported_funcs, imported_funcs, string_table))
}

pub fn typecheck_function<'a>(
	fd: &'a FuncDecl, 
	type_table: &'a SymTable<'a, Type>
) -> Result<TypeCheckedFunc, TypeError> {
	match fd.kind {
		FuncDeclKind::Public |
		FuncDeclKind::Private => {
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
				tipe: fd.tipe.clone(),
				imported: false,
			})
		}
	}
}

fn typecheck_statement_sequence<'a>(
	statements: &[Statement],
	return_type: &Type,
	type_table: &'a SymTable<'a, Type>
) -> Result<Vec<TypeCheckedStatement>, TypeError> {
	if statements.is_empty() {
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
				println!("return type: {:?}", return_type);
				println!("expr type:   {:?}", tc_expr.get_type());
				Err(new_type_error("return statement has wrong type"))
			}
		},
		Statement::Let(name, expr) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			let tce_type = tc_expr.get_type();
			Ok((TypeCheckedStatement::Let(*name, tc_expr), Some((*name, tce_type))))
		},
		Statement::Assign(name, expr) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			let var_type = match type_table.get(*name) {
				Some(t) => t,
				None => { return Err(new_type_error("assignment to non-existent variable")) }
			};
			if var_type.assignable(&tc_expr.get_type()) {
				Ok((TypeCheckedStatement::Assign(*name, tc_expr), None))
			} else {
				Err(new_type_error("mismatched types in assignment statement"))
			}
		}
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
		Expr::TupleRef(tref, idx) => {
			let tc_sub = typecheck_expr(&*tref, type_table)?;
			let uidx = idx.to_usize().unwrap();
			if let Type::Tuple(tv) = tc_sub.get_type() {
				if uidx < tv.len() {
					Ok(TypeCheckedExpr::TupleRef(Box::new(tc_sub), idx.clone(), tv[uidx].clone()))
				} else {
					Err(new_type_error("tuple field access to non-existent field"))
				}
			} else {
				Err(new_type_error("tuple field access to non-tuple value"))
			}
		}
		Expr::DotRef(sref, name) => {
			let tc_sub = typecheck_expr(&*sref, type_table)?;
			if let Type::Struct(v) = tc_sub.get_type() {
				for sf in v.iter() {
					if *name==sf.name {
						return Ok(TypeCheckedExpr::DotRef(Box::new(tc_sub), *name, sf.tipe.clone()));
					}
				}
				Err(new_type_error("reference to non-existent struct field"))

			} else {
				Err(new_type_error("struct field access to non-struct value"))
			}
		}
		Expr::ConstUint(n) => Ok(TypeCheckedExpr::ConstUint(n.clone())),
		Expr::ConstInt(n) => Ok(TypeCheckedExpr::ConstInt(n.clone())),
		Expr::FunctionCall(name, args) => {
			match type_table.get(*name) {
				Some(Type::Func(arg_types, ret_type)) => {
					let ret_type = ret_type.resolve_types(type_table)?;
					if args.len() == arg_types.len() {
						let mut tc_args = Vec::new();
						for i in 0..args.len() {
							let tc_arg = typecheck_expr(&args[i], type_table)?;
							tc_args.push(tc_arg);
							let resolved_arg_type = arg_types[i].resolve_types(&type_table)?;
							if !resolved_arg_type.assignable(&tc_args[i].get_type()) {
								println!("expected {:?}", resolved_arg_type);
								println!("actual   {:?}", tc_args[i].get_type());
								return Err(new_type_error("wrong argument type in function call"))
							}
						};
						Ok(TypeCheckedExpr::FunctionCall(*name, tc_args, ret_type))
					} else {
						Err(new_type_error("wrong number of args passed to function"))
					}
				},
				_ => Err(new_type_error("call to non-existent function"))
			}
		},
		Expr::ArrayRef(array, index) => {
			let tc_arr = typecheck_expr(&*array, type_table)?;
			let tc_idx = typecheck_expr(&*index, type_table)?;
			match tc_arr.get_type() {
				Type::Array(t) => {
					if tc_idx.get_type() == Type::Uint {
						Ok(TypeCheckedExpr::ArrayRef(Box::new(tc_arr), Box::new(tc_idx), *t))
					} else {
						Err(new_type_error("array index must be Uint"))
					}
				}
				Type::FixedArray(t, sz) => {
					if tc_idx.get_type() == Type::Uint {
						Ok(TypeCheckedExpr::FixedArrayRef(
							Box::new(tc_arr), 
							Box::new(tc_idx),
							sz,
							*t,
						))
					} else {
						Err(new_type_error("fixedarray index must be Uint"))
					}
				}
				_ => Err(new_type_error("fixedarray lookup in non-array type"))
			}
		}
		Expr::NewArray(size_expr, tipe) => Ok(TypeCheckedExpr::NewArray(
			Box::new(typecheck_expr(size_expr, type_table)?), 
			tipe.clone(), 
			Type::Array(Box::new(tipe.clone())),
		)),
		Expr::NewFixedArray(size, maybe_expr) => match maybe_expr {
			Some(expr) => {
				let tc_expr = typecheck_expr(expr, type_table)?;
				Ok(TypeCheckedExpr::NewFixedArray(
					*size, 
					Some(Box::new(tc_expr.clone())), 
					Type::FixedArray(Box::new(tc_expr.get_type()), *size),
				))
			}
			None => Ok(TypeCheckedExpr::NewFixedArray(
				*size,
				None, 
				Type::FixedArray(Box::new(Type::Any), *size),
			))
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
		Expr::ArrayMod(arr, index, val) => {
			let tc_arr = typecheck_expr(arr, type_table)?;
			let tc_index = typecheck_expr(index, type_table)?;
			let tc_val = typecheck_expr(val, type_table)?;
			if tc_index.get_type() != Type::Uint {
				return Err(new_type_error("array or block modifier requires uint index"));
			}
			match tc_arr.get_type() {
				Type::Array(t) => if t.assignable(&tc_val.get_type()) {
					Ok(TypeCheckedExpr::ArrayMod(
						Box::new(tc_arr), 
						Box::new(tc_index), 
						Box::new(tc_val), 
						Type::Array(t)
					))
				} else {
					Err(new_type_error("mismatched types in array modifier"))
				}
				Type::FixedArray(t, sz) => Ok(TypeCheckedExpr::FixedArrayMod(
					Box::new(tc_arr), 
					Box::new(tc_index), 
					Box::new(tc_val),
					sz, 
					Type::FixedArray(t, sz),
				)),
				_ => Err(new_type_error("[] modifier must operate on array or block"))
			}
		}
		Expr::StructMod(struc, name, val) => {
			let tc_struc = typecheck_expr(struc, type_table)?;
			let tc_val = typecheck_expr(val, type_table)?;
			let tcs_type = tc_struc.get_type();
			if let Type::Struct(fields) = &tcs_type {
				match tcs_type.get_struct_slot_by_name(*name) {
					Some(index) => {
						if fields[index].tipe.assignable(&tc_val.get_type()) {
							Ok(TypeCheckedExpr::StructMod(
								Box::new(tc_struc), 
								index, 
								Box::new(tc_val), 
								tcs_type
							))
						} else {
							Err(new_type_error("incorrect value type in struct modifier"))
						}
					}
					None => Err(new_type_error("struct modifier must use valid field name"))
				}
			} else {
				Err(new_type_error("struct modifier must operate on a struct"))
			}
		}
		Expr::UnsafeCast(expr, t) => Ok(TypeCheckedExpr::Cast(Box::new(typecheck_expr(expr, type_table)?), t.clone())),
		Expr::RawValue(v) => Ok(TypeCheckedExpr::RawValue(v.clone(), Type::Any)),
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
			Type::Tuple(_) |
			Type::Array(_) => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Len, Box::new(sub_expr), Type::Uint)),
			_ => Err(new_type_error("invalid operand type for len"))
		},
		UnaryOp::ToUint => match tc_type {
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::ToUint, Box::new(sub_expr), Type::Uint)),
			_ => Err(new_type_error("invalid operand type for uint()"))
		}
		UnaryOp::ToInt => match tc_type {
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::ToInt, Box::new(sub_expr), Type::Int)),
			_ => Err(new_type_error("invalid operand type for int()"))
		}
		UnaryOp::ToBytes32 => match tc_type {
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::ToBytes32, Box::new(sub_expr), Type::Bytes32)),
			_ => Err(new_type_error("invalid operand type for int()"))
		}
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
		BinaryOp::Times => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Int)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		},
		BinaryOp::Div => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::Sdiv, Box::new(tcs1), Box::new(tcs2), Type::Int)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		}
		BinaryOp::Mod => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::Smod, Box::new(tcs1), Box::new(tcs2), Type::Int)),
			_ => Err(new_type_error("invalid argument types to binary op"))
		}			
		BinaryOp::LessThan => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SLessThan, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			_ => Err(new_type_error("invalid argument types to binary op"))			
		}
		BinaryOp::GreaterThan => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SGreaterThan, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			_ => Err(new_type_error("invalid argument types to binary op"))			
		}
		BinaryOp::LessEq => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SLessEq, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			_ => Err(new_type_error("invalid argument types to binary op"))			
		}
		BinaryOp::GreaterEq => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SGreaterEq, Box::new(tcs1), Box::new(tcs2), Type::Bool)),
			_ => Err(new_type_error("invalid argument types to binary op"))			
		}
		BinaryOp::Equal |
		BinaryOp::NotEqual => if (subtype1 == Type::Any) || (subtype2 == Type::Any) || (subtype1 == subtype2) {
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
		BinaryOp::Smod |
		BinaryOp::Sdiv |
		BinaryOp::SLessThan |
		BinaryOp::SGreaterThan |
		BinaryOp::SLessEq |
		BinaryOp::SGreaterEq => { panic!("unexpected op in typecheck_binary_op"); }
	}
}