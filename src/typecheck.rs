use std::collections::HashMap;
use crate::ast::{Type, TopLevelDecl, FuncDecl, FuncDeclKind, FuncArg, Statement, IfArm, Expr, StructField, UnaryOp, BinaryOp};
use crate::symtable::SymTable;
use crate::stringtable::{StringId, StringTable};
use crate::link::{ExportedFunc, ImportedFunc};
use crate::mavm::{Label, Value};
use crate::uint256::Uint256;
use crate::builtins::builtin_func_decls;
use crate::pos::Location;

#[derive(Debug)]
pub struct TypeError {
	pub reason: &'static str,
	pub location: Option<Location>,
}

pub fn new_type_error(msg: &'static str, location: Option<Location>) -> TypeError {
	TypeError{ reason: msg, location }
}

#[derive(Debug)]
pub struct TypeCheckedFunc {
	pub name: StringId,
	pub args: Vec<FuncArg>,
	pub ret_type: Type,
	pub code: Vec<TypeCheckedStatement>,
	pub tipe: Type,
	pub imported: bool,
	pub location: Option<Location>,
}

#[derive(Debug, Clone)]
pub enum TypeCheckedStatement {
	Noop(Option<Location>),
	Panic(Option<Location>),
	ReturnVoid(Option<Location>),
	Return(TypeCheckedExpr, Option<Location>),
	Let(StringId, TypeCheckedExpr, Option<Location>),
	Assign(StringId, TypeCheckedExpr, Option<Location>),
	Loop(Vec<TypeCheckedStatement>, Option<Location>),
	While(TypeCheckedExpr, Vec<TypeCheckedStatement>, Option<Location>),
	If(Vec<TypeCheckedIfArm>, Option<Location>),
}

#[derive(Debug, Clone)]
pub enum TypeCheckedIfArm {
	Cond(TypeCheckedExpr, Vec<TypeCheckedStatement>),
	Catchall(Vec<TypeCheckedStatement>),
}

#[derive(Debug, Clone)]
pub enum TypeCheckedExpr {
	UnaryOp(UnaryOp, Box<TypeCheckedExpr>, Type, Option<Location>),
	Binary(BinaryOp, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type, Option<Location>),
	ShortcutOr(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Option<Location>),
	ShortcutAnd(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Option<Location>),
	VariableRef(StringId, Type, Option<Location>),
	TupleRef(Box<TypeCheckedExpr>, Uint256, Type, Option<Location>),
	DotRef(Box<TypeCheckedExpr>, StringId, Type, Option<Location>),
	ConstUint(Uint256, Option<Location>),
	ConstInt(Uint256, Option<Location>),
	FunctionCall(StringId, Vec<TypeCheckedExpr>, Type, Option<Location>),
	StructInitializer(Vec<TypeCheckedStructField>, Type, Option<Location>),
	ArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type, Option<Location>),
	FixedArrayRef(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, usize, Type, Option<Location>),
	Tuple(Vec<TypeCheckedExpr>, Type, Option<Location>),
	NewArray(Box<TypeCheckedExpr>, Type, Type, Option<Location>),
	NewFixedArray(usize, Option<Box<TypeCheckedExpr>>, Type, Option<Location>),
	ArrayMod(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Type, Option<Location>),
	FixedArrayMod(Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, Box<TypeCheckedExpr>, usize, Type, Option<Location>),
	StructMod(Box<TypeCheckedExpr>, usize, Box<TypeCheckedExpr>, Type, Option<Location>),
	Cast(Box<TypeCheckedExpr>, Type, Option<Location>),
	RawValue(Value, Type, Option<Location>),
}

impl<'a> TypeCheckedExpr {
	pub fn get_type(&self) -> Type {
		match self {
			TypeCheckedExpr::UnaryOp(_, _, t, _) => t.clone(),
			TypeCheckedExpr::Binary(_, _, _, t, _) => t.clone(),
			TypeCheckedExpr::ShortcutOr(_, _, _) |
			TypeCheckedExpr::ShortcutAnd(_, _, _) => Type::Bool,
			TypeCheckedExpr::VariableRef(_, t, _) => t.clone(),
			TypeCheckedExpr::TupleRef(_, _, t, _) => t.clone(),
			TypeCheckedExpr::DotRef(_, _, t, _) => t.clone(),
			TypeCheckedExpr::ConstUint(_, _) => Type::Uint,
			TypeCheckedExpr::ConstInt(_, _) => Type::Int,
			TypeCheckedExpr::FunctionCall(_, _, t, _) => t.clone(),
			TypeCheckedExpr::StructInitializer(_, t, _) => t.clone(),
			TypeCheckedExpr::ArrayRef(_, _, t, _) => t.clone(),
			TypeCheckedExpr::FixedArrayRef(_, _, _, t, _) => t.clone(),
			TypeCheckedExpr::Tuple(_, t, _) => t.clone(),
			TypeCheckedExpr::NewArray(_, _, t, _) => t.clone(),
			TypeCheckedExpr::NewFixedArray(_, _, t, _) => t.clone(),
			TypeCheckedExpr::ArrayMod(_, _, _, t, _) => t.clone(),
			TypeCheckedExpr::FixedArrayMod(_, _, _, _, t, _) => t.clone(),
			TypeCheckedExpr::StructMod(_, _, _, t, _) => t.clone(),
			TypeCheckedExpr::Cast(_, t, _) => t.clone(),
			TypeCheckedExpr::RawValue(_, t, _) => t.clone(),
		}
	}

	pub fn get_location(&self) -> Option<Location> {
		match self {
			TypeCheckedExpr::UnaryOp(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::Binary(_, _, _, _, loc) => loc.clone(),
			TypeCheckedExpr::ShortcutOr(_, _, loc) => loc.clone(),
			TypeCheckedExpr::ShortcutAnd(_, _, loc) => loc.clone(),
			TypeCheckedExpr::VariableRef(_, _, loc) => loc.clone(),
			TypeCheckedExpr::TupleRef(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::DotRef(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::ConstUint(_, loc) => loc.clone(),
			TypeCheckedExpr::ConstInt(_, loc) => loc.clone(),
			TypeCheckedExpr::FunctionCall(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::StructInitializer(_, _, loc) => loc.clone(),
			TypeCheckedExpr::ArrayRef(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::FixedArrayRef(_, _, _, _, loc) => loc.clone(),
			TypeCheckedExpr::Tuple(_, _, loc) => loc.clone(),
			TypeCheckedExpr::NewArray(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::NewFixedArray(_, _, _, loc) => loc.clone(),
			TypeCheckedExpr::ArrayMod(_, _, _, _, loc) => loc.clone(),
			TypeCheckedExpr::FixedArrayMod(_, _, _, _, _, loc) => loc.clone(),
			TypeCheckedExpr::StructMod(_, _, _, _, loc) => loc.clone(),
			TypeCheckedExpr::Cast(_, _, loc) => loc.clone(),
			TypeCheckedExpr::RawValue(_, _, loc) => loc.clone(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct TypeCheckedStructField {
	pub name: StringId,
	pub value: TypeCheckedExpr,
}

impl<'a> TypeCheckedStructField {
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
			TopLevelDecl::ImpTypeDecl(itd) => {
				named_types.insert(itd.name, &itd.tipe);
			}
		}
	}
	let type_table = SymTable::<Type>::new();
	let type_table = type_table.push_multi(named_types);
	let type_table = type_table.push_multi(hm);

	for func in funcs.iter() {
		match func.resolve_types(&type_table, func.location) {
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
				location: fd.location,
			})
		}
	}
}

fn typecheck_statement_sequence<'a>(
	statements: &'a [Statement],
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
	statement: &'a Statement,
	return_type: &Type,
	type_table: &'a SymTable<'a, Type>
) -> Result<(TypeCheckedStatement, Option<(StringId, Type)>), TypeError> {
	match statement {
		Statement::Noop(loc) => Ok((TypeCheckedStatement::Noop(loc.clone()), None)),
		Statement::Panic(loc) => Ok((TypeCheckedStatement::Panic(loc.clone()), None)),
		Statement::ReturnVoid(loc) => Ok((TypeCheckedStatement::ReturnVoid(loc.clone()), None)),
		Statement::Return(expr, loc) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			if return_type.assignable(&tc_expr.get_type()) {
				Ok((TypeCheckedStatement::Return(tc_expr, loc.clone()), None))
			} else {
				println!("return type: {:?}", return_type);
				println!("expr type:   {:?}", tc_expr.get_type());
				Err(new_type_error("return statement has wrong type", *loc))
			}
		},
		Statement::Let(name, expr, loc) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			let tce_type = tc_expr.get_type();
			Ok((TypeCheckedStatement::Let(*name, tc_expr, loc.clone()), Some((*name, tce_type))))
		},
		Statement::Assign(name, expr, loc) => {
			let tc_expr = typecheck_expr(expr, type_table)?;
			let var_type = match type_table.get(*name) {
				Some(t) => t,
				None => { return Err(new_type_error("assignment to non-existent variable", *loc)) }
			};
			if var_type.assignable(&tc_expr.get_type()) {
				Ok((TypeCheckedStatement::Assign(*name, tc_expr, loc.clone()), None))
			} else {
				Err(new_type_error("mismatched types in assignment statement", *loc))
			}
		}
		Statement::Loop(body, loc) => {
			let tc_body = typecheck_statement_sequence(body, return_type, type_table)?;
			Ok((TypeCheckedStatement::Loop(tc_body, loc.clone()), None))
		}
		Statement::While(cond, body, loc) => {
			let tc_cond = typecheck_expr(cond, type_table)?;
			match tc_cond.get_type() {
				Type::Bool => {
					let tc_body = typecheck_statement_sequence(body, return_type, type_table)?;
					Ok((TypeCheckedStatement::While(tc_cond, tc_body, loc.clone()), None))
				},
				_ => Err(new_type_error("while condition is not bool", *loc)),
			}
		}
		Statement::If(arms, loc) => {
			let mut tc_arms = Vec::new();
			for arm in arms {
				match arm {
					IfArm::Cond(cond, body) => {
						let tc_cond = typecheck_expr(&cond, type_table)?;
						match tc_cond.get_type() {
							Type::Bool => {
								tc_arms.push(TypeCheckedIfArm::Cond(
									tc_cond,
									typecheck_statement_sequence(body, return_type, type_table)?,
								));
							}
							_ => { return Err(new_type_error("if/elseif condition is not bool", *loc)); }
						}
					}
					IfArm::Catchall(body) => {
						tc_arms.push(TypeCheckedIfArm::Catchall(
							typecheck_statement_sequence(body, return_type, type_table)?,
						));
					}
				}
			}
			Ok((TypeCheckedStatement::If(tc_arms, loc.clone()), None))
		}
	}
}

fn typecheck_expr(
	expr: &Expr,
	type_table: &SymTable<Type>
) -> Result<TypeCheckedExpr, TypeError> {
	match expr {
		Expr::UnaryOp(op, subexpr, loc) => {
			let tc_sub = typecheck_expr(subexpr, type_table)?;
			typecheck_unary_op(*op, tc_sub, loc.clone())
		},
		Expr::Binary(op, sub1, sub2, loc) => {
			let tc_sub1 = typecheck_expr(sub1, type_table)?;
			let tc_sub2 = typecheck_expr(sub2, type_table)?;
			typecheck_binary_op(*op, tc_sub1, tc_sub2, loc.clone())
		},
		Expr::ShortcutOr(sub1, sub2, loc) => {
			let tc_sub1 = typecheck_expr(sub1, type_table)?;
			let tc_sub2 = typecheck_expr(sub2, type_table)?;
			if tc_sub1.get_type() != Type::Bool {
				return Err(new_type_error("operands to logical or must be boolean", *loc));
			}
			if tc_sub2.get_type() != Type::Bool {
				return Err(new_type_error("operands to logical or must be boolean", *loc));
			}
			Ok(TypeCheckedExpr::ShortcutOr(Box::new(tc_sub1), Box::new(tc_sub2), loc.clone()))
		}
		Expr::ShortcutAnd(sub1, sub2, loc) => {
			let tc_sub1 = typecheck_expr(sub1, type_table)?;
			let tc_sub2 = typecheck_expr(sub2, type_table)?;
			if tc_sub1.get_type() != Type::Bool {
				return Err(new_type_error("operands to logical and must be boolean", *loc));
			}
			if tc_sub2.get_type() != Type::Bool {
				return Err(new_type_error("operands to logical and must be boolean", *loc));
			}
			Ok(TypeCheckedExpr::ShortcutAnd(Box::new(tc_sub1), Box::new(tc_sub2), loc.clone()))
		}
		Expr::VariableRef(name, loc) => match type_table.get(*name) {
			None => {
				Err(new_type_error("referenced non-existent variable", *loc))
			},
			Some(t) => Ok(TypeCheckedExpr::VariableRef(*name, t.clone(), loc.clone()))
		}
		Expr::TupleRef(tref, idx, loc) => {
			let tc_sub = typecheck_expr(&*tref, type_table)?;
			let uidx = idx.to_usize().unwrap();
			if let Type::Tuple(tv) = tc_sub.get_type() {
				if uidx < tv.len() {
					Ok(TypeCheckedExpr::TupleRef(Box::new(tc_sub), idx.clone(), tv[uidx].clone(), loc.clone()))
				} else {
					Err(new_type_error("tuple field access to non-existent field", *loc))
				}
			} else {
				Err(new_type_error("tuple field access to non-tuple value", *loc))
			}
		}
		Expr::DotRef(sref, name, loc) => {
			let tc_sub = typecheck_expr(&*sref, type_table)?;
			if let Type::Struct(v) = tc_sub.get_type() {
				for sf in v.iter() {
					if *name==sf.name {
						return Ok(TypeCheckedExpr::DotRef(Box::new(tc_sub), *name, sf.tipe.clone(), loc.clone()));
					}
				}
				Err(new_type_error("reference to non-existent struct field", *loc))

			} else {
				Err(new_type_error("struct field access to non-struct value", *loc))
			}
		}
		Expr::ConstUint(n, loc) => Ok(TypeCheckedExpr::ConstUint(n.clone(), loc.clone())),
		Expr::ConstInt(n, loc) => Ok(TypeCheckedExpr::ConstInt(n.clone(), loc.clone())),
		Expr::FunctionCall(name, args, loc) => {
			match type_table.get(*name) {
				Some(Type::Func(arg_types, ret_type)) => {
					let ret_type = ret_type.resolve_types(type_table, *loc)?;
					if args.len() == arg_types.len() {
						let mut tc_args = Vec::new();
						for i in 0..args.len() {
							let tc_arg = typecheck_expr(&args[i], type_table)?;
							tc_args.push(tc_arg);
							let resolved_arg_type = arg_types[i].resolve_types(&type_table, *loc)?;
							if !resolved_arg_type.assignable(&tc_args[i].get_type()) {
								println!("expected {:?}", resolved_arg_type);
								println!("actual   {:?}", tc_args[i].get_type());
								return Err(new_type_error("wrong argument type in function call", *loc))
							}
						};
						Ok(TypeCheckedExpr::FunctionCall(*name, tc_args, ret_type, loc.clone()))
					} else {
						Err(new_type_error("wrong number of args passed to function", *loc))
					}
				},
				_ => Err(new_type_error("call to non-existent function", *loc))
			}
		},
		Expr::ArrayRef(array, index, loc) => {
			let tc_arr = typecheck_expr(&*array, type_table)?;
			let tc_idx = typecheck_expr(&*index, type_table)?;
			match tc_arr.get_type() {
				Type::Array(t) => {
					if tc_idx.get_type() == Type::Uint {
						Ok(TypeCheckedExpr::ArrayRef(Box::new(tc_arr), Box::new(tc_idx), *t, loc.clone()))
					} else {
						Err(new_type_error("array index must be Uint", *loc))
					}
				}
				Type::FixedArray(t, sz) => {
					if tc_idx.get_type() == Type::Uint {
						Ok(TypeCheckedExpr::FixedArrayRef(
							Box::new(tc_arr), 
							Box::new(tc_idx),
							sz,
							*t,
							loc.clone()
						))
					} else {
						Err(new_type_error("fixedarray index must be Uint", *loc))
					}
				}
				_ => Err(new_type_error("fixedarray lookup in non-array type", *loc))
			}
		}
		Expr::NewArray(size_expr, tipe, loc) => Ok(TypeCheckedExpr::NewArray(
			Box::new(typecheck_expr(size_expr, type_table)?), 
			tipe.clone(), 
			Type::Array(Box::new(tipe.clone())),
			loc.clone()
		)),
		Expr::NewFixedArray(size, maybe_expr, loc) => match maybe_expr {
			Some(expr) => {
				let tc_expr = typecheck_expr(expr, type_table)?;
				Ok(TypeCheckedExpr::NewFixedArray(
					*size, 
					Some(Box::new(tc_expr.clone())), 
					Type::FixedArray(Box::new(tc_expr.get_type()), *size),
					loc.clone(),
				))
			}
			None => Ok(TypeCheckedExpr::NewFixedArray(
				*size,
				None, 
				Type::FixedArray(Box::new(Type::Any), *size),
				loc.clone(),
			))
		}
		Expr::StructInitializer(fieldvec, loc) => {
			let mut tc_fields = Vec::new();
			let mut tc_fieldtypes = Vec::new();
			for field in fieldvec {
				let tc_expr = typecheck_expr(&field.value, type_table)?;
				tc_fields.push(TypeCheckedStructField::new(field.name, tc_expr.clone()));
				tc_fieldtypes.push(StructField::new(field.name, tc_expr.get_type()));
			}
			Ok(TypeCheckedExpr::StructInitializer(tc_fields, Type::Struct(tc_fieldtypes), loc.clone()))
		}
		Expr::Tuple(fields, loc) => {
			let mut tc_fields = Vec::new();
			let mut types = Vec::new();
			for field in fields {
				let tc_field = typecheck_expr(field, type_table)?;
				types.push(tc_field.get_type().clone());
				tc_fields.push(tc_field);
			}
			Ok(TypeCheckedExpr::Tuple(tc_fields, Type::Tuple(types), loc.clone()))
		}
		Expr::ArrayMod(arr, index, val, loc) => {
			let tc_arr = typecheck_expr(arr, type_table)?;
			let tc_index = typecheck_expr(index, type_table)?;
			let tc_val = typecheck_expr(val, type_table)?;
			if tc_index.get_type() != Type::Uint {
				return Err(new_type_error("array or block modifier requires uint index", *loc));
			}
			match tc_arr.get_type() {
				Type::Array(t) => if t.assignable(&tc_val.get_type()) {
					Ok(TypeCheckedExpr::ArrayMod(
						Box::new(tc_arr), 
						Box::new(tc_index), 
						Box::new(tc_val), 
						Type::Array(t),
						loc.clone()
					))
				} else {
					Err(new_type_error("mismatched types in array modifier", *loc))
				}
				Type::FixedArray(t, sz) => Ok(TypeCheckedExpr::FixedArrayMod(
					Box::new(tc_arr), 
					Box::new(tc_index), 
					Box::new(tc_val),
					sz, 
					Type::FixedArray(t, sz),
					loc.clone(),
				)),
				_ => Err(new_type_error("[] modifier must operate on array or block", *loc))
			}
		}
		Expr::StructMod(struc, name, val, loc) => {
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
								tcs_type,
								loc.clone()
							))
						} else {
							Err(new_type_error("incorrect value type in struct modifier", *loc))
						}
					}
					None => Err(new_type_error("struct modifier must use valid field name", *loc))
				}
			} else {
				Err(new_type_error("struct modifier must operate on a struct", *loc))
			}
		}
		Expr::UnsafeCast(expr, t, loc) => Ok(TypeCheckedExpr::Cast(Box::new(typecheck_expr(expr, type_table)?), t.clone(), loc.clone())),
		Expr::RawValue(v, loc) => Ok(TypeCheckedExpr::RawValue(v.clone(), Type::Any, loc.clone())),
	}
}

fn typecheck_unary_op<'a>(
	op: UnaryOp,
	sub_expr: TypeCheckedExpr,
	loc: Option<Location>,
) -> Result<TypeCheckedExpr, TypeError> {
	let tc_type = sub_expr.get_type();
	match op {
		UnaryOp::Minus => match tc_type {
			Type::Int => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Minus, Box::new(sub_expr), Type::Int, loc)),
			_ => Err(new_type_error("invalid operand type for unary minus", loc))
		},
		UnaryOp::BitwiseNeg => match tc_type {
			Type::Uint => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::BitwiseNeg, Box::new(sub_expr), Type::Uint, loc)),
			Type::Int => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::BitwiseNeg, Box::new(sub_expr), Type::Int, loc)),
			_ => Err(new_type_error("invalid operand type for bitwise negation", loc))
		},
		UnaryOp::Not => match tc_type {
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Not, Box::new(sub_expr), Type::Bool, loc)),
			_ => Err(new_type_error("invalid operand type for logical negation", loc))
		},
		UnaryOp::Hash => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Hash, Box::new(sub_expr), Type::Bytes32, loc)),
		UnaryOp::Len => match tc_type {
			Type::Tuple(_) |
			Type::Array(_) => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::Len, Box::new(sub_expr), Type::Uint, loc)),
			_ => Err(new_type_error("invalid operand type for len", loc))
		},
		UnaryOp::ToUint => match tc_type {
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::ToUint, Box::new(sub_expr), Type::Uint, loc)),
			_ => Err(new_type_error("invalid operand type for uint()", loc))
		}
		UnaryOp::ToInt => match tc_type {
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::ToInt, Box::new(sub_expr), Type::Int, loc)),
			_ => Err(new_type_error("invalid operand type for int()", loc))
		}
		UnaryOp::ToBytes32 => match tc_type {
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Ok(TypeCheckedExpr::UnaryOp(UnaryOp::ToBytes32, Box::new(sub_expr), Type::Bytes32, loc)),
			_ => Err(new_type_error("invalid operand type for int()", loc))
		}
	}
}

fn typecheck_binary_op<'a>(
	op: BinaryOp,
	tcs1: TypeCheckedExpr,
	tcs2: TypeCheckedExpr,
	loc: Option<Location>,
) -> Result<TypeCheckedExpr, TypeError> {
	let subtype1 = tcs1.get_type();
	let subtype2 = tcs2.get_type();
	match op {
		BinaryOp::Plus | 
		BinaryOp::Minus |
		BinaryOp::Times => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Int, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))
		},
		BinaryOp::Div => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::Sdiv, Box::new(tcs1), Box::new(tcs2), Type::Int, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))
		}
		BinaryOp::Mod => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::Smod, Box::new(tcs1), Box::new(tcs2), Type::Int, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))
		}			
		BinaryOp::LessThan => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SLessThan, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))			
		}
		BinaryOp::GreaterThan => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SGreaterThan, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))			
		}
		BinaryOp::LessEq => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SLessEq, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))			
		}
		BinaryOp::GreaterEq => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(BinaryOp::SGreaterEq, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))			
		}
		BinaryOp::Equal |
		BinaryOp::NotEqual => if (subtype1 == Type::Any) || (subtype2 == Type::Any) || (subtype1 == subtype2) {
				Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc))
			} else {
				Err(new_type_error("invalid argument types to binary op", loc))
			},
		BinaryOp::BitwiseAnd |
		BinaryOp::BitwiseOr |
		BinaryOp::BitwiseXor => match (subtype1, subtype2) {
			(Type::Uint, Type::Uint) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Uint, loc)),
			(Type::Int, Type::Int) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Int, loc)),
			(Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bytes32, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))
		},
		BinaryOp::LogicalAnd | 
		BinaryOp::LogicalOr => match (subtype1, subtype2) {
			(Type::Bool, Type::Bool) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bool, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))
		},
		BinaryOp::Hash => match (subtype1, subtype2) {
			(Type::Bytes32, Type::Bytes32) => Ok(TypeCheckedExpr::Binary(op, Box::new(tcs1), Box::new(tcs2), Type::Bytes32, loc)),
			_ => Err(new_type_error("invalid argument types to binary op", loc))
		}
		BinaryOp::Smod |
		BinaryOp::Sdiv |
		BinaryOp::SLessThan |
		BinaryOp::SGreaterThan |
		BinaryOp::SLessEq |
		BinaryOp::SGreaterEq => { panic!("unexpected op in typecheck_binary_op"); }
	}
}