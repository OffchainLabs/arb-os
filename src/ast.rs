use crate::stringtable::StringId;
use crate::symtable::SymTable;
use crate::typecheck::{TypeError, new_type_error};
use crate::mavm::Value;
use crate::uint256::Uint256;
use crate::xformcode::{self, TUPLE_SIZE};
use crate::pos::Location;
use serde::{Serialize, Deserialize};


#[derive(Debug, Clone)]
pub enum TopLevelDecl {
	TypeDecl(TypeDecl),
	FuncDecl(FuncDecl),
	ImpFuncDecl(ImportFuncDecl),
	ImpTypeDecl(ImportTypeDecl),
}

impl TopLevelDecl {
	pub fn concat_vecs(a: Vec<Self>, b: &Vec<Self>) -> Vec<Self> {
		let mut aa = a.clone();
		let mut bb = b.clone();
		aa.append(&mut bb);
		aa.to_vec()
	}
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
	pub name: StringId,
	pub tipe: Type,
}

pub fn new_type_decl(name: StringId, tipe: Type) -> TypeDecl {
	TypeDecl{ name, tipe }
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub enum Type {
	Void,
	Uint,
	Int,
	Bool,
	Bytes32,
	Tuple(Vec<Type>),
	Array(Box<Type>),
	FixedArray(Box<Type>, usize),
	Struct(Vec<StructField>),
	Named(StringId),
	Func(Vec<Type>, Box<Type>),
	Imported(StringId),
	Any,
}

impl Type {
	pub fn resolve_types(&self, type_table: &SymTable<Type>, location: Option<Location>) -> Result<Self, TypeError> {
		match self {
			Type::Void |
			Type::Uint |
			Type::Int |
			Type::Bool |
			Type::Bytes32 |
			Type::Imported(_) |
			Type::Any => Ok(self.clone()),
			Type::Tuple(tvec) => {
				let mut rvec = Vec::new();
				for t in tvec.iter() {
					rvec.push(t.resolve_types(type_table, location)?);
				}
				Ok(Type::Tuple(rvec))
			}
			Type::Array(t) => Ok(Type::Array(Box::new(t.resolve_types(type_table, location)?))),
			Type::FixedArray(t, s) => Ok(Type::FixedArray(Box::new(t.resolve_types(type_table, location)?), *s)),
			Type::Struct(vf) => {
				let mut fvec = Vec::new();
				for field in vf.iter() {
					fvec.push(field.resolve_types(type_table, location)?);
				}
				Ok(Type::Struct(fvec))
			},
			Type::Named(name) => {
				match type_table.get(*name) {
					Some(t) => Ok(t.resolve_types(type_table, location)?),
					None => { 
						Err(new_type_error("referenced non-existent type name", location))
					}
				}
			}
			Type::Func(args, ret) => {
				let rret = ret.resolve_types(type_table, location)?;
				let mut rargs = Vec::new();
				for arg in args.iter() {
					rargs.push(arg.resolve_types(type_table, location)?);
				}
				Ok(Type::Func(rargs, Box::new(rret)))
			}
		}
	}

	pub fn get_struct_slot_by_name(&self, name: StringId) -> Option<usize> {
		match self {
			Type::Struct(fields) => {
				for (i, field) in fields.iter().enumerate() {
					if field.name == name {
						return Some(i);
					}
				}
				None
			}
			_ => None
		}
	}

	pub fn assignable(&self, rhs: &Self) -> bool {
		match self {
			Type::Any => true,
			Type::Void |
			Type::Uint |
			Type::Int |
			Type::Bool |
			Type::Bytes32 |
			Type::Imported(_) => (self == rhs),
			Type::Tuple(tvec) => {
				if let Type::Tuple(tvec2) = rhs {
					type_vectors_assignable(tvec, tvec2)
				} else {
					false
				}
			}
			Type::Array(t) => {
				if let Type::Array(t2) = rhs {
					t.assignable(t2)
				} else {
					false
				}
			}
			Type::FixedArray(t, s) => {
				if let Type::FixedArray(t2, s2) = rhs {
					(s == s2) && t.assignable(t2)
				} else {
					false
				}
			}
			Type::Struct(fields) => {
				if let Type::Struct(fields2) = rhs {
					field_vectors_assignable(fields, fields2)
				} else {
					false
				}
			}
			Type::Named(_) => (self == rhs),
			Type::Func(args, ret) => {
				if let Type::Func(args2, ret2) = rhs {
					arg_vectors_assignable(args, args2) && (ret2.assignable(ret))  // note: rets in reverse order
				} else {
					false
				}
			}
		}
	}

	pub fn default_value(&self) -> Value {
		match self {
			Type::Void => { panic!("tried to get default value for void type"); }
			Type::Uint |
			Type::Int |
			Type::Bytes32 |
			Type::Bool => Value::Int(Uint256::zero()),
			Type::Tuple(tvec) => {
				let mut default_tup = Vec::new();
				for t in tvec {
					default_tup.push(t.default_value());
				}
				Value::Tuple(default_tup)
			}
			Type::Array(t) => Value::Tuple(vec![
				Value::Int(Uint256::one()),
				Value::Int(Uint256::one()),
				Value::Tuple(vec![t.default_value()]),
			]),
			Type::FixedArray(t, sz) => {
				let default_val = t.default_value();
				let mut val = Value::Tuple(vec![default_val; 8]);
				let mut chunk_size = 1;
				while chunk_size*TUPLE_SIZE < *sz {
					val = Value::Tuple(vec![val; 8]);
					chunk_size *= 8;
				}
				val
			}
			Type::Struct(fields) => {
				let mut vals = Vec::new();
				for field in fields {
					vals.push(field.tipe.default_value());
				}
				xformcode::value_from_field_list(vals)
			}
			Type::Named(_) => {
				panic!("tried to get default value for a named type");
			}
			Type::Func(_, _) => {
				panic!("tried to get default value for a function type");
			}
			Type::Imported(_) => {
				panic!("tried to get default value for an imported type");
			}
			Type::Any => Value::none(),
		}
	}
}

pub fn type_vectors_assignable(tvec1: &[Type], tvec2: &[Type]) -> bool {
	if tvec1.len() != tvec2.len() {
		return false;
	}
	for (i, t1) in tvec1.iter().enumerate() {
		if ! t1.assignable(&tvec2[i]) {
			return false;
		}
	}
	true
}

pub fn arg_vectors_assignable(tvec1: &[Type], tvec2: &[Type]) -> bool {
	if tvec1.len() != tvec2.len() {
		return false;
	}
	for (i, t1) in tvec1.iter().enumerate() {
		if ! t1.assignable(&tvec2[i]) {
			return false;
		}
	}
	true
}

pub fn field_vectors_assignable(tvec1: &[StructField], tvec2: &[StructField]) -> bool {
	if tvec1.len() != tvec2.len() {
		return false;
	}
	for (i, t1) in tvec1.iter().enumerate() {
		if ! t1.tipe.assignable(&tvec2[i].tipe) {
			return false;
		}
	}
	true
}

impl PartialEq for Type {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Type::Void, Type::Void) |
			(Type::Uint, Type::Uint) |
			(Type::Int, Type::Int) |
			(Type::Bool, Type::Bool) |
			(Type::Bytes32, Type::Bytes32) |
			(Type::Any, Type::Any) => true,
			(Type::Tuple(v1), Type::Tuple(v2)) => type_vectors_equal(&v1, &v2),
			(Type::Array(a1), Type::Array(a2)) => *a1 == *a2,
			(Type::FixedArray(a1, s1), Type::FixedArray(a2, s2)) => (s1 == s2) && (*a1 == *a2),
			(Type::Struct(f1), Type::Struct(f2)) => struct_field_vectors_equal(&f1, &f2),
			(Type::Named(n1), Type::Named(n2)) => (n1 == n2),
			(Type::Func(a1, r1), Type::Func(a2, r2)) => type_vectors_equal(&a1, &a2) && (*r1 == *r2),
			(Type::Imported(n1), Type::Imported(n2)) => (n1 == n2),
			(_, _) => false,
		}
	}
}

fn type_vectors_equal(v1: &[Type], v2: &[Type]) -> bool {
	if v1.len() != v2.len() {
		return false;
	}
	for i in 0..v1.len() {
		if v1[i] != 
		v2[i] {
			return false;
		}
	}
	true
}

fn struct_field_vectors_equal(f1: &[StructField], f2: &[StructField]) -> bool {
	if f1.len() != f2.len() {
		return false;
	}
	for i in 0..f1.len() {
		if f1[i] != f2[i] {
			return false;
		}
	}
	true
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructField {
	pub name: StringId,
	pub tipe: Type,
}

impl StructField {
	pub fn new(name: StringId, tipe: Type) -> StructField {
		StructField{ name, tipe }
	}

	pub fn resolve_types(&self, type_table: &SymTable<Type>, location: Option<Location>) -> Result<Self, TypeError> {
		let t = self.tipe.resolve_types(type_table, location)?;
		Ok(StructField{ name: self.name, tipe: t })
	}
}

#[derive(Debug, Clone)]
pub struct FuncArg {
	pub name: StringId,
	pub tipe: Type,
}

impl FuncArg {
	pub fn resolve_types(&self, type_table: &SymTable<Type>, location: Option<Location>) -> Result<Self, TypeError> {
		Ok(FuncArg { name: self.name, tipe: self.tipe.resolve_types(type_table, location)? })
	}
}

pub fn new_func_arg(name: StringId, tipe: Type) -> FuncArg {
	FuncArg{ name, tipe }
}

#[derive(Debug, Clone)]
pub struct ImportFuncDecl {
	pub name: StringId,
	pub arg_types: Vec<Type>,
	pub ret_type: Type,
	pub tipe: Type,
}

impl ImportFuncDecl {
	pub fn new(name: StringId, args: Vec<FuncArg>, ret_type: Type) -> Self {
		let mut arg_types = Vec::new();
		for arg in args.iter() {
			arg_types.push(arg.tipe.clone());
		}
		ImportFuncDecl{ 
			name, 
			arg_types: arg_types.clone(),
			ret_type: ret_type.clone(), 
			tipe: Type::Func(arg_types, Box::new(ret_type)),
		}
	}

	pub fn new_types(name: StringId, arg_types: Vec<Type>, ret_type: Type) -> Self {
		ImportFuncDecl{ 
			name, 
			arg_types: arg_types.clone(),
			ret_type: ret_type.clone(), 
			tipe: Type::Func(arg_types, Box::new(ret_type)),
		}
	}
}

#[derive(Clone, Debug)]
pub struct ImportTypeDecl {
	pub name: StringId,
	pub tipe: Type,
}

impl ImportTypeDecl {
	pub fn new(name: StringId) -> Self {
		ImportTypeDecl{ name, tipe: Type::Imported(name) }
	}
}

#[derive(Debug, Clone, Copy)]
pub enum FuncDeclKind {
	Public,
	Private,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
	pub name: StringId,
	pub args: Vec<FuncArg>,
	pub ret_type: Type,
	pub code: Vec<Statement>,
	pub tipe: Type,
	pub kind: FuncDeclKind,
	pub location: Option<Location>,
}

impl FuncDecl {
	pub fn new(
		name: StringId, 
		args: Vec<FuncArg>, 
		ret_type: Type, 
		code: Vec<Statement>, 
		exported: bool, 
		location: Option<Location>,
	) -> Self {
		let mut arg_types = Vec::new();
		let args_vec = args.to_vec();
		for arg in args.iter() {
			arg_types.push(arg.tipe.clone());
		}
		FuncDecl{ 
			name, 
			args: args_vec, 
			ret_type: ret_type.clone(), 
			code,
			tipe: Type::Func(arg_types, Box::new(ret_type)),
			kind: if exported { FuncDeclKind::Public } else { FuncDeclKind::Private },
			location,
		}
	}

	pub fn resolve_types(&self, type_table: &SymTable<Type>, location: Option<Location>) -> Result<Self, TypeError> {
		let mut rargs = Vec::new();
		for arg in self.args.iter() {
			rargs.push(arg.resolve_types(type_table, location)?);
		}
		let mut rcode = Vec::new();
		for stat in self.code.iter() {
			rcode.push(stat.resolve_types(type_table)?);
		}
		Ok(FuncDecl{
			name: self.name,
			args: rargs,
			ret_type: self.ret_type.resolve_types(type_table, location)?,
			code: rcode,
			tipe: self.tipe.resolve_types(type_table, location)?,
			kind: self.kind,
			location: self.location,
		})
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Noop(Option<Location>),
	Panic(Option<Location>),
	ReturnVoid(Option<Location>),
	Return(Expr, Option<Location>),
	Let(StringId, Expr, Option<Location>),
	Assign(StringId, Expr, Option<Location>),
	Loop(Vec<Statement>, Option<Location>),
	While(Expr, Vec<Statement>, Option<Location>),
	If(Vec<IfArm>, Option<Location>),
}

impl<'a> Statement {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		match self {
			Statement::Noop(loc) => Ok(Statement::Noop(loc.clone())),
			Statement::Panic(loc) => Ok(Statement::Panic(loc.clone())),
			Statement::ReturnVoid(loc) => Ok(Statement::ReturnVoid(loc.clone())),
			Statement::Return(expr, loc) => Ok(Statement::Return(expr.resolve_types(type_table)?, loc.clone())),
			Statement::Let(name, expr, loc) => Ok(Statement::Let(*name, expr.resolve_types(type_table)?, loc.clone())),
			Statement::Assign(name, expr, loc) => Ok(Statement::Assign(*name, expr.resolve_types(type_table)?, loc.clone())),
			Statement::Loop(body, loc) => Ok(
				Statement::Loop(Statement::resolve_types_vec(body.to_vec(), type_table)?, loc.clone())
			),
			Statement::While(c, body, loc) => Ok(
				Statement::While(c.resolve_types(type_table)?, Statement::resolve_types_vec(body.to_vec(), type_table)?, loc.clone())
			),
			Statement::If(arms, loc) => {
				let mut res_arms = Vec::new();
				for arm in arms {
					res_arms.push(arm.resolve_types(type_table)?);
				}
				Ok(Statement::If(res_arms, loc.clone()))
			}
		}
	}

	pub fn resolve_types_vec(v: Vec<Self>, type_table: &SymTable<Type>) -> Result<Vec<Self>, TypeError> {
		let mut vr = Vec::new();
		for s in v.iter() {
			vr.push(s.resolve_types(type_table)?);
		}
		Ok(vr)
	}
}

#[derive(Debug, Clone)]
pub enum IfArm {
	Cond(Expr, Vec<Statement>),
	Catchall(Vec<Statement>),
}

impl IfArm {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		match self {
			IfArm::Cond(cond, body) => Ok(IfArm::Cond(
				cond.resolve_types(type_table)?,
				Statement::resolve_types_vec(body.to_vec(), type_table)?,
			)),
			IfArm::Catchall(body) => Ok(IfArm::Catchall(
				Statement::resolve_types_vec(body.to_vec(), type_table)?,
			))
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expr {
	UnaryOp(UnaryOp, Box<Expr>, Option<Location>),
	Binary(BinaryOp, Box<Expr>, Box<Expr>, Option<Location>),
	ShortcutOr(Box<Expr>, Box<Expr>, Option<Location>),
	ShortcutAnd(Box<Expr>, Box<Expr>, Option<Location>),
	VariableRef(StringId, Option<Location>),
	TupleRef(Box<Expr>, Uint256, Option<Location>),
	DotRef(Box<Expr>, StringId, Option<Location>),
	ConstUint(Uint256, Option<Location>),
	ConstInt(Uint256, Option<Location>),
	FunctionCall(StringId, Vec<Expr>, Option<Location>),
	ArrayRef(Box<Expr>, Box<Expr>, Option<Location>),
	StructInitializer(Vec<FieldInitializer>, Option<Location>),
	Tuple(Vec<Expr>, Option<Location>),
	NewArray(Box<Expr>, Type, Option<Location>),
	NewFixedArray(usize, Option<Box<Expr>>, Option<Location>),
	ArrayMod(Box<Expr>, Box<Expr>, Box<Expr>, Option<Location>),
	StructMod(Box<Expr>, StringId, Box<Expr>, Option<Location>),
	UnsafeCast(Box<Expr>, Type, Option<Location>),
	RawValue(Value, Option<Location>),
}

impl<'a> Expr {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		match self {
			Expr::UnaryOp(op, be, loc) => Ok(Expr::UnaryOp(*op, Box::new(be.resolve_types(type_table)?), loc.clone())),
			Expr::Binary(op, be1, be2, loc) => Ok(Expr::Binary(
				*op, 
				Box::new(be1.resolve_types(type_table)?),
				Box::new(be2.resolve_types(type_table)?),
				loc.clone()
			)),
			Expr::ShortcutOr(be1, be2, loc) => Ok(Expr::ShortcutOr(
				Box::new(be1.resolve_types(type_table)?),
				Box::new(be2.resolve_types(type_table)?),
				loc.clone()
			)),
			Expr::ShortcutAnd(be1, be2, loc) => Ok(Expr::ShortcutAnd(
				Box::new(be1.resolve_types(type_table)?),
				Box::new(be2.resolve_types(type_table)?),
				loc.clone()
			)),
			Expr::VariableRef(name, loc) => Ok(Expr::VariableRef(*name, loc.clone())),
			Expr::TupleRef(be, idx, loc) => Ok(Expr::TupleRef(Box::new(be.resolve_types(type_table)?), idx.clone(), loc.clone())),
			Expr::DotRef(be, name, loc) => Ok(Expr::DotRef(Box::new(be.resolve_types(type_table)?), *name, loc.clone())),
			Expr::ConstUint(s, loc) => Ok(Expr::ConstUint(s.clone(), loc.clone())),
			Expr::ConstInt(s, loc) => Ok(Expr::ConstInt(s.clone(), loc.clone())),
			Expr::FunctionCall(name, args, loc) => {
				let mut rargs = Vec::new();
				for arg in args.iter() {
					rargs.push(arg.resolve_types(type_table)?);
				}
				Ok(Expr::FunctionCall(*name, rargs, loc.clone()))
			},
			Expr::ArrayRef(e1, e2, loc) => Ok(Expr::ArrayRef(
				Box::new(e1.resolve_types(type_table)?), 
				Box::new(e2.resolve_types(type_table)?),
				loc.clone()
			)),
			Expr::StructInitializer(fields, loc) => {
				let mut rfields = Vec::new();
				for field in fields.iter() {
					rfields.push(FieldInitializer::new(field.name, field.value.resolve_types(type_table)?));
				}
				Ok(Expr::StructInitializer(rfields, loc.clone()))
			}
			Expr::Tuple(evec, loc) => {
				let mut rvec = Vec::new();
				for expr in evec {
					rvec.push(expr.resolve_types(type_table)?);
				}
				Ok(Expr::Tuple(rvec, loc.clone()))
			}
			Expr::NewArray(sz, tipe, loc) => Ok(Expr::NewArray(
				Box::new(sz.resolve_types(type_table)?), 
				tipe.resolve_types(type_table, *loc)?,
				loc.clone()
			)),
			Expr::NewFixedArray(sz, init_expr, loc) => match &*init_expr {
				Some(expr) => Ok(Expr::NewFixedArray(
					*sz, 
					Some(Box::new(expr.resolve_types(type_table)?)),
					loc.clone()
				)),
				None => Ok(Expr::NewFixedArray(*sz, None, loc.clone())),
			}
			Expr::ArrayMod(e1, e2, e3, loc) => Ok(Expr::ArrayMod(
				Box::new(e1.resolve_types(type_table)?),
				Box::new(e2.resolve_types(type_table)?),
				Box::new(e3.resolve_types(type_table)?),
				loc.clone()
			)),
			Expr::StructMod(e1, i, e3, loc) => Ok(Expr::StructMod(
				Box::new(e1.resolve_types(type_table)?),
				*i,
				Box::new(e3.resolve_types(type_table)?),
				loc.clone()
			)),
			Expr::UnsafeCast(be, t, loc) => Ok(Expr::UnsafeCast(
				Box::new(be.resolve_types(type_table)?), 
				t.resolve_types(type_table, *loc)?,
				loc.clone()
			)),
			Expr::RawValue(v, loc) => Ok(Expr::RawValue(v.clone(), loc.clone())),
		}
	}

	pub fn get_location(&self) -> Option<Location> {
		match self {
			Expr::UnaryOp(_, _, loc) => loc.clone(),
			Expr::Binary(_, _, _, loc) => loc.clone(),
			Expr::ShortcutOr(_, _, loc) => loc.clone(),
			Expr::ShortcutAnd(_, _, loc) => loc.clone(),
			Expr::VariableRef(_, loc) => loc.clone(),
			Expr::TupleRef(_, _, loc) => loc.clone(),
			Expr::DotRef(_, _, loc) => loc.clone(),
			Expr::ConstUint(_, loc) => loc.clone(),
			Expr::ConstInt(_, loc) => loc.clone(),
			Expr::FunctionCall(_, _, loc) => loc.clone(),
			Expr::ArrayRef(_, _, loc) => loc.clone(),
			Expr::StructInitializer(_, loc) => loc.clone(),
			Expr::Tuple(_, loc) => loc.clone(),
			Expr::NewArray(_, _, loc) => loc.clone(),
			Expr::NewFixedArray(_, _, loc) => loc.clone(),
			Expr::ArrayMod(_, _, _, loc) => loc.clone(),
			Expr::StructMod(_, _, _, loc) => loc.clone(),
			Expr::UnsafeCast(_, _, loc) => loc.clone(),
			Expr::RawValue(_, loc) => loc.clone(),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
	Minus,
	BitwiseNeg,
	Not,
	Hash,
	Len,
	ToUint,
	ToInt,
	ToBytes32,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
	Plus,
	Minus,
	Times,
	Div,
	Mod,
	Sdiv,
	Smod,
	LessThan,
	GreaterThan,
	LessEq,
	GreaterEq,
	SLessThan,
	SGreaterThan,
	SLessEq,
	SGreaterEq,
	Equal,
	NotEqual,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	LogicalAnd,
	LogicalOr,
	Hash,
}

#[derive(Debug, Clone)]
pub struct FieldInitializer {
	pub name: StringId,
	pub value: Expr,
}

impl<'a> FieldInitializer {
	pub fn new(name: StringId, value: Expr) -> Self {
		FieldInitializer{ name, value }
	}
}
