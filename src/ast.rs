use crate::stringtable::StringId;
use crate::symtable::SymTable;
use crate::typecheck::{TypeError, new_type_error};
use serde::{Serialize, Deserialize};


#[derive(Debug, Clone)]
pub enum TopLevelDecl {
	TypeDecl(TypeDecl),
	FuncDecl(FuncDecl),
	ImpFuncDecl(ImportFuncDecl),
}

impl TopLevelDecl {
	pub fn concat_vecs(a: &mut Vec<Self>, b: &mut Vec<Self>) -> Vec<Self> {
		a.append(b);
		a.to_vec()
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
	Any,
}

impl Type {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		match self {
			Type::Void |
			Type::Uint |
			Type::Int |
			Type::Bool |
			Type::Bytes32 |
			Type::Any => Ok(self.clone()),
			Type::Tuple(tvec) => {
				let mut rvec = Vec::new();
				for t in tvec.iter() {
					rvec.push(t.resolve_types(type_table)?);
				}
				Ok(Type::Tuple(rvec))
			}
			Type::Array(t) => Ok(Type::Array(Box::new(t.resolve_types(type_table)?))),
			Type::FixedArray(t, s) => Ok(Type::FixedArray(Box::new(t.resolve_types(type_table)?), *s)),
			Type::Struct(vf) => {
				let mut fvec = Vec::new();
				for field in vf.iter() {
					fvec.push(field.resolve_types(type_table)?);
				}
				Ok(Type::Struct(fvec))
			},
			Type::Named(name) => {
				match type_table.get(*name) {
					Some(t) => Ok(t.resolve_types(type_table)?),
					None => { 
						Err(new_type_error("referenced non-existent type name"))
					}
				}
			}
			Type::Func(args, ret) => {
				let rret = ret.resolve_types(type_table)?;
				let mut rargs = Vec::new();
				for arg in args.iter() {
					rargs.push(arg.resolve_types(type_table)?);
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
			Type::Bytes32 => (self == rhs),
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

	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		let t = self.tipe.resolve_types(type_table)?;
		Ok(StructField{ name: self.name, tipe: t })
	}
}

#[derive(Debug, Clone)]
pub struct FuncArg {
	pub name: StringId,
	pub tipe: Type,
}

impl FuncArg {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		Ok(FuncArg { name: self.name, tipe: self.tipe.resolve_types(type_table)? })
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
}

impl FuncDecl {
	pub fn new(name: StringId, args: Vec<FuncArg>, ret_type: Type, code: Vec<Statement>, exported: bool) -> Self {
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
			kind: if exported { FuncDeclKind::Public } else { FuncDeclKind::Private }
		}
	}

	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		let mut rargs = Vec::new();
		for arg in self.args.iter() {
			rargs.push(arg.resolve_types(type_table)?);
		}
		let mut rcode = Vec::new();
		for stat in self.code.iter() {
			rcode.push(stat.resolve_types(type_table)?);
		}
		Ok(FuncDecl{
			name: self.name,
			args: rargs,
			ret_type: self.ret_type.resolve_types(type_table)?,
			code: rcode,
			tipe: self.tipe.resolve_types(type_table)?,
			kind: self.kind,
		})
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Noop,
	Panic,
	ReturnVoid,
	Return(Expr),
	Let(StringId, Expr),
	Assign(StringId, Expr),
	Loop(Vec<Statement>),
	While(Expr, Vec<Statement>),
	If(Expr, Vec<Statement>, Option<Vec<Statement>>),
}

impl Statement {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		match self {
			Statement::Noop => Ok(Statement::Noop),
			Statement::Panic => Ok(Statement::Panic),
			Statement::ReturnVoid => Ok(Statement::ReturnVoid),
			Statement::Return(expr) => Ok(Statement::Return(expr.resolve_types(type_table)?)),
			Statement::Let(name, expr) => Ok(Statement::Let(*name, expr.resolve_types(type_table)?)),
			Statement::Assign(name, expr) => Ok(Statement::Assign(*name, expr.resolve_types(type_table)?)),
			Statement::Loop(body) => Ok(
				Statement::Loop(Statement::resolve_types_vec(body.to_vec(), type_table)?)
			),
			Statement::While(c, body) => Ok(
				Statement::While(c.resolve_types(type_table)?, Statement::resolve_types_vec(body.to_vec(), type_table)?)
			),
			Statement::If(c, tbody, None) => Ok(
				Statement::If(c.resolve_types(type_table)?, Statement::resolve_types_vec(tbody.to_vec(), type_table)?, None)
			),
			Statement::If(c, tbody, Some(fbody)) => Ok(
				Statement::If(
					c.resolve_types(type_table)?, 
					Statement::resolve_types_vec(tbody.to_vec(), type_table)?,
					Some(Statement::resolve_types_vec(fbody.to_vec(), type_table)?)
				)
			),
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
pub enum Expr {
	UnaryOp(UnaryOp, Box<Expr>),
	Binary(BinaryOp, Box<Expr>, Box<Expr>),
	VariableRef(StringId),
	DotRef(Box<Expr>, StringId),
	ConstUint(StringId),
	ConstInt(StringId),
	FunctionCall(StringId, Vec<Expr>),
	ArrayRef(Box<Expr>, Box<Expr>),
	StructInitializer(Vec<FieldInitializer>),
	Tuple(Vec<Expr>),
	NewFixedArray(usize, Option<Box<Expr>>),
	ArrayMod(Box<Expr>, Box<Expr>, Box<Expr>),
	StructMod(Box<Expr>, StringId, Box<Expr>),
	UnsafeCast(Box<Expr>, Type),
}

impl Expr {
	pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
		match self {
			Expr::UnaryOp(op, be) => Ok(Expr::UnaryOp(*op, Box::new(be.resolve_types(type_table)?))),
			Expr::Binary(op, be1, be2) => Ok(Expr::Binary(
				*op, 
				Box::new(be1.resolve_types(type_table)?),
				Box::new(be2.resolve_types(type_table)?)
			)),
			Expr::VariableRef(name) => Ok(Expr::VariableRef(*name)),
			Expr::DotRef(be, name) => Ok(Expr::DotRef(Box::new(be.resolve_types(type_table)?), *name)),
			Expr::ConstUint(s) => Ok(Expr::ConstUint(*s)),
			Expr::ConstInt(s) => Ok(Expr::ConstInt(*s)),
			Expr::FunctionCall(name, args) => {
				let mut rargs = Vec::new();
				for arg in args.iter() {
					rargs.push(arg.resolve_types(type_table)?);
				}
				Ok(Expr::FunctionCall(*name, rargs))
			},
			Expr::ArrayRef(e1, e2) => Ok(Expr::ArrayRef(
				Box::new(e1.resolve_types(type_table)?), 
				Box::new(e2.resolve_types(type_table)?)
			)),
			Expr::StructInitializer(fields) => {
				let mut rfields = Vec::new();
				for field in fields.iter() {
					rfields.push(FieldInitializer::new(field.name, field.value.resolve_types(type_table)?));
				}
				Ok(Expr::StructInitializer(rfields))
			}
			Expr::Tuple(evec) => {
				let mut rvec = Vec::new();
				for expr in evec {
					rvec.push(expr.resolve_types(type_table)?);
				}
				Ok(Expr::Tuple(rvec))
			}
			Expr::NewFixedArray(sz, init_expr) => match &*init_expr {
				Some(expr) => Ok(Expr::NewFixedArray(
					*sz, 
					Some(Box::new(expr.resolve_types(type_table)?))
				)),
				None => Ok(Expr::NewFixedArray(*sz, None)),
			}
			Expr::ArrayMod(e1, e2, e3) => Ok(Expr::ArrayMod(
				Box::new(e1.resolve_types(type_table)?),
				Box::new(e2.resolve_types(type_table)?),
				Box::new(e3.resolve_types(type_table)?),
			)),
			Expr::StructMod(e1, i, e3) => Ok(Expr::StructMod(
				Box::new(e1.resolve_types(type_table)?),
				*i,
				Box::new(e3.resolve_types(type_table)?),
			)),
			Expr::UnsafeCast(be, t) => Ok(Expr::UnsafeCast(
				Box::new(be.resolve_types(type_table)?), 
				t.resolve_types(type_table)?
			)),
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

impl FieldInitializer {
	pub fn new(name: StringId, value: Expr) -> Self {
		FieldInitializer{ name, value }
	}
}
