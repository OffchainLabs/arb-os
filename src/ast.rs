use crate::stringtable::{StringId};
use crate::typetable::TypeTable;
use crate::typecheck::{TypeError, new_type_error};

#[derive(Debug)]
pub enum TopLevelDecl {
	TypeDecl(TypeDecl),
	FuncDecl(FuncDecl),
}

#[derive(Debug)]
pub struct TypeDecl {
	pub name: StringId,
	pub tipe: Type,
}

pub fn new_type_decl(name: StringId, tipe: Type) -> TypeDecl {
	return TypeDecl{ name, tipe };
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Type {
	Void,
	Uint,
	Int,
	Bool,
	Bytes32,
	Tuple(Vec<Type>),
	Array(Box<Type>),
	Struct(Vec<StructField>),
	Named(StringId),
	Func(Vec<Type>, Box<Type>),
	Any,
}

impl Type {
	pub fn resolve_types(&self, type_table: &TypeTable) -> Result<Self, TypeError> {
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
				return Ok(Type::Tuple(rvec));
			}
			Type::Array(t) => Ok(Type::Array(Box::new(t.resolve_types(type_table)?))),
			Type::Struct(vf) => {
				let mut fvec = Vec::new();
				for field in vf.iter() {
					fvec.push(field.resolve_types(type_table)?);
				}
				return Ok(Type::Struct(fvec));
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
}

pub fn types_equal(t1: &Type, t2: &Type) -> bool {
	match (t1, t2) {
		(Type::Void, Type::Void) => true,
		(Type::Uint, Type::Uint) => true,
		(Type::Int, Type::Int) => true,
		(Type::Bool, Type::Bool) => true,
		(Type::Bytes32, Type::Bytes32) => true,
		(Type::Tuple(v1), Type::Tuple(v2)) => type_vectors_equal(&v1, &v2),
		(Type::Array(a1), Type::Array(a2)) => types_equal(&*a1, &*a2),
		(Type::Named(n1), Type::Named(n2)) => (n1 == n2),
		(Type::Func(a1, r1), Type::Func(a2, r2)) => type_vectors_equal(&a1, &a2) && types_equal(&*r1, &*r2),
		(Type::Any, Type::Any) => true,
		(_, _) => false,
	}
}

fn type_vectors_equal(v1: &Vec<Type>, v2: &Vec<Type>) -> bool {
	if v1.len() != v2.len() {
		return false;
	}
	for i in 0..v1.len() {
		if ! types_equal(&v1[i], &v2[i]) {
			return false;
		}
	}
	return true;
}

#[derive(Debug)]
#[derive(Clone)]
pub struct StructField {
	pub name: StringId,
	pub tipe: Type,
}

impl StructField {
	pub fn resolve_types(&self, type_table: &TypeTable) -> Result<Self, TypeError> {
		let t = self.tipe.resolve_types(type_table)?;
		Ok(StructField{ name: self.name, tipe: t })
	}
}

pub fn new_struct_field<'a>(name: StringId, tipe: Type) -> StructField {
	return StructField{ name, tipe };
}

#[derive(Debug)]
#[derive(Clone)]
pub struct FuncArg {
	pub name: StringId,
	pub tipe: Type,
}

impl FuncArg {
	pub fn resolve_types(&self, type_table: &TypeTable) -> Result<Self, TypeError> {
		Ok(FuncArg { name: self.name, tipe: self.tipe.resolve_types(type_table)? })
	}
}

pub fn new_func_arg<'a>(name: StringId, tipe: Type) -> FuncArg {
	return FuncArg{ name, tipe }
}

#[derive(Debug)]
pub struct FuncDecl {
	pub name: StringId,
	pub args: Vec<FuncArg>,
	pub ret_type: Type,
	pub code: Vec<Statement>,
	pub tipe: Type,
}

impl FuncDecl {
	pub fn resolve_types(&self, type_table: &TypeTable) -> Result<Self, TypeError> {
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
			tipe: self.tipe.resolve_types(type_table)?
		})
	}
}

pub fn new_func_decl<'a>(name: StringId, args: Vec<FuncArg>, ret_type: Type, code: Vec<Statement>) -> FuncDecl {
	let mut arg_types = Vec::new();
	let args_vec = args.to_vec();
	for arg in args.iter() {
		arg_types.push(arg.tipe.clone());
	}
	return FuncDecl{ 
		name: name, 
		args: args_vec, 
		ret_type: ret_type.clone(), 
		code: code,
		tipe: Type::Func(arg_types, Box::new(ret_type)),
	};
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Statement {
	Noop,
	Return(Expr),
	Let(StringId, Expr),
	Loop(Vec<Statement>),
	While(Expr, Vec<Statement>),
	If(Expr, Vec<Statement>, Option<Vec<Statement>>),
}

impl Statement {
	pub fn resolve_types(&self, type_table: &TypeTable) -> Result<Self, TypeError> {
		match self {
			Statement::Noop => Ok(Statement::Noop),
			Statement::Return(expr) => Ok(Statement::Return(expr.resolve_types(type_table)?)),
			Statement::Let(name, expr) => Ok(Statement::Let(*name, expr.resolve_types(type_table)?)),
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

	pub fn resolve_types_vec(v: Vec<Self>, type_table: &TypeTable) -> Result<Vec<Self>, TypeError> {
		let mut vr = Vec::new();
		for s in v.iter() {
			vr.push(s.resolve_types(type_table)?);
		}
		Ok(vr)
	}
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Expr {
	UnaryOp(UnaryOp, Box<Expr>),
	Binary(BinaryOp, Box<Expr>, Box<Expr>),
	VariableRef(StringId),
	DotRef(Box<Expr>, StringId),
	ConstUint(StringId),
	FunctionCall(StringId, Vec<Expr>),
	ArrayRef(Box<Expr>, Box<Expr>),
}

impl Expr {
	pub fn resolve_types(&self, type_table: &TypeTable) -> Result<Self, TypeError> {
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
		}
	}
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Copy)]
pub enum UnaryOp {
	Minus,
	BitwiseNeg,
	Not,
	Hash,
	Len,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Copy)]
pub enum BinaryOp {
	Plus,
	Minus,
	Times,
	Div,
	Mod,
	LessThan,
	GreaterThan,
	LessEq,
	GreaterEq,
	Equal,
	NotEqual,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	LogicalAnd,
	LogicalOr,
	Hash,
}

