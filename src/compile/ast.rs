/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//!Contains types and utilities for constructing the mini AST

use super::symtable::SymTable;
use super::typecheck::{new_type_error, TypeError};
use crate::link::{value_from_field_list, TUPLE_SIZE};
use crate::mavm::{Instruction, Value};
use crate::pos::Location;
use crate::stringtable::StringId;
use crate::uint256::Uint256;
use serde::{Deserialize, Serialize};

///Debugging info serialized into mini executables, currently only contains a location.
#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub location: Option<Location>,
}

///A top level language declaration.  Represents any language construct that can be directly
/// embedded in a source file, and do not need to be contained in a function or other context.
#[derive(Debug, Clone)]
pub enum TopLevelDecl {
    TypeDecl(TypeDecl),
    FuncDecl(FuncDecl),
    VarDecl(GlobalVarDecl),
    ImpFuncDecl(ImportFuncDecl),
    ImpTypeDecl(ImportTypeDecl),
}

impl TopLevelDecl {
    pub fn concat_vecs(a: Vec<Self>, b: Vec<Self>) -> Vec<Self> {
        let mut aa = a;
        let mut bb = b;
        aa.append(&mut bb);
        aa.to_vec()
    }
}

///Type Declaration, contains the StringId corresponding to the type name, and the underlying Type.
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: StringId,
    pub tipe: Type,
}

pub fn new_type_decl(name: StringId, tipe: Type) -> TypeDecl {
    TypeDecl { name, tipe }
}

///A type in the mini language.
#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub enum Type {
    Void,
    Uint,
    Int,
    Bool,
    Bytes32,
    EthAddress,
    Tuple(Vec<Type>),
    Array(Box<Type>),
    FixedArray(Box<Type>, usize),
    Struct(Vec<StructField>),
    Named(StringId),
    Func(bool, Vec<Type>, Box<Type>),
    Map(Box<Type>, Box<Type>),
    Imported(StringId),
    Any,
    Every,
    Option(Box<Type>),
}

impl Type {
    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(
        &self,
        type_table: &SymTable<Type>,
        location: Option<Location>,
    ) -> Result<Self, TypeError> {
        match self {
            Type::Void
            | Type::Uint
            | Type::Int
            | Type::Bool
            | Type::Bytes32
            | Type::EthAddress
            | Type::Imported(_)
            | Type::Every
            | Type::Any => Ok(self.clone()),
            Type::Tuple(tvec) => {
                let mut rvec = Vec::new();
                for t in tvec.iter() {
                    rvec.push(t.resolve_types(type_table, location)?);
                }
                Ok(Type::Tuple(rvec))
            }
            Type::Array(t) => Ok(Type::Array(Box::new(
                t.resolve_types(type_table, location)?,
            ))),
            Type::FixedArray(t, s) => Ok(Type::FixedArray(
                Box::new(t.resolve_types(type_table, location)?),
                *s,
            )),
            Type::Struct(vf) => {
                let mut fvec = Vec::new();
                for field in vf.iter() {
                    fvec.push(field.resolve_types(type_table, location)?);
                }
                Ok(Type::Struct(fvec))
            }
            Type::Named(name) => match type_table.get(*name) {
                Some(t) => Ok(t.resolve_types(type_table, location)?),
                None => Err(new_type_error(
                    "referenced non-existent type name".to_string(),
                    location,
                )),
            },
            Type::Func(is_impure, args, ret) => {
                let rret = ret.resolve_types(type_table, location)?;
                let mut rargs = Vec::new();
                for arg in args.iter() {
                    rargs.push(arg.resolve_types(type_table, location)?);
                }
                Ok(Type::Func(*is_impure, rargs, Box::new(rret)))
            }
            Type::Map(key, val) => Ok(Type::Map(
                Box::new(key.resolve_types(type_table, location)?),
                Box::new(val.resolve_types(type_table, location)?),
            )),
            Type::Option(t) => Ok(Type::Option(Box::new(
                t.resolve_types(type_table, location)?,
            ))),
        }
    }

    ///If self is a Struct, and name is the StringID of a field of self, then returns Some(n), where
    /// n is the index of the field of self whose ID matches name.  Otherwise returns None.
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
            _ => None,
        }
    }

    ///Returns true if rhs is a subtype of self, and false otherwise
    pub fn assignable(&self, rhs: &Self) -> bool {
        if *rhs == Type::Every {
            return true;
        }
        match self {
            Type::Any => true,
            Type::Void
            | Type::Uint
            | Type::Int
            | Type::Bool
            | Type::Bytes32
            | Type::EthAddress
            | Type::Imported(_)
            | Type::Every => (self == rhs),
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
            Type::Func(is_impure, args, ret) => {
                if let Type::Func(is_impure2, args2, ret2) = rhs {
                    (*is_impure || !is_impure2)
                        && arg_vectors_assignable(args, args2)
                        && (ret2.assignable(ret)) // note: rets in reverse order
                } else {
                    false
                }
            }
            Type::Map(key1, val1) => {
                if let Type::Map(key2, val2) = rhs {
                    key1.assignable(key2) && (val1 == val2)
                } else {
                    false
                }
            }
            Type::Option(inner) => {
                if let Type::Option(inner2) = rhs {
                    inner.assignable(inner2)
                } else {
                    false
                }
            }
        }
    }

    ///Panics if specified type does not have a default value
    pub fn default_value(&self) -> Value {
        match self {
            Type::Void => {
                panic!("tried to get default value for void type");
            }
            Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                Value::Int(Uint256::zero())
            }
            Type::Tuple(tvec) => {
                let mut default_tup = Vec::new();
                for t in tvec {
                    default_tup.push(t.default_value());
                }
                Value::new_tuple(default_tup)
            }
            Type::Array(t) => Value::new_tuple(vec![
                Value::Int(Uint256::one()),
                Value::Int(Uint256::one()),
                Value::new_tuple(vec![t.default_value()]),
            ]),
            Type::FixedArray(t, sz) => {
                let default_val = t.default_value();
                let mut val = Value::new_tuple(vec![default_val; 8]);
                let mut chunk_size = 1;
                while chunk_size * TUPLE_SIZE < *sz {
                    val = Value::new_tuple(vec![val; 8]);
                    chunk_size *= 8;
                }
                val
            }
            Type::Struct(fields) => {
                let mut vals = Vec::new();
                for field in fields {
                    vals.push(field.tipe.default_value());
                }
                value_from_field_list(vals)
            }
            Type::Map(_key, _val) => {
                // an unusable dummy value -- application will panic if it accesses this
                Value::none()
            }
            Type::Named(_) => {
                panic!("tried to get default value for a named type");
            }
            Type::Func(_, _, _) => {
                panic!("tried to get default value for a function type");
            }
            Type::Imported(_) => {
                panic!("tried to get default value for an imported type");
            }
            Type::Any => Value::none(),
            Type::Every => {
                panic!("tried to get default value for the every type");
            }
            Type::Option(_) => Value::new_tuple(vec![Value::Int(Uint256::zero())]),
        }
    }
}

///Returns true if each type in tvec2 is a subtype of the type in tvec1 at the same index, and tvec1
/// and tvec2 have the same length.
pub fn type_vectors_assignable(tvec1: &[Type], tvec2: &[Type]) -> bool {
    tvec1.len() == tvec2.len() && tvec1.iter().zip(tvec2).all(|(t1, t2)| t1.assignable(t2))
}

///Identical to `type_vectors_assignable`
pub fn arg_vectors_assignable(tvec1: &[Type], tvec2: &[Type]) -> bool {
    tvec1.len() == tvec2.len() && tvec1.iter().zip(tvec2).all(|(t1, t2)| t1.assignable(t2))
}

///Identical to `type_vectors_assignable` but using StructField slices as inputs and comparing their
/// inner types.
pub fn field_vectors_assignable(tvec1: &[StructField], tvec2: &[StructField]) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.tipe.assignable(&t2.tipe))
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Void, Type::Void)
            | (Type::Uint, Type::Uint)
            | (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Bytes32, Type::Bytes32)
            | (Type::EthAddress, Type::EthAddress)
            | (Type::Any, Type::Any)
            | (Type::Every, Type::Every) => true,
            (Type::Tuple(v1), Type::Tuple(v2)) => type_vectors_equal(&v1, &v2),
            (Type::Array(a1), Type::Array(a2)) => *a1 == *a2,
            (Type::FixedArray(a1, s1), Type::FixedArray(a2, s2)) => (s1 == s2) && (*a1 == *a2),
            (Type::Struct(f1), Type::Struct(f2)) => struct_field_vectors_equal(&f1, &f2),
            (Type::Map(k1, v1), Type::Map(k2, v2)) => (*k1 == *k2) && (*v1 == *v2),
            (Type::Named(n1), Type::Named(n2)) => (n1 == n2),
            (Type::Func(i1, a1, r1), Type::Func(i2, a2, r2)) => {
                (i1 == i2) && type_vectors_equal(&a1, &a2) && (*r1 == *r2)
            }
            (Type::Imported(n1), Type::Imported(n2)) => (n1 == n2),
            (Type::Option(x), Type::Option(y)) => *x == *y,
            (_, _) => false,
        }
    }
}

///Returns true if the contents of the slices are equal
fn type_vectors_equal(v1: &[Type], v2: &[Type]) -> bool {
    v1 == v2
}

///Returns true if the contents of the slices are equal
fn struct_field_vectors_equal(f1: &[StructField], f2: &[StructField]) -> bool {
    f1 == f2
}

///Field of a struct, contains field name and underlying type.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructField {
    pub name: StringId,
    pub tipe: Type,
}

impl StructField {
    pub fn new(name: StringId, tipe: Type) -> StructField {
        StructField { name, tipe }
    }

    ///Converts partially specified type to fully specified type by converting named types via type
    /// table.
    pub fn resolve_types(
        &self,
        type_table: &SymTable<Type>,
        location: Option<Location>,
    ) -> Result<Self, TypeError> {
        let t = self.tipe.resolve_types(type_table, location)?;
        Ok(StructField {
            name: self.name,
            tipe: t,
        })
    }
}

///Argument to a function, contains field name and underlying type.
#[derive(Debug, Clone)]
pub struct FuncArg {
    pub name: StringId,
    pub tipe: Type,
}

impl FuncArg {
    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(
        &self,
        type_table: &SymTable<Type>,
        location: Option<Location>,
    ) -> Result<Self, TypeError> {
        Ok(FuncArg {
            name: self.name,
            tipe: self.tipe.resolve_types(type_table, location)?,
        })
    }
}

pub fn new_func_arg(name: StringId, tipe: Type) -> FuncArg {
    FuncArg { name, tipe }
}

///Represents a declaration of a global mini variable.
#[derive(Debug, Clone)]
pub struct GlobalVarDecl {
    pub name: StringId,
    pub tipe: Type,
    pub location: Option<Location>,
}

impl GlobalVarDecl {
    pub fn new(name: StringId, tipe: Type, location: Option<Location>) -> Self {
        GlobalVarDecl {
            name,
            tipe,
            location,
        }
    }

    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        Ok(GlobalVarDecl::new(
            self.name,
            self.tipe.resolve_types(type_table, self.location)?,
            self.location,
        ))
    }
}

///Represents an import of a mini function from another source file or external location.
/// is_impure, arg_types, and ret_type are assumed to correspond to the associated elements of tipe,
/// this must be upheld by users of this type.
#[derive(Debug, Clone)]
pub struct ImportFuncDecl {
    pub name: StringId,
    pub is_impure: bool,
    pub arg_types: Vec<Type>,
    pub ret_type: Type,
    pub tipe: Type,
}

impl ImportFuncDecl {
    ///Constructor taking a `Vec` of `FuncArg`s
    pub fn new(name: StringId, is_impure: bool, args: Vec<FuncArg>, ret_type: Type) -> Self {
        let mut arg_types = Vec::new();
        for arg in args.iter() {
            arg_types.push(arg.tipe.clone());
        }
        ImportFuncDecl {
            name,
            is_impure,
            arg_types: arg_types.clone(),
            ret_type: ret_type.clone(),
            tipe: Type::Func(is_impure, arg_types, Box::new(ret_type)),
        }
    }

    ///Identical to new but takes a `Vec` of `Type` instead of a `Vec` of `FuncArg`
    pub fn new_types(
        name: StringId,
        is_impure: bool,
        arg_types: Vec<Type>,
        ret_type: Type,
    ) -> Self {
        ImportFuncDecl {
            name,
            is_impure,
            arg_types: arg_types.clone(),
            ret_type: ret_type.clone(),
            tipe: Type::Func(is_impure, arg_types, Box::new(ret_type)),
        }
    }
}

///Represents a type imported from another source file or external location.
#[derive(Clone, Debug)]
pub struct ImportTypeDecl {
    pub name: StringId,
    pub tipe: Type,
}

impl ImportTypeDecl {
    pub fn new(name: StringId) -> Self {
        ImportTypeDecl {
            name,
            tipe: Type::Imported(name),
        }
    }
}

///Represents whether the FuncDecl that contains it is public or private.
#[derive(Debug, Clone, Copy)]
pub enum FuncDeclKind {
    Public,
    Private,
}

///Represents a top level function declaration.  The is_impure, args, and ret_type fields are
/// assumed to be derived from tipe, and this must be upheld by the user of this type.
#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: StringId,
    pub is_impure: bool,
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
        is_impure: bool,
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
        FuncDecl {
            name,
            is_impure,
            args: args_vec,
            ret_type: ret_type.clone(),
            code,
            tipe: Type::Func(is_impure, arg_types, Box::new(ret_type)),
            kind: if exported {
                FuncDeclKind::Public
            } else {
                FuncDeclKind::Private
            },
            location,
        }
    }

    ///Returns either a fully specified version of self, specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(
        &self,
        type_table: &SymTable<Type>,
        location: Option<Location>,
    ) -> Result<Self, TypeError> {
        let mut rargs = Vec::new();
        for arg in self.args.iter() {
            rargs.push(arg.resolve_types(type_table, location)?);
        }
        let mut rcode = Vec::new();
        for stat in self.code.iter() {
            rcode.push(stat.resolve_types(type_table)?);
        }
        Ok(FuncDecl {
            name: self.name,
            is_impure: self.is_impure,
            args: rargs,
            ret_type: self.ret_type.resolve_types(type_table, location)?,
            code: rcode,
            tipe: self.tipe.resolve_types(type_table, location)?,
            kind: self.kind,
            location: self.location,
        })
    }
}

///A statement in the mini language with associated `DebugInfo` that has not yet been type checked.
#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub debug_info: DebugInfo,
}

///A raw statement containing no debug information that has not yet been type checked.
#[derive(Debug, Clone)]
pub enum StatementKind {
    Noop(),
    Panic(),
    ReturnVoid(),
    Return(Expr),
    Expression(Expr),
    Let(MatchPattern, Expr),
    Assign(StringId, Expr),
    Loop(Vec<Statement>),
    While(Expr, Vec<Statement>),
    If(IfArm),
    IfLet(StringId, Expr, Vec<Statement>, Option<Vec<Statement>>),
    Asm(Vec<Instruction>, Vec<Expr>),
    DebugPrint(Expr),
}

impl Statement {
    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        Ok(Self {
            kind: self.kind.resolve_types(type_table)?,
            debug_info: self.debug_info.clone(),
        })
    }

    ///Applies `resolve_types` to each member of v using type_table, and collects the results into a
    /// `Vec` if all are successful.
    pub fn resolve_types_vec(
        v: Vec<Self>,
        type_table: &SymTable<Type>,
    ) -> Result<Vec<Self>, TypeError> {
        let mut vr = Vec::new();
        for s in v.iter() {
            vr.push(s.resolve_types(type_table)?);
        }
        Ok(vr)
    }
}

impl StatementKind {
    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        match &self {
            StatementKind::Noop() => Ok(StatementKind::Noop()),
            StatementKind::Panic() => Ok(StatementKind::Panic()),
            StatementKind::ReturnVoid() => Ok(StatementKind::ReturnVoid()),
            StatementKind::Return(expr) => {
                Ok(StatementKind::Return(expr.resolve_types(type_table)?))
            }
            StatementKind::Expression(expr) => {
                Ok(StatementKind::Expression(expr.resolve_types(type_table)?))
            }
            StatementKind::Let(pat, expr) => Ok(StatementKind::Let(
                pat.clone(),
                expr.resolve_types(type_table)?,
            )),
            StatementKind::Assign(name, expr) => Ok(StatementKind::Assign(
                *name,
                expr.resolve_types(type_table)?,
            )),
            StatementKind::Loop(body) => Ok(StatementKind::Loop(Statement::resolve_types_vec(
                body.to_vec(),
                type_table,
            )?)),
            StatementKind::While(c, body) => Ok(StatementKind::While(
                c.resolve_types(type_table)?,
                Statement::resolve_types_vec(body.to_vec(), type_table)?,
            )),
            StatementKind::If(arms) => Ok(StatementKind::If(arms.resolve_types(type_table)?)),
            StatementKind::Asm(insns, args) => {
                let mut rargs = Vec::new();
                for arg in args.iter() {
                    rargs.push(arg.resolve_types(type_table)?);
                }
                Ok(StatementKind::Asm(insns.to_vec(), rargs))
            }
            StatementKind::DebugPrint(e) => {
                Ok(StatementKind::DebugPrint(e.resolve_types(type_table)?))
            }
            StatementKind::IfLet(l, r, s, e) => Ok(StatementKind::IfLet(
                *l,
                r.resolve_types(type_table)?,
                s.iter()
                    .map(|x| x.resolve_types(type_table))
                    .collect::<Result<Vec<_>, _>>()?,
                e.clone()
                    .map(|block| block.iter().map(|x| x.resolve_types(type_table)).collect())
                    .transpose()?,
            )),
        }
    }
}

///Either a single identifier or a tuple of identifiers, used in mini let bindings.
#[derive(Debug, Clone)]
pub enum MatchPattern {
    Simple(StringId),
    Tuple(Vec<MatchPattern>),
}

///Represents an arm of an If-Else chain, is Cond(condition, block, possible_else, location) if it
/// contains a condition, and Catchall(block, location) if it is an else block.
#[derive(Debug, Clone)]
pub enum IfArm {
    Cond(Expr, Vec<Statement>, Option<Box<IfArm>>, Option<Location>),
    Catchall(Vec<Statement>, Option<Location>),
}

impl IfArm {
    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        match self {
            IfArm::Cond(cond, body, rest, loc) => Ok(IfArm::Cond(
                cond.resolve_types(type_table)?,
                Statement::resolve_types_vec(body.to_vec(), type_table)?,
                match rest {
                    Some(body) => Some(Box::new(body.resolve_types(type_table)?)),
                    None => None,
                },
                *loc,
            )),
            IfArm::Catchall(body, loc) => Ok(IfArm::Catchall(
                Statement::resolve_types_vec(body.to_vec(), type_table)?,
                *loc,
            )),
        }
    }
}

///Represents a constant mini value of type Option<T> for some type T.
#[derive(Debug, Clone)]
pub enum OptionConst {
    _Some(Box<Constant>),
    None(Type),
}

///Represents a mini constant value. This is different than `Value` as it encodes Options as distinct
/// from tuples.
#[derive(Debug, Clone)]
pub enum Constant {
    Uint(Uint256),
    Int(Uint256),
    Bool(bool),
    Option(OptionConst),
    Null,
}

impl OptionConst {
    ///Gets the type of the value
    pub(crate) fn type_of(&self) -> Type {
        Type::Option(Box::new(match self {
            OptionConst::_Some(c) => (*c).type_of(),
            OptionConst::None(t) => t.clone(),
        }))
    }

    ///Exracts the value from the Constant
    pub(crate) fn value(&self) -> Value {
        match self {
            OptionConst::_Some(c) => {
                Value::new_tuple(vec![Value::Int(Uint256::one()), c.clone().value()])
            }
            OptionConst::None(_) => Value::new_tuple(vec![Value::Int(Uint256::zero())]),
        }
    }

    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        match self {
            OptionConst::_Some(bc) => {
                Ok(OptionConst::_Some(Box::new(bc.resolve_types(type_table)?)))
            }
            OptionConst::None(t) => Ok(OptionConst::None(t.resolve_types(type_table, None)?)),
        }
    }
}

impl Constant {
    ///Gets the type of the value
    pub(crate) fn type_of(&self) -> Type {
        match self {
            Constant::Uint(_) => Type::Uint,
            Constant::Int(_) => Type::Int,
            Constant::Bool(_) => Type::Bool,
            Constant::Option(inner) => inner.type_of(),
            Constant::Null => Type::Void,
        }
    }

    ///Exracts the value from the Constant
    pub(crate) fn value(&self) -> Value {
        match self {
            Constant::Uint(ui) => Value::Int(ui.clone()),
            Constant::Int(i) => Value::Int(i.clone()),
            Constant::Bool(b) => Value::Int(Uint256::from_bool(*b)),
            Constant::Option(c) => c.value(),
            Constant::Null => Value::none(),
        }
    }

    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        if let Constant::Option(oc) = self {
            Ok(Constant::Option(oc.resolve_types(type_table)?))
        } else {
            Ok(self.clone())
        }
    }
}

///A mini expression that has not yet been type checked.
#[derive(Debug, Clone)]
pub enum Expr {
    UnaryOp(UnaryOp, Box<Expr>, Option<Location>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>, Option<Location>),
    ShortcutOr(Box<Expr>, Box<Expr>, Option<Location>),
    ShortcutAnd(Box<Expr>, Box<Expr>, Option<Location>),
    VariableRef(StringId, Option<Location>),
    TupleRef(Box<Expr>, Uint256, Option<Location>),
    DotRef(Box<Expr>, StringId, Option<Location>),
    Constant(Constant, Option<Location>),
    OptionInitializer(Box<Expr>, Option<Location>),
    FunctionCall(Box<Expr>, Vec<Expr>, Option<Location>),
    CodeBlock(Vec<Statement>, Option<Box<Expr>>, Option<Location>),
    ArrayOrMapRef(Box<Expr>, Box<Expr>, Option<Location>),
    StructInitializer(Vec<FieldInitializer>, Option<Location>),
    Tuple(Vec<Expr>, Option<Location>),
    NewArray(Box<Expr>, Type, Option<Location>),
    NewFixedArray(usize, Option<Box<Expr>>, Option<Location>),
    NewMap(Type, Type, Option<Location>),
    ArrayOrMapMod(Box<Expr>, Box<Expr>, Box<Expr>, Option<Location>),
    StructMod(Box<Expr>, StringId, Box<Expr>, Option<Location>),
    UnsafeCast(Box<Expr>, Type, Option<Location>),
    Asm(Type, Vec<Instruction>, Vec<Expr>, Option<Location>),
    Try(Box<Expr>, Option<Location>),
}

impl Expr {
    ///Returns an expression that applies unary operator op to e.
    pub fn new_unary(op: UnaryOp, e: Expr, loc: Option<Location>) -> Self {
        Expr::UnaryOp(op, Box::new(e), loc)
    }

    ///Returns an expression that applies binary operator op to e1 and e2.
    pub fn new_binary(op: BinaryOp, e1: Expr, e2: Expr, loc: Option<Location>) -> Self {
        Expr::Binary(op, Box::new(e1), Box::new(e2), loc)
    }

    ///Returns either a fully specified version of self specified by matching Named types to the
    /// contents of type_table, or a TypeError if a Named type does not have an associated entry.
    pub fn resolve_types(&self, type_table: &SymTable<Type>) -> Result<Self, TypeError> {
        match self {
            Expr::UnaryOp(op, be, loc) => Ok(Expr::UnaryOp(
                *op,
                Box::new(be.resolve_types(type_table)?),
                *loc,
            )),
            Expr::Binary(op, be1, be2, loc) => Ok(Expr::Binary(
                *op,
                Box::new(be1.resolve_types(type_table)?),
                Box::new(be2.resolve_types(type_table)?),
                *loc,
            )),
            Expr::ShortcutOr(be1, be2, loc) => Ok(Expr::ShortcutOr(
                Box::new(be1.resolve_types(type_table)?),
                Box::new(be2.resolve_types(type_table)?),
                *loc,
            )),
            Expr::ShortcutAnd(be1, be2, loc) => Ok(Expr::ShortcutAnd(
                Box::new(be1.resolve_types(type_table)?),
                Box::new(be2.resolve_types(type_table)?),
                *loc,
            )),
            Expr::VariableRef(name, loc) => Ok(Expr::VariableRef(*name, *loc)),
            Expr::TupleRef(be, idx, loc) => Ok(Expr::TupleRef(
                Box::new(be.resolve_types(type_table)?),
                idx.clone(),
                *loc,
            )),
            Expr::DotRef(be, name, loc) => Ok(Expr::DotRef(
                Box::new(be.resolve_types(type_table)?),
                *name,
                *loc,
            )),
            Expr::Constant(b, loc) => Ok(Expr::Constant(b.resolve_types(type_table)?, *loc)),
            Expr::FunctionCall(fexpr, args, loc) => {
                let mut rargs = Vec::new();
                for arg in args.iter() {
                    rargs.push(arg.resolve_types(type_table)?);
                }
                Ok(Expr::FunctionCall(
                    Box::new(fexpr.resolve_types(type_table)?),
                    rargs,
                    *loc,
                ))
            }
            Expr::CodeBlock(body, result, loc) => Ok(Expr::CodeBlock(
                Statement::resolve_types_vec(body.to_vec(), type_table)?,
                result
                    .clone()
                    .map(|exp| exp.resolve_types(type_table))
                    .transpose()?
                    .map(Box::new),
                *loc,
            )),
            Expr::ArrayOrMapRef(e1, e2, loc) => Ok(Expr::ArrayOrMapRef(
                Box::new(e1.resolve_types(type_table)?),
                Box::new(e2.resolve_types(type_table)?),
                *loc,
            )),
            Expr::StructInitializer(fields, loc) => {
                let mut rfields = Vec::new();
                for field in fields.iter() {
                    rfields.push(FieldInitializer::new(
                        field.name,
                        field.value.resolve_types(type_table)?,
                    ));
                }
                Ok(Expr::StructInitializer(rfields, *loc))
            }
            Expr::OptionInitializer(inner, loc) => Ok(Expr::OptionInitializer(
                Box::new(inner.resolve_types(type_table)?),
                *loc,
            )),
            Expr::Tuple(evec, loc) => {
                let mut rvec = Vec::new();
                for expr in evec {
                    rvec.push(expr.resolve_types(type_table)?);
                }
                Ok(Expr::Tuple(rvec, *loc))
            }
            Expr::NewArray(sz, tipe, loc) => Ok(Expr::NewArray(
                Box::new(sz.resolve_types(type_table)?),
                tipe.resolve_types(type_table, *loc)?,
                *loc,
            )),
            Expr::NewFixedArray(sz, init_expr, loc) => match &*init_expr {
                Some(expr) => Ok(Expr::NewFixedArray(
                    *sz,
                    Some(Box::new(expr.resolve_types(type_table)?)),
                    *loc,
                )),
                None => Ok(Expr::NewFixedArray(*sz, None, *loc)),
            },
            Expr::NewMap(key_type, value_type, loc) => Ok(Expr::NewMap(
                key_type.resolve_types(type_table, *loc)?,
                value_type.resolve_types(type_table, *loc)?,
                *loc,
            )),
            Expr::ArrayOrMapMod(e1, e2, e3, loc) => Ok(Expr::ArrayOrMapMod(
                Box::new(e1.resolve_types(type_table)?),
                Box::new(e2.resolve_types(type_table)?),
                Box::new(e3.resolve_types(type_table)?),
                *loc,
            )),
            Expr::StructMod(e1, i, e3, loc) => Ok(Expr::StructMod(
                Box::new(e1.resolve_types(type_table)?),
                *i,
                Box::new(e3.resolve_types(type_table)?),
                *loc,
            )),
            Expr::UnsafeCast(be, t, loc) => Ok(Expr::UnsafeCast(
                Box::new(be.resolve_types(type_table)?),
                t.resolve_types(type_table, *loc)?,
                *loc,
            )),
            Expr::Asm(t, insns, exprs, loc) => {
                let mut res_exprs = Vec::new();
                for ex in exprs {
                    res_exprs.push(ex.resolve_types(type_table)?);
                }
                Ok(Expr::Asm(
                    t.resolve_types(type_table, *loc)?,
                    insns.to_vec(),
                    res_exprs,
                    *loc,
                ))
            }
            Expr::Try(expr, loc) => Ok(Expr::Try(Box::new(expr.resolve_types(type_table)?), *loc)),
        }
    }
}

///A mini unary operator.
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
    ToAddress,
}

///A mini binary operator.
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
    _LogicalAnd,
    LogicalOr,
    Hash,
}

///Used in StructInitializer expressions to map expressions to fields of the struct.
#[derive(Debug, Clone)]
pub struct FieldInitializer {
    pub name: StringId,
    pub value: Expr,
}

impl FieldInitializer {
    pub fn new(name: StringId, value: Expr) -> Self {
        FieldInitializer { name, value }
    }
}
