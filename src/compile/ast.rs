/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//! Contains types and utilities for constructing the mini AST

use crate::compile::typecheck::{AbstractSyntaxTree, InliningMode, TypeCheckedNode};
use crate::compile::{CompileError, Lines, ModPath};
use crate::console::{human_readable_index, Color};
use crate::link::{value_from_field_list, Import, TUPLE_SIZE};
use crate::mavm::{Instruction, LabelId, Value};
use crate::pos::{BytePos, Location};
use crate::stringtable::{StringId, StringTable};
use crate::uint256::Uint256;
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap, HashSet};

/// This is a map of the types at a given location, with the Vec<String> representing the module path
/// and `StringId` of the type at that location.
pub type NamedTypes = HashMap<(ModPath, StringId), (Type, String)>;

/// Debugging info serialized into mini executables, currently only contains a location.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct DebugInfo {
    pub location: Option<Location>,
    pub attributes: Attributes,
}

/// A list of properties that an AST node has.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Attributes {
    /// Is true if the current node is a breakpoint, false otherwise.
    pub breakpoint: bool,
    pub inline: InliningMode,
    #[serde(skip)]
    /// Whether generated instructions should be printed to the console.
    pub codegen_print: bool,
}

impl DebugInfo {
    pub fn new(location: Option<Location>, attributes: Attributes) -> Self {
        DebugInfo {
            location,
            attributes,
        }
    }

    /// builds a `DebugInfo` in-place at the parsing site
    pub fn here(lines: &Lines, lno: usize, file: u64) -> Self {
        DebugInfo {
            location: lines.location(BytePos::from(lno), file),
            attributes: Attributes::default(),
        }
    }

    pub fn locs(&self) -> Vec<Location> {
        self.location.into_iter().collect()
    }
}

impl From<Option<Location>> for DebugInfo {
    fn from(location: Option<Location>) -> Self {
        DebugInfo {
            location,
            attributes: Attributes::default(),
        }
    }
}

/// A top level language declaration.  Represents any language construct that can be directly
/// embedded in a source file, and do not need to be contained in a function or other context.
#[derive(Debug, Clone)]
pub enum TopLevelDecl {
    TypeDecl(TypeDecl),
    FuncDecl(Func),
    VarDecl(GlobalVar),
    UseDecl(Import),
    ConstDecl,
}

/// Type Declaration, contains the StringId corresponding to the type name, and the underlying Type.
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: StringId,
    pub tipe: Type,
}

pub fn new_type_decl(name: StringId, tipe: Type) -> TypeDecl {
    TypeDecl { name, tipe }
}

/// A type in the mini language.
#[derive(Debug, Clone, Eq, Serialize, Deserialize, Derivative)]
#[derivative(Hash)]
pub enum Type {
    Void,
    Uint,
    Int,
    Bool,
    Bytes32,
    EthAddress,
    Buffer,
    Tuple(Vec<Type>),
    Array(Box<Type>),
    FixedArray(Box<Type>, usize),
    Struct(
        Vec<StructField>,
        #[serde(skip)]
        #[derivative(Hash = "ignore")]
        Vec<StructTrait>,
    ),
    Func(FuncProperties, Vec<Type>, Box<Type>),
    Map(Box<Type>, Box<Type>),
    Any,
    Every,
    Option(Box<Type>),
    Union(Vec<Type>),
    Nominal(Vec<String>, StringId, #[serde(default)] Vec<Type>),
    GenericSlot(usize),
    Generic(usize),
}

impl AbstractSyntaxTree for Type {
    fn child_nodes(&mut self) -> Vec<TypeCheckedNode> {
        match self {
            Type::Void
            | Type::Uint
            | Type::Int
            | Type::Bool
            | Type::Bytes32
            | Type::EthAddress
            | Type::Buffer
            | Type::Any
            | Type::Every
            | Type::GenericSlot(..)
            | Type::Generic(..)
            | Type::Nominal(_, _, _) => vec![],
            Type::Tuple(types) | Type::Union(types) => {
                types.iter_mut().map(|t| TypeCheckedNode::Type(t)).collect()
            }
            Type::Array(tipe) | Type::FixedArray(tipe, _) | Type::Option(tipe) => {
                vec![TypeCheckedNode::Type(tipe)]
            }
            Type::Struct(fields, methods) => fields
                .iter_mut()
                .map(|field| TypeCheckedNode::Type(&mut field.tipe))
                .chain(
                    methods
                        .iter_mut()
                        .map(|method| &mut method.spec)
                        .flatten()
                        .map(|tipe| TypeCheckedNode::Type(tipe)),
                )
                .collect(),
            Type::Func(_, args, ret) => {
                let mut nodes = vec![TypeCheckedNode::Type(ret)];
                nodes.extend(args.iter_mut().map(|t| TypeCheckedNode::Type(t)));
                nodes
            }
            Type::Map(key, value) => vec![TypeCheckedNode::Type(key), TypeCheckedNode::Type(value)],
        }
    }

    /// for iteration purposes we say types themselves are not view
    fn is_view(&mut self, _: &NamedTypes) -> bool {
        false
    }

    /// for iteration purposes we say types themselves are not write
    fn is_write(&mut self, _: &NamedTypes) -> bool {
        false
    }
}

impl Type {
    /// Gets the representation of a `Nominal` type, based on the types in `NamedTypes`, returns self
    /// if the type is not `Nominal`, or a `CompileError` if the type of `self` cannot be resolved in
    /// `NamedTypes`.
    pub fn rep(&self, nominals: &NamedTypes) -> Result<Self, CompileError> {
        let mut base_type = self.clone();

        while let Type::Nominal(path, id, spec) = base_type.clone() {
            base_type = nominals
                .get(&(path.clone(), id))
                .cloned()
                .ok_or(CompileError::new_type_error(
                    format!("No type at {:?}, {}", path, id),
                    vec![],
                ))?
                .0
                .make_specific(&spec)?;
        }
        Ok(base_type)
    }

    /// Finds all nominal sub-types present under a type
    pub fn find_nominals(&self) -> Vec<usize> {
        match self {
            Type::Nominal(_, id, _) => {
                vec![*id]
            }
            Type::Array(tipe) | Type::FixedArray(tipe, ..) | Type::Option(tipe) => {
                tipe.find_nominals()
            }
            Type::Tuple(entries) => {
                let mut tipes = vec![];
                for entry in entries {
                    tipes.extend(entry.find_nominals());
                }
                tipes
            }
            Type::Func(_, args, ret) => {
                let mut tipes = ret.find_nominals();
                for arg in args {
                    tipes.extend(arg.find_nominals());
                }
                tipes
            }
            Type::Struct(fields, methods) => {
                let mut tipes = vec![];
                for field in fields {
                    tipes.extend(field.tipe.find_nominals());
                }
                for spec_type in methods.iter().map(|method| &method.spec).flatten() {
                    tipes.extend(spec_type.find_nominals());
                }
                tipes
            }

            Type::Map(domain_tipe, codomain_tipe) => {
                let mut tipes = domain_tipe.find_nominals();
                tipes.extend(codomain_tipe.find_nominals());
                tipes
            }
            _ => vec![],
        }
    }

    /// Find all types matching some critereon
    /// |take| decides whether to take a value, returning true when to do so
    pub fn find<Take>(&self, take: &mut Take) -> Vec<Type>
    where
        Take: FnMut(&Self) -> bool,
    {
        let mut found = vec![];
        macro_rules! find {
            ($tipe:expr) => {
                found.extend($tipe.find(take));
            };
        }
        match &self {
            Self::Tuple(contents) | Self::Union(contents) | Self::Nominal(_, _, contents) => {
                contents.iter().for_each(|val| find!(val));
            }
            Self::Option(inner) | Self::Array(inner) | Self::FixedArray(inner, _) => {
                find!(inner);
            }
            Self::Map(key, value) => {
                find!(key);
                find!(value);
            }
            Self::Func(_, args, ret) => {
                args.iter().for_each(|val| find!(val));
                find!(ret);
            }
            Self::Struct(fields, methods) => {
                fields.iter().for_each(|field| find!(field.tipe));
                methods
                    .iter()
                    .for_each(|method| method.spec.iter().for_each(|tipe| find!(tipe)))
            }
            _ => {}
        }
        if take(self) {
            found.push(self.clone());
        }
        found
    }

    /// Surgically replace types potentially nested within others.
    /// |via| makes the type substitution.
    pub fn replace<Via>(&mut self, via: &mut Via)
    where
        Via: FnMut(&mut Self),
    {
        match self {
            Self::Tuple(ref mut contents)
            | Self::Union(ref mut contents)
            | Self::Nominal(_, _, ref mut contents) => {
                contents.iter_mut().for_each(|val| val.replace(via));
            }
            Self::Option(ref mut inner)
            | Self::Array(ref mut inner)
            | Self::FixedArray(ref mut inner, _) => inner.replace(via),
            Self::Map(ref mut key, ref mut value) => {
                key.replace(via);
                value.replace(via);
            }
            Self::Func(_, ref mut args, ref mut ret) => {
                args.iter_mut().for_each(|val| val.replace(via));
                ret.replace(via);
            }
            Self::Struct(ref mut fields, ref mut methods) => {
                fields.iter_mut().for_each(|field| field.tipe.replace(via));
                methods
                    .iter_mut()
                    .for_each(|method| method.spec.iter_mut().for_each(|tipe| tipe.replace(via)))
            }
            _ => {}
        }
        via(self);
    }

    /// Count the number of generic slots under this type. Since slots must be filled in at call sites,
    /// this tells you how many parameters must be passed in.
    pub fn count_generic_slots(&self) -> usize {
        let params = self.find(&mut |tipe| matches!(tipe, Type::GenericSlot(_)));

        let mut seen = HashSet::new();

        for param in params {
            match param {
                Type::GenericSlot(slot) => drop(seen.insert(slot)),
                x => panic!("find() matched something wrong {:?}", x),
            }
        }
        return seen.len();
    }

    /// Makes a specific version of this type, where the nominals listed are specialized.
    pub fn make_specific(&self, specialization: &Vec<Type>) -> Result<Self, CompileError> {
        let mut tipe = self.clone();
        let mut failure = None;
        tipe.replace(&mut |tipe| {
            if let Type::GenericSlot(slot) = tipe {
                match specialization.get(*slot) {
                    Some(specific) => *tipe = specific.clone(),
                    None => failure = Some(*slot),
                }
            }
        });
        match failure {
            Some(slot) => Err(CompileError::new(
                "Generics error",
                format!(
                    "Specialization {} is missing its {} type.",
                    Color::red("<1st, 2nd, ...>"),
                    Color::red(human_readable_index(slot + 1))
                ),
                vec![],
            )),
            None => Ok(tipe),
        }
    }

    /// Makes a generic version of this type, where the nominals listed are generalized.
    pub fn make_generic(&self, generalization: &Vec<StringId>) -> Self {
        let slots = generalization
            .into_iter()
            .enumerate()
            .map(|(index, id)| (*id, index))
            .collect::<HashMap<_, _>>();

        let mut tipe = self.clone();
        tipe.replace(&mut |tipe| {
            if let Type::Nominal(_, id, _) = tipe {
                if let Some(slot) = slots.get(&id) {
                    *tipe = Type::GenericSlot(*slot);
                }
            }
        });
        tipe
    }

    /// Converts all slots to immutable generics. This ensures they are never changed again at call sites.
    pub fn commit_generic_slots(&self) -> Self {
        let mut tipe = self.clone();
        tipe.replace(&mut |tipe| {
            if let Type::GenericSlot(slot) = tipe {
                *tipe = Type::Generic(*slot);
            }
        });
        tipe
    }

    /// If self is a Struct, and name is the StringID of a field of self, then returns Some(n), where
    /// n is the index of the field of self whose ID matches name.  Otherwise returns None.
    pub fn get_struct_slot_by_name(&self, name: String) -> Option<usize> {
        match self {
            Type::Struct(fields, _) => {
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

    pub fn covariant_castable(
        &self,
        rhs: &Self,
        nominals: &NamedTypes,
        mut seen: HashSet<(Type, Type)>,
    ) -> bool {
        if *rhs == Type::Every {
            return true;
        }
        match self {
            Type::Any => *rhs != Type::Void,
            Type::Uint | Type::Int | Type::Bool | Type::Bytes32 | Type::EthAddress => match &rhs {
                Type::Uint | Type::Int | Type::Bool | Type::Bytes32 | Type::EthAddress => true,
                _ => false,
            },
            Type::Buffer | Type::Void | Type::Every => rhs == self,
            Type::Tuple(tvec) => {
                if let Ok(Type::Tuple(tvec2)) = rhs.rep(nominals) {
                    type_vectors_covariant_castable(tvec, &tvec2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Array(t) => {
                if let Ok(Type::Array(t2)) = rhs.rep(nominals) {
                    t.covariant_castable(&t2, nominals, seen)
                } else {
                    false
                }
            }
            Type::FixedArray(t, s) => {
                if let Ok(Type::FixedArray(t2, s2)) = rhs.rep(nominals) {
                    (*s == s2) && t.covariant_castable(&t2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Struct(fields, _) => {
                if let Ok(Type::Struct(fields2, _)) = rhs.rep(nominals) {
                    field_vectors_covariant_castable(fields, &fields2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Nominal(_, _, _) => {
                if let (Ok(left), Ok(right)) = (self.rep(nominals), rhs.rep(nominals)) {
                    if seen.insert((left.clone(), right.clone())) {
                        left.covariant_castable(&right, nominals, seen)
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
            Type::Func(_, args, ret) => {
                if let Type::Func(_, args2, ret2) = rhs {
                    //note: The order of arg2 and args, and ret and ret2 are in this order to ensure contravariance in function arg types
                    type_vectors_covariant_castable(args2, args, nominals, seen.clone())
                        && (ret.covariant_castable(ret2, nominals, seen))
                } else {
                    false
                }
            }
            Type::Map(key1, val1) => {
                if let Type::Map(key2, val2) = rhs {
                    if let Ok(val2) = val2.rep(nominals) {
                        key1.covariant_castable(key2, nominals, seen.clone())
                            && (val1.covariant_castable(&val2, nominals, seen))
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Type::Option(_) => {
                if let Ok(Type::Option(_)) = rhs.rep(nominals) {
                    true
                } else {
                    false
                }
            }
            Type::Union(inner) => {
                if let Ok(Type::Union(inner2)) = rhs.rep(nominals) {
                    type_vectors_covariant_castable(&*inner2, inner, nominals, seen.clone())
                } else {
                    false
                }
            }
            Type::GenericSlot(..) => panic!("tried to covariant cast a generic"),
            Type::Generic(..) => panic!("tried to cast a generic"),
        }
    }

    pub fn castable(
        &self,
        rhs: &Self,
        nominals: &NamedTypes,
        mut seen: HashSet<(Type, Type)>,
    ) -> bool {
        if *rhs == Type::Every {
            return true;
        }
        match self {
            Type::Any => *rhs != Type::Void,
            Type::Uint | Type::Int | Type::Bytes32 => match &rhs {
                Type::Uint | Type::Int | Type::Bytes32 => true,
                _ => false,
            },
            Type::EthAddress => match &rhs {
                Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress => true,
                _ => false,
            },
            Type::Bool => match &rhs {
                Type::Uint | Type::Int | Type::Bool | Type::Bytes32 | Type::EthAddress => true,
                _ => false,
            },
            Type::Buffer | Type::Void | Type::Every => rhs == self,
            Type::Tuple(tvec) => {
                if let Ok(Type::Tuple(tvec2)) = rhs.rep(nominals) {
                    type_vectors_castable(tvec, &tvec2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Array(t) => {
                if let Ok(Type::Array(t2)) = rhs.rep(nominals) {
                    t.castable(&t2, nominals, seen)
                } else {
                    false
                }
            }
            Type::FixedArray(t, s) => {
                if let Ok(Type::FixedArray(t2, s2)) = rhs.rep(nominals) {
                    (*s == s2) && t.castable(&t2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Struct(fields, _) => {
                if let Ok(Type::Struct(fields2, _)) = rhs.rep(nominals) {
                    field_vectors_castable(fields, &fields2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Nominal(_, _, _) => {
                if let (Ok(left), Ok(right)) = (self.rep(nominals), rhs.rep(nominals)) {
                    if seen.insert((left.clone(), right.clone())) {
                        left.castable(&right, nominals, seen)
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
            Type::Func(prop, args, ret) => {
                if let Type::Func(prop2, args2, ret2) = rhs {
                    //note: The order of arg2 and args, and ret and ret2 are in this order to ensure contravariance in function arg types
                    let (view1, write1) = prop.purity();
                    let (view2, write2) = prop2.purity();

                    (view1 || !view2)
                        && (write1 || !write2)
                        && type_vectors_castable(args2, args, nominals, seen.clone())
                        && (ret.castable(ret2, nominals, seen))
                } else {
                    false
                }
            }
            Type::Map(key1, val1) => {
                if let Type::Map(key2, val2) = rhs {
                    if let Ok(val2) = val2.rep(nominals) {
                        key1.castable(key2, nominals, seen.clone())
                            && (val1.castable(&val2, nominals, seen))
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Type::Option(inner) => {
                if let Ok(Type::Option(inner2)) = rhs.rep(nominals) {
                    inner.castable(&inner2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Union(inner) => {
                if let Ok(Type::Union(inner2)) = rhs.rep(nominals) {
                    type_vectors_castable(&*inner2, inner, nominals, seen.clone())
                } else {
                    false
                }
            }
            Type::GenericSlot(..) => panic!("tried to cast a generic"),
            Type::Generic(..) => panic!("tried to cast a generic"),
        }
    }

    /// Returns true if rhs is a subtype of self, and false otherwise
    pub fn assignable(
        &self,
        rhs: &Self,
        nominals: &NamedTypes,
        mut seen: HashSet<(Type, Type)>,
    ) -> bool {
        if *rhs == Type::Every {
            return true;
        }
        match self {
            Type::Any => *rhs != Type::Void,
            Type::Void
            | Type::Uint
            | Type::Int
            | Type::Bool
            | Type::Bytes32
            | Type::EthAddress
            | Type::Buffer
            | Type::Every => match rhs.rep(nominals) {
                Ok(right) => right == *self,
                Err(_) => false,
            },
            Type::Tuple(tvec) => {
                if let Ok(Type::Tuple(tvec2)) = rhs.rep(nominals) {
                    type_vectors_assignable(tvec, &tvec2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Array(t) => {
                if let Ok(Type::Array(t2)) = rhs.rep(nominals) {
                    t.assignable(&t2, nominals, seen)
                } else {
                    false
                }
            }
            Type::FixedArray(t, s) => {
                if let Ok(Type::FixedArray(t2, s2)) = rhs.rep(nominals) {
                    (*s == s2) && t.assignable(&t2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Struct(fields, _) => {
                if let Ok(Type::Struct(fields2, _)) = rhs.rep(nominals) {
                    field_vectors_assignable(fields, &fields2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Nominal(_, _, _) => {
                if let (Ok(left), Ok(right)) = (self.rep(nominals), rhs.rep(nominals)) {
                    if seen.insert((left.clone(), right.clone())) {
                        left.assignable(&right, nominals, seen.clone())
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
            Type::Func(prop, args, ret) => {
                if let Type::Func(prop2, args2, ret2) = rhs {
                    //note: The order of arg2 and args, and ret and ret2 are in this order to ensure contravariance in function arg types
                    let (view1, write1) = prop.purity();
                    let (view2, write2) = prop2.purity();

                    (view1 || !view2)
                        && (write1 || !write2)
                        && arg_vectors_assignable(args2, args, nominals, seen.clone())
                        && (ret.assignable(ret2, nominals, seen))
                } else {
                    false
                }
            }
            Type::Map(key1, val1) => {
                if let Type::Map(key2, val2) = rhs {
                    if let Ok(val2) = val2.rep(nominals) {
                        key1.assignable(key2, nominals, seen.clone())
                            && (val1.assignable(&val2, nominals, seen))
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Type::Option(inner) => {
                if let Ok(Type::Option(inner2)) = rhs.rep(nominals) {
                    inner.assignable(&inner2, nominals, seen)
                } else {
                    false
                }
            }
            Type::Union(types) => {
                if let Ok(Type::Union(types2)) = rhs.rep(nominals) {
                    type_vectors_assignable(types, &types2, nominals, seen)
                } else {
                    false
                }
            }
            Type::GenericSlot(slot) => {
                if let Ok(Type::GenericSlot(slot2)) = rhs.rep(nominals) {
                    *slot == slot2
                } else {
                    false
                }
            }
            Type::Generic(slot) => {
                if let Ok(Type::Generic(slot2)) = rhs.rep(nominals) {
                    *slot == slot2
                } else {
                    false
                }
            }
        }
    }

    pub fn first_mismatch(
        &self,
        rhs: &Self,
        nominals: &NamedTypes,
        mut seen: HashSet<(Type, Type)>,
    ) -> Option<TypeMismatch> {
        if *rhs == Type::Every {
            return None;
        }
        match self {
            Type::Any => {
                if *rhs != Type::Void {
                    None
                } else {
                    Some(TypeMismatch::Type(Type::Any, Type::Void))
                }
            }
            Type::Void
            | Type::Uint
            | Type::Int
            | Type::Bool
            | Type::Bytes32
            | Type::EthAddress
            | Type::Buffer
            | Type::Every => {
                if self == rhs {
                    None
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::GenericSlot(slot) => match rhs {
                Type::GenericSlot(slot2) if slot == slot2 => {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
                _ => None,
            },
            Type::Generic(slot) => match rhs {
                Type::Generic(slot2) if slot == slot2 => {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
                _ => None,
            },
            Type::Tuple(tvec) => {
                if let Ok(Type::Tuple(tvec2)) = rhs.rep(nominals) {
                    for (index, (left, right)) in tvec.iter().zip(tvec2.iter()).enumerate() {
                        if let Some(inner) = left.first_mismatch(right, nominals, seen.clone()) {
                            return Some(TypeMismatch::Tuple(index, Box::new(inner)));
                        }
                    }
                    if tvec.len() != tvec2.len() {
                        return Some(TypeMismatch::TupleLength(tvec.len(), tvec2.len()));
                    }
                    None
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::Array(t) => {
                if let Ok(Type::Array(t2)) = rhs.rep(nominals) {
                    t.first_mismatch(&t2, nominals, seen)
                        .map(|mismatch| TypeMismatch::ArrayMismatch(Box::new(mismatch)))
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::FixedArray(t, s) => {
                if let Ok(Type::FixedArray(t2, s2)) = rhs.rep(nominals) {
                    if let Some(inner) = t.first_mismatch(&t2, nominals, seen) {
                        Some(TypeMismatch::ArrayMismatch(Box::new(inner)))
                    } else if *s != s2 {
                        Some(TypeMismatch::ArrayLength(*s, s2))
                    } else {
                        None
                    }
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::Struct(fields, _) => {
                if let Ok(Type::Struct(fields2, _)) = rhs.rep(nominals) {
                    field_vectors_mismatch(fields, &fields2, nominals, seen)
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::Nominal(_, _, _) => match (self.rep(nominals), rhs.rep(nominals)) {
                (Ok(left), Ok(right)) => {
                    if seen.insert((self.clone(), rhs.clone())) {
                        left.first_mismatch(&right, nominals, seen)
                    } else {
                        None
                    }
                }
                (Ok(_), Err(_)) => Some(TypeMismatch::UnresolvedRight(self.clone())),
                (Err(_), Ok(_)) => Some(TypeMismatch::UnresolvedLeft(rhs.clone())),
                (Err(_), Err(_)) => Some(TypeMismatch::UnresolvedBoth(self.clone(), rhs.clone())),
            },
            Type::Func(prop, args, ret) => {
                if let Type::Func(prop2, args2, ret2) = rhs {
                    let (view1, write1) = prop.purity();
                    let (view2, write2) = prop2.purity();

                    for (index, (left, right)) in args.iter().zip(args2.iter()).enumerate() {
                        if let Some(inner) = left.first_mismatch(right, nominals, seen.clone()) {
                            return Some(TypeMismatch::FuncArg(index, Box::new(inner)));
                        }
                    }
                    if args.len() != args2.len() {
                        return Some(TypeMismatch::FuncArgLength(args.len(), args2.len()));
                    }
                    if let Some(inner) = ret.first_mismatch(ret2, nominals, seen) {
                        return Some(TypeMismatch::FuncReturn(Box::new(inner)));
                    }
                    if !view1 && view2 {
                        return Some(TypeMismatch::View);
                    }
                    if !write1 && write2 {
                        return Some(TypeMismatch::Write);
                    }
                    None
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::Map(key1, val1) => {
                if let Type::Map(key2, val2) = rhs {
                    if let Ok(val2) = val2.rep(nominals) {
                        key1.first_mismatch(key2, nominals, seen.clone())
                            .map(|mismatch| (true, mismatch))
                            .or_else(|| {
                                val1.first_mismatch(&val2, nominals, seen)
                                    .map(|mismatch| (false, mismatch))
                            })
                            .map(|(is_key, mismatch)| TypeMismatch::Map {
                                is_key,
                                inner: Box::new(mismatch),
                            })
                    } else {
                        Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                    }
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::Option(inner) => {
                if let Ok(Type::Option(inner2)) = rhs.rep(nominals) {
                    inner
                        .first_mismatch(&inner2, nominals, seen)
                        .map(|mismatch| TypeMismatch::Option(Box::new(mismatch)))
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
            Type::Union(types) => {
                if let Ok(Type::Union(types2)) = rhs.rep(nominals) {
                    for (index, (left, right)) in types.iter().zip(types2.iter()).enumerate() {
                        if let Some(inner) = left.first_mismatch(right, nominals, seen.clone()) {
                            return Some(TypeMismatch::Union(index, Box::new(inner)));
                        }
                    }
                    if types.len() != types2.len() {
                        return Some(TypeMismatch::UnionLength(types.len(), types2.len()));
                    }
                    None
                } else {
                    Some(TypeMismatch::Type(self.clone(), rhs.clone()))
                }
            }
        }
    }

    pub fn mismatch_string(&self, rhs: &Type, nominals: &NamedTypes) -> Option<String> {
        let (left, right) = (&self.rep(nominals).ok()?, &rhs.rep(nominals).ok()?);
        self.first_mismatch(rhs, nominals, HashSet::new())
            .map(|mismatch| {
                format!(
                    "{}{}",
                    {
                        //This will be a lot simpler to write in 1.53 when or-patterns syntax stabilizes
                        match left {
                            Type::Any
                            | Type::Void
                            | Type::Uint
                            | Type::Int
                            | Type::Bool
                            | Type::Bytes32
                            | Type::EthAddress
                            | Type::Buffer
                            | Type::Every => String::new(),
                            _ => match right {
                                Type::Any
                                | Type::Void
                                | Type::Uint
                                | Type::Int
                                | Type::Bool
                                | Type::Bytes32
                                | Type::EthAddress
                                | Type::Buffer
                                | Type::Every => String::new(),
                                _ => format!(
                                    "\nleft: {}\nright: {}\nFirst mismatch: ",
                                    Color::red(left.print(nominals)),
                                    Color::red(right.print(nominals)),
                                ),
                            },
                        }
                    },
                    mismatch.print(nominals)
                )
            })
    }

    /// Returns a tuple containing `Type`s default value and a `bool` representing whether use of
    /// that default is type-safe.
    // TODO: have this resolve nominal types
    pub fn default_value(&self) -> (Value, bool) {
        match self {
            Type::Void => (Value::none(), false),
            Type::Buffer => (Value::new_buffer(vec![]), true),
            Type::Uint | Type::Int | Type::Bytes32 | Type::EthAddress | Type::Bool => {
                (Value::Int(Uint256::zero()), true)
            }
            Type::Tuple(tvec) => {
                let mut default_tup = Vec::new();
                let mut is_safe = true;
                for t in tvec {
                    let (def, safe) = t.default_value();
                    default_tup.push(def);
                    is_safe = is_safe && safe;
                }
                (Value::new_tuple(default_tup), is_safe)
            }
            Type::Array(t) => {
                let (def, safe) = t.default_value();
                (
                    Value::new_tuple(vec![
                        Value::Int(Uint256::one()),
                        Value::Int(Uint256::one()),
                        Value::new_tuple(vec![def]),
                    ]),
                    safe,
                )
            }
            Type::FixedArray(t, sz) => {
                let (default_val, safe) = t.default_value();
                let mut val = Value::new_tuple(vec![default_val; 8]);
                let mut chunk_size = 1;
                while chunk_size * TUPLE_SIZE < *sz {
                    val = Value::new_tuple(vec![val; 8]);
                    chunk_size *= 8;
                }
                (val, safe)
            }
            Type::Struct(fields, _) => {
                let mut vals = Vec::new();
                let mut is_safe = true;
                for field in fields {
                    let (val, safe) = field.tipe.default_value();
                    vals.push(val);
                    is_safe = is_safe && safe;
                }
                (value_from_field_list(vals), is_safe)
            }
            Type::Map(_, _) | Type::Func(_, _, _) | Type::Nominal(_, _, _) => {
                (Value::none(), false)
            }
            Type::Any => (Value::none(), true),
            Type::Every => (Value::none(), false),
            Type::Option(_) => (Value::new_tuple(vec![Value::Int(Uint256::zero())]), true),
            Type::Union(_) => (Value::none(), false),
            Type::GenericSlot(..) => panic!("Generics don't have default values"),
            Type::Generic(..) => panic!("Generics don't have default values"),
        }
    }

    pub fn display(&self) -> String {
        self.display_indented(0, "::", None, false, &NamedTypes::new())
            .0
    }

    pub fn print(&self, nominals: &NamedTypes) -> String {
        self.display_indented(0, "::", None, false, nominals).0
    }

    pub fn display_separator(
        &self,
        separator: &str,
        prefix: Option<&str>,
        include_pathname: bool,
        nominals: &NamedTypes,
    ) -> (String, HashSet<(Type, String)>) {
        self.display_indented(0, separator, prefix, include_pathname, nominals)
    }

    fn display_indented(
        &self,
        indent_level: usize,
        separator: &str,
        prefix: Option<&str>,
        include_pathname: bool,
        nominals: &NamedTypes,
    ) -> (String, HashSet<(Type, String)>) {
        let mut type_set = HashSet::new();
        match self {
            Type::Void => ("void".to_string(), type_set),
            Type::Uint => ("uint".to_string(), type_set),
            Type::Int => ("int".to_string(), type_set),
            Type::Bool => ("bool".to_string(), type_set),
            Type::Bytes32 => ("bytes32".to_string(), type_set),
            Type::EthAddress => ("address".to_string(), type_set),
            Type::Buffer => ("buffer".to_string(), type_set),
            Type::GenericSlot(id) => (format!("generic' {}", id), type_set),
            Type::Generic(id) => (format!("generic {}", id), type_set),
            Type::Tuple(subtypes) => {
                let mut out = "(".to_string();
                for s in subtypes {
                    let (displayed, subtypes) = s.display_indented(
                        indent_level,
                        separator,
                        prefix,
                        include_pathname,
                        nominals,
                    );
                    out.push_str(&(displayed + ", "));
                    type_set.extend(subtypes);
                }
                if !subtypes.is_empty() {
                    out.pop();
                    out.pop();
                }
                out.push(')');
                (out, type_set)
            }
            Type::Array(t) => {
                let (displayed, subtypes) =
                    t.display_indented(indent_level, separator, prefix, include_pathname, nominals);
                (format!("[]{}", displayed), subtypes)
            }
            Type::FixedArray(t, size) => {
                let (displayed, subtypes) =
                    t.display_indented(indent_level, separator, prefix, include_pathname, nominals);
                (format!("[{}]{}", size, displayed), subtypes)
            }
            Type::Struct(fields, methods) => {
                let mut out = "struct {\n".to_string();
                for _ in 0..indent_level {
                    out.push_str("    ");
                }
                for field in fields {
                    //This should indent further when dealing with sub-structs
                    let (displayed, subtypes) = field.tipe.display_indented(
                        indent_level + 1,
                        separator,
                        prefix,
                        include_pathname,
                        nominals,
                    );
                    out.push_str(&format!("    {}: {},\n", field.name, displayed));
                    for _ in 0..indent_level {
                        out.push_str("    ");
                    }
                    type_set.extend(subtypes);
                }
                for method in methods {
                    out.push_str(&format!(
                        "    trait {} {},\n",
                        method.trait_name, method.func_name
                    ));
                }
                out.push('}');
                (out, type_set)
            }
            Type::Nominal(path, id, spec) => {
                let out = format!(
                    "{}{}{}{}",
                    prefix.unwrap_or(""),
                    if include_pathname {
                        path.iter()
                            .map(|name| name.clone() + "_")
                            .collect::<String>()
                    } else {
                        format!("")
                    },
                    nominals
                        .get(&(path.clone(), *id))
                        .map(|(_, name)| name.clone())
                        .unwrap_or(format!("???")),
                    match spec.len() {
                        0 => format!(""),
                        _ => {
                            let mut out = format!("<");
                            for s in spec {
                                let (displayed, subtypes) = s.display_indented(
                                    indent_level,
                                    separator,
                                    prefix,
                                    include_pathname,
                                    nominals,
                                );
                                out.push_str(&(displayed + ", "));
                                type_set.extend(subtypes);
                            }
                            if !spec.is_empty() {
                                out.pop();
                                out.pop();
                            }
                            out.push_str("> ");
                            // TODO: Make this work for recursive generics
                            /*out.push_str(&format!(
                                "> := {}",
                                match self.rep(nominals) {
                                    Ok(tipe) => tipe.print(nominals),
                                    Err(_) => format!("???"),
                                }
                            ));*/
                            out
                        }
                    }
                );
                type_set.insert((
                    self.clone(),
                    nominals
                        .get(&(path.clone(), *id))
                        .map(|d| d.1.clone())
                        .unwrap_or_else(|| "bad".to_string()),
                ));
                (out, type_set)
            }
            Type::Func(prop, args, ret) => {
                let mut out = String::new();
                if prop.view {
                    out.push_str("view ");
                }
                if prop.write {
                    out.push_str("write ");
                }
                out.push_str("func(");
                for arg in args {
                    let (displayed, subtypes) = arg.display_indented(
                        indent_level,
                        separator,
                        prefix,
                        include_pathname,
                        nominals,
                    );
                    out.push_str(&(displayed + ", "));
                    type_set.extend(subtypes)
                }
                out.push(')');
                if **ret != Type::Void {
                    let (displayed, subtypes) = ret.display_indented(
                        indent_level,
                        separator,
                        prefix,
                        include_pathname,
                        nominals,
                    );
                    out.push_str(" -> ");
                    out.push_str(&displayed);
                    type_set.extend(subtypes);
                }
                (out, type_set)
            }
            Type::Map(key, val) => {
                let (key_display, key_subtypes) = key.display_indented(
                    indent_level,
                    separator,
                    prefix,
                    include_pathname,
                    nominals,
                );
                type_set.extend(key_subtypes);
                let (val_display, val_subtypes) = val.display_indented(
                    indent_level,
                    separator,
                    prefix,
                    include_pathname,
                    nominals,
                );
                type_set.extend(val_subtypes);
                (format!("map<{},{}>", key_display, val_display), type_set)
            }
            Type::Any => ("any".to_string(), type_set),
            Type::Every => ("every".to_string(), type_set),
            Type::Option(t) => {
                let (display, subtypes) =
                    t.display_indented(indent_level, separator, prefix, include_pathname, nominals);
                (format!("option<{}> ", display), subtypes)
            }
            Type::Union(types) => {
                let mut s = String::from("union<");
                let mut subtypes = HashSet::new();
                for tipe in types {
                    let (name, new_subtypes) = tipe.display_indented(
                        indent_level + 1,
                        separator,
                        prefix,
                        include_pathname,
                        nominals,
                    );
                    s.push_str(&name);
                    s.push_str(", ");
                    subtypes.extend(new_subtypes);
                }
                s.push('>');
                (s, subtypes)
            }
        }
    }
}

/// Checks generic parameter names for those that may be duplicates or unused.
pub fn check_generic_parameters(
    params: Vec<(StringId, DebugInfo)>,
    string_table: &StringTable,
) -> Result<Vec<StringId>, CompileError> {
    let mut seen = HashSet::new();
    for (id, debug) in params.iter() {
        if !seen.insert(id) {
            return Err(CompileError::new(
                "Parser error",
                format!(
                    "Duplicate generic parameter {}",
                    Color::red(string_table.name_from_id(*id))
                ),
                debug.locs(),
            ));
        }
    }

    Ok(params.into_iter().map(|(name, _)| name).collect())
}

pub fn check_typedef(
    id: StringId,
    tipe: Type,
    params: Vec<(StringId, DebugInfo)>,
    string_table: &StringTable,
) -> Result<Type, CompileError> {
    let names = check_generic_parameters(params.clone(), &string_table)?;
    let tipe = tipe.make_generic(&names);

    for (index, (name, debug_info)) in params.into_iter().enumerate() {
        let unused = tipe
            .find(&mut |tipe: &Type| *tipe == Type::GenericSlot(index))
            .is_empty();
        if unused {
            Err(CompileError::new(
                "Generics error",
                format!(
                    "Type {}'s generic arg {} isn't used",
                    Color::red(string_table.name_from_id(id)),
                    Color::red(string_table.name_from_id(name)),
                ),
                debug_info.locs(),
            ))?;
        }
    }

    Ok(tipe)
}

pub fn type_vectors_covariant_castable(
    tvec1: &[Type],
    tvec2: &[Type],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.covariant_castable(t2, nominals, seen.clone()))
}

pub fn type_vectors_castable(
    tvec1: &[Type],
    tvec2: &[Type],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.castable(t2, nominals, seen.clone()))
}

/// Returns true if each type in tvec2 is a subtype of the type in tvec1 at the same index, and tvec1
/// and tvec2 have the same length.
pub fn type_vectors_assignable(
    tvec1: &[Type],
    tvec2: &[Type],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.assignable(t2, nominals, seen.clone()))
}

fn field_vectors_covariant_castable(
    tvec1: &[StructField],
    tvec2: &[StructField],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.tipe.covariant_castable(&t2.tipe, nominals, seen.clone()))
}

fn field_vectors_castable(
    tvec1: &[StructField],
    tvec2: &[StructField],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.tipe.castable(&t2.tipe, nominals, seen.clone()))
}

/// Identical to `type_vectors_assignable`
pub fn arg_vectors_assignable(
    tvec1: &[Type],
    tvec2: &[Type],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1
            .iter()
            .zip(tvec2)
            .all(|(t1, t2)| t1.assignable(t2, nominals, seen.clone()))
}

pub fn field_vectors_mismatch(
    tvec1: &[StructField],
    tvec2: &[StructField],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> Option<TypeMismatch> {
    for (t1, t2) in tvec1.iter().zip(tvec2.iter()) {
        if let Some(mismatch) = t1.tipe.first_mismatch(&t2.tipe, nominals, seen.clone()) {
            return Some(TypeMismatch::FieldType(t1.name.clone(), Box::new(mismatch)));
        }
        if t1.name != t2.name {
            return Some(TypeMismatch::FieldName(t1.name.clone(), t2.name.clone()));
        }
    }
    if tvec1.len() != tvec2.len() {
        return Some(TypeMismatch::Length(tvec1.len(), tvec2.len()));
    }
    None
}

/// Identical to `type_vectors_assignable` but using StructField slices as inputs and comparing their
/// inner types.
fn field_vectors_assignable(
    tvec1: &[StructField],
    tvec2: &[StructField],
    nominals: &NamedTypes,
    seen: HashSet<(Type, Type)>,
) -> bool {
    tvec1.len() == tvec2.len()
        && tvec1.iter().zip(tvec2).all(|(t1, t2)| {
            t1.tipe.assignable(&t2.tipe, nominals, seen.clone()) && t1.name == t2.name
        })
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
            | (Type::Buffer, Type::Buffer)
            | (Type::Every, Type::Every) => true,
            (Type::Tuple(v1), Type::Tuple(v2)) => type_vectors_equal(&v1, &v2),
            (Type::Array(a1), Type::Array(a2)) => *a1 == *a2,
            (Type::FixedArray(a1, s1), Type::FixedArray(a2, s2)) => (s1 == s2) && (*a1 == *a2),
            (Type::Struct(f1, m1), Type::Struct(f2, m2)) => (f1, m1) == (f2, m2),
            (Type::Map(k1, v1), Type::Map(k2, v2)) => (*k1 == *k2) && (*v1 == *v2),
            (Type::Func(p1, a1, r1), Type::Func(p2, a2, r2)) => {
                (p1 == p2) && type_vectors_equal(&a1, &a2) && (*r1 == *r2)
            }
            (Type::Nominal(p1, id1, s1), Type::Nominal(p2, id2, s2)) => {
                (p1, id1, s1) == (p2, id2, s2)
            }
            (Type::Option(x), Type::Option(y)) => *x == *y,
            (Type::Union(x), Type::Union(y)) => type_vectors_equal(x, y),
            (Type::GenericSlot(x), Type::GenericSlot(y)) => *x == *y,
            (Type::Generic(x), Type::Generic(y)) => *x == *y,
            (_, _) => false,
        }
    }
}

/// Returns true if the contents of the slices are equal
fn type_vectors_equal(v1: &[Type], v2: &[Type]) -> bool {
    v1 == v2
}

#[derive(Debug)]
pub enum TypeMismatch {
    Type(Type, Type),
    FieldName(String, String),
    FieldType(String, Box<TypeMismatch>),
    UnresolvedRight(Type),
    UnresolvedLeft(Type),
    UnresolvedBoth(Type, Type),
    Length(usize, usize),
    ArrayMismatch(Box<TypeMismatch>),
    ArrayLength(usize, usize),
    Tuple(usize, Box<TypeMismatch>),
    TupleLength(usize, usize),
    FuncArg(usize, Box<TypeMismatch>),
    FuncArgLength(usize, usize),
    FuncReturn(Box<TypeMismatch>),
    Map {
        is_key: bool,
        inner: Box<TypeMismatch>,
    },
    Option(Box<TypeMismatch>),
    Union(usize, Box<TypeMismatch>),
    UnionLength(usize, usize),
    View,
    Write,
}

impl TypeMismatch {
    fn print(&self, nominals: &NamedTypes) -> String {
        match self {
            TypeMismatch::Type(left, right) => format!(
                "expected {} got {}",
                Color::red(left.print(nominals)),
                Color::red(right.print(nominals))
            ),
            TypeMismatch::FieldType(name, problem) => format!(
                "in field {}: {}",
                Color::red(name),
                Color::red(problem.print(nominals))
            ),
            TypeMismatch::FieldName(left, right) => format!(
                "expected field name {}, got {}",
                Color::red(left),
                Color::red(right)
            ),
            TypeMismatch::UnresolvedRight(tipe) => format!(
                "could not resolve right-hand type {}",
                Color::red(tipe.print(nominals))
            ),
            TypeMismatch::UnresolvedLeft(tipe) => format!(
                "could not resolve left-hand type {}",
                Color::red(tipe.print(nominals))
            ),
            TypeMismatch::UnresolvedBoth(left, right) => format!(
                "could not resolve both right hand type {} and left hand type {}",
                Color::red(left.print(nominals)),
                Color::red(right.print(nominals))
            ),
            TypeMismatch::Length(left, right) => format!(
                "structs of different lengths: expected length {} got length {}",
                Color::red(left),
                Color::red(right)
            ),
            TypeMismatch::ArrayLength(left, right) => format!(
                "arrays of different lengths: expected length {} got length {}",
                Color::red(left),
                Color::red(right)
            ),
            TypeMismatch::ArrayMismatch(mismatch) => {
                format!("inner array type mismatch {}", mismatch.print(nominals))
            }
            TypeMismatch::Tuple(index, mismatch) => format!(
                "in tuple field {}: {}",
                Color::red(index),
                mismatch.print(nominals)
            ),
            TypeMismatch::TupleLength(left, right) => format!(
                "tuples of different lengths: expected length {} got length {}",
                Color::red(left),
                Color::red(right)
            ),
            TypeMismatch::FuncArg(index, mismatch) => format!(
                "in function argument {}: {}",
                Color::red(index + 1),
                mismatch.print(nominals)
            ),
            TypeMismatch::FuncArgLength(left, right) => format!(
                "left func has {} args but right func has {} args",
                Color::red(left),
                Color::red(right)
            ),
            TypeMismatch::FuncReturn(mismatch) => {
                format!("in function return type: {}", mismatch.print(nominals))
            }
            TypeMismatch::Map { is_key, inner } => format!(
                "in map {}: {}",
                if *is_key { "key" } else { "value" },
                inner.print(nominals)
            ),
            TypeMismatch::Option(mismatch) => {
                format!("in inner option type: {}", mismatch.print(nominals))
            }
            TypeMismatch::Union(index, mismatch) => format!(
                "In type {} of union: {}",
                Color::red(index + 1),
                mismatch.print(nominals)
            ),
            TypeMismatch::UnionLength(left, right) => format!(
                "left union has {} args but right union has {} args",
                Color::red(left),
                Color::red(right)
            ),
            TypeMismatch::View => format!(
                "assigning {} function to non-view function",
                Color::red("view")
            ),
            TypeMismatch::Write => format!(
                "assigning {} function to non-view function",
                Color::red("write")
            ),
        }
    }
}

/// Field of a struct, contains field name and underlying type.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct StructField {
    pub name: String,
    pub tipe: Type,
}

impl StructField {
    pub fn new(name: String, tipe: Type) -> StructField {
        StructField { name, tipe }
    }
}

/// Trait of a struct, contains field name and underlying func ref.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct StructTrait {
    /// The name of the trait.
    pub trait_name: String,
    /// The name of the top-level type that contains this trait.
    pub type_name: String,
    /// The name of the func assigned to this trait.
    pub func_name: String,
    /// The path to the trait.
    pub path: ModPath,
    /// The specialization to apply at the time of use.
    pub spec: Vec<Type>,
    /// The unique id of the type in the module that has this trait.
    pub type_unique_id: u64,
    /// The unique id of the func assigned to this trait.
    pub func_unique_id: u64,
}

impl StructTrait {
    pub fn new(
        trait_name: String,
        func_name: String,
        type_name: String,
        path: ModPath,
        spec: Vec<Type>,
    ) -> StructTrait {
        let type_unique_id = Import::unique_id(&path, &type_name);
        let func_unique_id = Import::unique_id(&path, &func_name);
        StructTrait {
            trait_name,
            type_name,
            func_name,
            path,
            spec,
            type_unique_id,
            func_unique_id,
        }
    }
}

/// Argument to a function, contains field name and underlying type.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FuncArg {
    pub name: StringId,
    pub tipe: Type,
    pub debug_info: DebugInfo,
}

pub fn new_func_arg(name: StringId, tipe: Type, debug_info: DebugInfo) -> FuncArg {
    FuncArg {
        name,
        tipe,
        debug_info,
    }
}

/// Represents a declaration of a global mini variable.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GlobalVar {
    #[serde(alias = "name_id")]
    pub id: StringId,
    pub name: String,
    pub tipe: Type,
    pub offset: Option<usize>,
    #[serde(default)]
    pub debug_info: DebugInfo,
}

impl GlobalVar {
    pub fn new(id: StringId, name: String, tipe: Type, debug_info: DebugInfo) -> Self {
        Self {
            id,
            name,
            tipe,
            offset: None,
            debug_info,
        }
    }
}

/// Represents a top level function declaration.  The view, write, args, and ret_type fields are
/// assumed to be derived from tipe, and this must be upheld by the user of this type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func<T = Statement> {
    pub name: String,
    pub id: StringId,
    pub args: Vec<FuncArg>,
    pub ret_type: Type,
    pub code: Vec<T>,
    pub tipe: Type,
    pub public: bool,
    pub captures: BTreeSet<StringId>,
    /// The names of this func's generic types. The order specifies which goes where.
    pub generics: Vec<StringId>,
    /// The minimum tuple-tree size needed to generate this func
    pub frame_size: usize,
    /// A global id unique to this function used for building jump labels
    pub unique_id: Option<LabelId>,
    /// Additional properties like viewness that this func has
    pub properties: FuncProperties,
    pub debug_info: DebugInfo,
}

impl Func {
    pub fn new(
        name: String,
        id: StringId,
        public: bool,
        view: bool,
        write: bool,
        closure: bool,
        args: Vec<FuncArg>,
        ret_type: Option<Type>,
        code: Vec<Statement>,
        captures: BTreeSet<StringId>,
        generics: Vec<StringId>,
        frame_size: usize,
        debug_info: DebugInfo,
    ) -> Self {
        let mut arg_types = Vec::new();
        let args_vec = args.to_vec();
        for arg in args.iter() {
            arg_types.push(arg.tipe.clone());
        }
        let prop = FuncProperties::new(view, write, closure, public);
        let ret_type = ret_type.unwrap_or(Type::Void);
        Func {
            name,
            id,
            args: args_vec,
            ret_type: ret_type.clone(),
            code,
            tipe: Type::Func(prop, arg_types, Box::new(ret_type)),
            public,
            captures,
            generics,
            frame_size,
            unique_id: None,
            properties: prop,
            debug_info,
        }
    }
}

/// The properties of a function or closure.
#[derive(Debug, Clone, Copy, Eq, Serialize, Deserialize, Derivative)]
#[derivative(Hash)]
pub struct FuncProperties {
    pub view: bool,
    pub write: bool,
    pub closure: bool,
    #[serde(default)]
    #[derivative(Hash = "ignore")]
    pub public: bool,
}

/// We only want equality when comparing types, for which only purity makes sense
impl PartialEq for FuncProperties {
    fn eq(&self, other: &Self) -> bool {
        self.purity() == other.purity()
    }
}

impl FuncProperties {
    pub fn new(view: bool, write: bool, closure: bool, public: bool) -> Self {
        FuncProperties {
            view,
            write,
            closure,
            public,
        }
    }

    pub fn pure() -> Self {
        Self::new(false, false, false, false)
    }

    pub fn purity(&self) -> (bool, bool) {
        (self.view, self.write)
    }
}

/// A statement in the mini language with associated `DebugInfo` that has not yet been type checked.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
    pub kind: StatementKind,
    pub debug_info: DebugInfo,
}

/// A raw statement containing no debug information that has not yet been type checked.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
    ReturnVoid(),
    Return(Expr),
    Break(Option<Expr>, Option<String>),
    Expression(Expr),
    Let(MatchPattern, Expr),
    Assign(StringId, Expr),
    While(Expr, Vec<Statement>),
    Asm(Vec<Instruction>, Vec<Expr>),
    DebugPrint(Expr),
    Assert(Expr),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MatchPattern<T = ()> {
    pub(crate) kind: MatchPatternKind<MatchPattern<T>>,
    pub(crate) debug_info: DebugInfo,
    pub(crate) cached: T,
}

/// Either a single identifier or a tuple of identifiers, used in mini let bindings.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum MatchPatternKind<T> {
    Bind(StringId),
    Assign(StringId),
    Tuple(Vec<T>),
}

impl<T> MatchPattern<T> {
    pub fn new_bind(id: StringId, debug_info: DebugInfo, cached: T) -> Self {
        Self {
            kind: MatchPatternKind::Bind(id),
            debug_info,
            cached,
        }
    }
    pub fn new_assign(id: StringId, debug_info: DebugInfo, cached: T) -> Self {
        Self {
            kind: MatchPatternKind::Assign(id),
            debug_info,
            cached,
        }
    }
    pub fn new_tuple(id: Vec<MatchPattern<T>>, debug_info: DebugInfo, cached: T) -> Self {
        Self {
            kind: MatchPatternKind::Tuple(id),
            debug_info,
            cached,
        }
    }
    pub fn collect_identifiers(&self) -> Vec<(StringId, bool, DebugInfo)> {
        match &self.kind {
            MatchPatternKind::Bind(id) => vec![(*id, false, self.debug_info)],
            MatchPatternKind::Assign(id) => vec![(*id, true, self.debug_info)],
            MatchPatternKind::Tuple(pats) => pats
                .iter()
                .flat_map(|pat| pat.collect_identifiers())
                .collect(),
        }
    }
}

/// An identifier or array index for left-hand-side substructure assignments
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubData {
    Dot(StringId),
    ArrayOrMap(Expr),
}

/// Represents a constant mini value of type Option<T> for some type T.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum OptionConst {
    _Some(Box<Constant>),
    None(Type),
}

/// Represents a mini constant value. This is different than `Value` as it encodes Options as distinct
/// from tuples.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Constant {
    Uint(Uint256),
    Int(Uint256),
    Bool(bool),
    Option(OptionConst),
    Null,
}

impl OptionConst {
    /// Gets the type of the value
    pub(crate) fn type_of(&self) -> Type {
        Type::Option(Box::new(match self {
            OptionConst::_Some(c) => (*c).type_of(),
            OptionConst::None(t) => t.clone(),
        }))
    }

    /// Exracts the value from the Constant
    pub(crate) fn value(&self) -> Value {
        match self {
            OptionConst::_Some(c) => {
                Value::new_tuple(vec![Value::Int(Uint256::one()), c.clone().value()])
            }
            OptionConst::None(_) => Value::new_tuple(vec![Value::Int(Uint256::zero())]),
        }
    }
}

impl Constant {
    /// Gets the type of the value
    pub(crate) fn type_of(&self) -> Type {
        match self {
            Constant::Uint(_) => Type::Uint,
            Constant::Int(_) => Type::Int,
            Constant::Bool(_) => Type::Bool,
            Constant::Option(inner) => inner.type_of(),
            Constant::Null => Type::Void,
        }
    }

    /// Exracts the value from the Constant
    pub(crate) fn value(&self) -> Value {
        match self {
            Constant::Uint(ui) => Value::Int(ui.clone()),
            Constant::Int(i) => Value::Int(i.clone()),
            Constant::Bool(b) => Value::Int(Uint256::from_bool(*b)),
            Constant::Option(c) => c.value(),
            Constant::Null => Value::none(),
        }
    }
}

/// A mini expression that has not yet been type checked with an associated `DebugInfo`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub debug_info: DebugInfo,
}

/// A mini expression that has not yet been type checked, contains no debug information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    UnaryOp(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Trinary(TrinaryOp, Box<Expr>, Box<Expr>, Box<Expr>),
    ShortcutOr(Box<Expr>, Box<Expr>),
    ShortcutAnd(Box<Expr>, Box<Expr>),
    VariableRef(StringId, Vec<Type>),
    TupleRef(Box<Expr>, Uint256),
    DotRef(Box<Expr>, String),
    Constant(Constant),
    OptionInitializer(Box<Expr>),
    FunctionCall(Box<Expr>, Vec<Expr>),
    CodeBlock(CodeBlock),
    ArrayOrMapRef(Box<Expr>, Box<Expr>),
    StructInitializer(Vec<FieldInitializer>),
    Tuple(Vec<Expr>),
    NewArray(Box<Expr>, Type),
    NewFixedArray(usize, Option<Box<Expr>>),
    NewMap(Type, Type),
    NewUnion(Vec<Type>, Box<Expr>),
    ArrayOrMapMod(Box<Expr>, Box<Expr>, Box<Expr>),
    StructMod(Box<Expr>, String, Box<Expr>),
    WeakCast(Box<Expr>, Type),
    Cast(Box<Expr>, Type),
    CovariantCast(Box<Expr>, Type),
    UnsafeCast(Box<Expr>, Type),
    Asm(Type, Vec<Instruction>, Vec<Expr>),
    Error,
    GetGas,
    SetGas(Box<Expr>),
    Try(Box<Expr>),
    If(Box<Expr>, CodeBlock, Option<CodeBlock>),
    IfLet(StringId, Box<Expr>, CodeBlock, Option<CodeBlock>),
    Loop(Vec<Statement>),
    UnionCast(Box<Expr>, Type),
    NewBuffer,
    Quote(Vec<u8>),
    Closure(Func),
}

impl Expr {
    /// Returns an expression that applies unary operator op to e.
    pub fn new_unary(op: UnaryOp, e: Expr, loc: Option<Location>) -> Self {
        Self {
            kind: ExprKind::UnaryOp(op, Box::new(e)),
            debug_info: DebugInfo::from(loc),
        }
    }

    /// Returns an expression that applies binary operator op to e1 and e2.
    pub fn new_binary(op: BinaryOp, e1: Expr, e2: Expr, loc: Option<Location>) -> Self {
        Self {
            kind: ExprKind::Binary(op, Box::new(e1), Box::new(e2)),
            debug_info: DebugInfo::from(loc),
        }
    }

    /// Returns an expression that applies trinary operator op to e1, e2, and e3.
    pub fn new_trinary(op: TrinaryOp, e1: Expr, e2: Expr, e3: Expr, loc: Option<Location>) -> Self {
        Self {
            kind: ExprKind::Trinary(op, Box::new(e1), Box::new(e2), Box::new(e3)),
            debug_info: DebugInfo::from(loc),
        }
    }

    /// Creates an expression whose DebugInfo is populated in-place at the parsing site
    pub fn lno(kind: ExprKind, lines: &Lines, lno: usize, file: u64) -> Self {
        Self::new(kind, DebugInfo::here(lines, lno, file))
    }

    pub fn new(kind: ExprKind, debug_info: DebugInfo) -> Self {
        Self { kind, debug_info }
    }
}

/// A mini unary operator.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
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

/// A mini binary operator.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
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
    ShiftLeft,
    ShiftRight,
    _LogicalAnd,
    LogicalOr,
    Hash,
    GetBuffer8,
    GetBuffer64,
    GetBuffer256,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum TrinaryOp {
    SetBuffer8,
    SetBuffer64,
    SetBuffer256,
}

/// Used in StructInitializer expressions to map expressions to fields of the struct.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FieldInitializer<T = Expr> {
    pub name: String,
    pub value: T,
}

impl<T> FieldInitializer<T> {
    pub fn new(name: String, value: T) -> Self {
        FieldInitializer { name, value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeBlock {
    pub body: Vec<Statement>,
    pub ret_expr: Option<Box<Expr>>,
}

impl CodeBlock {
    pub fn new(body: Vec<Statement>, ret_expr: Option<Box<Expr>>) -> Self {
        Self { body, ret_expr }
    }
}
