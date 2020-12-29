/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Contains declaration of structs and methods for sym-table structs.

use crate::stringtable::StringId;
use std::collections::HashMap;

///A sym-table, which acts as a hybrid between a HashMap and a linked list.
///
/// Currently the links between nodes are handled with references, however in the future this may be
/// changed to owned pointers.  Individual nodes are either a single pair of a StringId, and a
/// reference to T, or a HashMap with StringId keys and T reference values.
#[derive(Debug, Clone)]
pub enum SymTable<'a, T> {
    Empty,
    Single(StringId, &'a T, &'a SymTable<'a, T>),
    Multi(HashMap<StringId, &'a T>, &'a SymTable<'a, T>),
}

impl<'a, T> SymTable<'a, T> {
    ///Constructs an empty SymTable
    pub fn new() -> Self {
        SymTable::Empty
    }

    ///Adds a node to the front of the linked list containing a single T reference pair
    pub fn push_one(self: &'a SymTable<'a, T>, sid: StringId, t: &'a T) -> Self {
        SymTable::Single(sid, t, self)
    }

    ///Adds a node to the front of the linked list containing a hashmap
    pub fn push_multi(self: &'a SymTable<'a, T>, hm: HashMap<StringId, &'a T>) -> Self {
        SymTable::Multi(hm, self)
    }

    ///Looks up sid in each node and returns the first match found, and None if no match is found.
    pub fn get(&self, sid: StringId) -> Option<&'a T> {
        match self {
            SymTable::Empty => None,
            SymTable::Single(s, t, rest) => {
                if *s == sid {
                    Some(t)
                } else {
                    rest.get(sid)
                }
            }
            SymTable::Multi(hm, rest) => match hm.get(&sid) {
                Some(t) => Some(t),
                None => rest.get(sid),
            },
        }
    }
}

///Identical to SymTable, except it stores owned values of T rather than references.
#[derive(Clone)]
pub enum CopyingSymTable<'a, T: Copy> {
    Empty,
    Single(StringId, T, &'a CopyingSymTable<'a, T>),
    Multi(HashMap<StringId, T>, &'a CopyingSymTable<'a, T>),
}

impl<'a, T: Copy> CopyingSymTable<'a, T> {
    /// Constructs an empty SymTable
    pub fn new() -> Self {
        CopyingSymTable::Empty
    }

    ///Adds a node to the front of the linked list containing a single `T`-`ID` pair
    pub fn push_one(self: &'a CopyingSymTable<'a, T>, sid: StringId, t: T) -> Self {
        CopyingSymTable::Single(sid, t, self)
    }

    ///Adds a node to the front of the linked list containing a hashmap
    pub fn push_multi(self: &'a CopyingSymTable<'a, T>, hm: HashMap<StringId, T>) -> Self {
        CopyingSymTable::Multi(hm, self)
    }

    ///Looks up sid in each node and returns the first match found, and None if no match is found.
    pub fn get(&self, sid: StringId) -> Option<T> {
        match self {
            CopyingSymTable::Empty => None,
            CopyingSymTable::Single(s, t, rest) => {
                if *s == sid {
                    Some(*t)
                } else {
                    rest.get(sid)
                }
            }
            CopyingSymTable::Multi(hm, rest) => match hm.get(&sid) {
                Some(t) => Some(*t),
                None => rest.get(sid),
            },
        }
    }
}
