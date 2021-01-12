/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

//!Contains declaration of structs and methods for sym-table structs.

use crate::stringtable::StringId;
use std::collections::HashMap;

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
