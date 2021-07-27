/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::ModulePath;
use crate::link::Import;
use crate::pos::Location;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub type StringId = usize;

/// Maps `String`s to `usize` IDs.
#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct StringTable {
    table: HashMap<String, StringId>,
    by_id: Vec<String>,
}

impl StringTable {
    pub fn new() -> Self {
        let table: HashMap<String, StringId> = HashMap::new();
        let by_id = Vec::new();
        StringTable { table, by_id }
    }

    /// Returns the `StringID` associated with `name` if it exists, if not creates a new entry and
    /// returns the newly created ID.
    pub fn draw(&mut self, name: String) -> StringId {
        match self.table.get(&name) {
            Some(id) => *id,
            None => {
                let new_id = self.table.len();
                self.table.insert(name.clone(), new_id);
                self.by_id.push(name.clone());
                new_id
            }
        }
    }

    /// If an ID exists, returns it, if not returns `None`.
    pub fn get(&self, name: &str) -> Option<StringId> {
        self.table.get(name).cloned()
    }

    /// Takes a `usize` ID and returns the associated `String`
    pub fn name_from_id(&self, name: StringId) -> &String {
        // TODO: Getting rid of this
        &self.by_id[name as usize]
    }

    /// Creates a prepopulated `StringTable` and import set that includes the compiler builtin functions.
    pub fn builtins_table(path: &ModulePath) -> (Self, Vec<Import>) {
        let mut string_table = Self::new();
        let mut imports = vec![];

        if path.iter().ne(&["core", "array"]) {
            for op in &["New", "Get", "Set", "Resize"] {
                let name = format!("builtin_array{}", op);
                let id = string_table.draw(name.clone());
                let core = vec!["core".to_string(), "array".to_string()];
                imports.push(Import::new(
                    core,
                    name.clone(),
                    Ident::new(id, Location::default(), name),
                ));
            }
        }
        if path.iter().ne(&["core", "kvs"]) {
            for op in &["New", "Get", "Set", "Delete", "Forall"] {
                let name = format!("builtin_kvs{}", op);
                let id = string_table.draw(name.clone());
                let core = vec!["core".to_string(), "kvs".to_string()];
                imports.push(Import::new(
                    core,
                    name.clone(),
                    Ident::new(id, Location::default(), name),
                ));
            }
        }

        (string_table, imports)
    }
}

/// Represents an identifier parsed at a given location.
#[derive(Clone, Debug, Serialize, Deserialize, Eq)]
pub struct Ident {
    pub id: StringId,
    pub loc: Location,
    pub string: String,
}

impl Ident {
    pub fn new(id: StringId, loc: Location, string: String) -> Self {
        Ident { id, loc, string }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
        (self.id == other.id) && (self.loc.file_id == other.loc.file_id)
    }
}
