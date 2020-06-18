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

use std::collections::HashMap;

pub type StringId = usize;

#[derive(Clone, Debug, Default)]
pub struct StringTable<'a> {
    next_id: StringId,
    table: HashMap<&'a str, StringId>,
    by_id: Vec<&'a str>,
}

impl<'a> StringTable<'a> {
    pub fn new() -> Self {
        let table: HashMap<&'a str, StringId> = HashMap::new();
        let by_id = Vec::new();
        StringTable {
            next_id: 0,
            table,
            by_id,
        }
    }

    pub fn get(&mut self, name: &'a str) -> StringId {
        match self.table.get(name) {
            Some(id) => *id,
            None => {
                let new_id = self.next_id;
                self.next_id += 1;
                self.table.insert(name, new_id);
                self.by_id.push(name);
                new_id
            }
        }
    }

    pub fn get_if_exists(&self, name: &'a str) -> Option<&StringId> {
        self.table.get(name)
    }

    pub fn name_from_id(&self, name: StringId) -> &'a str {
        self.by_id[name as usize]
    }
}
