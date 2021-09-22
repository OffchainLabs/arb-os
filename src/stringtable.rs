/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use serde::de::{Error, MapAccess, Visitor};
use serde::{de, Deserialize, Deserializer, Serialize};
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Default)]
pub struct StringId {
    path: Vec<String>,
    pub id: String,
}

impl fmt::Display for StringId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for component in &self.path {
            write!(f, "{}::", component)?
        }
        write!(f, "{}", self.id)
    }
}

struct StringIdVisitor;

impl<'de> Visitor<'de> for StringIdVisitor {
    type Value = StringId;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("Expected hex string")
    }
    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(StringId::new(vec![], v.to_string()))
    }
    fn visit_map<E>(self, mut map: E) -> Result<Self::Value, E::Error>
    where
        E: MapAccess<'de>,
    {
        let (path_key, path) = map.next_entry::<&str, Vec<String>>()?.unwrap();
        if path_key != "path" {
            return Err(E::Error::custom("wrong key name: expected \"path\""));
        }
        let (id_key, id) = map.next_entry::<&str, String>()?.unwrap();
        if id_key != "id" {
            return Err(E::Error::custom("wrong key name: expected \"id\""));
        }
        Ok(StringId::new(path, id))
    }
}

impl<'de> Deserialize<'de> for StringId {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(StringIdVisitor)
    }
}

impl StringId {
    pub(crate) fn new(path: Vec<String>, id: String) -> Self {
        Self { path, id }
    }
}
