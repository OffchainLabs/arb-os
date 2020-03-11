use std::collections::HashMap;
use crate::ast::Type;
use crate::stringtable::{StringId};


#[derive(Debug)]
#[derive(Clone)]
pub enum TypeTable<'a> {
	Empty,
	Single(StringId, &'a Type, &'a TypeTable<'a>),
	Multi(HashMap<StringId, &'a Type>, &'a TypeTable<'a>)
}

impl<'a> TypeTable<'a> {
	pub fn new() -> Self {
		TypeTable::Empty
	}

	pub fn push_one(self: &'a TypeTable<'a>, sid: StringId, t: &'a Type) -> Self {
		TypeTable::Single(sid, t, self)
	}

	pub fn push_multi(self: &'a TypeTable<'a>, hm: HashMap<StringId, &'a Type>) -> Self {
		TypeTable::Multi(hm, self)
	}

	pub fn get(&self, sid: StringId) -> Option<&'a Type> {
		match self {
			TypeTable::Empty => None,
			TypeTable::Single(s, t, rest) => if *s==sid {
					Some(t)
				} else {
					rest.get(sid)
				},
			TypeTable::Multi(hm, rest) => match hm.get(&sid) {
				Some(t) => Some(t),
				None => rest.get(sid)
			}
		}
	}
}
