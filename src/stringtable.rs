use std::collections::HashMap;

pub type StringId = u64;

#[derive(Debug)]
pub struct StringTable<'a> {
	next_id: StringId,
	table: HashMap<&'a str, StringId>,
	by_id: Vec<&'a str>,
}

impl<'a> StringTable<'a> {
	pub fn new() -> Self {
		let table: HashMap<&'a str, StringId> = HashMap::new();
		let by_id = Vec::new();
		StringTable{ next_id: 0, table, by_id }
	}

	pub fn get(&mut self, name: &'a str) -> StringId {
		match self.table.get(name) {
			Some(id) => *id,
			None => {
				let new_id = self.next_id;
				self.next_id = 1 + self.next_id;
				self.table.insert(name, new_id);
				self.by_id.push(name);
				new_id
			}
		}
	}

	pub fn name_from_id(&self, name: StringId) -> &'a str {
		self.by_id[name as usize]
	}
}