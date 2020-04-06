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
		StringTable{ next_id: 0, table, by_id }
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

	pub fn get_as_usize(&self, id: StringId) -> Option<usize> {
		let the_str = self.name_from_id(id);
		let result: Result<usize, _> = the_str.parse();
		match result {
			Ok(res) => Some(res),
			Err(_) => None
		}
	}
}