use std::fmt::{self, Debug};
use crate::stringtable::StringId;
use crate::mavm::{Label, CodePt};
use crate::ast::Type;
use crate::stringtable::StringTable;
use serde::{Serialize, Deserialize};


#[derive(Serialize, Deserialize)]
pub struct ImportedFunc {
	pub name_id: StringId,
	pub name: String,
	pub codept: CodePt,
}

impl ImportedFunc {
	pub fn new(name_id: StringId, string_table: &StringTable) -> Self {
		ImportedFunc{ 
			name_id, 
			name: string_table.name_from_id(name_id).to_string(), 
			codept: CodePt::new_external(name_id), 
		}
	}
}

impl Debug for ImportedFunc {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), fmt::Error> {
		write!(f, "ImportedFunc({}, {:?})", self.name, self.codept)
	}
}

pub struct ExportedFunc {
	pub name_id: StringId,
	pub name: String,
	pub label: Label,
	pub tipe: Type,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExportedFuncPoint {
	pub name_id: StringId,
	pub name: String,
	pub codept: CodePt,
	pub tipe: Type,
}

impl<'a> ExportedFunc {
	pub fn new(name_id: StringId, label: Label, tipe: Type, string_table: &StringTable) -> Self {
		Self{ name_id, name: string_table.name_from_id(name_id).to_string(), label, tipe }
	}

	pub fn resolve(&self, codept: CodePt) -> ExportedFuncPoint {
		ExportedFuncPoint{ name_id: self.name_id, name: self.name.clone(), codept, tipe: self.tipe.clone() }
	}
}