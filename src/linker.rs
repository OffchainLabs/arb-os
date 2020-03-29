use std::fmt::{self, Debug};
use crate::stringtable::StringId;
use crate::mavm::{Label, Value, CodePt, Instruction};
use crate::ast::Type;
use crate::stringtable::StringTable;
use crate::compile::{CompiledProgram, CompileError};
use serde::{Serialize, Deserialize};


#[derive(Serialize, Deserialize)]
pub struct LinkedProgram {
    pub code: Vec<Instruction>,
    pub static_val: Value,
    pub exported_funcs: Vec<ExportedFuncPoint>,
    pub imported_funcs: Vec<ImportedFunc>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ImportedFunc {
	pub name_id: StringId,
	pub slot_num: usize,
	pub name: String,
}

impl ImportedFunc {
	pub fn new(slot_num: usize, name_id: StringId, string_table: &StringTable) -> Self {
		ImportedFunc{ 
			name_id,
			slot_num, 
			name: string_table.name_from_id(name_id).to_string(), 
		}
	}

	pub fn relocate(self, _int_offset: usize, ext_offset: usize) -> Self {
		ImportedFunc{
			name_id: self.name_id,
			slot_num: self.slot_num + ext_offset,
			name: self.name,
		}
	}
}

impl Debug for ImportedFunc {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), fmt::Error> {
		write!(f, "ImportedFunc({}. {})", self.slot_num, self.name)
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportedFunc {
	pub name: String,
	pub label: Label,
	pub tipe: Type,
}

impl ExportedFunc {
	pub fn relocate(self, int_offset: usize, ext_offset: usize) -> Self {
		ExportedFunc{
			name: self.name,
			label: self.label.relocate(int_offset, ext_offset),
			tipe: self.tipe,
		}
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExportedFuncPoint {
	pub name: String,
	pub codept: CodePt,
	pub tipe: Type,
}

impl<'a> ExportedFunc {
	pub fn new(name_id: StringId, label: Label, tipe: Type, string_table: &StringTable) -> Self {
		Self{ name: string_table.name_from_id(name_id).to_string(), label, tipe }
	}

	pub fn resolve(&self, codept: CodePt) -> ExportedFuncPoint {
		ExportedFuncPoint{ name: self.name.clone(), codept, tipe: self.tipe.clone() }
	}
}

pub fn postlink_compile<'a>(
	program: CompiledProgram,
    debug: bool,
) -> Result<LinkedProgram, CompileError<'a>> {             
    let (code_2, jump_table) = crate::striplabels::fix_nonforward_labels(&program.code, &program.imported_funcs);
    if debug {
        println!("========== after fix_backward_labels ===========");
        for (idx, insn) in code_2.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }
    let code_3 = crate::xformcode::fix_tuple_size(&code_2);
    if debug {
        println!("=========== after fix_tuple_size ==============");
        for (idx, insn) in code_3.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }
    let code_4 = crate::optimize::peephole(&code_3);
    if debug {
        println!("============ after peephole optimization ===========");
        for (idx, insn) in code_4.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }
    let (code_final, 
        jump_table_final, 
        exported_funcs_final) = crate::striplabels::strip_labels(
            &code_4, 
            &jump_table, 
            &program.exported_funcs,
            &program.imported_funcs,
        );
    let jump_table_value = crate::xformcode::jump_table_to_value(jump_table_final);

    if debug {
        println!("============ after strip_labels =============");
        println!("static: {}", jump_table_value);
        for (idx, insn) in code_final.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }

    Ok(LinkedProgram{ 
        code: code_final, 
        static_val: jump_table_value, 
        exported_funcs: exported_funcs_final,
        imported_funcs: program.imported_funcs,
    })
}

pub fn link<'a>(progs: &Vec<CompiledProgram>) -> Result<CompiledProgram, CompileError<'a>> {
	let mut insns_so_far: usize = 0;
	let mut statics_so_far: usize = 0;
	let mut int_offsets = Vec::new();
	let mut ext_offsets = Vec::new();
	for prog in progs {
		int_offsets.push(insns_so_far);
		insns_so_far += prog.code.len();
		ext_offsets.push(statics_so_far);
		statics_so_far += prog.imported_funcs.len();
	}

	let mut relocated_progs = Vec::new();
	for (i, prog) in progs.iter().enumerate() {
		relocated_progs.push(prog.clone().relocate(int_offsets[i], ext_offsets[i]));
	}

	Err(CompileError::new("link: not yet implemented"))
}
