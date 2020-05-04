use std::fmt::{self, Debug};
use std::io;
use std::collections::HashMap;
use crate::stringtable::StringId;
use crate::mavm::{Label, Value, CodePt, Instruction, Opcode};
use crate::ast::Type;
use crate::stringtable::StringTable;
use crate::compile::{CompiledProgram, CompileError, SourceFileMap};
use crate::xformcode::make_uninitialized_tuple;
use crate::builtins::add_auto_link_progs;
use serde::{Serialize, Deserialize};


#[derive(Serialize, Deserialize)]
pub struct LinkedProgram {
    pub code: Vec<Instruction>,
    pub static_val: Value,
    pub exported_funcs: Vec<ExportedFuncPoint>,
    pub imported_funcs: Vec<ImportedFunc>,
}

impl<'a> LinkedProgram {
	pub fn to_output(&'a self, output: &mut dyn io::Write, format: Option<&str>) {
		match format {
			Some("pretty") => {
				writeln!(output, "exported: {:?}", self.exported_funcs).unwrap();
				writeln!(output, "imported: {:?}", self.imported_funcs).unwrap();
				writeln!(output, "static: {}", self.static_val).unwrap();
				for (idx, insn) in self.code.iter().enumerate() {
					writeln!(output, "{:04}:  {}", idx, insn).unwrap();
				}
			}
			None |
			Some("json") => {
				match serde_json::to_string(self) {
					Ok(prog_str) => {
						writeln!(output, "{}", prog_str).unwrap();
					}
					Err(e) => {
						writeln!(output, "json serialization error: {:?}", e).unwrap();
					}
				}
			}
			Some("bincode") => {
				match bincode::serialize(self) {
					Ok(encoded) => {
						if let Err(e) = output.write_all(&encoded) {
							writeln!(output, "bincode write error: {:?}", e).unwrap();
					   }
					}
					Err(e) => {
						writeln!(output, "bincode serialization error: {:?}", e).unwrap();
					}
				}
			}
			Some(weird_value) => { writeln!(output, "invalid format: {}", weird_value).unwrap(); }
		} 
	}
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
		write!(f, "ImportedFunc({}, {})", self.slot_num, self.name)
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportedFunc {
	pub name: String,
	pub label: Label,
	pub tipe: Type,
}

impl ExportedFunc {
	pub fn relocate(self, int_offset: usize, ext_offset: usize, func_offset: usize) -> (Self, usize) {
		let (relocated_label, new_func_offset) = self.label.relocate(int_offset, ext_offset, func_offset);
		(
			ExportedFunc{
				name: self.name,
				label: relocated_label,
				tipe: self.tipe,
			},
			new_func_offset
		)
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
	if debug {
        println!("========== after initial linking ===========");
        for (idx, insn) in program.code.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }          
    let (code_2, jump_table) = crate::striplabels::fix_nonforward_labels(&program.code, &program.imported_funcs);
    if debug {
        println!("========== after fix_backward_labels ===========");
        for (idx, insn) in code_2.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }
    let code_3 = crate::xformcode::fix_tuple_size(&code_2, program.global_num_limit);
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
		exported_funcs_final) = 
			match crate::striplabels::strip_labels(
            	&code_4, 
            	&jump_table, 
           	 	&program.exported_funcs,
            	&program.imported_funcs,
        	) {
				Ok(tup) => tup,
				Err(label) => { 
					println!("missing label {:?}", label);
					return Err(CompileError::new("reference to non-existent function", None)); 
				}
			};
    let jump_table_value = crate::xformcode::jump_table_to_value(jump_table_final);

    if debug {
        println!("============ after strip_labels =============");
        println!("static: {}", jump_table_value);
        for (idx, insn) in code_final.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
        println!("============ after full compile/link =============");
    }

    Ok(LinkedProgram{ 
        code: code_final, 
        static_val: jump_table_value, 
        exported_funcs: exported_funcs_final,
        imported_funcs: program.imported_funcs,
    })
}

pub fn link<'a>(progs_in: &[CompiledProgram]) -> Result<CompiledProgram, CompileError<'a>> {
	let progs = add_auto_link_progs(progs_in)?;
	let mut insns_so_far: usize = 1;   // leave 1 insn of space at beginning for initialization
	let mut imports_so_far: usize = 0;
	let mut int_offsets = Vec::new();
	let mut ext_offsets = Vec::new();
	let mut merged_source_file_map = SourceFileMap::new_empty();
	let mut global_num_limit = 0;

	for prog in &progs {
		merged_source_file_map.push(prog.code.len(), prog.source_file_map.get(0));
		int_offsets.push(insns_so_far);
		insns_so_far += prog.code.len();
		ext_offsets.push(imports_so_far);
		imports_so_far += prog.imported_funcs.len();
	}

	let mut relocated_progs = Vec::new();
	let mut func_offset: usize = 0;
	for (i, prog) in progs.iter().enumerate() {
		let (relocated_prog, new_func_offset) = prog.clone().relocate(
			int_offsets[i], 
			ext_offsets[i], 
			func_offset,
			global_num_limit,
			prog.clone().source_file_map,
		);
		global_num_limit = relocated_prog.global_num_limit;
		relocated_progs.push(relocated_prog);
		func_offset = new_func_offset;
	}

	// Initialize globals
	let mut linked_code = vec![Instruction::from_opcode_imm(Opcode::Rset, make_uninitialized_tuple(global_num_limit), None)];
	
	let mut linked_exports = Vec::new();
	let mut linked_imports = Vec::new();
	for mut rel_prog in relocated_progs {
		linked_code.append(&mut rel_prog.code);
		linked_exports.append(&mut rel_prog.exported_funcs);
		linked_imports.append(&mut rel_prog.imported_funcs);
	}

	let mut exports_map = HashMap::new();
	let mut label_xlate_map = HashMap::new();
	for exp in &linked_exports {
		exports_map.insert(exp.name.clone(), exp.label);
	}
	for imp in &linked_imports {
		if let Some(label) = exports_map.get(&imp.name) {
			label_xlate_map.insert(Label::External(imp.slot_num), label);
		}
	}

	let mut linked_xlated_code = Vec::new();
	for insn in linked_code {
		linked_xlated_code.push(insn.xlate_labels(&label_xlate_map));
	}

	Ok(CompiledProgram::new(linked_xlated_code, linked_exports, linked_imports, global_num_limit, merged_source_file_map))
}
