use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use serde::{Serialize, Deserialize};
use crate::stringtable;
use crate::mavm::Instruction;
use crate::link::{ExportedFunc, ImportedFunc};
use crate::source::Lines;
use crate::pos::{Location, BytePos};
use lalrpop_util;
extern crate regex;

lalrpop_mod!(pub mini); 


#[derive(Clone, Serialize, Deserialize)]
pub struct CompiledProgram {
    pub code: Vec<Instruction>,
    pub exported_funcs: Vec<ExportedFunc>,
    pub imported_funcs: Vec<ImportedFunc>,
    pub global_num_limit: usize,
    pub source_file_map: SourceFileMap,
}

impl<'a> CompiledProgram {
    pub fn new(
        code: Vec<Instruction>, 
        exported_funcs: Vec<ExportedFunc>, 
        imported_funcs: Vec<ImportedFunc>,
        global_num_limit: usize,
        source_file_map: SourceFileMap,
    ) -> Self {
        CompiledProgram{ code, exported_funcs, imported_funcs, global_num_limit, source_file_map }
    }

    pub fn relocate(
        self, 
        int_offset: usize, 
        ext_offset: usize, 
        func_offset: usize, 
        globals_offset: usize,
        source_file_map: SourceFileMap
    ) -> (Self, usize) {
        let mut relocated_code = Vec::new();
        let mut max_func_offset = func_offset;
        for insn in &self.code {
            let (relocated_insn, new_func_offset) = insn.clone().relocate(int_offset, ext_offset, func_offset, globals_offset);
            relocated_code.push(relocated_insn);
            if max_func_offset < new_func_offset {
                max_func_offset = new_func_offset;
            }
        }

        let mut relocated_exported_funcs = Vec::new();
        for exp_func in self.exported_funcs {
            let (relocated_exp_func, new_func_offset) = exp_func.relocate(int_offset, ext_offset, func_offset);
            relocated_exported_funcs.push(relocated_exp_func);
            if max_func_offset < new_func_offset {
                max_func_offset = new_func_offset;
            }
        }

        let mut relocated_imported_funcs = Vec::new();
        for imp_func in self.imported_funcs {
            relocated_imported_funcs.push(imp_func.relocate(int_offset, ext_offset));
        }

        (
            CompiledProgram::new(
                relocated_code, 
                relocated_exported_funcs, 
                relocated_imported_funcs, 
                self.global_num_limit + globals_offset,
                source_file_map
            ), 
            max_func_offset
        )
    }

    pub fn to_output(&self, output: &mut dyn io::Write, format: Option<&str>) {
		match format {
			Some("pretty") => {
				writeln!(output, "exported: {:?}", self.exported_funcs).unwrap();
				writeln!(output, "imported: {:?}", self.imported_funcs).unwrap();
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

pub fn compile_from_file<'a>(path: &Path, debug: bool) -> Result<CompiledProgram, CompileError<'a>> {
   let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {:?}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    s = match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {:?}", display, why),
        Ok(_) => s,
    };
    //print!("read-in file:\n{}", s);

    let parse_result: Result<CompiledProgram, serde_json::Error> = serde_json::from_str(&s);
    match parse_result {
        Ok(compiled_prog) => Ok(compiled_prog),
        Err(_) => compile_from_source(s, display, debug),  // json parsing failed, try to parse as source code
    }
}

pub fn compile_from_source<'a>(
    s: String, 
    pathname: std::path::Display, 
    debug: bool,
) -> Result<CompiledProgram, CompileError<'a>> {
    let comment_re = regex::Regex::new(r"//.*").unwrap();
    let s = comment_re.replace_all(&s, "");
    let mut string_table_1 = stringtable::StringTable::new();
    let lines = Lines::new(s.bytes());
    let res = match mini::DeclsParser::new().parse(&mut string_table_1, &lines, &s) {
        Ok(r) => r,
        Err(e) => match e {
            lalrpop_util::ParseError::UnrecognizedToken{ token: (offset, tok, _), expected: _ } => {
                panic!("unexpected token at {:?} {:?}", lines.location(BytePos::from(offset)).unwrap(), tok);
            }
            _ => { panic!("{:?}", e); }
        }
    };
    let mut checked_funcs = Vec::new();
    let res2 = crate::typecheck::typecheck_top_level_decls(&res, &mut checked_funcs, string_table_1);
    match res2 {
    	Ok((exported_funcs, imported_funcs, global_vars, string_table)) => { 
            let mut code = Vec::new();
    		match crate::codegen::mavm_codegen(checked_funcs, &mut code, &string_table, &imported_funcs, &global_vars) {
                Ok(code_out) => {
                    if debug {
                        println!("========== after initial codegen ===========");
                        println!("Exported: {:?}", exported_funcs);
                        println!("Imported: {:?}", imported_funcs);
                        for (idx, insn) in code_out.iter().enumerate() {
                         println!("{:04}:  {}", idx, insn);
                        }
                    }
                    Ok(CompiledProgram::new(code_out.to_vec(), exported_funcs, imported_funcs, global_vars.len(), SourceFileMap::new(code_out.len(), pathname.to_string())))
                }
                Err(e) => Err(CompileError::new(e.reason, e.location)),
            }
        },
        Err(res3) => Err(CompileError::new(res3.reason, res3.location)),
    }
} 

#[derive(Debug)]
pub struct CompileError<'a> {
    description: &'a str,
    location: Option<Location>,
}

impl<'a> CompileError<'a> {
    pub fn new(description: &'a str, location: Option<Location>) -> Self {
        CompileError{ description, location }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct SourceFileMap {
    offsets: Vec<(usize, String)>,
    end:     usize,
}

impl SourceFileMap {
    pub fn new(size: usize, first_filepath: String) -> Self {
        SourceFileMap {
            offsets: vec![(0, first_filepath),],
            end: size,
        }
    }

    pub fn new_empty() -> Self {
        SourceFileMap{ offsets: Vec::new(), end: 0 }
    }

    pub fn push(&mut self, size: usize, filepath: String) {
        self.offsets.push((self.end, filepath));
        self.end += size;
    }

    pub fn get(&self, offset: usize) -> String {
        for i in 0..(self.offsets.len()-1) {
            if offset < self.offsets[i+1].0 {
                return self.offsets[i].1.clone();
            }
        }
        if offset < self.end {
            return self.offsets[self.offsets.len()-1].1.clone();
        }
        panic!("SourceFileMap: bounds check error");
    }
}