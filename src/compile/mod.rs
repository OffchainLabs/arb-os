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

use crate::link::{ExportedFunc, ImportedFunc};
use crate::mavm::Instruction;
use crate::pos::{BytePos, Location};
use crate::stringtable;
use lalrpop_util::lalrpop_mod;
use mini::DeclsParser;
use serde::{Deserialize, Serialize};
use std::fmt::Formatter;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

pub use ast::Type;
pub use source::Lines;

pub mod ast;
mod codegen;
mod source;
mod symtable;
mod typecheck;
lalrpop_mod!(mini);

pub(crate) trait MiniProperties {
    fn is_pure(&self) -> bool;
}

#[derive(Clone, Serialize, Deserialize)]
pub struct CompiledProgram {
    pub code: Vec<Instruction>,
    pub exported_funcs: Vec<ExportedFunc>,
    pub imported_funcs: Vec<ImportedFunc>,
    pub global_num_limit: usize,
    pub source_file_map: Option<SourceFileMap>,
}

impl CompiledProgram {
    pub fn new(
        code: Vec<Instruction>,
        exported_funcs: Vec<ExportedFunc>,
        imported_funcs: Vec<ImportedFunc>,
        global_num_limit: usize,
        source_file_map: Option<SourceFileMap>,
    ) -> Self {
        CompiledProgram {
            code,
            exported_funcs,
            imported_funcs,
            global_num_limit,
            source_file_map,
        }
    }

    pub fn relocate(
        self,
        int_offset: usize,
        ext_offset: usize,
        func_offset: usize,
        globals_offset: usize,
        source_file_map: Option<SourceFileMap>,
    ) -> (Self, usize) {
        let mut relocated_code = Vec::new();
        let mut max_func_offset = func_offset;
        for insn in &self.code {
            let (relocated_insn, new_func_offset) =
                insn.clone()
                    .relocate(int_offset, ext_offset, func_offset, globals_offset);
            relocated_code.push(relocated_insn);
            if max_func_offset < new_func_offset {
                max_func_offset = new_func_offset;
            }
        }

        let mut relocated_exported_funcs = Vec::new();
        for exp_func in self.exported_funcs {
            let (relocated_exp_func, new_func_offset) =
                exp_func.relocate(int_offset, ext_offset, func_offset);
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
                source_file_map,
            ),
            max_func_offset,
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
            None | Some("json") => match serde_json::to_string(self) {
                Ok(prog_str) => {
                    writeln!(output, "{}", prog_str).unwrap();
                }
                Err(e) => {
                    writeln!(output, "json serialization error: {:?}", e).unwrap();
                }
            },
            Some("bincode") => match bincode::serialize(self) {
                Ok(encoded) => {
                    if let Err(e) = output.write_all(&encoded) {
                        writeln!(output, "bincode write error: {:?}", e).unwrap();
                    }
                }
                Err(e) => {
                    writeln!(output, "bincode serialization error: {:?}", e).unwrap();
                }
            },
            Some(weird_value) => {
                writeln!(output, "invalid format: {}", weird_value).unwrap();
            }
        }
    }
}

pub fn compile_from_file(path: &Path, debug: bool) -> Result<CompiledProgram, CompileError> {
    let display = path.display();

    let mut file = File::open(&path)
        .map_err(|why| CompileError::new(format!("couldn't open {}: {:?}", display, why), None))?;

    let mut s = String::new();
    file.read_to_string(&mut s)
        .map_err(|why| CompileError::new(format!("couldn't read {}: {:?}", display, why), None))?;
    //print!("read-in file:\n{}", s);

    serde_json::from_str(&s).or_else(|_| compile_from_source(s, display, debug))
}

pub fn compile_from_source(
    s: String,
    pathname: std::path::Display,
    debug: bool,
) -> Result<CompiledProgram, CompileError> {
    let comment_re = regex::Regex::new(r"//.*").unwrap();
    let s = comment_re.replace_all(&s, "");
    let mut string_table_1 = stringtable::StringTable::new();
    let lines = Lines::new(s.bytes());
    let res = match DeclsParser::new().parse(&mut string_table_1, &lines, &s) {
        Ok(r) => r,
        Err(e) => match e {
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (offset, tok, end),
                expected: _,
            } => {
                return Err(CompileError::new(
                    format!("unexpected token: {}, Type: {:?}", &s[offset..end], tok),
                    Some(lines.location(BytePos::from(offset)).unwrap()),
                ));
            }
            _ => {
                return Err(CompileError::new(format!("{:?}", e), None));
            }
        },
    };
    let mut checked_funcs = Vec::new();
    let (exported_funcs, imported_funcs, global_vars, string_table) =
        typecheck::typecheck_top_level_decls(&res, &mut checked_funcs, string_table_1)
            .map_err(|res3| CompileError::new(res3.reason.to_string(), res3.location))?;
    let mut code = Vec::new();
    checked_funcs.iter().for_each(|func| {
        if !func.is_pure() && func.properties.pure {
            println!(
                "Warning: func {} is impure but not marked impure",
                func.name
            )
        }
    });

    let code_out = codegen::mavm_codegen(
        checked_funcs,
        &mut code,
        &string_table,
        &imported_funcs,
        &global_vars,
    )
    .map_err(|e| CompileError::new(e.reason.to_string(), e.location))?;
    if debug {
        println!("========== after initial codegen ===========");
        println!("Exported: {:?}", exported_funcs);
        println!("Imported: {:?}", imported_funcs);
        for (idx, insn) in code_out.iter().enumerate() {
            println!("{:04}:  {}", idx, insn);
        }
    }
    Ok(CompiledProgram::new(
        code_out.to_vec(),
        exported_funcs,
        imported_funcs,
        global_vars.len(),
        Some(SourceFileMap::new(code_out.len(), pathname.to_string())),
    ))
}

#[derive(Debug, Clone)]
pub struct CompileError {
    description: String,
    location: Option<Location>,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(loc) = self.location {
            write!(f, "{},\n{}", self.description, loc)?;
        } else {
            write!(f, "{},\n No location", self.description)?;
        }
        Ok(())
    }
}

impl CompileError {
    pub fn new(description: String, location: Option<Location>) -> Self {
        CompileError {
            description,
            location,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct SourceFileMap {
    offsets: Vec<(usize, String)>,
    end: usize,
}

impl SourceFileMap {
    pub fn new(size: usize, first_filepath: String) -> Self {
        SourceFileMap {
            offsets: vec![(0, first_filepath)],
            end: size,
        }
    }

    pub fn new_empty() -> Self {
        SourceFileMap {
            offsets: Vec::new(),
            end: 0,
        }
    }

    pub fn push(&mut self, size: usize, filepath: String) {
        self.offsets.push((self.end, filepath));
        self.end += size;
    }

    pub fn get(&self, offset: usize) -> String {
        for i in 0..(self.offsets.len() - 1) {
            if offset < self.offsets[i + 1].0 {
                return self.offsets[i].1.clone();
            }
        }
        if offset < self.end {
            return self.offsets[self.offsets.len() - 1].1.clone();
        }
        panic!("SourceFileMap: bounds check error");
    }
}
