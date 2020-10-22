/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

//! Contains utilities for compiling mini source code.

use crate::link::{ExportedFunc, Import, ImportedFunc};
use crate::mavm::Instruction;
use crate::pos::{BytePos, Location};
use crate::stringtable::StringTable;
use ast::{FuncDecl, GlobalVarDecl, TypeTree};
use lalrpop_util::lalrpop_mod;
use mini::DeclsParser;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{self, Read};
use std::path::Path;
use symtable::SymTable;
use typecheck::TypeCheckedFunc;

pub use ast::{TopLevelDecl, Type};
pub use source::Lines;

mod ast;
mod codegen;
mod source;
mod symtable;
mod typecheck;
lalrpop_mod!(mini);

///Trait that identifies what mini compiler tracked properties a value implementing this trait has.
///
///Currently only purity/impurity is tracked, however more properties may be added in the future.
pub(crate) trait MiniProperties {
    ///Returns false if the value reads or writes mini global state, and true otherwise.
    fn is_pure(&self) -> bool;
}

#[derive(Clone, Debug)]
struct Module {
    imported_funcs: Vec<ImportedFunc>,
    funcs: Vec<FuncDecl>,
    named_types: HashMap<usize, Type>,
    global_vars: Vec<GlobalVarDecl>,
    string_table: StringTable,
    func_table: HashMap<usize, Type>,
    name: String,
}

#[derive(Clone, Debug)]
struct TypeCheckedModule {
    checked_funcs: Vec<TypeCheckedFunc>,
    string_table: StringTable,
    imported_funcs: Vec<ImportedFunc>,
    exported_funcs: Vec<ExportedFunc>,
    global_vars: Vec<GlobalVarDecl>,
    name: String,
}

impl Module {
    fn new(
        imported_funcs: Vec<ImportedFunc>,
        funcs: Vec<FuncDecl>,
        named_types: HashMap<usize, Type>,
        global_vars: Vec<GlobalVarDecl>,
        string_table: StringTable,
        func_table: HashMap<usize, Type>,
        name: String,
    ) -> Self {
        Self {
            imported_funcs,
            funcs,
            named_types,
            global_vars,
            string_table,
            func_table,
            name,
        }
    }
}

impl TypeCheckedModule {
    fn new(
        checked_funcs: Vec<TypeCheckedFunc>,
        string_table: StringTable,
        imported_funcs: Vec<ImportedFunc>,
        exported_funcs: Vec<ExportedFunc>,
        global_vars: Vec<GlobalVarDecl>,
        name: String,
    ) -> Self {
        Self {
            checked_funcs,
            string_table,
            imported_funcs,
            exported_funcs,
            global_vars,
            name,
        }
    }
    fn inline(&mut self) {
        let mut new_funcs = self.checked_funcs.clone();
        for f in &mut new_funcs {
            for s in &mut f.code {
                s.inline(&self.checked_funcs);
            }
        }
    }
}

///Represents a mini program that has been compiled and possibly linked, but has not had post-link
/// compilation steps applied. Is directly serialized to and from .mao files.
#[derive(Clone, Serialize, Deserialize)]
pub struct CompiledProgram {
    pub code: Vec<Instruction>,
    pub exported_funcs: Vec<ExportedFunc>,
    pub imported_funcs: Vec<ImportedFunc>,
    pub global_num_limit: usize,
    pub source_file_map: Option<SourceFileMap>,
    pub file_name_chart: HashMap<u64, String>,
}

impl CompiledProgram {
    pub fn new(
        code: Vec<Instruction>,
        exported_funcs: Vec<ExportedFunc>,
        imported_funcs: Vec<ImportedFunc>,
        global_num_limit: usize,
        source_file_map: Option<SourceFileMap>,
        file_name_chart: HashMap<u64, String>,
    ) -> Self {
        CompiledProgram {
            code,
            exported_funcs,
            imported_funcs,
            global_num_limit,
            source_file_map,
            file_name_chart,
        }
    }

    ///Takes self by value and returns a tuple. The first value in the tuple is modified version of
    /// self with internal code references shifted forward by int_offset instructions, external code
    /// references offset by ext_offset instructions, function references shifted forward by
    /// func_offset, num_globals modified assuming the first *globals_offset* slots are occupied,
    /// and source_file_map replacing the existing source_file_map field.
    ///
    /// The second value of the tuple is the function offset after applying this operation.
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
                self.file_name_chart,
            ),
            max_func_offset,
        )
    }

    ///Writes self to output in format "format".  Supported values are: "pretty", "json", or
    /// "bincode" if None is specified, json is used, and if an invalid format is specified this
    /// value appended by "invalid format: " will be written instead
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

///Returns either a CompiledProgram generated from source code at path, otherwise returns a
/// CompileError.
///
/// The file_id specified will be used as the file_id in locations originating from this source
/// file, and if debug is set to true, then compiler internal debug information will be printed.
pub fn compile_from_file(
    path: &Path,
    file_name_chart: &mut HashMap<u64, String>,
    _debug: bool,
) -> Result<Vec<CompiledProgram>, CompileError> {
    let library = path
        .parent()
        .map(|par| {
            par.file_name()
                .map(|lib| {
                    let res = lib.to_str();
                    if res == Some("builtin") {
                        Some("core")
                    } else if res == Some("stdlib") {
                        Some("std")
                    } else {
                        None
                    }
                })
                .unwrap_or(None)
        })
        .unwrap_or(None);
    if path.is_dir() {
        compile_from_folder(path, library, "main", file_name_chart)
    } else if let (Some(parent), Some(file_name)) = (path.parent(), path.file_stem()) {
        compile_from_folder(
            parent,
            library,
            file_name.to_str().ok_or_else(|| {
                CompileError::new(format!("File name {:?} must be UTF-8", file_name), None)
            })?,
            file_name_chart,
        )
    } else {
        Err(CompileError::new(
            format!("Could not parse {} as valid path", path.display()),
            None,
        ))
    }
}

pub fn compile_from_folder(
    folder: &Path,
    library: Option<&str>,
    main: &str,
    file_name_chart: &mut HashMap<u64, String>,
) -> Result<Vec<CompiledProgram>, CompileError> {
    let (mut programs, import_map) = create_program_tree(folder, library, main, file_name_chart)?;
    for (name, imports) in &import_map {
        for import in imports {
            let mut named_type = None;
            let mut imp_func = None;
            let mut imp_func_decl = None;
            let import_path = import.path.clone();
            if let Some(program) = programs.get_mut(&import_path) {
                let index = program.string_table.get(import.name.clone());
                let type_table = SymTable::new();
                let type_table = type_table
                    .push_multi(program.named_types.iter().map(|(i, t)| (*i, t)).collect());
                named_type = program
                    .named_types
                    .get(&index)
                    .map(|t| t.resolve_types(&type_table, None))
                    .transpose()
                    .map_err(|e| CompileError::new(format!("Type error: {:?}", e), None))?;
                imp_func = program
                    .func_table
                    .get(&index)
                    .map(|decl| {
                        decl.resolve_types(&type_table, None)
                            .map_err(|e| CompileError::new(format!("Type error: {:?}", e), None))
                    })
                    .transpose()?;
                imp_func_decl = program
                    .funcs
                    .iter()
                    .find(|func| func.name == index)
                    .cloned();
            }
            let origin_program = programs.get_mut(name).ok_or_else(|| {
                CompileError::new(
                    format!(
                        "Internal error: Can not find originating file for import \"{}::{}\"",
                        import.path.get(0).cloned().unwrap_or_else(String::new),
                        import.name
                    ),
                    None,
                )
            })?;
            let index = origin_program.string_table.get(import.name.clone());
            if let Some(named_type) = named_type {
                origin_program.named_types.insert(index, named_type);
            } else if let Some(imp_func) = imp_func {
                origin_program.func_table.insert(index, imp_func);
                let imp_func_decl = imp_func_decl.ok_or(CompileError::new(
                    format!(
                        "Internal error: Imported function {} has no associated decl",
                        origin_program.string_table.name_from_id(index)
                    ),
                    None,
                ))?;
                origin_program.imported_funcs.push(ImportedFunc::new(
                    origin_program.imported_funcs.len(),
                    index,
                    &origin_program.string_table,
                    imp_func_decl
                        .args
                        .iter()
                        .map(|arg| arg.tipe.clone())
                        .collect(),
                    imp_func_decl.ret_type,
                    imp_func_decl.is_impure,
                ));
            } else {
                println!(
                    "Warning: import \"{}::{}\" does not correspond to a type or function",
                    import.path.get(0).cloned().unwrap_or_else(String::new),
                    import.name
                );
            }
        }
    }
    let mut typechecked = vec![];
    let mut progs = vec![];
    let type_tree = create_type_tree(&programs);
    let mut output = vec![programs
        .remove(&if let Some(lib) = library {
            vec![lib.to_string(), main.to_string()]
        } else {
            vec![main.to_string()]
        })
        .expect("no main")];
    output.append(&mut {
        let mut out: Vec<_> = programs.values().cloned().collect();
        out.sort_by(|module1, module2| module2.name.cmp(&module1.name));
        out
    });
    for Module {
        imported_funcs,
        funcs,
        named_types,
        global_vars,
        string_table,
        func_table: hm,
        name,
    } in output
    {
        let mut checked_funcs = vec![];
        let (exported_funcs, imported_funcs, global_vars, string_table) =
            typecheck::typecheck_top_level_decls(
                imported_funcs,
                funcs,
                named_types,
                global_vars,
                string_table,
                hm,
                &mut checked_funcs,
                &type_tree,
            )
            .map_err(|res3| CompileError::new(res3.reason.to_string(), res3.location))?;
        checked_funcs.iter().for_each(|func| {
            let detected_purity = func.is_pure();
            let declared_purity = func.properties.pure;
            if !detected_purity && declared_purity {
                println!(
                    "Warning: func {} is impure but not marked impure",
                    string_table.name_from_id(func.name)
                )
            } else if detected_purity && !declared_purity {
                println!(
                    "Warning: func {} is declared impure but does not contain impure code",
                    string_table.name_from_id(func.name)
                )
            }
        });
        typechecked.push(TypeCheckedModule::new(
            checked_funcs,
            string_table,
            imported_funcs,
            exported_funcs,
            global_vars,
            name,
        ));
    }
    typechecked.iter_mut().for_each(|module| module.inline());
    for TypeCheckedModule {
        checked_funcs,
        string_table,
        imported_funcs,
        exported_funcs,
        global_vars,
        name,
    } in typechecked
    {
        let code_out =
            codegen::mavm_codegen(checked_funcs, &string_table, &imported_funcs, &global_vars)
                .map_err(|e| CompileError::new(e.reason.to_string(), e.location))?;
        progs.push(CompiledProgram::new(
            code_out.to_vec(),
            exported_funcs,
            imported_funcs,
            global_vars.len(),
            Some(SourceFileMap::new(
                code_out.len(),
                folder.join(name.clone()).display().to_string(),
            )),
            HashMap::new(),
        ))
    }
    Ok(progs)
}

fn path_display(path: &Vec<String>) -> String {
    let mut s = "".to_string();
    if let Some(first) = path.get(0) {
        s.push_str(first);
    }
    for item in path.iter().skip(1) {
        s.push_str("::");
        s.push_str(item);
    }
    s
}

///Creates a HashMap containing a list of modules and imports generated by interpreting the contents
/// of `folder` as source code. Returns a `CompileError` if the contents of `folder` fail to parse.
fn create_program_tree(
    folder: &Path,
    library: Option<&str>,
    main: &str,
    file_name_chart: &mut HashMap<u64, String>,
) -> Result<
    (
        HashMap<Vec<String>, Module>,
        HashMap<Vec<String>, Vec<Import>>,
    ),
    CompileError,
> {
    let mut paths = if let Some(lib) = library {
        vec![vec![lib.to_owned(), main.to_owned()]]
    } else {
        vec![vec![main.to_owned()]]
    };
    let mut programs = HashMap::new();
    let mut import_map = HashMap::new();
    let mut seen_paths = HashSet::new();
    while let Some(name) = paths.pop() {
        if seen_paths.contains(&name) {
            continue;
        } else {
            seen_paths.insert(name.clone());
        }
        let path = if name[0] == "std" {
            format!("../stdlib/{}", name[1])
        } else if name[0] == "core" {
            format!("../builtin/{}", name[1])
        } else {
            name[0].clone()
        } + ".mini";
        let mut file = File::open(folder.join(path.clone())).map_err(|why| {
            CompileError::new(
                format!("Can not open {}/{}: {:?}", folder.display(), path, why),
                None,
            )
        })?;

        let mut source = String::new();
        file.read_to_string(&mut source).map_err(|why| {
            CompileError::new(
                format!("Can not read {}/{}: {:?}", folder.display(), path, why),
                None,
            )
        })?;
        let mut file_hasher = DefaultHasher::new();
        name.hash(&mut file_hasher);
        let file_id = file_hasher.finish();
        file_name_chart.insert(file_id, path_display(&name));
        let mut string_table = StringTable::new();
        let (imports, imported_funcs, funcs, named_types, global_vars, string_table, hm) =
            typecheck::sort_top_level_decls(
                &parse_from_source(source, file_id, &name, &mut string_table)?,
                string_table,
            );
        paths.append(&mut imports.iter().map(|imp| imp.path.clone()).collect());
        import_map.insert(name.clone(), imports);
        programs.insert(
            name.clone(),
            Module::new(
                imported_funcs,
                funcs,
                named_types,
                global_vars,
                string_table,
                hm,
                path,
            ),
        );
    }
    Ok((programs, import_map))
}

fn create_type_tree(program_tree: &HashMap<Vec<String>, Module>) -> TypeTree {
    program_tree
        .iter()
        .map(|(path, program)| {
            program
                .named_types
                .iter()
                .map(|(id, tipe)| ((path.clone(), *id), tipe.clone()))
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect()
}

///Converts source string `source` into a series of `TopLevelDecl`s, uses identifiers from
/// `string_table` and records new ones in it as well.  The `file_id` argument is used to construct
/// file information for the location fields.
pub fn parse_from_source(
    source: String,
    file_id: u64,
    file_path: &[String],
    string_table: &mut StringTable,
) -> Result<Vec<TopLevelDecl>, CompileError> {
    let comment_re = regex::Regex::new(r"//.*").unwrap();
    let source = comment_re.replace_all(&source, "");
    let lines = Lines::new(source.bytes());
    DeclsParser::new()
        .parse(string_table, &lines, file_id, file_path, &source)
        .map_err(|e| match e {
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (offset, tok, end),
                expected: _,
            } => CompileError::new(
                format!(
                    "unexpected token: {}, Type: {:?}",
                    &source[offset..end],
                    tok
                ),
                Some(lines.location(BytePos::from(offset), file_id).unwrap()),
            ),
            _ => CompileError::new(format!("{:?}", e), None),
        })
}

///Represents any error encountered during compilation.
#[derive(Debug, Clone)]
pub struct CompileError {
    pub description: String,
    pub location: Option<Location>,
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

///Lists the offset of each source file contained by a CompiledProgram in offsets, and the
/// instruction directly following the last in the CompiledProgram.
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

    ///Panics if offset is past the end of the SourceFileMap
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
