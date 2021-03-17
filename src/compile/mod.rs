/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

//! Contains utilities for compiling mini source code.

use crate::link::{ExportedFunc, Import, ImportedFunc};
use crate::mavm::Instruction;
use crate::pos::{BytePos, Location};
use crate::stringtable::StringTable;
use ast::{Func, TypeTree};
use lalrpop_util::lalrpop_mod;
use mini::DeclsParser;
use miniconstants::init_constant_table;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{self, Read};
use std::path::Path;
use typecheck::TypeCheckedFunc;

pub use ast::{DebugInfo, GlobalVarDecl, StructField, TopLevelDecl, Type};
pub use source::Lines;
pub use typecheck::{AbstractSyntaxTree, TypeCheckedNode};

mod ast;
mod codegen;
pub mod miniconstants;
mod source;
mod typecheck;
lalrpop_mod!(mini);

///Represents the contents of a source file after parsing.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
struct Module {
    //TODO: Remove this field
    ///The list of imported functions imported through the old import/export system
    imported_funcs: Vec<ImportedFunc>,
    ///List of functions defined locally within the source file
    funcs: Vec<Func>,
    ///Map from `StringId`s in this file to the `Type`s they represent.
    named_types: HashMap<usize, Type>,
    ///List of global variables defined within this file.
    global_vars: Vec<GlobalVarDecl>,
    ///Map from `StringId`s to the names they derived from.
    string_table: StringTable,
    ///Map from `StringId`s to the types of the functions they represent.
    func_table: HashMap<usize, Type>,
    ///The name of the module, this may be removed later.
    name: String,
}

///Represents the contents of a source file after type checking is done.
#[derive(Clone, Debug)]
struct TypeCheckedModule {
    /// List of functions defined locally within the source file that have been validated by
    /// typechecking
    checked_funcs: Vec<TypeCheckedFunc>,
    ///Map from `StringId`s to the names they derived from.
    string_table: StringTable,
    ///The list of imported functions imported through the old import/export system
    imported_funcs: Vec<ImportedFunc>,
    ///The list of exported functions exported through the old import/export system
    exported_funcs: Vec<ExportedFunc>,
    ///List of global variables defined in this module.
    global_vars: Vec<GlobalVarDecl>,
    ///The name of the module, this may be removed later.
    name: String,
}

impl Module {
    fn new(
        imported_funcs: Vec<ImportedFunc>,
        funcs: Vec<Func>,
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
    ///Inlines functions in the AST by replacing function calls with `CodeBlock` expressions where
    /// appropriate
    fn inline(&mut self) {
        let mut new_funcs = self.checked_funcs.clone();
        for f in &mut new_funcs {
            f.inline(
                &self.checked_funcs,
                &self.imported_funcs,
                &self.string_table,
            )
        }
        self.checked_funcs = new_funcs;
    }
}

///Represents a mini program or module that has been compiled and possibly linked, but has not had
/// post-link compilation steps applied. Is directly serialized to and from .mao files.
#[derive(Clone, Serialize, Deserialize)]
pub struct CompiledProgram {
    ///Instructions to be run to execute the program/module
    pub code: Vec<Instruction>,
    ///The list of exported functions exported through the old import/export system
    pub exported_funcs: Vec<ExportedFunc>,
    ///The list of imported functions imported through the old import/export system
    pub imported_funcs: Vec<ImportedFunc>,
    ///Highest ID used for any global in this program, used for linking
    pub globals: Vec<GlobalVarDecl>,
    ///Contains list of offsets of the various modules contained in this program
    pub source_file_map: Option<SourceFileMap>,
    ///Map from u64 hashes of file names to the `String`s they originate from
    pub file_name_chart: HashMap<u64, String>,
}

impl CompiledProgram {
    pub fn new(
        code: Vec<Instruction>,
        exported_funcs: Vec<ExportedFunc>,
        imported_funcs: Vec<ImportedFunc>,
        globals: Vec<GlobalVarDecl>,
        source_file_map: Option<SourceFileMap>,
        file_name_chart: HashMap<u64, String>,
    ) -> Self {
        CompiledProgram {
            code,
            exported_funcs,
            imported_funcs,
            globals,
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
        globals_offset: Vec<GlobalVarDecl>,
        source_file_map: Option<SourceFileMap>,
    ) -> (Self, usize) {
        let mut relocated_code = Vec::new();
        let mut max_func_offset = func_offset;
        for insn in &self.code {
            let (relocated_insn, new_func_offset) =
                insn.clone()
                    .relocate(int_offset, ext_offset, func_offset, globals_offset.len());
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
                {
                    let mut new_vec = globals_offset.clone();
                    new_vec.append(&mut self.globals.clone());
                    new_vec
                },
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
    file_name_chart: &mut BTreeMap<u64, String>,
    _debug: bool,
    inline: bool,
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
        compile_from_folder(path, library, "main", file_name_chart, inline)
    } else if let (Some(parent), Some(file_name)) = (path.parent(), path.file_stem()) {
        compile_from_folder(
            parent,
            library,
            file_name.to_str().ok_or_else(|| {
                CompileError::new(format!("File name {:?} must be UTF-8", file_name), None)
            })?,
            file_name_chart,
            inline,
        )
    } else {
        Err(CompileError::new(
            format!("Could not parse {} as valid path", path.display()),
            None,
        ))
    }
}

///Prints the AST nodes with indentation representing their depth, currently not used.
fn _print_node(node: &mut TypeCheckedNode, state: &String, mut_state: &mut usize) -> bool {
    for _ in 0..*mut_state {
        print!("{}", state);
    }
    println!("{:?}", node);
    *mut_state += 1;
    true
}

///Compiles a `Vec<CompiledProgram>` from a folder or generates a `CompileError` if a problem is
///encountered during compilation.
///
///The `folder` argument gives the path to the folder, `library` optionally contains a library
///prefix attached to the front of all paths, `main` contains the name of the main file in the
///folder, `file_name_chart` contains a map from the `u64` hashes of file names to the `Strings`
///they represent, useful for formatting errors, and `inline` determines whether inlining is used
///when compiling this folder.
pub fn compile_from_folder(
    folder: &Path,
    library: Option<&str>,
    main: &str,
    file_name_chart: &mut BTreeMap<u64, String>,
    inline: bool,
) -> Result<Vec<CompiledProgram>, CompileError> {
    //Parsing step
    let (mut programs, import_map) = create_program_tree(folder, library, main, file_name_chart)?;
    //Resolution of imports (use statements)
    for (name, imports) in &import_map {
        for import in imports {
            let import_path = import.path.clone();
            let (named_type, imp_func) = if let Some(program) = programs.get_mut(&import_path) {
                //Looks up info from target program
                let index = program.string_table.get(import.name.clone());
                let named_type = program.named_types.get(&index).cloned();
                let imp_func = program.func_table.get(&index).cloned();
                (named_type, imp_func)
            } else {
                return Err(CompileError::new(
                    format!(
                        "Internal error: Can not find target file for import \"{}::{}\"",
                        import.path.get(0).cloned().unwrap_or_else(String::new),
                        import.name
                    ),
                    None,
                ));
            };
            //Modifies origin program to include import
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
                origin_program.named_types.insert(index, named_type.clone());
            } else if let Some(imp_func) = imp_func {
                origin_program.func_table.insert(index, imp_func.clone());
                origin_program.imported_funcs.push(ImportedFunc::new(
                    origin_program.imported_funcs.len(),
                    index,
                    &origin_program.string_table,
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
    //Conversion of programs `HashMap` to `Vec` for typechecking
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
    //Typechecking loop
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
        let (exported_funcs, global_vars, string_table) = typecheck::typecheck_top_level_decls(
            funcs,
            named_types,
            global_vars,
            string_table,
            hm,
            &mut checked_funcs,
            &type_tree,
        )
        .map_err(|res3| CompileError::new(res3.reason.to_string(), res3.location))?;
        checked_funcs.iter_mut().for_each(|func| {
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
    /*for module in &mut typechecked {
        for _func in &mut module.checked_funcs {
            func.recursive_apply(print_node, &"    ".to_string(), &mut 0);
        }
    }*/
    //Inlining stage
    if inline {
        typechecked.iter_mut().for_each(|module| module.inline());
    }
    //Codegen loop
    for TypeCheckedModule {
        checked_funcs,
        string_table,
        imported_funcs,
        exported_funcs,
        global_vars,
        name,
    } in typechecked
    {
        let code_out = codegen::mavm_codegen(
            checked_funcs,
            &string_table,
            &imported_funcs,
            &global_vars,
            file_name_chart,
        )
        .map_err(|e| CompileError::new(e.reason.to_string(), e.location))?;
        progs.push(CompiledProgram::new(
            code_out.to_vec(),
            exported_funcs,
            imported_funcs,
            global_vars,
            Some(SourceFileMap::new(
                code_out.len(),
                folder.join(name.clone()).display().to_string(),
            )),
            HashMap::new(),
        ))
    }
    Ok(progs)
}

///Converts the `Vec<String>` used to identify a path into a single formatted string
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

///Parsing stage of the compiler, creates a `HashMap` containing a list of modules and imports
/// generated by interpreting the contents of `folder` as source code. Returns a `CompileError` if
/// the contents of `folder` fail to parse.
fn create_program_tree(
    folder: &Path,
    library: Option<&str>,
    main: &str,
    file_name_chart: &mut BTreeMap<u64, String>,
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
        let path = if name.len() == 1 {
            name[0].clone()
        } else if name[0] == "std" {
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
        let (imports, funcs, named_types, global_vars, hm) = typecheck::sort_top_level_decls(
            &parse_from_source(source, file_id, &name, &mut string_table)?,
        );
        paths.append(&mut imports.iter().map(|imp| imp.path.clone()).collect());
        import_map.insert(name.clone(), imports);
        programs.insert(
            name.clone(),
            Module::new(
                vec![],
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

///Constructor for `TypeTree`
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
    let mut constants = init_constant_table();
    DeclsParser::new()
        .parse(
            string_table,
            &lines,
            file_id,
            file_path,
            &mut constants,
            &source,
        )
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
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => CompileError::new(
                format!(
                    "unexpected end of file: expected one of: {}",
                    format!("{:?}",&expected)
                ),
                lines.location(location.into(), file_id),
            ),
            _ => CompileError::new(format!("{:?}", e), None),
        })
}

///Represents any error encountered during compilation.
#[derive(Debug, Clone)]
pub struct CompileError {
    ///What the error is.
    pub description: String,
    ///Where the error happened.
    pub location: Option<Location>,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.description)
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

    ///Adds a new source file to self with offset at current end
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
