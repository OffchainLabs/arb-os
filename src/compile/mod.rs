/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

//! Contains utilities for compiling mini source code.

use crate::console::Color;
use crate::link::xformcode;
use crate::link::{link, postlink_compile, Import, LinkedProgram};
use crate::mavm::{Instruction, Label, LabelId};
use crate::optimize::{BasicGraph, Computer};
use crate::pos::{BytePos, Location};
use crate::stringtable::{StringId, StringTable};
use ast::Func;
use clap::Clap;
use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
use mini::DeclsParser;
use miniconstants::init_constant_table;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{self, Read};
use std::path::Path;
use typecheck::TypeCheckedFunc;

pub use ast::{DebugInfo, FuncProperties, GlobalVar, StructField, TopLevelDecl, Type, TypeTree};
pub use codegen::{FrameSize, SlotNum};
pub use source::Lines;
pub use typecheck::{AbstractSyntaxTree, TypeCheckedNode};

mod ast;
mod codegen;
pub mod miniconstants;
mod source;
pub mod translate;
mod typecheck;
lalrpop_mod!(mini);

/// Command line options for compile subcommand.
#[derive(Clap, Debug, Default)]
pub struct CompileStruct {
    pub input: Vec<String>,
    #[clap(short, long)]
    pub debug_mode: bool,
    #[clap(short, long)]
    pub test_mode: bool,
    #[clap(short, long)]
    pub warnings_are_errors: bool,
    #[clap(short, long)]
    pub output: Option<String>,
    #[clap(short = 'O', long, default_value = "0")]
    pub optimization_level: usize,
    #[clap(short, long)]
    pub format: Option<String>,
    #[clap(short, long)]
    pub consts_file: Option<String>,
    #[clap(short, long)]
    pub must_use_global_consts: bool,
    #[clap(short, long)]
    pub release_build: bool,
    #[clap(short, long)]
    pub no_builtins: bool,
}

/// Represents the contents of a source file after parsing.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Module {
    /// List of functions defined locally within the source file
    funcs: Vec<Func>,
    /// Map from `StringId`s in this file to the `Type`s they represent.
    named_types: HashMap<StringId, Type>,
    /// List of constants used in this file.
    constants: HashSet<String>,
    /// List of global variables defined within this file.
    global_vars: Vec<GlobalVar>,
    /// List of imported constructs within this file.
    imports: Vec<Import>,
    /// Map from `StringId`s to the names they derived from.
    string_table: StringTable,
    /// Map from `StringId`s to the types of the functions they represent.
    func_table: HashMap<StringId, Type>,
    /// The path to the module
    path: Vec<String>,
    /// The name of the module, this may be removed later.
    name: String,
}

/// Represents the contents of a source file after type checking is done.
#[derive(Clone, Debug)]
struct TypeCheckedModule {
    /// Collection of functions defined locally within the source file that have been validated by
    /// typechecking
    checked_funcs: BTreeMap<StringId, TypeCheckedFunc>,
    /// Map from `StringId`s to the names they derived from.
    string_table: StringTable,
    /// Map from `StringId`s in this file to the `Type`s they represent.
    named_types: HashMap<StringId, Type>,
    /// List of constants used in this file.
    constants: HashSet<String>,
    /// List of global variables defined in this module.
    global_vars: Vec<GlobalVar>,
    /// The list of imports declared via `use` statements.
    imports: Vec<Import>,
    /// The path to the module
    path: Vec<String>,
}

impl CompileStruct {
    pub fn invoke(&self) -> Result<(LinkedProgram, ErrorSystem), ErrorSystem> {
        // Initialize rayon to use a large stack size. We do this here rather than
        // main() so that tests are affected.
        drop(
            rayon::ThreadPoolBuilder::new()
                .stack_size(4 * 8192 * 1024)
                .build_global(),
        );

        let mut error_system = ErrorSystem {
            errors: vec![],
            warnings: vec![],
            warnings_are_errors: self.warnings_are_errors,
            warn_color: match self.warnings_are_errors {
                true => Color::PINK,
                false => Color::YELLOW,
            },
            file_info_chart: BTreeMap::new(),
        };

        let mut unlinked_progs = vec![];
        let mut file_info_chart = BTreeMap::new();
        let mut globals = vec![];

        for filename in &self.input {
            let path = Path::new(filename);
            let constants_path = match &self.consts_file {
                Some(path) => Some(Path::new(path)),
                None => None,
            };
            let (progs, all_globals) = match compile_from_file(
                path,
                &mut file_info_chart,
                constants_path,
                self.must_use_global_consts,
                &mut error_system,
                self.optimization_level,
                self.release_build,
                !self.no_builtins,
            ) {
                Ok(idk) => idk,
                Err(err) => {
                    error_system.errors.push(err);
                    error_system.file_info_chart = file_info_chart;
                    return Err(error_system);
                }
            };

            globals = all_globals;

            unlinked_progs.extend(progs);
        }

        // If this condition is true it means that __fixedLocationGlobal will not be at
        // index [0], but rather [0][0] or [0][0][0] etc
        if globals.len() >= 58 {
            panic!("Too many globals defined in program, location of first global is not correct")
        }

        let linked_prog = link(unlinked_progs, globals, &mut error_system, self.test_mode);

        let postlinked_prog =
            match postlink_compile(linked_prog, file_info_chart.clone(), self.test_mode) {
                Ok(idk) => idk,
                Err(err) => {
                    error_system.errors.push(err);
                    error_system.file_info_chart = file_info_chart;
                    return Err(error_system);
                }
            };

        error_system.file_info_chart = file_info_chart;

        if error_system.warnings.len() > 0 && error_system.warnings_are_errors {
            error_system.errors.push(CompileError::new(
                "Compile Error",
                "Found warning with -w on",
                vec![],
            ));
            Err(error_system)
        } else {
            Ok((postlinked_prog, error_system))
        }
    }
}

impl Module {
    fn new(
        funcs: Vec<Func>,
        named_types: HashMap<usize, Type>,
        constants: HashSet<String>,
        global_vars: Vec<GlobalVar>,
        imports: Vec<Import>,
        string_table: StringTable,
        func_table: HashMap<usize, Type>,
        path: Vec<String>,
        name: String,
    ) -> Self {
        Self {
            funcs,
            named_types,
            constants,
            global_vars,
            imports,
            string_table,
            func_table,
            path,
            name,
        }
    }
}

impl TypeCheckedModule {
    fn new(
        checked_funcs: BTreeMap<StringId, TypeCheckedFunc>,
        string_table: StringTable,
        named_types: HashMap<usize, Type>,
        constants: HashSet<String>,
        global_vars: Vec<GlobalVar>,
        imports: Vec<Import>,
        path: Vec<String>,
    ) -> Self {
        Self {
            checked_funcs,
            string_table,
            constants,
            named_types,
            global_vars,
            imports,
            path,
        }
    }

    /// Propagates inherited attributes down top-level decls.
    fn propagate_attributes(&mut self) {
        for (_id, func) in &mut self.checked_funcs {
            let attributes = func.debug_info.attributes.clone();
            TypeCheckedNode::propagate_attributes(func.child_nodes(), &attributes);
        }
    }

    /// Reasons about control flow and construct usage within the typechecked AST
    fn flowcheck(&mut self, error_system: &mut ErrorSystem) {
        let mut flow_warnings = vec![];

        let mut imports: BTreeMap<StringId, Import> = BTreeMap::new();

        for import in self.imports.iter() {
            let id = self.string_table.get_if_exists(&import.name).unwrap();

            if let Some(prior) = imports.get(&id) {
                flow_warnings.push(CompileError::new_warning(
                    "Compile Warning",
                    format!(
                        "use statement {} is a duplicate",
                        Color::color(error_system.warn_color, &import.name)
                    ),
                    prior
                        .location
                        .into_iter()
                        .chain(import.location.into_iter())
                        .collect(),
                ));
            }

            if import.path[0] != "core" {
                imports.insert(id, import.clone());
            }
        }

        for global in &self.global_vars {
            for nominal in &global.tipe.find_nominals() {
                imports.remove(nominal);
            }
        }

        for (_id, tipe) in &self.named_types {
            for nominal in &tipe.find_nominals() {
                imports.remove(nominal);
            }
        }

        for (_id, func) in &mut self.checked_funcs {
            flow_warnings.extend(func.flowcheck(
                &mut imports, // will remove from imports everything used
                &mut self.string_table,
                error_system,
            ));
        }

        for (_id, import) in imports {
            flow_warnings.push(CompileError::new_warning(
                "Compile Warning",
                format!(
                    "use statement {} is unnecessary",
                    Color::color(error_system.warn_color, import.name)
                ),
                import.location.into_iter().collect(),
            ));
        }

        flow_warnings.sort_by(|a, b| {
            a.locations
                .last()
                .unwrap()
                .line
                .to_usize()
                .cmp(&b.locations.last().unwrap().line.to_usize())
        });

        error_system.warnings.extend(flow_warnings);
    }
}

/// Maps the `StringId` of a capture to a slot in a func's frame
pub type ClosureAssignments = HashMap<StringId, SlotNum>;

pub struct CompiledFunc {
    /// Name of the func from which it was derived
    pub name: String,
    /// Path of the program
    pub path: Vec<String>,
    /// Instructions that effect the behavior of this func's mini source code
    pub code: Vec<Instruction>,
    /// The slot assignments for any captures this func (closure) may use
    pub captures: ClosureAssignments,
    /// The size of the function's frame
    pub frame_size: FrameSize,
    /// The funcs this func depends on (includes both calls & pointers)
    pub dependencies: HashSet<LabelId>,
    /// All globals accessible to this func
    pub globals: Vec<GlobalVar>,
    /// Tree of the types
    pub type_tree: TypeTree,
    /// This func's globally-unique identifier
    pub unique_id: LabelId,
    /// This func's debug info
    pub debug_info: DebugInfo,
}

impl CompiledFunc {
    pub fn new(
        name: String,
        path: Vec<String>,
        code: Vec<Instruction>,
        captures: ClosureAssignments,
        frame_size: FrameSize,
        dependencies: HashSet<LabelId>,
        globals: Vec<GlobalVar>,
        type_tree: TypeTree,
        debug_info: DebugInfo,
    ) -> Self {
        let unique_id = Import::unique_id(&path, &name);
        CompiledFunc {
            name,
            path,
            code,
            captures,
            frame_size,
            dependencies,
            globals,
            type_tree,
            unique_id,
            debug_info,
        }
    }
}

/// Represents a mini program or module that has been compiled and possibly linked, but has not had
/// post-link compilation steps applied. Is directly serialized to and from .mao files.
#[derive(Clone, Serialize, Deserialize)]
pub struct CompiledProgram {
    /// Name of the program, usually the func from which it was derived
    pub name: String,
    /// Path of the program
    pub path: Vec<String>,
    /// Instructions to be run to execute the program/module
    pub code: Vec<Instruction>,
    /// All globals used in this program
    pub globals: Vec<GlobalVar>,
    /// Tree of the types
    pub type_tree: TypeTree,
    /// A global id unique to the source (usually a func) from which this program was compiled
    pub unique_id: LabelId,
    /// This program's debug info
    pub debug_info: DebugInfo,
}

impl CompiledProgram {
    pub fn new(
        name: String,
        path: Vec<String>,
        code: Vec<Instruction>,
        globals: Vec<GlobalVar>,
        type_tree: TypeTree,
        debug_info: DebugInfo,
    ) -> Self {
        let unique_id = Import::unique_id(&path, &name);
        CompiledProgram {
            name,
            path,
            code,
            globals,
            type_tree,
            unique_id,
            debug_info,
        }
    }

    /// Writes self to output in format "format".  Supported values are: "pretty", "json", or
    /// "bincode" if None is specified, json is used, and if an invalid format is specified this
    /// value appended by "invalid format: " will be written instead
    pub fn _to_output(&self, output: &mut dyn io::Write, format: Option<&str>) {
        match format {
            Some("pretty") => {
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

/// Returns either a CompiledProgram generated from source code at path, otherwise returns a
/// CompileError.
///
/// The file_id specified will be used as the file_id in locations originating from this source
/// file, and if debug is set to true, then compiler internal debug information will be printed.
pub fn compile_from_file(
    path: &Path,
    file_info_chart: &mut BTreeMap<u64, FileInfo>,
    constants_path: Option<&Path>,
    must_use_global_consts: bool,
    error_system: &mut ErrorSystem,
    optimization_level: usize,
    release_build: bool,
    builtins: bool,
) -> Result<(Vec<CompiledFunc>, Vec<GlobalVar>), CompileError> {
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
                    } else if res == Some("stdlib2") {
                        Some("std2")
                    } else {
                        None
                    }
                })
                .unwrap_or(None)
        })
        .unwrap_or(None);
    if path.is_dir() {
        compile_from_folder(
            path,
            library,
            "main",
            file_info_chart,
            constants_path,
            must_use_global_consts,
            error_system,
            optimization_level,
            release_build,
            builtins,
        )
    } else if let (Some(parent), Some(file_name)) = (path.parent(), path.file_stem()) {
        compile_from_folder(
            parent,
            library,
            file_name.to_str().ok_or_else(|| {
                CompileError::new(
                    "Compile error",
                    format!("File name {:?} must be UTF-8", file_name),
                    vec![],
                )
            })?,
            file_info_chart,
            constants_path,
            must_use_global_consts,
            error_system,
            optimization_level,
            release_build,
            builtins,
        )
    } else {
        Err(CompileError::new(
            "Compile error",
            format!(
                "Could not parse {} as valid path",
                Color::red(path.display())
            ),
            vec![],
        ))
    }
}

/// Prints the AST nodes with indentation representing their depth, currently not used.
fn _print_node(node: &mut TypeCheckedNode, state: &String, mut_state: &mut usize) -> bool {
    for _ in 0..*mut_state {
        print!("{}", state);
    }
    println!("{:?}", node);
    *mut_state += 1;
    true
}

/// Compiles a `Vec<CompiledProgram>` from a folder along with it's `Vec<GlobalVar>`
/// or generates a `CompileError` if a problem is encountered during compilation.
///
/// The `folder` argument gives the path to the folder, `library` optionally contains a library
/// prefix attached to the front of all paths, `main` contains the name of the main file in the
/// folder, `file_info_chart` contains a map from the `u64` hashes of file names to the `FileInfo`
/// they represent, useful for formatting errors
pub fn compile_from_folder(
    folder: &Path,
    library: Option<&str>,
    main: &str,
    file_info_chart: &mut BTreeMap<u64, FileInfo>,
    constants_path: Option<&Path>,
    must_use_global_consts: bool,
    error_system: &mut ErrorSystem,
    optimization_level: usize,
    release_build: bool,
    builtins: bool,
) -> Result<(Vec<CompiledFunc>, Vec<GlobalVar>), CompileError> {
    let constants_default = folder.join("constants.json");
    let constants_path = match constants_path {
        Some(path) => Some(path),
        None => match constants_default.exists() {
            true => Some(constants_default.as_path()),
            false => None,
        },
    };

    let (mut programs, mut import_map) = create_program_tree(
        folder,
        library,
        main,
        file_info_chart,
        constants_path,
        error_system,
        builtins,
    )?;

    resolve_imports(&mut programs, &mut import_map, error_system)?;

    // Conversion of programs from `HashMap` to `Vec` for typechecking
    let type_tree = create_type_tree(&programs);
    let mut modules = vec![programs
        .remove(&if let Some(lib) = library {
            vec![lib.to_string(), main.to_string()]
        } else {
            vec![main.to_string()]
        })
        .expect("no main")];
    modules.append(&mut {
        let mut out: Vec<_> = programs.values().cloned().collect();
        out.sort_by(|module1, module2| module2.name.cmp(&module1.name));
        out
    });
    let mut typechecked_modules =
        typecheck_programs(&type_tree, modules, file_info_chart, error_system)?;

    if must_use_global_consts {
        check_global_constants(&typechecked_modules, constants_path, error_system);
    }

    // Control flow analysis stage
    for module in &mut typechecked_modules {
        module.flowcheck(error_system);
    }

    for module in &mut typechecked_modules {
        module.propagate_attributes();
    }

    let (progs, globals) = codegen_modules(
        typechecked_modules,
        type_tree,
        optimization_level,
        release_build,
    )?;
    Ok((progs, globals))
}

/// Converts the `Vec<String>` used to identify a path into a single formatted string
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

/// Parsing stage of the compiler, creates a `HashMap` containing a list of modules and imports
/// generated by interpreting the contents of `folder` as source code. Returns a `CompileError` if
/// the contents of `folder` fail to parse.
fn create_program_tree(
    folder: &Path,
    library: Option<&str>,
    main: &str,
    file_info_chart: &mut BTreeMap<u64, FileInfo>,
    constants_path: Option<&Path>,
    error_system: &mut ErrorSystem,
    builtins: bool,
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
    while let Some(path) = paths.pop() {
        if seen_paths.contains(&path) {
            continue;
        } else {
            seen_paths.insert(path.clone());
        }
        let name = if path.len() == 1 {
            path[0].clone()
        } else if path[0] == "std" {
            format!("../stdlib/{}", path[1])
        } else if path[0] == "std2" {
            format!("../stdlib2/{}", path[1])
        } else if path[0] == "core" {
            format!("../builtin/{}", path[1])
        } else {
            path[0].clone()
        } + ".mini";
        let mut file = File::open(folder.join(name.clone())).map_err(|why| {
            CompileError::new(
                "Compile error",
                format!("Can not open {}/{}: {:?}", folder.display(), name, why),
                vec![],
            )
        })?;

        let mut source = String::new();
        file.read_to_string(&mut source).map_err(|why| {
            CompileError::new(
                "Compile error",
                format!("Can not read {}/{}: {:?}", folder.display(), name, why),
                vec![],
            )
        })?;
        let mut file_hasher = DefaultHasher::new();
        path.hash(&mut file_hasher);
        let file_id = file_hasher.finish();

        file_info_chart.insert(
            file_id,
            FileInfo {
                name: path_display(&path),
                path: folder.join(name.clone()).display().to_string(),
                contents: source.split("\n").map(|x| x.to_string()).collect(),
            },
        );

        let mut string_table = StringTable::new();
        let mut used_constants = HashSet::new();
        let (imports, funcs, named_types, global_vars, func_table) =
            typecheck::sort_top_level_decls(
                parse_from_source(
                    source,
                    file_id,
                    &path,
                    &mut string_table,
                    constants_path,
                    &mut used_constants,
                    error_system,
                )?,
                path.clone(),
                &mut string_table,
                builtins,
            );
        paths.append(&mut imports.iter().map(|imp| imp.path.clone()).collect());
        import_map.insert(path.clone(), imports.clone());
        programs.insert(
            path.clone(),
            Module::new(
                funcs,
                named_types,
                used_constants,
                global_vars,
                imports,
                string_table,
                func_table,
                path,
                name,
            ),
        );
    }
    Ok((programs, import_map))
}

fn resolve_imports(
    modules: &mut HashMap<Vec<String>, Module>,
    import_map: &mut HashMap<Vec<String>, Vec<Import>>,
    error_system: &mut ErrorSystem,
) -> Result<(), CompileError> {
    for (name, imports) in import_map {
        for import in imports {
            let import_path = import.path.clone();
            let (named_type, imp_func) = if let Some(module) = modules.get_mut(&import_path) {
                // Looks up info from target module
                let string_id = module
                    .string_table
                    .get_if_exists(&import.name.clone())
                    .ok_or(CompileError::new(
                        "Import Error",
                        format!(
                            "Symbol {} does not exist in {}",
                            Color::red(&import.name),
                            Color::red(&import.path.join("/"))
                        ),
                        import.location.into_iter().collect(),
                    ))?;
                let named_type = module.named_types.get(&string_id).cloned();
                let imp_func = module.func_table.get(&string_id).cloned();
                (named_type, imp_func)
            } else {
                return Err(CompileError::new(
                    "Internal error",
                    format!(
                        "Can not find target file for import \"{}::{}\"",
                        import.path.get(0).cloned().unwrap_or_else(String::new),
                        import.name
                    ),
                    import.location.into_iter().collect(),
                ));
            };

            // Modifies origin module to include import
            let origin_module = modules.get_mut(name).ok_or_else(|| {
                CompileError::new(
                    "Internal error",
                    format!(
                        "Can not find originating file for import {}::{}",
                        Color::red(import.path.get(0).map(String::as_str).unwrap_or_default()),
                        Color::red(&import.name)
                    ),
                    import.location.into_iter().collect(),
                )
            })?;

            let string_id = match origin_module.string_table.get_if_exists(&import.name) {
                Some(string_id) => string_id,
                None => {
                    return Err(CompileError::new(
                        "Internal error",
                        format!("Import {} has no string id", import.name),
                        import.loc(),
                    ))
                }
            };

            if let Some(named_type) = named_type {
                origin_module
                    .named_types
                    .insert(string_id, named_type.clone());
            } else if let Some(imp_func) = imp_func {
                let public = match imp_func {
                    Type::Func(prop, _, _) => prop.public,
                    x => panic!(
                        "Func {} somehow has non-func type {}",
                        import.name,
                        x.display()
                    ),
                };

                match public {
                    true => {
                        origin_module.func_table.insert(string_id, imp_func.clone());
                    }
                    false => {
                        return Err(CompileError::new(
                            format!("Import error"),
                            format!("Func {} is private", Color::red(&import.name)),
                            import.loc(),
                        ))
                    }
                }
            } else {
                error_system.warnings.push(CompileError::new_warning(
                    "Compile Warning",
                    format!(
                        "import \"{}::{}\" does not correspond to a type or function",
                        import.path.get(0).cloned().unwrap_or_else(String::new),
                        import.name
                    ),
                    import.location.into_iter().collect(),
                ));
            }
        }
    }
    Ok(())
}

/// Constructor for `TypeTree`
fn create_type_tree(program_tree: &HashMap<Vec<String>, Module>) -> TypeTree {
    program_tree
        .iter()
        .map(|(path, program)| {
            program
                .named_types
                .iter()
                .map(|(id, tipe)| {
                    (
                        (path.clone(), *id),
                        (
                            tipe.clone(),
                            program_tree
                                .get(path)
                                .map(|module| module.string_table.name_from_id(*id).clone())
                                .unwrap(),
                        ),
                    )
                })
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect()
}

fn typecheck_programs(
    type_tree: &TypeTree,
    modules: Vec<Module>,
    _file_info_chart: &mut BTreeMap<u64, FileInfo>,
    error_system: &mut ErrorSystem,
) -> Result<Vec<TypeCheckedModule>, CompileError> {
    let (typechecked_modules, module_issues) = modules
        .into_par_iter()
        .map(
            |Module {
                 funcs,
                 named_types,
                 constants,
                 global_vars,
                 imports,
                 string_table,
                 func_table,
                 path,
                 name: _,
             }| {
                let mut typecheck_issues = vec![];
                let (mut checked_funcs, global_vars, string_table) =
                    typecheck::typecheck_top_level_decls(
                        funcs,
                        &named_types,
                        global_vars,
                        &imports,
                        string_table,
                        func_table,
                        type_tree,
                        &path,
                    )?;

                for (id, func) in &mut checked_funcs {
                    let detected_view = func.is_view(type_tree);
                    let detected_write = func.is_write(type_tree);

                    let name = string_table.name_from_id(*id);

                    if detected_view && !func.properties.view {
                        typecheck_issues.push(CompileError::new_type_error(
                            format!(
                                "Func {} is {} but was not declared so",
                                Color::red(name),
                                Color::red("view")
                            ),
                            func.debug_info.locs(),
                        ));
                    }

                    if detected_write && !func.properties.write {
                        typecheck_issues.push(CompileError::new_type_error(
                            format!(
                                "Func {} is {} but was not declared so",
                                Color::red(name),
                                Color::red("write")
                            ),
                            func.debug_info.locs(),
                        ));
                    }

                    if !detected_view && func.properties.view {
                        typecheck_issues.push(CompileError::new_warning(
                            "Typecheck warning",
                            format!(
                                "Func {} is marked {} but isn't",
                                Color::color(error_system.warn_color, name),
                                Color::color(error_system.warn_color, "view")
                            ),
                            func.debug_info.locs(),
                        ));
                    }

                    if !detected_write && func.properties.write {
                        typecheck_issues.push(CompileError::new_warning(
                            "Typecheck warning",
                            format!(
                                "Func {} is marked {} but isn't",
                                Color::color(error_system.warn_color, name),
                                Color::color(error_system.warn_color, "write")
                            ),
                            func.debug_info.locs(),
                        ));
                    }

                    if !detected_view
                        && !detected_write
                        && func.properties.nouts == 0
                        && !func.properties.sensitive
                    {
                        typecheck_issues.push(CompileError::new_warning(
                            "Optimizer warning",
                            format!(
                                "Func {} is prunable since it's {} and {}. Should it be marked {}?",
                                Color::color(error_system.warn_color, name),
                                Color::color(error_system.warn_color, "pure"),
                                Color::color(error_system.warn_color, "void"),
                                Color::color(error_system.warn_color, "sensitive"),
                            ),
                            func.debug_info.locs(),
                        ));
                    }

                    // don't allow an incorrect purity to trip up future stages
                    func.properties.write |= detected_view;
                    func.properties.view |= detected_write;
                }

                Ok((
                    TypeCheckedModule::new(
                        checked_funcs,
                        string_table,
                        named_types,
                        constants,
                        global_vars,
                        imports,
                        path,
                    ),
                    typecheck_issues,
                ))
            },
        )
        .collect::<Result<(Vec<TypeCheckedModule>, Vec<Vec<CompileError>>), CompileError>>()?;

    for issue in module_issues.into_iter().flatten() {
        match issue.is_warning {
            true => error_system.warnings.push(issue),
            false => error_system.errors.push(issue),
        }
    }

    Ok(typechecked_modules)
}

fn check_global_constants(
    modules: &Vec<TypeCheckedModule>,
    constants_path: Option<&Path>,
    error_system: &mut ErrorSystem,
) {
    let mut global_constants = init_constant_table(constants_path).unwrap();
    for module in modules {
        for constant in &module.constants {
            global_constants.remove(constant);
        }
    }

    for (constant, _) in global_constants {
        if !constant.starts_with('_') {
            error_system.warnings.push(CompileError::new_warning(
                "Compile Warning",
                format!(
                    "global constant {} is never used",
                    Color::color(error_system.warn_color, constant),
                ),
                vec![],
            ));
        }
    }
}

fn codegen_modules(
    typechecked_modules: Vec<TypeCheckedModule>,
    type_tree: TypeTree,
    optimization_level: usize,
    release_build: bool,
) -> Result<(Vec<CompiledFunc>, Vec<GlobalVar>), CompileError> {
    let mut work_list = vec![];
    let mut globals_so_far = 0;

    for mut module in typechecked_modules {
        // assign globals to the right of all prior
        let mut global_vars = HashMap::new();
        for mut global in module.global_vars {
            global.offset = Some(globals_so_far);
            global_vars.insert(global.id, global);
            globals_so_far += 1;
        }

        // add universal labels to functions
        for (_, func) in &mut module.checked_funcs {
            let unique_id = Import::unique_id(&module.path, &func.name);
            func.unique_id = Some(unique_id);
        }

        let mut func_labels = HashMap::new(); // local StringId to global Labels
        for (id, func) in &module.checked_funcs {
            match func.properties.closure {
                true => func_labels.insert(*id, Label::Closure(func.unique_id.unwrap())),
                false => func_labels.insert(*id, Label::Func(func.unique_id.unwrap())),
            };
        }
        for import in &module.imports {
            match import.id {
                Some(id) => drop(func_labels.insert(id, Label::Func(import.unique_id))),
                None => panic!("Import without id {:#?}", &import),
            }
        }

        for (_, func) in module.checked_funcs {
            work_list.push((
                func,
                func_labels.clone(),
                module.string_table.clone(),
                global_vars.clone(),
                module.path.clone(),
            ));
        }
    }

    let pure_funcs = work_list
        .par_iter()
        .map(|(func, func_labels, string_table, _, module_path)| {
            let func_name = func.name.clone();
            let debug_info = func.debug_info;

            if !func.properties.is_pure() {
                return Ok(None);
            }

            let (code, mut label_gen, frame_size) = codegen::mavm_codegen_func(
                func.clone(),
                string_table,
                &HashMap::new(),
                func_labels,
                release_build,
            )?;

            let code = translate::expand_calls(code, &mut label_gen);
            let code = translate::untag_jumps(code);
            let code = translate::replace_phi_nodes(code);
            let code = xformcode::fold_tuples(code, 0)?;

            // a pure program has no globals, not even a jump table since
            // the emulator can handle backward jumps
            let globals = vec![];

            // we won't handle closures
            let captures = HashMap::new();

            // we won't do reachability analysis for this program
            let dependencies = HashSet::new();

            Ok(Some(CompiledFunc::new(
                func_name,
                module_path.to_vec(),
                code,
                captures,
                frame_size,
                dependencies,
                globals,
                type_tree.clone(),
                debug_info,
            )))
        })
        .collect::<Result<Vec<Option<CompiledFunc>>, CompileError>>()?;

    let pure_funcs: Vec<CompiledFunc> = pure_funcs.into_iter().filter_map(|x| x).collect();
    let computer = Computer::new(pure_funcs)?;

    let mut funcs = work_list
        .into_par_iter()
        .map(|(func, func_labels, string_table, globals, module_path)| {
            let func_name = func.name.clone();
            let debug_info = func.debug_info;

            let (code, mut label_gen, frame_size) = codegen::mavm_codegen_func(
                func,
                &string_table,
                &globals,
                &func_labels,
                release_build,
            )?;

            let mut graph = BasicGraph::new(code);

            graph.pop_useless_locals();
            graph.graph_reduce(&computer, optimization_level);
            graph.color(frame_size, optimization_level);
            let frame_size = graph.shrink_frame();

            let (code, dependencies) = graph.flatten();
            let code = translate::expand_pops(code);
            let code = translate::expand_calls(code, &mut label_gen);
            let code = translate::untag_jumps(code);
            let code = translate::replace_phi_nodes(code);
            let (code, captures) = translate::read_capture_data(code);

            let globals: Vec<_> = globals.into_iter().map(|g| g.1).collect();

            Ok(CompiledFunc::new(
                func_name,
                module_path,
                code,
                captures,
                frame_size,
                dependencies,
                globals,
                type_tree.clone(),
                debug_info,
            ))
        })
        .collect::<Result<Vec<CompiledFunc>, CompileError>>()?;

    let mut capture_map = HashMap::new();
    let mut frame_sizes = HashMap::new();
    for func in &funcs {
        let func_id = func.unique_id;
        capture_map.insert(func_id, func.captures.clone());
        frame_sizes.insert(func_id, func.frame_size);
    }
    for func in &mut funcs {
        func.code = translate::pack_closures(&func.code, &capture_map, &frame_sizes);
    }

    let mut globals = BTreeMap::new();
    for func in &funcs {
        for global in func.globals.iter() {
            globals.insert(global.offset, global.clone()); // ensure duplicates aren't present
        }
    }

    let mut globals: Vec<_> = globals.into_iter().map(|x| x.1).collect();
    globals.push(GlobalVar::new(
        usize::MAX,
        "_jump_table".to_string(),
        Type::Any,
        DebugInfo::default(),
    ));

    Ok((funcs, globals))
}

pub fn comma_list(input: &[String]) -> String {
    let mut base = String::new();
    if input.len() > 0 {
        for object in input.iter().take(input.len() - 1) {
            base.push_str(object);
            base.push(',');
            base.push(' ');
        }
        base.push_str(&input[input.len() - 1]);
    }
    base
}

/// Converts source string `source` into a series of `TopLevelDecl`s, uses identifiers from
/// `string_table` and records new ones in it as well. The `file_id` argument is used to construct
/// file information for the location fields.
pub fn parse_from_source(
    source: String,
    file_id: u64,
    file_path: &[String],
    string_table: &mut StringTable,
    constants_path: Option<&Path>,
    used_constants: &mut HashSet<String>,
    error_system: &mut ErrorSystem,
) -> Result<(Vec<TopLevelDecl>, BTreeMap<StringId, Func>), CompileError> {
    let lines = Lines::new(source.bytes());
    let mut constants = init_constant_table(constants_path)?;
    let mut local_constants = HashMap::<String, Location>::new();
    let mut closures = BTreeMap::new();

    let parsed = DeclsParser::new()
        .parse(
            string_table,
            &lines,
            file_id,
            file_path,
            &mut constants,
            &mut local_constants,
            used_constants,
            &mut closures,
            error_system,
            &source,
        )
        .map_err(|e| match e {
            ParseError::UnrecognizedToken {
                token: (offset, _tok, end),
                expected,
            } => CompileError::new(
                String::from("Compile error: unexpected token"),
                format!(
                    "{}, expected one of: {}",
                    &source[offset..end],
                    comma_list(&expected),
                ),
                vec![lines.location(BytePos::from(offset), file_id).unwrap()],
            ),
            ParseError::InvalidToken { location } => CompileError::new(
                "Compile error",
                format!("found invalid token"),
                lines
                    .location(location.into(), file_id)
                    .into_iter()
                    .collect(),
            ),
            ParseError::UnrecognizedEOF { location, expected } => CompileError::new(
                String::from("Compile error: unexpected end of file"),
                format!("expected one of: {}", comma_list(&expected)),
                lines
                    .location(location.into(), file_id)
                    .into_iter()
                    .collect(),
            ),
            ParseError::ExtraToken {
                token: (offset, _tok, end),
            } => CompileError::new(
                String::from("Compile error: extra token"),
                format!("{}", &source[offset..end],),
                vec![lines.location(BytePos::from(offset), file_id).unwrap()],
            ),
            ParseError::User { error } => error,
        })?;

    for (constant, loc) in local_constants {
        if !used_constants.contains(&constant) {
            error_system.warnings.push(CompileError::new_warning(
                "Compile Warning",
                format!(
                    "Constant {} is never used",
                    Color::color(error_system.warn_color, constant),
                ),
                vec![loc],
            ));
        }
    }

    Ok((parsed, closures))
}

/// Represents any error encountered during compilation.
#[derive(Debug, Clone)]
pub struct CompileError {
    /// The error title
    pub title: String,
    /// What the error is.
    pub description: String,
    /// Where the error happened.
    pub locations: Vec<Location>,
    /// Whether the error should not stop compilation
    pub is_warning: bool,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.description)
    }
}

impl<L, T> From<CompileError> for ParseError<L, T, CompileError> {
    fn from(error: CompileError) -> ParseError<L, T, CompileError> {
        ParseError::User { error }
    }
}

impl CompileError {
    pub fn new<S, U>(title: S, description: U, locations: Vec<Location>) -> Self
    where
        S: std::string::ToString,
        U: std::string::ToString,
    {
        CompileError {
            title: title.to_string(),
            description: description.to_string(),
            locations,
            is_warning: false,
        }
    }

    pub fn new_warning<S, U>(title: S, description: U, locations: Vec<Location>) -> Self
    where
        S: std::string::ToString,
        U: std::string::ToString,
    {
        CompileError {
            title: title.to_string(),
            description: description.to_string(),
            locations,
            is_warning: true,
        }
    }

    pub fn new_type_error<S>(description: S, locations: Vec<Location>) -> Self
    where
        S: std::string::ToString,
    {
        CompileError {
            title: String::from("Typecheck Error"),
            description: description.to_string(),
            locations,
            is_warning: false,
        }
    }

    pub fn new_codegen_error<S>(description: S, location: Option<Location>) -> Self
    where
        S: std::string::ToString,
    {
        CompileError {
            title: String::from("Codegen Error"),
            description: description.to_string(),
            locations: location.into_iter().collect(),
            is_warning: false,
        }
    }

    pub fn internal<S>(description: S, locations: Vec<Location>) -> Self
    where
        S: std::string::ToString,
    {
        CompileError {
            title: String::from("Internal Error"),
            description: description.to_string(),
            locations,
            is_warning: false,
        }
    }

    pub fn pretty_fmt(
        &self,
        file_info_chart: &BTreeMap<u64, FileInfo>,
        warnings_are_errors: bool,
    ) -> String {
        let blue = Color::BLUE;
        let reset = Color::RESET;

        let err_color = match self.is_warning {
            true => match warnings_are_errors {
                true => Color::PINK,
                false => Color::YELLOW,
            },
            false => Color::RED,
        };

        let last_line = &self.locations.last();

        let mut pretty = format!(
            "{}: {}\n{}    --> {}{}\n",
            Color::color(err_color, &self.title),
            self.description,
            blue,
            match last_line {
                None => String::from("Could not determine location of error"),
                Some(location) => match file_info_chart.get(&location.file_id) {
                    None => String::from("file with id ") + &location.file_id.to_string(),
                    Some(info) => format!(
                        "{}{} line {} column {}{}",
                        info.path,
                        reset,
                        Color::blue(location.line),
                        blue,
                        location.column,
                    ),
                },
            },
            reset
        );

        pretty += &self
            .locations
            .iter()
            .map(|location| match file_info_chart.get(&location.file_id) {
                None => String::new(),
                Some(info) => format!(
                    "     {}|\n{: <4} | {}{}\n",
                    blue,
                    location.line,
                    reset,
                    match info.contents.get(location.line.to_usize()) {
                        None => "could not recover line",
                        Some(line) => line,
                    }
                ),
            })
            .collect::<String>();

        pretty += &self
            .locations
            .last()
            .into_iter()
            .map(|x| {
                format!(
                    "     {}{:0space$}{}\n",
                    Color::blue("|"),
                    " ",
                    Color::color(err_color, "^"),
                    space = x.column.to_usize() + 1
                )
            })
            .collect::<String>();

        pretty
    }

    pub fn print(&self, file_info_chart: &BTreeMap<u64, FileInfo>, warnings_are_errors: bool) {
        eprintln!("{}", self.pretty_fmt(file_info_chart, warnings_are_errors));
    }
}

/// A collection of all compiler warnings encountered and the mechanism to handle them.
pub struct ErrorSystem {
    /// All compilation errors
    pub errors: Vec<CompileError>,
    /// All compilation warnings
    pub warnings: Vec<CompileError>,
    /// Whether these should halt compilation
    pub warnings_are_errors: bool,
    /// The color to use when highlighting parts of the body text
    pub warn_color: &'static str,
    /// File information that helps the error system pretty-print errors and warnings
    pub file_info_chart: BTreeMap<u64, FileInfo>,
}

impl ErrorSystem {
    pub fn print(&self) {
        for warning in &self.warnings {
            warning.print(&self.file_info_chart, self.warnings_are_errors);
        }
        for error in &self.errors {
            error.print(&self.file_info_chart, self.warnings_are_errors);
        }
    }
}

/// Lists the offset of each source file contained by a CompiledProgram in offsets, and the
/// instruction directly following the last in the CompiledProgram.
#[derive(Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FileInfo {
    pub name: String,
    #[serde(skip)]
    pub path: String,
    #[serde(skip)]
    pub contents: Vec<String>,
}

impl Debug for FileInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "FileInfo {{path: {:?}, name: {:?}, contents:...}}",
            self.path, self.name
        )
    }
}

impl Display for FileInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}
