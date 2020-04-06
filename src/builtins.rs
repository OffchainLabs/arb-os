use std::path::Path;
use std::clone::Clone;
use crate::ast::{Type, ImportFuncDecl};
use crate::stringtable::StringTable;
use crate::compile::{CompiledProgram, CompileError, compile_from_file};

/*
pub fn add_builtins_to_string_table(mut string_table: StringTable) -> StringTable {
    let builtin_names = vec![
        "builtin_arraySize",
        "builtin_arrayNew",
        "builtin_arrayGet",
        "builtin_arraySet",
        "builtin_arraySwap",
    ];
    for name in builtin_names {
        string_table.get(name);  // ignore return value
    }
    return string_table;
}
*/

pub fn builtin_func_decls<'a>(mut string_table: StringTable<'a>) -> (Vec<ImportFuncDecl>, StringTable<'a>) {
    let imps = vec![
        ImportFuncDecl::new_types(
            string_table.get("builtin_arraySize"),
            vec![Type::Any],
            Type::Int,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayNew"),
            vec![Type::Int, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayGet"),
            vec![Type::Any, Type::Int],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arraySet"),
            vec![Type::Any, Type::Int, Type::Any],
            Type::Any,
        ),
    ];
    (imps, string_table)
}


pub fn add_auto_link_progs<'a>(
	progs_in: &Vec<CompiledProgram>,
) -> Result<Vec<CompiledProgram>, CompileError<'a>> {
	let builtin_pathnames = vec![
		"builtin/array.mao"
	];
	let mut progs = progs_in.clone();
	for pathname in builtin_pathnames {
		let path = Path::new(pathname); 
		match compile_from_file(path, false) {
			Ok(compiled_program) => { 
				progs.push(compiled_program); 
			}
			Err(e) => { return Err(e); },
		}
	}
	Ok(progs)
}
