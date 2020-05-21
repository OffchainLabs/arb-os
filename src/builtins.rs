use std::path::Path;
use crate::ast::{Type, ImportFuncDecl};
use crate::stringtable::StringTable;
use crate::compile::{CompiledProgram, CompileError, compile_from_file};


pub fn builtin_func_decls<'a>(mut string_table: StringTable<'a>) -> (Vec<ImportFuncDecl>, StringTable<'a>) {
    let imps = vec![
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayNew"),
            false, 
            vec![Type::Uint, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arrayGet"),
            false,
            vec![Type::Any, Type::Uint],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_arraySet"),
            false,
            vec![Type::Any, Type::Uint, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsNew"),
            false,
            vec![],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsHasKey"),
            false,
            vec![Type::Any, Type::Any],
            Type::Bool,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsGet"),
            false,
            vec![Type::Any, Type::Any],
            Type::Tuple(vec![Type::Any, Type::Bool]),
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsSet"),
            false,
            vec![Type::Any, Type::Any, Type::Any],
            Type::Any,
        ),
        ImportFuncDecl::new_types(
            string_table.get("builtin_kvsDelete"),
            false,
            vec![Type::Any, Type::Any],
            Type::Any,
        )
    ];
    (imps, string_table)
}


pub fn add_auto_link_progs<'a>(
	progs_in: &[CompiledProgram],
) -> Result<Vec<CompiledProgram>, CompileError<'a>> {
	let builtin_pathnames = vec![
        "builtin/array.mao",
        "builtin/kvs.mao",
	];
	let mut progs = progs_in.to_owned();
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
