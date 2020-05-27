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

use crate::ast::{ImportFuncDecl, Type};
use crate::compile::{compile_from_file, CompileError, CompiledProgram};
use crate::stringtable::StringTable;
use std::path::Path;

pub fn builtin_func_decls<'a>(
    mut string_table: StringTable<'a>,
) -> (Vec<ImportFuncDecl>, StringTable<'a>) {
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
        ImportFuncDecl::new_types(string_table.get("builtin_kvsNew"), false, vec![], Type::Any),
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
        ),
    ];
    (imps, string_table)
}

pub fn add_auto_link_progs(
    progs_in: &[CompiledProgram],
) -> Result<Vec<CompiledProgram>, CompileError> {
    let builtin_pathnames = vec!["builtin/array.mao", "builtin/kvs.mao"];
    let mut progs = progs_in.to_owned();
    for pathname in builtin_pathnames {
        let path = Path::new(pathname);
        match compile_from_file(path, false) {
            Ok(compiled_program) => {
                progs.push(compiled_program);
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
    Ok(progs)
}
