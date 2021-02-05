use crate::compile::{AbstractSyntaxTree, StructField, Type, TypeCheckedNode};
use crate::link::LinkedProgram;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;

pub(crate) fn gen_upgrade_code(input: &Path, output: &Path, output_file: &Path) -> Result<(), ()> {
    let mut code = File::create(output_file).map_err(|_| ())?;
    let input_fields = get_globals_from_file(input)?;
    let output_fields = get_globals_from_file(output)?;
    let intersection: HashSet<&StructField> = input_fields
        .iter()
        .collect::<HashSet<_>>()
        .intersection(&(output_fields.iter().collect::<HashSet<_>>()))
        .cloned()
        .collect();
    let output_only: HashSet<&StructField> = output_fields
        .iter()
        .collect::<HashSet<_>>()
        .difference(&(input_fields.iter().collect()))
        .cloned()
        .collect();
    let input_struct = Type::Struct(input_fields.clone());
    let output_struct = Type::Struct(output_fields.clone());
    writeln!(
        code,
        "{}",
        type_decl_string(&"InputGlobals".to_string(), &input_struct)
    )
    .map_err(|_| ())?;
    writeln!(
        code,
        "{}",
        type_decl_string(&"OutputGlobals".to_string(), &output_struct)
    )
    .map_err(|_| ())?;
    writeln!(
        code,
        "func upgrade(input_globals: InputGlobals) -> OutputGlobals {{"
    )
    .map_err(|_| ())?;
    for field in intersection {
        writeln!(
            code,
            "    {}",
            let_string(&field.name, &format!("input_globals.{}", field.name))
        )
        .map_err(|_| ())?;
    }
    for field in output_only {
        writeln!(
            code,
            "    {}",
            let_string(&field.name, &"panic".to_string())
        )
        .map_err(|_| ())?;
    }
    writeln!(code, "    return struct {{").map_err(|_| ())?;
    for field in output_fields {
        writeln!(code, "        {}: {},", field.name, field.name).map_err(|_| ())?;
    }
    writeln!(code, "    }};").map_err(|_| ())?;
    writeln!(code, "}}").map_err(|_| ())?;
    Ok(())
}

fn get_globals_from_file(path: &Path) -> Result<Vec<StructField>, ()> {
    let mut file = File::open(&path).map_err(|_| ())?;
    let mut s = String::new();
    file.read_to_string(&mut s).map_err(|_| ())?;
    let globals: LinkedProgram = serde_json::from_str(&s).map_err(|_| ())?;

    let mut fields = vec![];
    for global in globals.globals {
        if global.name_id != usize::max_value() {
            let mut tipe = global.tipe;
            if let Type::Nominal(_, _) = tipe {
                tipe = Type::Any;
            } else {
                tipe.recursive_apply(replace_nominal, &(), &mut ());
            }
            fields.push(StructField::new(global.name, tipe))
        }
    }
    Ok(fields)
}

fn type_decl_string(type_name: &String, tipe: &Type) -> String {
    format!("type {} = {}", type_name, tipe.display())
}

fn let_string(name: &String, expr: &String) -> String {
    format!("let {} = {};", name, expr)
}

fn replace_nominal(node: &mut TypeCheckedNode, _state: &(), _mut_state: &mut ()) -> bool {
    match node {
        TypeCheckedNode::Type(tipe) => {
            if let Type::Nominal(_, _) = tipe {
                **tipe = Type::Any;
                false
            } else {
                true
            }
        }
        _ => true,
    }
}
