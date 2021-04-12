use crate::compile::{AbstractSyntaxTree, StructField, Type, TypeCheckedNode, TypeTree};
use crate::link::LinkedProgram;
use crate::GenUpgrade;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

#[derive(Debug)]
pub struct GenCodeError {
    reason: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ConfigFile {
    data: HashSet<String>,
}

impl GenCodeError {
    fn new(reason: String) -> Self {
        GenCodeError { reason }
    }
}

impl Display for GenCodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.reason)
    }
}

pub(crate) fn gen_upgrade_code(input: GenUpgrade) -> Result<(), GenCodeError> {
    let GenUpgrade {
        from,
        to,
        out_file,
        impl_file,
        config_file,
    } = input;
    let map = if let Some(config_file) = config_file {
        let mut file = File::open(config_file)
            .map_err(|_| GenCodeError::new("Could not open config file".to_string()))?;
        let mut s = String::new();
        file.read_to_string(&mut s)
            .map_err(|_| GenCodeError::new("Could not read config file to string".to_string()))?;
        toml::from_str(&s)
            .map_err(|_| GenCodeError::new("Failed to parse config file as toml".to_string()))?
    } else {
        ConfigFile {
            data: HashSet::new(),
        }
    };
    let mut code = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(&out_file)
        .map_err(|_| {
            GenCodeError::new(format!(
                "Could not create output file \"{}\"",
                out_file.to_str().unwrap_or("")
            ))
        })?;
    writeln!(code, "").unwrap();
    writeln!(
        code,
        "// This file is machine-generated. Don't edit it unless you know what you're doing."
    )
    .unwrap();
    writeln!(code, "").unwrap();
    let (mut input_fields, in_recursers, in_tree) = get_globals_from_file(&from)?;
    let (mut output_fields, out_recursers, out_tree) = get_globals_from_file(&to)?;
    output_fields.push(StructField::new(String::from("_jump_table"), Type::Any));
    input_fields.push(StructField::new(String::from("_jump_table"), Type::Any));
    let mut intersection: HashSet<&StructField> = input_fields
        .iter()
        .collect::<HashSet<_>>()
        .intersection(&(output_fields.iter().collect::<HashSet<_>>()))
        .cloned()
        .collect();
    intersection.remove(&StructField::new(String::from("_jump_table"), Type::Any));
    let output_only: HashSet<&StructField> = output_fields
        .iter()
        .collect::<HashSet<_>>()
        .difference(&(input_fields.iter().collect()))
        .cloned()
        .collect();
    let input_struct = Type::Struct(input_fields.clone());
    let output_struct = Type::Struct(output_fields.clone());
    let mut output_only: Vec<_> = output_only.into_iter().collect();
    output_only.sort_by(|left, right| left.name.cmp(&right.name));
    let mut intersection: Vec<_> = intersection.into_iter().collect();
    intersection.sort_by(|left, right| left.name.cmp(&right.name));
    for field in output_only.iter() {
        writeln!(code, "use {}::set_{}_onUpgrade;", impl_file, field.name)
            .map_err(|_| GenCodeError::new("Failed to write use statement".to_string()))?;
    }
    for field in &intersection {
        if map.data.contains(&field.name) {
            writeln!(code, "use {}::set_{}_onUpgrade;", impl_file, field.name)
                .map_err(|_| GenCodeError::new("Failed to write use statement".to_string()))?;
        }
    }
    if map.data.contains("_jump_table") {
        writeln!(code, "use {}::set_jump_table;", impl_file)
            .map_err(|_| GenCodeError::new("Failed to write use statement".to_string()))?;
    }
    writeln!(code).map_err(|_| GenCodeError::new("Failed to write empty line".to_string()))?;
    expand_things(&mut code, in_recursers, in_tree);
    expand_things(&mut code, out_recursers, out_tree);
    writeln!(code).map_err(|_| GenCodeError::new("Failed to write empty line".to_string()))?;
    writeln!(
        code,
        "{}",
        type_decl_string(&"GlobalsBeforeUpgrade".to_string(), &input_struct)
    )
    .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    writeln!(code, "").unwrap();
    writeln!(
        code,
        "{}",
        type_decl_string(&"GlobalsAfterUpgrade".to_string(), &output_struct)
    )
    .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    writeln!(code, "").unwrap();
    writeln!(
        code,
        "public {}func remapGlobalsForUpgrade(input_globals: GlobalsBeforeUpgrade) -> GlobalsAfterUpgrade {{",
        if map.data.contains("_jump_table") {
            ""
        } else {
            "impure "
        }
    )
    .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    for field in intersection {
        if map.data.contains(&field.name) {
            writeln!(
                code,
                "    {}",
                let_string(
                    &field.name,
                    &format!("set_{}_onUpgrade(input_globals)", field.name)
                )
            )
            .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
        } else {
            writeln!(
                code,
                "    {}",
                let_string(&field.name, &format!("input_globals.{}", field.name))
            )
            .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
        }
    }
    for field in output_only {
        writeln!(
            code,
            "    {}",
            let_string(
                &field.name,
                &format!("set_{}_onUpgrade(input_globals)", field.name)
            )
        )
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    }
    if map.data.contains("_jump_table") {
        writeln!(code, "    let _jump_table = set_jump_table(input_globals);")
            .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    } else {
        writeln!(
            code,
            "    let _jump_table = (asm() GlobalsAfterUpgrade {{ rget }})._jump_table;"
        )
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    }
    writeln!(code, "    return struct {{")
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    for field in output_fields {
        writeln!(code, "        {}: {},", field.name, field.name)
            .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    }
    writeln!(code, "    }};")
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    writeln!(code, "}}")
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    Ok(())
}

fn expand_things(code: &mut File, mut recursers: HashSet<Type>, type_tree: TypeTree) -> () {
    let mut total_recursers = recursers.clone();
    while !recursers.is_empty() {
        let mut new_recursers = HashSet::new();
        for thing in recursers {
            writeln!(code, "type {} = {}", thing.display_separator("_").0, {
                if let Type::Nominal(a, b) = thing.clone() {
                    let (displayed, subtypes) =
                        type_tree.get(&(a, b)).unwrap().display_separator("_");
                    new_recursers.extend(subtypes);
                    displayed
                } else {
                    unimplemented!()
                }
            })
            .unwrap();
        }
        recursers = new_recursers
            .difference(&total_recursers)
            .cloned()
            .collect();
        total_recursers.extend(new_recursers);
    }
}

fn get_globals_from_file(
    path: &Path,
) -> Result<(Vec<StructField>, HashSet<Type>, TypeTree), GenCodeError> {
    let mut file = File::open(&path).map_err(|_| {
        GenCodeError::new(format!(
            "Could not create file \"{}\"",
            path.to_str().unwrap_or("")
        ))
    })?;
    let mut s = String::new();
    file.read_to_string(&mut s).map_err(|_| {
        GenCodeError::new(format!(
            "Failed to read \"{}\" to string",
            path.to_str().unwrap_or("")
        ))
    })?;
    let globals: LinkedProgram = serde_json::from_str(&s).map_err(|_| {
        GenCodeError::new(format!(
            "Failed to deserialize file \"{}\"",
            path.to_str().unwrap_or("")
        ))
    })?;

    let type_tree = globals.type_tree.into_type_tree();

    let mut stuff = Rc::new(RefCell::new(HashSet::new()));

    let mut fields = vec![];
    for global in globals.globals {
        if global.name_id != usize::max_value() {
            let mut tipe = global.tipe;
            if let Type::Nominal(file_path, id) = tipe {
                tipe = type_tree
                    .get(&(file_path.clone(), id))
                    .cloned()
                    .unwrap_or(Type::Any);
            } else {
                tipe.recursive_apply(replace_nominal, &type_tree, &mut (vec![], stuff.clone()));
            }
            fields.push(StructField::new(global.name, tipe))
        }
    }
    let real_stuff = Rc::get_mut(&mut stuff).unwrap().clone().into_inner();
    Ok((fields, real_stuff, type_tree))
}

fn type_decl_string(type_name: &String, tipe: &Type) -> String {
    format!("type {} = {}", type_name, tipe.display_separator("_").0)
}

fn let_string(name: &String, expr: &String) -> String {
    format!("let {} = {};", name, expr)
}

fn replace_nominal(
    node: &mut TypeCheckedNode,
    state: &TypeTree,
    mut_state: &mut (Vec<Type>, Rc<RefCell<HashSet<Type>>>),
) -> bool {
    match node {
        TypeCheckedNode::Type(tipe) => {
            if **tipe == Type::Option(Box::new(Type::Nominal(vec!["pluggables".to_string()], 1))) {
                panic!("this does show up")
            }
            if mut_state.0.iter().any(|thing| thing == *tipe) {
                let mut to_render = mut_state.1.borrow_mut();
                to_render.insert(tipe.clone());
                return false;
            }
            if let Type::Nominal(path, id) = tipe {
                mut_state.0.push(Type::Nominal(path.clone(), *id));
                **tipe = state
                    .get(&(path.clone(), *id))
                    .cloned()
                    .unwrap_or(Type::Any);
            }
            true
        }
        _ => true,
    }
}
