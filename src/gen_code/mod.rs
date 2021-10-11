//
// Copyright 2020-2021, Offchain Labs, Inc. All rights reserved.
//

use crate::compile::{AbstractSyntaxTree, StructField, Type, TypeCheckedNode, TypeTree};
use crate::console::Color;
use crate::link::LinkedProgram;
use crate::stringtable::StringId;
use crate::GenUpgrade;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::io::Write;
use std::mem;
use std::path::Path;
use std::rc::Rc;

#[derive(Debug)]
pub struct GenCodeError {
    pub(crate) reason: String,
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
        "//\n// This file is machine-generated. Don't edit it unless you know what you're doing."
    )
    .unwrap();
    writeln!(
        code,
        "//\n// Copyright 2020-2021, Offchain Labs, Inc. All rights reserved.\n//"
    )
    .unwrap();
    writeln!(code, "").unwrap();
    let (mut input_fields, in_recursers, in_tree, old_arbos_version) =
        get_globals_and_version_from_file(&from, false)?;
    let (mut output_fields, out_recursers, out_tree, _) =
        get_globals_and_version_from_file(&to, true)?;
    output_fields.push(StructField::new(String::from("_jump_table"), Type::Any));
    input_fields.push(StructField::new(String::from("_jump_table"), Type::Any));

    let input_map = input_fields
        .iter()
        .map(|field| (&field.name, &field.tipe))
        .collect::<HashMap<_, _>>();

    let mut intersection = HashSet::new();
    let mut output_only = HashSet::new();

    for field in &output_fields {
        if let Some(tipe) = input_map.get(&field.name) {
            if field
                .tipe
                .assignable(*tipe, &out_tree, &in_tree, HashSet::new())
            {
                intersection.insert(field);
            } else {
                output_only.insert(field);
            }
        } else {
            output_only.insert(field);
        };
    }
    output_only.remove(&StructField::new(String::from("_jump_table"), Type::Any));

    intersection.remove(&StructField::new(String::from("_jump_table"), Type::Any));

    let input_struct = Type::Struct(input_fields.clone());
    let output_struct = Type::Struct(output_fields.clone());
    let mut output_only: Vec<_> = output_only.into_iter().collect();
    output_only.sort_by(|left, right| left.name.cmp(&right.name));
    let mut intersection: Vec<_> = intersection.into_iter().collect();
    intersection.sort_by(|left, right| left.name.cmp(&right.name));
    for field in output_only.iter() {
        if field.name == String::from("_jump_table") {
            panic!()
        }
        writeln!(code, "use {}::set_{}_onUpgrade;", impl_file, field.name)
            .map_err(|_| GenCodeError::new("Failed to write use statement".to_string()))?;
    }
    for field in &intersection {
        if field.name == String::from("_jump_table") {
            panic!()
        }
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
    write_subtypes(&mut code, in_recursers, Some("before__"), &in_tree)?;
    write_subtypes(&mut code, out_recursers, Some("after__"), &out_tree)?;
    writeln!(code).map_err(|_| GenCodeError::new("Failed to write empty line".to_string()))?;
    writeln!(
        code,
        "{}",
        type_decl_string(
            &"GlobalsBeforeUpgrade".to_string(),
            &input_struct,
            Some("before__"),
            &in_tree
        )
    )
    .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    writeln!(code, "").unwrap();
    writeln!(
        code,
        "{}",
        type_decl_string(
            &"GlobalsAfterUpgrade".to_string(),
            &output_struct,
            Some("after__"),
            &out_tree
        )
    )
    .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    writeln!(code, "").unwrap();
    writeln!(
        code,
        "public {} func remapGlobalsForUpgrade(input_globals: GlobalsBeforeUpgrade) -> (GlobalsAfterUpgrade, uint) {{",
        if map.data.contains("_jump_table") {
            ""
        } else {
            "view"
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
    writeln!(code, "    return (struct {{")
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    for field in output_fields {
        writeln!(code, "        {}: {},", field.name, field.name)
            .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    }
    writeln!(code, "    }}, {});", old_arbos_version)
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;
    writeln!(code, "}}")
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;

    // generate a dummy function, so we don't end up with an empty code file, which is an error
    writeln!(code, "\n\nfunc __dummy__() {{ return; }}\n\n")
        .map_err(|_| GenCodeError::new("Failed to write to output file".to_string()))?;

    Ok(())
}

fn write_subtypes(
    code: &mut File,
    mut subtypes: HashMap<StringId, Type>,
    prefix: Option<&str>,
    type_tree: &TypeTree,
) -> Result<(), GenCodeError> {
    let mut total_subtypes = subtypes.clone();
    while !subtypes.is_empty() {
        let mut new_subtypes = HashMap::new();
        let mut vec_subtypes = subtypes.iter().collect::<Vec<_>>();
        vec_subtypes.sort_by(|(_, lower), (_, higher)| {
            lower
                .display_separator("_", None, true, type_tree)
                .0
                .cmp(&higher.display_separator("_", None, true, type_tree).0)
        });
        for (id, subtype) in vec_subtypes {
            writeln!(
                code,
                "type {}{}{} = {};",
                prefix.unwrap_or(""),
                if let Type::Nominal(a, _) = subtype.clone() {
                    a.path
                        .iter()
                        .map(|name| name.clone() + "_")
                        .collect::<String>()
                } else {
                    format!("")
                },
                &id.id,
                {
                    if let Type::Nominal(b, _) = subtype.clone() {
                        let (displayed, subtypes) = type_tree
                            .get(&b)
                            .expect(&format!("{}", b))
                            .display_separator("_", prefix, true, type_tree);
                        new_subtypes.extend(subtypes);
                        displayed
                    } else {
                        return Err(GenCodeError::new(format!(
                            "Encountered non nominal type in subtypes list: \"{}\"",
                            subtype.display()
                        )));
                    }
                }
            )
            .unwrap();
        }
        subtypes = HashMap::new() /*new_subtypes.difference(&total_subtypes).cloned().collect()*/;
        for (id, tipe) in &new_subtypes {
            if let Some(val) = total_subtypes.get(id) {
                if !(tipe == val) {
                    subtypes.insert(id.clone(), tipe.clone());
                }
            } else {
                subtypes.insert(id.clone(), tipe.clone());
            }
        }
        total_subtypes.extend(new_subtypes);
    }
    Ok(())
}

fn get_globals_and_version_from_file(
    path: &Path,
    fix: bool,
) -> Result<(Vec<StructField>, HashMap<StringId, Type>, TypeTree, u64), GenCodeError> {
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
    let globals: LinkedProgram = serde_json::from_str(&s).map_err(|error| {
        GenCodeError::new(format!(
            "Failed to deserialize file \"{}\"\n{}",
            Color::red(path.to_str().unwrap_or("")),
            error
        ))
    })?;

    let type_tree = globals.type_tree.into_type_tree(fix);

    let mut state: (Vec<_>, Rc<RefCell<HashMap<StringId, Type>>>) =
        (vec![], Rc::new(RefCell::new(HashMap::new())));

    let mut fields = vec![];

    let mut old_state = state.clone();
    for global in globals.globals {
        if global.id != StringId::new(vec!["/meta".to_string()], String::new()) {
            let mut tipe = global.tipe;
            if let Type::Nominal(id, _) = tipe {
                tipe = type_tree.get(&id).cloned().expect(&format!("{}", id));
            }
            tipe.recursive_apply(replace_nominal, &type_tree, &mut state);

            fields.push(StructField::new(global.name, tipe))
        }
    }
    loop {
        if state == old_state {
            break;
        }
        old_state = state.clone();
        let mut new_types = HashMap::new();
        let mut cool_temp = HashMap::new();

        for (id, tipe) in state.1.borrow().iter() {
            if let Some(value) = old_state.1.borrow().get(id) {
                if value != tipe {
                    cool_temp.insert(id.clone(), tipe.clone());
                }
            } else {
                cool_temp.insert(id.clone(), tipe.clone());
            }
        }
        for (id, diff) in cool_temp {
            let new_type = {
                let mut new = if let Type::Nominal(id, _) = diff {
                    type_tree.get(&id).cloned().unwrap() //_or(Type::Any)
                } else {
                    diff.clone()
                };
                new.recursive_apply(replace_nominal, &type_tree, &mut state);
                new
            };
            new_types.insert(id, new_type);
        }
        (&mut *(*state.1).borrow_mut()).extend(new_types);
    }
    mem::drop(old_state);
    let subtypes = Rc::get_mut(&mut state.1).unwrap().clone().into_inner();
    Ok((fields, subtypes, type_tree, globals.arbos_version))
}

fn type_decl_string(
    type_name: &String,
    tipe: &Type,
    prefix: Option<&str>,
    type_tree: &TypeTree,
) -> String {
    format!(
        "type {} = {};",
        type_name,
        tipe.display_separator("_", prefix, true, type_tree).0
    )
}

fn let_string(name: &String, expr: &String) -> String {
    format!("let {} = {};", name, expr)
}

fn replace_nominal(
    node: &mut TypeCheckedNode,
    state: &TypeTree,
    mut_state: &mut (Vec<Type>, Rc<RefCell<HashMap<StringId, Type>>>),
) -> bool {
    match node {
        TypeCheckedNode::Type(tipe) => {
            if mut_state.0.iter().any(|thing| thing == *tipe) {
                let to_render = &mut *(*mut_state.1).borrow_mut();
                to_render.insert(
                    if let Type::Nominal(id, _) = tipe {
                        id.clone()
                    } else {
                        panic!() //format!("Bad")
                    },
                    tipe.clone(),
                );
                return false;
            }
            if let Type::Nominal(id, spec) = tipe {
                mut_state.0.push(Type::Nominal(id.clone(), spec.clone()));
                let to_render = &mut *(*mut_state.1).borrow_mut();
                to_render.insert(id.clone(), Type::Nominal(id.clone(), spec.clone()));
                **tipe = state.get(&id).cloned().unwrap_or(Type::Any);
            }
            true
        }
        _ => true,
    }
}
