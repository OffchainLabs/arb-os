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
        "// This file is machine-generated. Don't edit it unless you know what you're doing."
    )
    .unwrap();
    writeln!(code, "").unwrap();
    let (mut input_fields, in_recursers, in_tree, old_arbos_version) =
        get_globals_and_version_from_file(&from)?;
    let (mut output_fields, out_recursers, out_tree, _) = get_globals_and_version_from_file(&to)?;
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
        "public {}func remapGlobalsForUpgrade(input_globals: GlobalsBeforeUpgrade) -> (GlobalsAfterUpgrade, uint) {{",
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
    mut subtypes: HashSet<(Type, String)>,
    prefix: Option<&str>,
    type_tree: &TypeTree,
) -> Result<(), GenCodeError> {
    let mut total_subtypes = subtypes.clone();
    while !subtypes.is_empty() {
        let mut new_subtypes = HashSet::new();
        let mut vec_subtypes = subtypes.iter().collect::<Vec<_>>();
        vec_subtypes.sort_by(|(lower, _), (higher, _)| {
            lower
                .display_separator("_", None, true, type_tree)
                .0
                .cmp(&higher.display_separator("_", None, true, type_tree).0)
        });
        for (subtype, name) in vec_subtypes {
            writeln!(
                code,
                "type {}{}{} = {}",
                prefix.unwrap_or(""),
                if let Type::Nominal(a, _) = subtype.clone() {
                    a.iter().map(|name| name.clone() + "_").collect::<String>()
                } else {
                    format!("")
                },
                name,
                {
                    if let Type::Nominal(a, b) = subtype.clone() {
                        let (displayed, subtypes) = type_tree
                            .get(&(a, b))
                            .unwrap()
                            .0
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
        subtypes = new_subtypes.difference(&total_subtypes).cloned().collect();
        total_subtypes.extend(new_subtypes);
    }
    Ok(())
}

fn get_globals_and_version_from_file(
    path: &Path,
) -> Result<(Vec<StructField>, HashSet<(Type, String)>, TypeTree, u64), GenCodeError> {
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

    let mut state: (Vec<_>, Rc<RefCell<HashSet<(Type, String)>>>) =
        (vec![], Rc::new(RefCell::new(HashSet::new())));

    let mut fields = vec![];

    let mut old_state = state.clone();
    for global in globals.globals {
        if global.name_id != usize::max_value() {
            let mut tipe = global.tipe;
            if let Type::Nominal(file_path, id) = tipe {
                tipe = type_tree
                    .get(&(file_path.clone(), id))
                    .cloned()
                    .unwrap_or((Type::Any, "fail1".to_string()))
                    .0;
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
        let mut new_types = HashSet::new();
        let cool_temp = state
            .1
            .borrow()
            .difference(&*old_state.1.borrow())
            .cloned()
            .collect::<Vec<_>>();
        for diff in cool_temp {
            let new_type = {
                let mut new = if let Type::Nominal(file_path, id) = diff.0 {
                    type_tree
                        .get(&(file_path.clone(), id))
                        .cloned()
                        .unwrap_or((Type::Any, "fail2".to_string()))
                } else {
                    diff.clone()
                };
                new.0
                    .recursive_apply(replace_nominal, &type_tree, &mut state);
                new
            };
            new_types.insert(new_type);
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
        "type {} = {}",
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
    mut_state: &mut (Vec<Type>, Rc<RefCell<HashSet<(Type, String)>>>),
) -> bool {
    match node {
        TypeCheckedNode::Type(tipe) => {
            if mut_state.0.iter().any(|thing| thing == *tipe) {
                let to_render = &mut *(*mut_state.1).borrow_mut();
                to_render.insert((
                    tipe.clone(),
                    if let Type::Nominal(path, id) = tipe {
                        state
                            .get(&(path.clone(), *id))
                            .map(|(_, name)| name.clone())
                            .unwrap_or(format!("Bad"))
                    } else {
                        format!("Bad")
                    },
                ));
                return false;
            }
            if let Type::Nominal(path, id) = tipe {
                mut_state.0.push(Type::Nominal(path.clone(), *id));
                let to_render = &mut *(*mut_state.1).borrow_mut();
                to_render.insert((
                    Type::Nominal(path.clone(), *id),
                    state
                        .get(&(path.clone(), *id))
                        .map(|(_, name)| name.clone())
                        .unwrap_or(format!("Bad")),
                ));
                **tipe = state
                    .get(&(path.clone(), *id))
                    .cloned()
                    .unwrap_or((Type::Any, "fail4".to_string()))
                    .0;
            }
            true
        }
        _ => true,
    }
}
