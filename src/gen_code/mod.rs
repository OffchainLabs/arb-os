use crate::compile::{AbstractSyntaxTree, StructField, Type, TypeCheckedNode};
use crate::link::LinkedProgram;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub(crate) fn gen_upgrade_code(path: &Path) -> Result<(), ()> {
    let mut file = File::open(&path).map_err(|_| ())?;
    let mut s = String::new();
    file.read_to_string(&mut s).map_err(|_| ())?;
    let prog: LinkedProgram = serde_json::from_str(&s).map_err(|_| ())?;
    let mut fields = vec![];
    for global in prog.globals {
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
    let globals_struct = Type::Struct(fields);
    println!("{}", globals_struct.display());
    Ok(())
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
