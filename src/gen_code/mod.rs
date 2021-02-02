use crate::compile::{StructField, Type};
use crate::link::LinkedProgram;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub(crate) fn gen_upgrade_code(path: &Path) -> Result<(), ()> {
    let mut file = File::open(&path).map_err(|e| ())?;
    let mut s = String::new();
    file.read_to_string(&mut s).map_err(|e| ())?;
    let prog: LinkedProgram = serde_json::from_str(&s).map_err(|e| ())?;
    let mut fields = vec![];
    for global in prog.globals {
        if global.name_id != usize::max_value() {
            fields.push(StructField::new(
                global.name,
                if let Type::Nominal(_, _) = global.tipe {
                    Type::Any
                } else {
                    global.tipe
                },
            ))
        }
    }
    let globals_struct = Type::Struct(fields);
    println!("{}", globals_struct.display());
    Ok(())
}
