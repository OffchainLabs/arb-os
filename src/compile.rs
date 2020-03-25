use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::stringtable;
use crate::emulator::CompiledProgram;

lalrpop_mod!(pub mini); 


pub fn compile_from_file<'a>(path: &Path, debug: bool) -> Result<CompiledProgram, CompileError<'a>> {
   let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {:?}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    s = match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {:?}", display, why),
        Ok(_) => s,
    };

    compile(s, debug)
}

pub fn compile<'a>(s: String, debug: bool) -> Result<CompiledProgram, CompileError<'a>> {
    let mut string_table = stringtable::StringTable::new();
    let res = mini::DeclsParser::new()
    	.parse(&mut string_table, &s)
    	.unwrap();
    let mut checked_funcs = Vec::new();
    let res2 = crate::typecheck::typecheck_top_level_decls(&res, &mut checked_funcs, &string_table);
    match res2 {
    	Ok((exported_funcs, imported_funcs)) => { 
            let mut code = Vec::new();
    		match crate::codegen::mavm_codegen(checked_funcs, &mut code, &string_table) {
                Ok(code_out) => {
                    if debug {
                        println!("========== after initial codegen ===========");
                        for (idx, insn) in code_out.iter().enumerate() {
                         println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let (code_2, jump_table) = crate::striplabels::fix_nonforward_labels(code_out, &imported_funcs);
                    if debug {
                        println!("========== after fix_backward_labels ===========");
                        for (idx, insn) in code_2.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let code_3 = crate::xformcode::fix_tuple_size(&code_2);
                    if debug {
                        println!("=========== after fix_tuple_size ==============");
                        for (idx, insn) in code_3.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let code_4 = crate::optimize::peephole(&code_3);
                    if debug {
                        println!("============ after peephole optimization ===========");
                        for (idx, insn) in code_4.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let (code_final, 
                        jump_table_final, 
                        exported_funcs_final) = crate::striplabels::strip_labels(
                            &code_4, 
                            &jump_table, 
                            &exported_funcs,
                            &imported_funcs,
                        );
                    let jump_table_value = crate::xformcode::jump_table_to_value(jump_table_final);

                    if debug {
                        println!("============ after strip_labels =============");
                        println!("static: {}", jump_table_value);
                        for (idx, insn) in code_final.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }

                    Ok(CompiledProgram{ 
                        code: code_final, 
                        static_val: jump_table_value, 
                        exported_funcs: exported_funcs_final,
                        imported_funcs,
                    })
                },
                Err(e) => Err(CompileError::new(e.reason)),
            }
    	},
        Err(res3) => Err(CompileError::new(res3.reason)),
    }
}

#[derive(Debug)]
pub struct CompileError<'a> {
    description: &'a str,
}

impl<'a> CompileError<'a> {
    fn new(description: &'a str) -> Self {
        CompileError{ description }
    }
}
