use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::env;
use crate::emulator::CompiledProgram;

pub mod ast;
pub mod typecheck;
pub mod symtable;
pub mod stringtable;
pub mod mavm;
pub mod uint256;
pub mod codegen;
pub mod striplabels;
pub mod xformcode;
pub mod optimize;
pub mod emulator;

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub mini); 


const DEBUG: bool = false;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("usage: cargo run [program.mini]");
        return;
    }

    let path = Path::new(&args[1]);
    
    match compile_from_file(path) {
        Ok(compiled_program) => {
            match serde_json::to_string(&compiled_program) {
                Ok(prog_str) => {
                    println!("jsonified program: {}", prog_str);
                }
                Err(e) => {
                    println!("json serialization error: {:?}", e);
                }
            } 
        }
        Err(e) => {
            println!("Compilation error: {:?}", e);
        }
    }
}

pub fn compile_from_file<'a>(path: &Path) -> Result<CompiledProgram, CompileError<'a>> {
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

    compile(s)
}

pub fn compile<'a>(s: String) -> Result<CompiledProgram, CompileError<'a>> {
    let mut string_table = stringtable::StringTable::new();
    let res = mini::DeclsParser::new()
    	.parse(&mut string_table, &s)
    	.unwrap();
    let mut checked_funcs = Vec::new();
    let res2 = crate::typecheck::typecheck_top_level_decls(&res, &mut checked_funcs);
    match res2 {
    	Some(res3) => Err(CompileError::new(res3.reason)),
    	None => { 
            let mut code = Vec::new();
    		match crate::codegen::mavm_codegen(checked_funcs, &mut code, &string_table) {
                Ok(code_out) => {
                    if DEBUG {
                        println!("========== after initial codegen ===========");
                        for (idx, insn) in code_out.iter().enumerate() {
                         println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let (code_2, jump_table) = crate::striplabels::fix_backward_labels(code_out);
                    if DEBUG {
                        println!("========== after fix_backward_labels ===========");
                        for (idx, insn) in code_2.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let code_3 = crate::xformcode::fix_tuple_size(&code_2);
                    if DEBUG {
                        println!("=========== after fix_tuple_size ==============");
                        for (idx, insn) in code_3.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let code_4 = crate::optimize::peephole(&code_3);
                    if DEBUG {
                        println!("============ after peephole optimization ===========");
                        for (idx, insn) in code_4.iter().enumerate() {
                            println!("{:04}:  {}", idx, insn);
                        }
                    }
                    let (code_final, jump_table_final) = crate::striplabels::strip_labels(&code_4, &jump_table);
                    if DEBUG {
                        println!("============ after strip_labels =============");
                    }
                    let jump_table_value = crate::xformcode::jump_table_to_value(jump_table_final);
                    println!("static: {}", jump_table_value);
                    for (idx, insn) in code_final.iter().enumerate() {
                        println!("{:04}:  {}", idx, insn);
                    }

                    Ok(CompiledProgram{ code: code_final, static_val: jump_table_value })
                },
                Err(e) => Err(CompileError::new(e.reason)),
            }
    	},
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

