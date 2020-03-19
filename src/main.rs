use std::error::Error;
use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::env;

pub mod ast;
pub mod typecheck;
pub mod symtable;
pub mod stringtable;
pub mod mavm;
pub mod codegen;
pub mod striplabels;
pub mod xformcode;
pub mod optimize;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        print!("usage: cargo run [program.mini]\n");
        return;
    }

    let path = Path::new(&args[1]);
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display,
                                                   why.description()),
        Ok(file) => file,
    };

    let mut s = String::new();
    s = match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {}", display,
                                                   why.description()),
        Ok(_) => s,
    };
    
    let mut string_table = stringtable::StringTable::new();
    let res = mini::DeclsParser::new()
    	.parse(&mut string_table, &s)
    	.unwrap();
    let mut checked_funcs = Vec::new();
    let res2 = crate::typecheck::typecheck_top_level_decls(&res, &mut checked_funcs);
    match res2 {
    	Some(res3) => { print!("type error: {:?}\n", res3); }
    	None => { 
            let mut code = Vec::new();
    		match crate::codegen::mavm_codegen(checked_funcs, &mut code, &string_table) {
                Ok(code_out) => {
                    print!("========== after initial codegen ===========\n");
                    for (idx, insn) in code_out.iter().enumerate() {
                        print!("{:04}:  {}\n", idx, insn);
                    }
                    let (code_2, jump_table) = crate::striplabels::fix_backward_labels(code_out);
                    print!("========== after fix_backward_labels ===========\n");
                    for (idx, insn) in code_2.iter().enumerate() {
                        print!("{:04}:  {}\n", idx, insn);
                    }
                    let code_3 = crate::xformcode::fix_tuple_size(&code_2);
                    print!("=========== after fix_tuple_size ==============\n");
                    for (idx, insn) in code_3.iter().enumerate() {
                        print!("{:04}:  {}\n", idx, insn);
                    }
                    let code_4 = crate::optimize::peephole(&code_3);
                    print!("============ after peephole optimization ===========\n");
                    for (idx, insn) in code_4.iter().enumerate() {
                        print!("{:04}:  {}\n", idx, insn);
                    }
                    let (code_final, jump_table_final) = crate::striplabels::strip_labels(&code_4, &jump_table);
                    print!("============ after strip_labels =============\n");
                    let jump_table_value = crate::xformcode::jump_table_to_value(jump_table_final);
                    print!("static: {}\n", jump_table_value);
                    for (idx, insn) in code_final.iter().enumerate() {
                        print!("{:04}:  {}\n", idx, insn);
                    }
                }
                Err(e) => { print!("Error generating code: {:?}\n", e); }
            }
    	},
    }
}

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub mini); 

#[test]
fn test_mini() {
	let res = mini::DeclsParser::new()
		.parse("struct foo { uint bar, struct baz quux, }")
		.unwrap();
	let res2 = typecheck_top_level_decls(res).unwrap();
}
