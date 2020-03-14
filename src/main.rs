use std::error::Error;
use std::fs::File;
use std::path::Path;
use std::io::Read;

pub mod ast;
pub mod typecheck;
pub mod symtable;
pub mod stringtable;
pub mod mavm;
pub mod codegen;

fn main() {
    // Create a path to the desired file
    let path = Path::new("foo.mini");
    let display = path.display();

    // Open the path in read-only mode, returns `io::Result<File>`
    let mut file = match File::open(&path) {
        // The `description` method of `io::Error` returns a string that
        // describes the error
        Err(why) => panic!("couldn't open {}: {}", display,
                                                   why.description()),
        Ok(file) => file,
    };

    // Read the file contents into a string, returns `io::Result<usize>`
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
    		let code_out = crate::codegen::mavm_codegen(checked_funcs, &mut code, &string_table);
            print!("{:#?}\n", code_out);
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
