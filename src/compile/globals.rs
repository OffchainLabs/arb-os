/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

use crate::compile::{CompileError, TypeCheckedModule};
use crate::console::Color;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

/// Assign an ordering to the global variables
pub fn order_globals(
    globals_path: Option<&Path>,
    modules: &mut Vec<TypeCheckedModule>,
) -> Result<(), CompileError> {
    let mut global_count = 0;
    for module in modules.iter_mut() {
        // naively assign offsets one-by-one
        for global in &mut module.global_vars {
            global.offset = Some(global_count);
            global_count += 1;
        }
    }

    let mut globals: HashMap<_, _> = modules
        .iter_mut()
        .map(|module| module.global_vars.iter_mut())
        .flatten()
        .map(|global| (global.name.to_owned(), global))
        .collect();

    macro_rules! error {
        (@$text:expr $(,$args:expr)* $(,)?) => {
            CompileError::new("globals file error", format!($text, $(Color::red($args),)*), vec![])
        };
        ($text:expr $(,$args:expr)* $(,)?) => {
            return Err(CompileError::new("globals file error", format!($text, $(Color::red($args),)*), vec![]))
        };
    }

    if let Some(path) = globals_path {
        // Copy the order of the globals in the file provided

        let filename = path.display();

        let file = File::open(path).map_err(|_| error!(@"failed to open {}", &filename))?;
        let lines: Vec<_> = BufReader::new(file).lines().enumerate().collect();

        if lines.len() != global_count {
            error!(
                "{} has {} globals but {} exist",
                filename,
                lines.len(),
                global_count
            );
        }

        for (index, line) in lines {
            let line = line.map_err(|err| error!(@"failed to read line {}", err))?;
            match globals.get_mut(&line) {
                Some(global) => global.offset = Some(index),
                None => error!("{}'s global {} doesn't exist", filename, line),
            }
        }
    }
    Ok(())
}
