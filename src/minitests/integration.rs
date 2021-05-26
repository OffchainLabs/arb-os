use crate::compile::CompileStruct;
use crate::mavm::Value;
use crate::run::{run, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use std::rc::Rc;

fn compile_run_cycle(input: String) -> Machine {
    let mut compile = CompileStruct::default();
    compile.input = vec![input];
    compile.test_mode = true;
    compile.consts_file = Some(format!("arb_os/constants.json"));
    let mexe = compile.invoke().unwrap();
    let mut machine = Machine::new(
        mexe,
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
    );
    run(&mut machine, vec![], false).unwrap();
    machine
}

#[test]
fn test_basic() {
    let machine = compile_run_cycle("test-programs/basic.mini".to_string());
    assert_eq!(machine.stack_top(), None);
}

#[test]
fn test_xif_else() {
    let machine = compile_run_cycle("test-programs/xif-else.mini".to_string());
    assert_eq!(machine.stack_top(), Some(&Value::Int(Uint256::zero())));
}

#[test]
fn test_codeblocks() {
    let machine = compile_run_cycle("test-programs/codeblocks.mini".to_string());
    assert_eq!(
        machine.stack_top(),
        Some(&Value::Tuple(Rc::new(vec![
            Value::Int(Uint256::zero()),
            Value::Int(Uint256::from_u64(25))
        ])))
    );
}

#[test]
fn test_warnings() {
    let mut compile = CompileStruct::default();
    compile.input = vec!["minitests/warn-when-unused.mini".to_string()];
    compile.warnings_are_errors = true;
    
    let (mut warnings, file_info_chart) = match compile.invoke() {
        Ok(_) => panic!("No compile error was emitted despite the -w flag."),
        Err((_, warnings, file_info_chart)) => (warnings, file_info_chart),
    };
    
    warnings.sort_by(
        |a, b|
        a.locations.last().unwrap().line.to_usize().cmp(&b.locations.last().unwrap().line.to_usize())
    );
    
    let warnings: Vec<Vec<usize>> = warnings.into_iter()
        .map(|warning| warning.locations.into_iter()
             .filter(|loc| file_info_chart.get(&loc.file_id).unwrap().name == "warn-when-unused")
             .map(|loc| loc.line.to_usize() + 1)
             .collect::<Vec<usize>>()
        )
        .filter(|vec| !vec.is_empty())
        .collect();
    
    let correct: &[&[usize]] = &[
        &[8, 9], &[10, 11], &[16], &[17], &[18], &[30], &[41], &[46], &[46], &[48], &[56], &[57], &[61],
        &[65], &[67], &[68], &[71], &[71], &[82], &[84], &[89], &[91], &[79, 81, 95], &[103], &[110],
        &[97, 99, 113], &[116], &[121], &[129], &[141], &[146], &[149], &[153], &[173], &[178], 
        &[183], &[183]
    ];
    
    let pairs = warnings.iter().zip(correct.iter());
    
    for (warning_lines, correct_lines) in pairs {
        
        let mut found_issue = warning_lines.len() != correct_lines.len();
        
        for (warning_line, correct_line) in warning_lines.iter().zip(correct_lines.iter()) {
            found_issue = found_issue || warning_line != correct_line;
        }
        
        if found_issue {
            let last_warning = warning_lines.last().unwrap();
            let last_correct = correct_lines.last().unwrap();
            
            if last_warning < last_correct {
                panic!("Unexpected warning on line {}", last_warning);
            } else if last_warning > last_correct {
                panic!("Missing warning on line {}", last_correct);
            } else {
                panic!("Warning mismatch on line {}", last_correct);
            }
        }
    }
    
    if warnings.len() < correct.len() {
        panic!("Too few warnings");
    } else if warnings.len() > correct.len() {
        panic!("Too many warnings");
    }
}
