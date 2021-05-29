use crate::compile::CompileStruct;
use crate::mavm::Value;
use crate::run::{run, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use std::collections::BTreeSet;
use std::sync::Arc;

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
        Some(&Value::Tuple(Arc::new(vec![
            Value::Int(Uint256::zero()),
            Value::Int(Uint256::from_u64(25))
        ])))
    );
}

#[test]
fn test_warnings() {
    fn check_warnings(file_path: &str, sources: BTreeSet<String>, correct: &[&[usize]]) {
        let mut compile = CompileStruct::default();
        compile.input = vec![file_path.to_string()];
        compile.warnings_are_errors = true;

        let (mut warnings, file_info_chart) = match compile.invoke() {
            Ok(_) => panic!("No compile error was emitted despite the -w flag."),
            Err((_, warnings, file_info_chart)) => (warnings, file_info_chart),
        };

        warnings.sort_by(|a, b| {
            a.locations
                .last()
                .unwrap()
                .line
                .to_usize()
                .cmp(&b.locations.last().unwrap().line.to_usize())
        });

        let warnings: Vec<Vec<usize>> = warnings
            .into_iter()
            .map(|warning| {
                warning
                    .locations
                    .into_iter()
                    .filter(|loc| {
                        sources.contains(&file_info_chart.get(&loc.file_id).unwrap().name)
                    })
                    .map(|loc| loc.line.to_usize() + 1)
                    .collect::<Vec<usize>>()
            })
            .filter(|vec| !vec.is_empty())
            .collect();

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

    // check single-file warnings
    check_warnings(
        "minitests/warn-when-unused.mini",
        vec!["warn-when-unused".to_string()].into_iter().collect(),
        &[
            &[8, 9],
            &[10, 11],
            &[16],
            &[17],
            &[18],
            &[30],
            &[43],
            &[48],
            &[48],
            &[50],
            &[58],
            &[59],
            &[63],
            &[67],
            &[69],
            &[70],
            &[73],
            &[73],
            &[84],
            &[86],
            &[91],
            &[93],
            &[81, 83, 97],
            &[105],
            &[112],
            &[99, 101, 115],
            &[118],
            &[123],
            &[131],
            &[143],
            &[148],
            &[151],
            &[155],
            &[175],
            &[180],
            &[185],
            &[185],
            &[188],
            &[189],
            &[191],
            &[192],
        ],
    );

    // check directory callgraph warnings
    check_warnings(
        "minitests/callgraph",
        vec!["main".to_string(), "other".to_string()]
            .into_iter()
            .collect(),
        &[
            &[15],
            &[17], // special warning from other.mini
            &[19],
            &[20],
            &[21],
            &[26],
        ],
    );
}
