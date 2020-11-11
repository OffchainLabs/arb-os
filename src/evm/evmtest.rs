/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use std::collections::HashMap;
use std::io;
use std::path::Path;
use crate::uint256::Uint256;
use crate::evm::abi::_ArbosTest;
use crate::run::{load_from_file, RuntimeEnvironment};


pub fn run_evm_tests(dir_path: &Path) -> io::Result<()> {
    let mut num_success = 0;
    let mut num_fail = 0;
    for entry in dir_path.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            run_evm_tests(&path)?;
        } else {
            let contents = std::fs::read_to_string(path.clone())
                .expect("Something went wrong reading the file");
            let json: serde_json::Value = serde_json::from_str(&contents).expect("JSON was not well-formatted");
            let success = run_one_test(json, &path);
            println!("{} {}", if success { "..ok"} else { "FAIL"}, path.to_str().unwrap());
            if success {
                num_success = num_success+1;
            } else {
                num_fail = num_fail+1;
            }
        }
    }
    println!("{} successes, {} failures", num_success, num_fail);
    Ok(())
}

fn run_one_test(json: serde_json::Value, path: &Path) -> bool {
    if let serde_json::Value::Object(omap) = json {
        for (_, v) in omap {
            let post: &serde_json::Value = &v["post"];
            if let serde_json::Value::Object(postmap) = post {
                for(_, spec) in postmap {
                    let code_str: String = spec["code"].to_string();
                    let code_str = &code_str[3..(code_str.len()-1)];
                    let mut code = hex::decode(code_str.clone()).unwrap();
                    code.extend(&[0u8]);  // append final STOP instruction, which is missing in some test cases
                    let storage_expected = &spec["storage"];
                    let storage_actual = run_code(&code);
                    return compare_storage(storage_expected, storage_actual);
                }
            }
        }
    }

    false
}

fn run_code(code: &[u8]) -> HashMap<Uint256, Uint256> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();

    let arbos_test = _ArbosTest::_new(&wallet, false);

    let result = arbos_test._run(&mut machine, code.to_vec()).unwrap();

    let mut ret = HashMap::new();
    let mut offset = 0;
    while offset < result.len() {
        let k = Uint256::from_bytes(&result[offset..offset+32]);
        let v = Uint256::from_bytes(&result[offset+32..offset+64]);
        ret.insert(k, v);
        offset = offset + 64;
    }
    ret
}

fn compare_storage(expected: &serde_json::Value, actual: HashMap<Uint256, Uint256>) -> bool {
    if let serde_json::Value::Object(map) = expected {
        if map.len() != actual.len() {
            return false;
        }
        for (k, v) in map {
            let k = hex::decode(&k[2..]).unwrap();
            if let serde_json::Value::String(vs) = v {
                let v = hex::decode(&vs[2..]).unwrap();
                if actual.get(&Uint256::from_bytes(&k)) != Some(&Uint256::from_bytes(&v)) {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    } else {
        false
    }
}