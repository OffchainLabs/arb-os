/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::evm::abi::ArbosTest;
use crate::run::{load_from_file, RuntimeEnvironment};
use crate::uint256::Uint256;
use std::collections::{HashMap, HashSet};
use std::io;
use std::path::Path;

pub fn run_evm_tests(dir_path: &Path, logfiles_path: Option<&Path>) -> io::Result<()> {
    let mut num_success = 0;
    let mut num_fail = 0;
    for entry in dir_path.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            run_evm_tests(&path, logfiles_path)?;
        } else {
            let contents = std::fs::read_to_string(path.clone())
                .expect("Something went wrong reading the file");
            let json: serde_json::Value =
                serde_json::from_str(&contents).expect("JSON was not well-formatted");
            let success = run_one_test(json, &path, logfiles_path, path.to_str().unwrap());
            println!(
                "{} {}",
                if success { "..ok" } else { "FAIL" },
                path.to_str().unwrap()
            );
            if success {
                num_success = num_success + 1;
            } else {
                num_fail = num_fail + 1;
            }
        }
    }
    println!("{} successes, {} failures", num_success, num_fail);
    Ok(())
}

fn run_one_test(
    json: serde_json::Value,
    _path: &Path,
    logfiles_path: Option<&Path>,
    raw_filename: &str,
) -> bool {
    if let serde_json::Value::Object(omap) = json {
        for (_, v) in omap {
            let addr_str = &v["exec"]["address"].to_string();
            let addr_str = &addr_str[1..(addr_str.len() - 1)];
            let mut code = bytevec_from_jval(&v["pre"][addr_str]["code"].to_string());
            code.extend(&[0u8]);
            let data = bytevec_from_jval(&v["exec"]["data"].to_string());
            let pre_storage = storage_from_jval(v["pre"][addr_str]["storage"].clone());
            let storage_actual = run_code(&code, &data, pre_storage, logfiles_path, raw_filename);

            if v["post"] == serde_json::Value::Null {
                return storage_actual.len() == 0;
            } else {
                let storage_expected = storage_from_jval(v["post"][addr_str]["storage"].clone());
                return compare_storage(&storage_expected, &storage_actual);
            }
        }
    }

    false
}

fn bytevec_from_jval(s: &str) -> Vec<u8> {
    hex::decode(&s[3..(s.len() - 1)]).unwrap()
}

fn uint256_from_jval(s: &str) -> Uint256 {
    Uint256::from_bytes(&bytevec_from_jval(s))
}

fn storage_from_jval(jval: serde_json::Value) -> HashMap<Uint256, Uint256> {
    let mut ret = HashMap::new();
    if jval != serde_json::Value::Null {
        if let serde_json::Value::Object(omap) = jval {
            for (k, v) in omap {
                let k_ui = Uint256::from_bytes(&hex::decode(&k[2..]).unwrap());
                let v_ui = uint256_from_jval(&v.to_string());
                ret.insert(k_ui, v_ui);
            }
        } else {
            println!("{}", jval);
            panic!("can't parse incoming storage");
        }
    }
    ret
}

fn run_code(
    code: &[u8],
    data: &[u8],
    _pre_storage: HashMap<Uint256, Uint256>,
    logfiles_path: Option<&Path>,
    raw_filename: &str,
) -> HashMap<Uint256, Uint256> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let arbos_test = ArbosTest::new(false);

    let result = arbos_test
        .run(&mut machine, code.to_vec(), data.to_vec(), vec![])   //TODO: serialize pre_storage
        .unwrap();

    if let Some(logs_path) = logfiles_path {
        let logfile_name = raw_filename.replace("/", "_").replace("_json", ".aoslog");
        let this_log_path = [logs_path.to_str().unwrap(), &logfile_name].concat();
        machine
            .runtime_env
            .recorder
            .to_file(Path::new(&this_log_path))
            .unwrap();
    }

    let mut ret = HashMap::new();
    let mut offset = 0;
    while offset < result.len() {
        let k = Uint256::from_bytes(&result[offset..offset + 32]);
        let v = Uint256::from_bytes(&result[offset + 32..offset + 64]);
        ret.insert(k, v);
        offset = offset + 64;
    }
    ret
}

fn compare_storage(
    expected: &HashMap<Uint256, Uint256>,
    actual: &HashMap<Uint256, Uint256>,
) -> bool {
    let zero = Uint256::zero();
    for (k, v) in expected {
        if !v.is_zero() {
            if actual.get(k) != Some(v) {
                println!(
                    "storage mismatch at {}: expected {}, got {}",
                    k,
                    v,
                    if let Some(vv) = actual.get(k) {
                        vv
                    } else {
                        &zero
                    }
                );
                return false;
            }
        }
    }
    for (k, v) in actual {
        if !v.is_zero() {
            if expected.get(k) != Some(v) {
                println!(
                    "storage mismatch at {}: expected {}, got {}",
                    k,
                    if let Some(vv) = expected.get(k) {
                        vv
                    } else {
                        &zero
                    },
                    v
                );
                return false;
            }
        }
    }

    true
}

fn _compare_storage_save(expected: &serde_json::Value, actual: HashMap<Uint256, Uint256>) -> bool {
    if let serde_json::Value::Object(map) = expected {
        let mut all_ks = HashSet::new();
        for (k, v) in map {
            let k = hex::decode(&k[2..]).unwrap();
            let k_ui = Uint256::from_bytes(&k);
            all_ks.insert(k_ui.clone());
            if let serde_json::Value::String(vs) = v {
                let v = hex::decode(&vs[2..]).unwrap();
                if actual.get(&k_ui).unwrap_or(&Uint256::zero()) != &Uint256::from_bytes(&v) {
                    if let Some(act) = actual.get(&Uint256::from_bytes(&k)) {
                        println!(
                            "-- storage[{}] = {}, should be {}",
                            k_ui,
                            act,
                            Uint256::from_bytes(&v)
                        );
                    } else {
                        println!(
                            "-- storage[{}] = 0, should be {}",
                            k_ui,
                            Uint256::from_bytes(&v)
                        );
                    }
                    return false;
                }
            } else {
                println!("fail2");
                return false;
            }
        }
        for (k, v) in actual {
            if (!v.is_zero()) && (!all_ks.contains(&k)) {
                println!("-- storage[{}] = {}, should be 0", k, v);
                return false;
            }
        }
        true
    } else {
        println!("fail3");
        false
    }
}
