/*
* Copyright 2020, Offchain Labs, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

use std::path::Path;
use crate::run::{RuntimeEnvironment, load_from_file};
use crate::uint256::Uint256;


pub fn make_benchmarks() {
    let benchmarks: Vec<(fn(&Path)->u64, &str, &str)> = vec![
        (benchmark_boot, "boot ArbOS", "boot"),
        (benchmark_erc20_100, "100 ERC-20 deposits", "erc20_100"),
    ];

    for benchmark in benchmarks {
        let (bm_func, _description, filename) = benchmark;
        let filename = "benchmarks/".to_owned() + filename + ".aoslog";
        let _gas_used = bm_func(Path::new(&filename));

    }
}

pub fn benchmark_boot(log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let gas_used = machine.run(None);
    machine.runtime_env.recorder.to_file(log_to).unwrap();
    gas_used
}

pub fn benchmark_erc20_100(log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    for _ in 0..100 {
        machine.runtime_env.insert_erc20_deposit_message(
            Uint256::from_u64(1025),
            Uint256::from_u64(1026),
            Uint256::from_u64(1027),
            Uint256::from_u64(5000),
        );
    }

    let gas_used = machine.run(None);
    machine.runtime_env.recorder.to_file(log_to).unwrap();
    gas_used
}