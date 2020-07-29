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

use crate::evm::abi::AbiForContract;
use crate::run::{load_from_file, RuntimeEnvironment};
use crate::uint256::Uint256;
use ethers_signers::Signer;
use std::path::Path;

pub fn make_benchmarks() {
    let benchmarks: Vec<(fn(u64, &Path) -> u64, u64, &str, &str)> = vec![
        (benchmark_boot, 1, "boot ArbOS", "boot"),
        (benchmark_erc20, 100, "100 ERC-20 deposits", "erc20_100"),
        (benchmark_erc20, 1000, "1000 ERC-20 deposits", "erc20_1000"),
        (benchmark_add, 100, "100 null txs", "nulltx_100"),
        (benchmark_add, 1000, "1000 null txs", "nulltx_1000"),
        (
            benchmark_add_batched,
            100,
            "100 signed batched null txs",
            "nulltx_batch_100",
        ),
        (
            benchmark_add_batched,
            500,
            "500 signed batched null txs",
            "nulltx_batch_500",
        ),
        (
            benchmark_xcontract_batched,
            100,
            "100 signed batched xcontract call txs",
            "xcontract_batch_100",
        ),
        (
            benchmark_xcontract_batched,
            500,
            "500 signed batched xcontract call txs",
            "xcontract_batch_500",
        ),
    ];

    for benchmark in benchmarks {
        let (bm_func, iterations, description, filename) = benchmark;
        let filename = "benchmarks/".to_owned() + filename + ".aoslog";
        let gas_used = bm_func(iterations, Path::new(&filename));
        println!("ArbGas for {}: {}", description, gas_used)
    }
}

pub fn benchmark_boot(_iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let gas_used = machine.run(None);
    machine.runtime_env.recorder.to_file(log_to).unwrap();
    gas_used
}

pub fn benchmark_erc20(iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    for _ in 0..iterations {
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

pub fn benchmark_add(iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1025);
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, false);
            if let Some(contract_addr) = result {
                assert_ne!(contract_addr, Uint256::zero());
                contract
            } else {
                panic!("deploy failed");
            }
        }
        Err(e) => {
            panic!("error loading contract: {:?}", e);
        }
    };

    for _ in 0..iterations {
        let _result = contract
            .call_function(
                my_addr.clone(),
                "add",
                vec![
                    ethabi::Token::Uint(ethabi::Uint::one()),
                    ethabi::Token::Uint(ethabi::Uint::one()),
                ]
                .as_ref(),
                &mut machine,
                Uint256::zero(),
                false,
            )
            .unwrap();
    }

    machine.runtime_env.recorder.to_file(log_to).unwrap();
    machine.get_total_gas_usage().to_u64().unwrap()
}

pub fn benchmark_add_batched(iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let _my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let my_addr = Uint256::from_u64(1025);
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, false);
            if let Some(contract_addr) = result {
                assert_ne!(contract_addr, Uint256::zero());
                contract
            } else {
                panic!("deploy failed");
            }
        }
        Err(e) => {
            panic!("error loading contract: {:?}", e);
        }
    };

    let mut batch = machine.runtime_env.new_batch();

    for _ in 0..iterations {
        let _tx_id = contract
            .add_function_call_to_batch(
                &mut batch,
                my_addr.clone(),
                "add",
                vec![
                    ethabi::Token::Uint(ethabi::Uint::one()),
                    ethabi::Token::Uint(ethabi::Uint::one()),
                ]
                .as_ref(),
                &mut machine,
                Uint256::zero(),
                &wallet,
            )
            .unwrap();
    }

    machine
        .runtime_env
        .insert_batch_message(Uint256::from_usize(1025), &batch);

    machine.run(None);

    machine.runtime_env.recorder.to_file(log_to).unwrap();
    machine.get_total_gas_usage().to_u64().unwrap()
}

pub fn benchmark_xcontract_batched(iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let wallet = rt_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(1000000),
    );
    let _ = machine.run(None); // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")
            .unwrap();
    if fib_contract.deploy(&[], &mut machine, false) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")
            .unwrap();
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        false,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let mut batch = machine.runtime_env.new_batch();

    for _ in 0..iterations {
        let _tx_id = pc_contract
            .add_function_call_to_batch(
                &mut batch,
                my_addr.clone(),
                "deposit",
                &[],
                &mut machine,
                Uint256::from_usize(1),
                &wallet,
            )
            .unwrap();
    }

    machine
        .runtime_env
        .insert_batch_message(Uint256::from_usize(1025), &batch);

    machine.run(None);

    machine.runtime_env.recorder.to_file(log_to).unwrap();
    machine.get_total_gas_usage().to_u64().unwrap()
}
