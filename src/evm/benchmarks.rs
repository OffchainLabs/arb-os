/*
* Copyright 2020, Offchain Labs, Inc. All rights reserved.
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
    ];

    for benchmark in benchmarks {
        let (bm_func, iterations, description, filename) = benchmark;
        let filename = "benchmarks/".to_owned() + filename + ".aoslog";
        let gas_used = bm_func(iterations, Path::new(&filename));
        println!("ArbGas for {}: {}", description, gas_used)
    }
}

pub fn benchmark_boot(_iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let gas_used = machine.run(None);
    machine.runtime_env.recorder.to_file(log_to).unwrap();
    gas_used
}

pub fn benchmark_erc20(iterations: u64, log_to: &Path) -> u64 {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1025);
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), false, false);
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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let _my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let my_addr = Uint256::from_u64(1025);
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), false, false);
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
