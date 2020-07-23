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

mod runtime_env;
use crate::mavm::Value;
use crate::run::{
    bytestack_from_bytes, load_from_file, ArbosReceipt, RuntimeEnvironment, TxBatch, TxBatchType,
};
use crate::uint256::Uint256;
use abi::AbiForContract;
use ethabi::Function;
use ethers_signers::Signer;
use std::path::Path;

mod abi;

#[derive(Clone)]
pub struct CallInfo<'a> {
    function_name: &'a str,
    args: &'a [ethabi::Token],
    payment: Uint256,
    mutating: bool,
}

pub fn evm_xcontract_call_with_constructors(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_usize(1025);
    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")?;
    if fib_contract.deploy(&[], &mut machine, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let (logs, sends) = pc_contract.call_function(
        my_addr.clone(),
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    let (logs, sends) = pc_contract.call_function(
        my_addr,
        "transferFib",
        vec![
            ethabi::Token::Address(ethabi::Address::from_low_u64_be(1025)),
            ethabi::Token::Uint(ethabi::Uint::try_from(1).unwrap()),
        ]
        .as_ref(),
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(true)
}

pub fn evm_test_create(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_usize(1025);
    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")?;
    if fib_contract.deploy(&[], &mut machine, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let (logs, sends) = pc_contract.call_function(
        my_addr.clone(),
        "testCreate",
        &[],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(true)
}

pub fn evm_xcontract_call_using_batch(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);

    let wallet = rt_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")?;
    if fib_contract.deploy(&[], &mut machine, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let mut batch = TxBatch::new(
        TxBatchType::AggregatorBatch(Uint256::from_usize(1025)),
        &machine.runtime_env,
    );
    let tx_id_1 = pc_contract.add_function_call_to_batch(
        &mut machine.runtime_env,
        &mut batch,
        my_addr.clone(),
        "deposit",
        &[],
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract.add_function_call_to_batch(
        &mut machine.runtime_env,
        &mut batch,
        my_addr.clone(),
        "transferFib",
        vec![
            ethabi::Token::Address(ethereum_types::H160::from_slice(
                &my_addr.to_bytes_minimal(),
            )),
            ethabi::Token::Uint(ethabi::Uint::try_from(1).unwrap()),
        ]
        .as_ref(),
        Uint256::zero(),
        &wallet,
    )?;

    batch.send(&mut machine.runtime_env);
    //machine
    //    .runtime_env
    //    .insert_batch_message(Uint256::from_usize(1025), &batch);

    let num_logs_before = machine.runtime_env.get_all_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_logs();
    let sends = machine.runtime_env.get_all_sends();
    let logs = &logs[num_logs_before..];
    let sends = &sends[num_sends_before..];

    assert_eq!(logs.len(), 2);
    assert_eq!(sends.len(), 0);

    assert!(logs[0].succeeded());
    assert_eq!(logs[0].get_request_id(), tx_id_1);
    let gas_used_so_far_1 = logs[0].get_gas_used_so_far();

    assert!(logs[1].succeeded());
    assert_eq!(logs[1].get_request_id(), tx_id_2);
    assert_eq!(
        gas_used_so_far_1.add(&logs[1].get_gas_used()),
        logs[1].get_gas_used_so_far()
    );

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(true)
}

pub fn evm_direct_deploy_add(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, debug);
            if let Some(contract_addr) = result {
                assert_ne!(contract_addr, Uint256::zero());
            } else {
                panic!("deploy failed");
            }
        }
        Err(e) => {
            panic!("error loading contract: {:?}", e);
        }
    }

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn evm_test_arbsys(log_to: Option<&Path>, debug: bool) {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_usize(1025);
    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(10000),
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&vec![], &mut machine, debug);
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
    let result = contract.call_function(
        my_addr.clone(),
        "getSeqNum",
        vec![].as_ref(),
        &mut machine,
        Uint256::zero(),
        debug,
    );
    match result {
        Ok((logs, sends)) => {
            assert_eq!(logs.len(), 1);
            assert_eq!(sends.len(), 0);
            assert!(logs[0].succeeded());
            let decoded_result = contract
                .get_function("getSeqNum")
                .unwrap()
                .decode_output(&logs[0].get_return_data())
                .unwrap();
            assert_eq!(
                decoded_result[0],
                ethabi::Token::Uint(ethabi::Uint::try_from(2).unwrap())
            );
        }
        Err(e) => {
            panic!(e.to_string());
        }
    }

    let result = contract.call_function(
        my_addr.clone(),
        "withdrawMyEth",
        vec![].as_ref(),
        &mut machine,
        Uint256::from_usize(5000),
        debug,
    );
    match result {
        Ok((logs, sends)) => {
            assert_eq!(logs.len(), 1);
            assert!(logs[0].succeeded());
            assert_eq!(sends.len(), 1);
            let mut expected_bytes = my_addr.to_bytes_be();
            expected_bytes.extend(Uint256::from_usize(5000).to_bytes_be());
            assert_eq!(
                sends[0],
                Value::new_tuple(vec![
                    Value::Int(Uint256::zero()),
                    Value::Int(contract.address),
                    bytestack_from_bytes(&expected_bytes),
                ]),
            )
        }
        Err(e) => {
            panic!(e.to_string());
        }
    }

    if let Some(path) = log_to {
        let _ = machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn evm_direct_deploy_and_call_add(log_to: Option<&Path>, debug: bool) {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_usize(1025);
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, debug);
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

    let result = contract.call_function(
        my_addr,
        "add",
        vec![
            ethabi::Token::Uint(ethabi::Uint::one()),
            ethabi::Token::Uint(ethabi::Uint::one()),
        ]
        .as_ref(),
        &mut machine,
        Uint256::zero(),
        debug,
    );
    match result {
        Ok((logs, sends)) => {
            assert_eq!(logs.len(), 1);
            assert_eq!(sends.len(), 0);
            assert!(logs[0].succeeded());
            let decoded_result = contract
                .get_function("add")
                .unwrap()
                .decode_output(&logs[0].get_return_data())
                .unwrap();
            assert_eq!(
                decoded_result[0],
                ethabi::Token::Uint(ethabi::Uint::try_from(2).unwrap())
            );
        }
        Err(e) => {
            panic!(e.to_string());
        }
    }

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn evm_test_sequencer_support(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), Some(10));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();
    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, debug);
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

    let wallet = machine.runtime_env.new_wallet();

    machine.runtime_env.set_blocknum_timestamp(3, None);
    let mut batch = TxBatch::new(
        TxBatchType::AggregatorBatch(Uint256::from_usize(1025)),
        &machine.runtime_env,
    );
    let tx_id_1 = contract.add_function_call_to_batch(
        &mut machine.runtime_env,
        &mut batch,
        Uint256::from_bytes(wallet.address().as_bytes()),
        "currentBlockNum",
        &[],
        Uint256::zero(),
        &wallet,
    )?;
    batch.send(&mut machine.runtime_env);

    machine.runtime_env.add_to_blocknum(11);
    let mut batch = TxBatch::new(TxBatchType::SequencerBatch, &machine.runtime_env);
    let tx_id_2 = contract.add_function_call_to_batch(
        &mut machine.runtime_env,
        &mut batch,
        Uint256::from_bytes(wallet.address().as_bytes()),
        "currentBlockNum",
        &[],
        Uint256::zero(),
        &wallet,
    )?;
    batch.send(&mut machine.runtime_env);

    // run the machine, and collect logs and sends
    let num_logs_before = machine.runtime_env.get_all_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_logs();
    let sends = machine.runtime_env.get_all_sends();
    let logs = &logs[num_logs_before..];
    let sends = &sends[num_sends_before..];

    assert_eq!(logs.len(), 2);
    assert_eq!(sends.len(), 0);

    assert_log_result(
        &logs[0],
        None,
        Some(tx_id_1),
        Some((
            contract.get_function("currentBlockNum")?,
            &Uint256::from_u64(14),
        )),
    );
    assert_log_result(
        &logs[1],
        None,
        Some(tx_id_2),
        Some((
            contract.get_function("currentBlockNum")?,
            &Uint256::from_u64(14),
        )),
    );

    // now try some
    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(())
}

pub fn assert_log_result(
    log_item: &ArbosReceipt,
    return_code: Option<u64>,    // None means to assert success (0)
    request_id: Option<Uint256>, // None means don't check
    first_return_val: Option<(&Function, &Uint256)>, // None means don't check
) {
    assert_eq!(
        log_item.get_return_code(),
        if let Some(rc) = return_code {
            Uint256::from_u64(rc)
        } else {
            Uint256::zero()
        }
    );

    if let Some(req_id) = request_id {
        assert_eq!(log_item.get_request_id(), req_id);
    }

    // verify first return value
    if let Some((function, expected_val)) = first_return_val {
        let actual_val = function
            .decode_output(&log_item.get_return_data().clone())
            .unwrap();
        assert_eq!(
            actual_val[0],
            ethabi::Token::Uint(ethabi::Uint::from_big_endian(&expected_val.to_bytes_be()))
        );
    }
}

pub fn mint_erc20_and_get_balance(log_to: Option<&Path>, debug: bool) {
    let token_addr = Uint256::from_usize(32563);
    let me = Uint256::from_usize(1025);
    let million = Uint256::from_usize(1000000);

    let mut rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    rt_env.insert_erc20_deposit_message(me.clone(), token_addr.clone(), me.clone(), million);
    let mut calldata: Vec<u8> = vec![0x70, 0xa0, 0x82, 0x31]; // code for balanceOf method
    calldata.extend(me.to_bytes_be());
    rt_env.insert_tx_message(
        me,
        Uint256::from_usize(1000000000),
        Uint256::zero(),
        token_addr,
        Uint256::zero(),
        &calldata,
    );

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let num_logs_before = machine.runtime_env.get_all_logs().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_logs();
    assert_eq!(logs.len(), num_logs_before + 2);
    assert!(logs[logs.len() - 2].succeeded());
    assert!(logs[logs.len() - 1].succeeded());

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn mint_erc721_and_get_balance(log_to: Option<&Path>, debug: bool) {
    let token_addr = Uint256::from_usize(32563);
    let me = Uint256::from_usize(1025);
    let million = Uint256::from_usize(1000000);

    let mut rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    rt_env.insert_erc721_deposit_message(me.clone(), token_addr.clone(), me.clone(), million);
    let mut calldata: Vec<u8> = vec![0x70, 0xa0, 0x82, 0x31]; // code for balanceOf method
    calldata.extend(me.to_bytes_be());
    rt_env.insert_tx_message(
        me,
        Uint256::from_usize(1000000000),
        Uint256::zero(),
        token_addr,
        Uint256::zero(),
        &calldata,
    );

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let num_logs_before = machine.runtime_env.get_all_logs().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_logs();
    assert_eq!(logs.len(), num_logs_before + 2);
    assert!(logs[logs.len() - 2].succeeded());
    assert!(logs[logs.len() - 1].succeeded());

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn make_logs_for_all_arbos_tests() {
    evm_direct_deploy_add(
        Some(Path::new("testlogs/evm_direct_deploy_add.aoslog")),
        false,
    );
    evm_direct_deploy_and_call_add(
        Some(Path::new("testlogs/evm_direct_deploy_and_call_add.aoslog")),
        false,
    );

    let _ = evm_xcontract_call_with_constructors(
        Some(Path::new(
            "testlogs/evm_xcontract_call_with_constructors.aoslog",
        )),
        false,
        false,
    );
    let _ = evm_xcontract_call_using_batch(
        Some(Path::new("testlogs/evm_xcontract_call_using_batch.aoslog")),
        false,
        false,
    );
    let _ = evm_test_create(
        Some(Path::new("testlogs/evm_test_create.aoslog")),
        false,
        false,
    );
    let _ = evm_test_sequencer_support(
        Some(Path::new("testlogs/evm_test_sequencer_support")),
        false,
    );
    evm_test_arbsys(Some(Path::new("testlogs/evm_test_arbsys.aoslog")), false);
    mint_erc20_and_get_balance(Some(Path::new("testlogs/erc20_test.aoslog")), false);
    mint_erc721_and_get_balance(Some(Path::new("testlogs/erc721_test.aoslog")), false);
}
