/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::evm::abi::FunctionTable;
use crate::evm::abi::{ArbAddressTable, ArbBLS, ArbFunctionTable, ArbSys};
use crate::mavm::Value;
use crate::run::{bytestack_from_bytes, load_from_file, RuntimeEnvironment};
use crate::uint256::Uint256;
use abi::AbiForContract;
use ethers_signers::Signer;
use std::path::Path;

mod abi;
pub mod benchmarks;

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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
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
    if fib_contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        None,
        None,
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

#[cfg(test)]
pub fn evm_deploy_using_non_eip159_signature(
    log_to: Option<&Path>,
    debug: bool,
) -> Result<bool, ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let num_wei = 21700000000140000u64;

    let my_addr = Uint256::from_usize(1025);
    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        Uint256::from_string_hex("9c5a87452d4FAC0cbd53BDCA580b20A45526B3AB").unwrap(),
        Uint256::from_u64(num_wei),
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    // submit a "universal deployer" tx referenced by Discord user Agust1211
    //     see https://gist.github.com/Agusx1211/de05dabf918d448d315aa018e2572031
    machine.runtime_env.insert_l2_message(
        my_addr,
        &hex::decode("04f9010880852416b84e01830222e08080b8b66080604052348015600f57600080fd5b50609980601d6000396000f3fe60a06020601f369081018290049091028201604052608081815260009260609284918190838280828437600092018290525084519495509392505060208401905034f5604080516001600160a01b0383168152905191935081900360200190a0505000fea26469706673582212205a310755225e3c740b2f013fb6343f4c205e7141fcdf15947f5f0e0e818727fb64736f6c634300060a00331ca01820182018201820182018201820182018201820182018201820182018201820a01820182018201820182018201820182018201820182018201820182018201820").expect("Hex decoding failed"),
        false,
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this deploy message

    let logs = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(logs.len(), 1);
    assert!(logs[0].succeeded());

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(true)
}

pub fn evm_test_arbsys_direct(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbsys = ArbSys::new(&wallet, debug);
    let arb_address_table = ArbAddressTable::new(&wallet, debug);
    let arb_bls = ArbBLS::new(&wallet, debug);

    let tx_count = arbsys.get_transaction_count(&mut machine, my_addr.clone())?;
    assert_eq!(tx_count, Uint256::one());

    let addr_table_index = arb_address_table.register(&mut machine, my_addr.clone())?;
    let lookup_result = arb_address_table.lookup(&mut machine, my_addr.clone())?;
    assert_eq!(addr_table_index, lookup_result);

    let recovered_addr = arb_address_table.lookup_index(&mut machine, lookup_result)?;
    assert_eq!(recovered_addr, my_addr);

    let my_addr_compressed = arb_address_table.compress(&mut machine, my_addr.clone())?;
    let (my_addr_decompressed, offset) =
        arb_address_table.decompress(&mut machine, &my_addr_compressed, Uint256::zero())?;
    assert_eq!(my_addr.clone(), my_addr_decompressed);
    assert_eq!(offset, Uint256::from_usize(my_addr_compressed.len()));

    assert_eq!(Uint256::from_u64(2), arb_address_table.size(&mut machine)?);

    let an_addr = Uint256::from_u64(581351734971918347);
    let an_addr_compressed = arb_address_table.compress(&mut machine, an_addr.clone())?;
    let (an_addr_decompressed, offset) =
        arb_address_table.decompress(&mut machine, &an_addr_compressed, Uint256::zero())?;
    assert_eq!(an_addr.clone(), an_addr_decompressed);
    assert_eq!(offset, Uint256::from_usize(an_addr_compressed.len()));

    let x0 = Uint256::from_u64(17);
    let x1 = Uint256::from_u64(35);
    let y0 = Uint256::from_u64(71);
    let y1 = Uint256::from_u64(143);
    println!("registering BLS key");
    arb_bls.register(&mut machine, x0.clone(), x1.clone(), y0.clone(), y1.clone())?;
    println!("reading BLS key");
    let (ox0, ox1, oy0, oy1) = arb_bls.get_public_key(&mut machine, my_addr.clone())?;
    assert_eq!(x0, ox0);
    assert_eq!(x1, ox1);
    assert_eq!(y0, oy0);
    assert_eq!(y1, oy1);

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(())
}

pub fn evm_test_function_table_access(
    log_to: Option<&Path>,
    debug: bool,
) -> Result<(), ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbsys = ArbSys::new(&wallet, debug);
    let arb_function_table = ArbFunctionTable::new(&wallet, debug);

    let gtc_short_sig = arbsys
        .contract_abi
        .short_signature_for_function("getTransactionCount")
        .unwrap();
    let mut func_table = FunctionTable::new();
    arbsys.contract_abi.append_to_compression_func_table(
        &mut func_table,
        "getTransactionCount",
        false,
        Uint256::from_u64(10000000),
    )?;
    arb_function_table.upload(&mut machine, &func_table)?;

    println!("Checking size");
    assert_eq!(
        arb_function_table.size(&mut machine, my_addr.clone())?,
        Uint256::one()
    );

    println!("Getting item");
    let (func_code, is_payable, gas_limit) =
        arb_function_table.get(&mut machine, my_addr, Uint256::zero())?;
    assert_eq!(
        func_code,
        Uint256::from_bytes(&gtc_short_sig).shift_left(256 - 32)
    );
    assert_eq!(is_payable, false);
    assert_eq!(gas_limit, Uint256::from_u64(10000000));

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(())
}

pub fn evm_test_create(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
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
    if fib_contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        None,
        None,
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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));

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
    if fib_contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        None,
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let mut batch = machine.runtime_env.new_batch();
    let tx_id_1 = pc_contract.add_function_call_to_batch(
        &mut batch,
        my_addr.clone(),
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract.add_function_call_to_batch(
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
        &mut machine,
        Uint256::zero(),
        &wallet,
    )?;

    machine
        .runtime_env
        .insert_batch_message(Uint256::from_usize(1025), &batch);

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
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

pub fn _evm_xcontract_call_using_sequencer_batch(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let sequencer_addr = Uint256::from_usize(1337);
    let rt_env = RuntimeEnvironment::new_options(
        Uint256::from_usize(1111),
        Some((
            sequencer_addr.clone(),
            Uint256::from_u64(20),
            Uint256::from_u64(20 * 30),
        )),
    );

    let wallet = rt_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")?;
    if fib_contract.deploy(
        &[],
        &mut machine,
        Uint256::zero(),
        Some(Uint256::from_u64(30)),
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        Some(Uint256::from_u64(30)),
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);

    let mut batch = machine.runtime_env._new_sequencer_batch(None);
    let tx_id_1 = pc_contract.add_function_call_to_batch(
        &mut batch,
        my_addr.clone(),
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract.add_function_call_to_batch(
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
        &mut machine,
        Uint256::zero(),
        &wallet,
    )?;

    machine
        .runtime_env
        .insert_batch_message(sequencer_addr, &batch);

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
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

pub fn _evm_xcontract_call_sequencer_slow_path(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let sequencer_addr = Uint256::from_usize(1337);
    let rt_env = RuntimeEnvironment::new_options(
        Uint256::from_usize(1111),
        Some((
            sequencer_addr.clone(),
            Uint256::from_u64(20),
            Uint256::from_u64(20 * 30),
        )),
    );

    let wallet = rt_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")?;
    if fib_contract.deploy(
        &[],
        &mut machine,
        Uint256::zero(),
        Some(Uint256::from_u64(30)),
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        Some(Uint256::from_u64(30)),
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let mut batch = machine.runtime_env.new_batch();
    let tx_id_1 = pc_contract.add_function_call_to_batch(
        &mut batch,
        my_addr.clone(),
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract.add_function_call_to_batch(
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
        &mut machine,
        Uint256::zero(),
        &wallet,
    )?;

    machine
        .runtime_env
        .insert_batch_message(sequencer_addr, &batch);

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
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

pub fn _evm_xcontract_call_sequencer_reordering(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let sequencer_addr = Uint256::from_usize(1337);
    let rt_env = RuntimeEnvironment::new_options(
        Uint256::from_usize(1111),
        Some((
            sequencer_addr.clone(),
            Uint256::from_u64(20),
            Uint256::from_u64(20 * 30),
        )),
    );

    let wallet = rt_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/Fibonacci.json")?;
    if fib_contract.deploy(
        &[],
        &mut machine,
        Uint256::zero(),
        Some(Uint256::from_u64(30)),
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        Some(Uint256::from_u64(30)),
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);

    let mut slow_batch = machine.runtime_env.new_batch();
    let mut seq_batch = machine
        .runtime_env
        ._new_sequencer_batch(Some((Uint256::from_u64(3), Uint256::from_u64(40))));

    let tx_id_1 = pc_contract.add_function_call_to_batch(
        &mut seq_batch,
        my_addr.clone(),
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract.add_function_call_to_batch(
        &mut slow_batch,
        my_addr.clone(),
        "transferFib",
        vec![
            ethabi::Token::Address(ethereum_types::H160::from_slice(
                &my_addr.to_bytes_minimal(),
            )),
            ethabi::Token::Uint(ethabi::Uint::try_from(1).unwrap()),
        ]
        .as_ref(),
        &mut machine,
        Uint256::zero(),
        &wallet,
    )?;

    machine
        .runtime_env
        .insert_batch_message(my_addr, &slow_batch);

    machine
        .runtime_env
        ._advance_time(Uint256::one(), Uint256::from_u64(13), false);

    machine
        .runtime_env
        .insert_batch_message(sequencer_addr, &seq_batch);

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(30), Uint256::from_u64(30 * 13), true);

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
    let sends = machine.runtime_env.get_all_sends();
    let logs = &logs[num_logs_before..];
    let sends = &sends[num_sends_before..];

    assert_eq!(logs.len(), 2);
    assert_eq!(sends.len(), 0);

    assert!(logs[0].succeeded());
    assert_eq!(logs[0].get_request_id(), tx_id_1);

    assert!(logs[1].succeeded());
    assert_eq!(logs[1].get_request_id(), tx_id_2);

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(true)
}

pub fn _evm_xcontract_call_using_compressed_batch(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));

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
    if fib_contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug) == None {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract =
        AbiForContract::new_from_file("contracts/fibonacci/build/contracts/PaymentChannel.json")?;
    if pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        None,
        None,
        debug,
    ) == None
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let mut batch = machine.runtime_env.new_batch();
    let tx_id_1 = pc_contract._add_function_call_to_compressed_batch(
        &mut batch,
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract._add_function_call_to_compressed_batch(
        &mut batch,
        "transferFib",
        vec![
            ethabi::Token::Address(ethereum_types::H160::from_slice(
                &my_addr.to_bytes_minimal(),
            )),
            ethabi::Token::Uint(ethabi::Uint::try_from(1).unwrap()),
        ]
        .as_ref(),
        &mut machine,
        Uint256::zero(),
        &wallet,
    )?;

    machine
        .runtime_env
        .insert_batch_message(Uint256::from_usize(1025), &batch);

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let num_sends_before = machine.runtime_env.get_all_sends().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug);
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

pub fn evm_deploy_buddy_contract(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(
                &[],
                &mut machine,
                Uint256::zero(),
                None,
                Some(Uint256::from_u64(1025)),
                debug,
            );
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

pub fn _evm_test_payment_in_constructor(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
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
            let result = contract.deploy(
                &vec![],
                &mut machine,
                Uint256::from_u64(10000),
                None,
                None,
                debug,
            );

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
        "withdraw5000",
        vec![].as_ref(),
        &mut machine,
        Uint256::zero(),
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

pub fn evm_test_arbsys(log_to: Option<&Path>, debug: bool) {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
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
            let result = contract.deploy(&vec![], &mut machine, Uint256::zero(), None, None, debug);
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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_usize(1025);
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug);
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

pub fn _evm_test_same_address_deploy(log_to: Option<&Path>, debug: bool) {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_usize(1025);
    let (contract, orig_contract_addr) =
        match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
            Ok(mut contract) => {
                let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug);
                if let Some(contract_addr) = result {
                    assert_ne!(contract_addr, Uint256::zero());
                    (contract, contract_addr)
                } else {
                    panic!("deploy failed");
                }
            }
            Err(e) => {
                panic!("error loading contract: {:?}", e);
            }
        };

    match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut new_contract) => {
            let result = new_contract.deploy(
                &[],
                &mut machine,
                Uint256::zero(),
                None,
                Some(orig_contract_addr),
                debug,
            );
            assert_eq!(result, None);
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

pub fn evm_direct_deploy_and_compressed_call_add(log_to: Option<&Path>, debug: bool) {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let wallet = rt_env.new_wallet();
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());
    let contract = match AbiForContract::new_from_file("contracts/add/build/contracts/Add.json") {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug);
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

    let result = contract.call_function_compressed(
        my_addr,
        "add",
        vec![
            ethabi::Token::Uint(ethabi::Uint::one()),
            ethabi::Token::Uint(ethabi::Uint::one()),
        ]
        .as_ref(),
        &mut machine,
        Uint256::zero(),
        &wallet,
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

pub fn evm_payment_to_empty_address(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1025);
    let dest_addr = Uint256::from_u64(4242);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_u64(20000),
    );
    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr,
        Uint256::from_u64(1000000000),
        Uint256::zero(),
        dest_addr,
        Uint256::from_u64(10000),
        &vec![],
    );

    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let receipts = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(receipts.len(), 1);
    assert_eq!(receipts[0].get_request_id(), tx_id);
    assert!(receipts[0].succeeded());

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn evm_eval_sha256(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1025);

    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr,
        Uint256::from_u64(1000000000),
        Uint256::zero(),
        Uint256::from_u64(2), // sha256 precompile
        Uint256::from_u64(0),
        &vec![0xCCu8],
    );

    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let receipts = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(receipts.len(), 1);
    assert_eq!(receipts[0].get_request_id(), tx_id);
    assert!(receipts[0].succeeded());
    let return_data = receipts[0].get_return_data();
    let return_uint = Uint256::from_bytes(&return_data);
    assert_eq!(
        return_uint,
        Uint256::from_string_hex(
            "1dd8312636f6a0bf3d21fa2855e63072507453e93a5ced4301b364e91c9d87d6"
        )
        .unwrap()
    );

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

pub fn mint_erc20_and_get_balance(log_to: Option<&Path>, debug: bool) {
    let token_addr = Uint256::from_usize(32563);
    let me = Uint256::from_usize(1025);
    let million = Uint256::from_usize(1000000);

    let mut rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
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

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
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

    let mut rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
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

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
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
    let _ = evm_test_arbsys_direct(
        Some(Path::new("testlogs/evm_test_arbsys_direct.aoslog")),
        false,
    );
    let _ = evm_test_function_table_access(
        Some(Path::new("testlogs/evm_test_function_table_access.aoslog")),
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
    /*let _ = evm_xcontract_call_using_compressed_batch(
        Some(Path::new("testlogs/evm_xcontract_call_using_batch.aoslog")),
        false,
        false,
    );*/
    evm_direct_deploy_and_compressed_call_add(
        Some(Path::new(
            "testlogs/evm_direct_deploy_and_compressed_call_add.aoslog",
        )),
        false,
    );
    let _ = evm_test_create(
        Some(Path::new("testlogs/evm_test_create.aoslog")),
        false,
        false,
    );
    evm_deploy_buddy_contract(
        Some(Path::new("testlogs/deploy_buddy_contract.aoslog")),
        false,
    );
    evm_test_arbsys(Some(Path::new("testlogs/evm_test_arbsys.aoslog")), false);
    evm_eval_sha256(Some(Path::new("testlogs/evm_eval_sha256.aoslog")), false);
    evm_payment_to_empty_address(
        Some(Path::new("testlogs/payment_to_empty_address.aoslog")),
        false,
    );
    mint_erc20_and_get_balance(Some(Path::new("testlogs/erc20_test.aoslog")), false);
    mint_erc721_and_get_balance(Some(Path::new("testlogs/erc721_test.aoslog")), false);
}
