use super::abi::{ArbAggregator, ArbGasInfo, ArbInfo, ArbOwner, ArbReplayableTx, ArbStatistics};
use super::*;
use crate::compile::miniconstants::init_constant_table;
use crate::run::{load_from_file, RuntimeEnvironment};
use crate::uint256::Uint256;
use ethers_signers::Signer;
use std::path::Path;

#[test]
fn test_payment_to_self() {
    let _ = _evm_payment_to_self(None, false).unwrap();
}

pub fn _evm_payment_to_self(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1025);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_u64(20000),
    );

    let arbinfo = ArbInfo::_new(false);
    let balance = arbinfo._get_balance(&mut machine, &my_addr)?;
    assert_eq!(balance, Uint256::from_u64(20000));

    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr.clone(),
        Uint256::from_u64(1000000000),
        Uint256::zero(),
        my_addr.clone(),
        Uint256::from_u64(10000),
        &vec![],
        false,
    );

    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let receipts = machine.runtime_env.get_all_receipt_logs();
    let last_rcpt = receipts.len() - 1;
    assert_eq!(receipts[last_rcpt].get_request_id(), tx_id);
    assert!(receipts[last_rcpt].succeeded());

    let new_balance = arbinfo._get_balance(&mut machine, &my_addr)?;
    assert_eq!(new_balance, Uint256::from_u64(20000));

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}

#[test]
fn _test_upgrade_arbos_to_different_version() {
    _test_upgrade_arbos_over_itself_impl().unwrap();
}

fn _test_upgrade_arbos_over_itself_impl() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos_before.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    if add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy Add contract");
    }

    let arbowner = ArbOwner::_new(&wallet, false);

    let arbsys_orig_binding = ArbSys::new(&wallet, false);
    assert_eq!(
        arbsys_orig_binding._arbos_version(&mut machine)?,
        Uint256::one()
    );

    arbowner._give_ownership(&mut machine, my_addr, Some(Uint256::zero()))?;

    let uploader = CodeUploader::_new_from_file(Path::new("arb_os/arbos-upgrade.mexe"));
    arbowner._start_code_upload(&mut machine)?;

    let mut accum = vec![];
    for buf in uploader.instructions {
        accum.extend(buf);
        if (accum.len() > 3000) {
            arbowner._continue_code_upload(&mut machine, accum)?;
            accum = vec![];
        }
    }
    if (accum.len() > 0) {
        arbowner._continue_code_upload(&mut machine, accum)?;
    }

    arbowner._finish_code_upload_as_arbos_upgrade(&mut machine)?;

    let wallet2 = machine.runtime_env.new_wallet();
    let arbsys = ArbSys::new(&wallet2, false);
    let arbos_version = arbsys._arbos_version(&mut machine)?;
    assert_eq!(arbos_version, Uint256::one());
    let arbos_version_orig = arbsys_orig_binding._arbos_version(&mut machine)?;
    assert_eq!(arbos_version, arbos_version_orig);

    Ok(())
}

#[test]
pub fn test_gas_charging_underfunded() {
    match _evm_run_with_gas_charging(None, Uint256::_from_gwei(20), false, false) {
        Ok(result) => assert_eq!(result, false),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_gas_charging_fully_funded() {
    match _evm_run_with_gas_charging(None, Uint256::_from_eth(1), false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

pub fn _evm_run_with_gas_charging(
    log_to: Option<&Path>,
    funding: Uint256,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    // returns Ok(true) if success, Ok(false) if insufficient gas money, Err otherwise
    use std::convert::TryFrom;
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    machine
        .runtime_env
        .insert_eth_deposit_message(my_addr.clone(), my_addr.clone(), funding);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle these ETH deposit messages

    println!("First deploy ...");
    let mut fib_contract = AbiForContract::new_from_file(&test_contract_path("Fibonacci"))?;
    if let Err(receipt) = fib_contract.deploy(&[], &mut machine, Uint256::zero(), None, debug) {
        if receipt.unwrap().get_return_code() == Uint256::from_u64(3) {
            return Ok(false);
        } else {
            panic!("unexpected failure deploying Fibonacci contract");
        }
    }

    println!("Second deploy ...");
    let mut pc_contract = AbiForContract::new_from_file(&test_contract_path("PaymentChannel"))?;
    if let Err(receipt) = pc_contract.deploy(
        &[ethabi::Token::Address(ethereum_types::H160::from_slice(
            &fib_contract.address.to_bytes_be()[12..],
        ))],
        &mut machine,
        Uint256::zero(),
        None,
        debug,
    ) {
        if receipt.unwrap().get_return_code() == Uint256::from_u64(3) {
            return Ok(false);
        } else {
            panic!("unexpected failure deploying PaymentChannel contract");
        }
    }

    // turn on gas charging
    let arbowner = ArbOwner::_new(&wallet, false);
    arbowner._set_fees_enabled(&mut machine, true, true)?;
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, false);

    println!("Function call ...");
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

    if !logs[0].succeeded() {
        if logs[0].get_return_code() == Uint256::from_u64(3) {
            return Ok(false);
        } else {
            panic!();
        }
    }

    let (logs, sends) = pc_contract.call_function(
        my_addr,
        "transferFib",
        &[
            ethabi::Token::Address(ethabi::Address::from_low_u64_be(1025)),
            ethabi::Token::Uint(ethabi::Uint::try_from(1).unwrap()),
        ],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);

    if !logs[0].succeeded() {
        if logs[0].get_return_code() == Uint256::from_u64(3) {
            return Ok(false);
        } else {
            panic!();
        }
    }

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(true)
}

#[test]
fn test_arbowner() {
    match _evm_test_arbowner(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn _evm_test_arbowner(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::_new(&wallet, debug);

    arbowner._give_ownership(&mut machine, my_addr, Some(Uint256::zero()))?;

    arbowner._start_code_upload(&mut machine)?;

    let mcode = vec![0x90u8, 1u8, 0u8, 42u8]; // debugprint(42)
    arbowner._continue_code_upload(&mut machine, mcode)?;

    arbowner._finish_code_upload_as_arbos_upgrade(&mut machine)?;

    arbowner._set_seconds_per_send(&mut machine, Uint256::from_u64(10))?;

    arbowner._set_gas_accounting_params(
        &mut machine,
        Uint256::from_u64(100_000_000),
        Uint256::from_u64(6_000_000_000),
        Uint256::from_u64(1_000_000_000),
    )?;

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}

#[test]
fn test_arbgasinfo() {
    match _evm_test_arbgasinfo(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn _evm_test_arbgasinfo(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::_new(&wallet, debug);
    let arbgasinfo = ArbGasInfo::_new(&wallet, debug);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::_from_eth(100),
    );

    let (l2tx, l1calldata, storage, basegas, conggas, totalgas) =
        arbgasinfo._get_prices_in_wei(&mut machine)?;
    assert!(l2tx.is_zero());
    assert!(l1calldata.is_zero());
    assert!(storage.is_zero());
    assert!(basegas.is_zero());
    assert!(conggas.is_zero());
    assert_eq!(basegas.add(&conggas), totalgas);

    arbowner._set_fees_enabled(&mut machine, true, true)?;
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);

    let (l2tx, l1calldata, storage, basegas, conggas, totalgas) =
        arbgasinfo._get_prices_in_wei(&mut machine)?;
    println!(
        "L2 tx {}, L1 calldata {}, L2 storage {}, base gas {}, congestion gas {}, total gas {}",
        l2tx, l1calldata, storage, basegas, conggas, totalgas
    );
    assert_eq!(l2tx, Uint256::from_u64(642483725000000));
    assert_eq!(l1calldata, Uint256::from_u64(2778308000000));
    assert_eq!(storage, Uint256::from_u64(301990000000000));
    assert_eq!(basegas, Uint256::from_u64(15099500));
    assert!(conggas.is_zero());
    assert_eq!(basegas.add(&conggas), totalgas);

    let (l2tx, l1calldata, storage) = arbgasinfo._get_prices_in_arbgas(&mut machine)?;
    println!(
        "L2 tx / ag {}, L1 calldata / ag {}, L2 storage / ag {}",
        l2tx, l1calldata, storage
    );
    assert_eq!(l2tx, Uint256::from_u64(42550000));
    assert_eq!(l1calldata, Uint256::from_u64(184000));
    assert_eq!(storage, Uint256::from_u64(20000000));

    let (speed_limit, gas_pool_max, tx_gas_limit) =
        arbgasinfo._get_gas_accounting_params(&mut machine)?;
    println!(
        "speed limit {}, pool max {}, tx gas limit {}",
        speed_limit, gas_pool_max, tx_gas_limit
    );
    assert_eq!(speed_limit, Uint256::from_u64(100_000_000));
    assert_eq!(gas_pool_max, Uint256::from_u64(6_000_000_000));
    assert_eq!(tx_gas_limit, Uint256::from_u64(1_000_000_000));

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}

pub fn _evm_test_rate_control(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());
    let arbowner = ArbOwner::_new(&wallet, debug);

    arbowner._give_ownership(&mut machine, my_addr, Some(Uint256::zero()))?;

    let const_table = init_constant_table();

    let (r1, r2) = arbowner._get_fee_recipients(&mut machine)?;
    assert_eq!(&r1, const_table.get("NetFee_defaultRecipient").unwrap());
    assert_eq!(
        &r2,
        const_table.get("CongestionFee_defaultRecipient").unwrap()
    );

    let new_r1 = r1.add(&Uint256::one());
    let new_r2 = r2.add(&Uint256::one());
    arbowner._set_fee_recipients(&mut machine, new_r1.clone(), new_r2.clone())?;
    let (updated_r1, updated_r2) = arbowner._get_fee_recipients(&mut machine)?;
    assert_eq!(new_r1, updated_r1);
    assert_eq!(new_r2, updated_r2);

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}

#[test]
fn test_rollup_tracker() {
    _do_rollup_tracker_ops();
}

pub fn _do_rollup_tracker_ops() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let owner = Uint256::from_bytes(wallet.address().as_bytes());
    let arbowner = ArbOwner::_new(&wallet, false);
    arbowner
        ._give_ownership(&mut machine, owner.clone(), Some(Uint256::zero()))
        .unwrap();

    let my_addr = Uint256::from_u64(11025);
    let claimer = Uint256::from_u64(4242);

    machine.runtime_env.insert_eth_deposit_message(
        owner.clone(),
        owner.clone(),
        Uint256::_from_eth(1),
    );
    machine.runtime_env.insert_eth_deposit_message(
        claimer.clone(),
        claimer.clone(),
        Uint256::_from_eth(1),
    );

    arbowner
        ._add_to_reserve_funds(&mut machine, Uint256::_from_eth(1))
        .unwrap();

    _insert_create_node(
        &mut machine.runtime_env,
        &Uint256::one(),
        &Uint256::zero(),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );
    _insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(2),
        &Uint256::one(),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    _insert_claim_node(&mut machine.runtime_env, &Uint256::from_u64(2), &claimer);

    _insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(3),
        &Uint256::one(),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    _insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(4),
        &Uint256::from_u64(2),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    _insert_new_stake(
        &mut machine.runtime_env,
        &Uint256::from_u64(4),
        &claimer,
        None,
    );

    _insert_rollup_debug(&mut machine.runtime_env);
    let _ = machine.run(None);

    _insert_confirm_node(&mut machine.runtime_env, &Uint256::zero());
    _insert_confirm_node(&mut machine.runtime_env, &Uint256::one());
    _insert_confirm_node(&mut machine.runtime_env, &Uint256::from_u64(2));
    _insert_reject_node(&mut machine.runtime_env, &Uint256::from_u64(3));
    _insert_confirm_node(&mut machine.runtime_env, &Uint256::from_u64(4));

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(100), None, true);

    _insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(5),
        &Uint256::from_u64(4),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    let _ = machine.run(None);

    // There isn't really a result we can check here, so this test just confirms that nothing crashes.
}

pub fn _insert_create_node(
    rt_env: &mut RuntimeEnvironment,
    height_l2: &Uint256,
    prev: &Uint256,
    height_l1: Option<&Uint256>,
    deadline_l1_delta: &Uint256,
    asserter: Uint256,
) {
    let height_l1 = &height_l1.unwrap_or(&rt_env.current_block_num);
    let mut buf = vec![0u8];
    buf.extend(height_l2.to_bytes_be());
    buf.extend(prev.to_bytes_be());
    buf.extend(height_l1.to_bytes_be());
    buf.extend(height_l1.add(deadline_l1_delta).to_bytes_be());
    buf.extend(asserter.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf);
}

pub fn _insert_confirm_node(rt_env: &mut RuntimeEnvironment, height_l2: &Uint256) {
    let mut buf = vec![1u8];
    buf.extend(height_l2.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf);
}

pub fn _insert_reject_node(rt_env: &mut RuntimeEnvironment, height_l2: &Uint256) {
    let mut buf = vec![2u8];
    buf.extend(height_l2.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf);
}

pub fn _insert_new_stake(
    rt_env: &mut RuntimeEnvironment,
    height_l2: &Uint256,
    staker: &Uint256,
    stake_time: Option<Uint256>,
) {
    let mut buf = vec![3u8];
    buf.extend(height_l2.to_bytes_be());
    buf.extend(staker.to_bytes_be());
    buf.extend(
        stake_time
            .unwrap_or(rt_env.current_block_num.clone())
            .to_bytes_be(),
    );
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf);
}

pub fn _insert_claim_node(rt_env: &mut RuntimeEnvironment, height_l2: &Uint256, claimer: &Uint256) {
    let mut buf = vec![4u8];
    buf.extend(height_l2.to_bytes_be());
    buf.extend(claimer.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf);
}

pub fn _insert_rollup_debug(rt_env: &mut RuntimeEnvironment) {
    rt_env.insert_l1_message(8u8, Uint256::zero(), &[255u8]);
}

#[test]
fn test_arbaggregator() {
    match _evm_test_arbaggregator(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn _evm_test_arbaggregator(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbagg = ArbAggregator::_new(debug);

    let pref_agg = arbagg._get_preferred_aggregator(&mut machine, my_addr.clone())?;
    assert_eq!(pref_agg, (Uint256::zero(), true));

    let new_pref_agg = Uint256::from_u64(4242);
    arbagg._set_preferred_aggregator(&mut machine, new_pref_agg.clone(), my_addr.clone())?;
    let pref_agg = arbagg._get_preferred_aggregator(&mut machine, my_addr.clone())?;
    assert_eq!(pref_agg, (new_pref_agg, false));

    let def_agg = arbagg._get_default_aggregator(&mut machine)?;
    assert_eq!(def_agg, Uint256::zero());

    let new_def_agg = Uint256::from_u64(9696);
    arbagg._set_default_aggregator(&mut machine, new_def_agg.clone(), None)?;
    let def_agg = arbagg._get_default_aggregator(&mut machine)?;
    assert_eq!(def_agg, new_def_agg);

    assert!(arbagg
        ._set_default_aggregator(&mut machine, Uint256::from_u64(12345), Some(my_addr))
        .is_err());

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}

#[test]
fn test_retryable() {
    match _test_retryable(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{}", e),
    }
}

pub fn _test_retryable(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1234);

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    if add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy Add contract");
    }

    let beneficiary = Uint256::from_u64(9185);

    let (txid, _) = add_contract._send_retryable_tx(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
        Uint256::zero(),
        None,
        Some(beneficiary.clone()),
        None,
        None,
    )?;
    assert!(txid != Uint256::zero());
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let arb_replayable = ArbReplayableTx::_new(debug);
    let timeout = arb_replayable._get_timeout(&mut machine, txid.clone())?;
    assert!(timeout > machine.runtime_env.current_timestamp);

    let (keepalive_price, reprice_time) =
        arb_replayable._get_keepalive_price(&mut machine, txid.clone())?;
    assert_eq!(keepalive_price, Uint256::zero());
    assert!(reprice_time > machine.runtime_env.current_timestamp);

    let keepalive_ret = arb_replayable._keepalive(&mut machine, txid.clone(), keepalive_price)?;

    let new_timeout = arb_replayable._get_timeout(&mut machine, txid.clone())?;
    assert_eq!(keepalive_ret, new_timeout);
    assert!(new_timeout > timeout);

    arb_replayable._redeem(&mut machine, txid.clone())?;

    let new_timeout = arb_replayable._get_timeout(&mut machine, txid.clone())?;
    assert_eq!(new_timeout, Uint256::zero()); // verify that txid has been removed

    // make another one, and have the beneficiary cancel it
    let (txid, _) = add_contract._send_retryable_tx(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
        Uint256::zero(),
        None,
        Some(beneficiary.clone()),
        None,
        None,
    )?;
    assert!(txid != Uint256::zero());
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let out_beneficiary = arb_replayable._get_beneficiary(&mut machine, txid.clone())?;
    assert_eq!(out_beneficiary, beneficiary);

    arb_replayable._cancel(&mut machine, txid.clone(), beneficiary.clone())?;

    assert_eq!(
        arb_replayable._get_timeout(&mut machine, txid)?,
        Uint256::zero()
    ); // verify txid no longer exists

    let amount_to_pay = Uint256::from_u64(1_000_000);
    let _txid = machine.runtime_env._insert_retryable_tx_message(
        my_addr.clone(),
        Uint256::from_u64(7890245789245), // random non-contract address
        amount_to_pay.clone(),
        amount_to_pay.clone(),
        Uint256::zero(),
        my_addr.clone(),
        my_addr.clone(),
        Uint256::zero(),
        Uint256::zero(),
        &[],
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let all_logs = machine.runtime_env.get_all_receipt_logs();
    let last_log = &all_logs[all_logs.len() - 1];
    assert!(last_log.succeeded());

    let (txid, redeemid) = add_contract._send_retryable_tx(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
        Uint256::zero(),
        None,
        Some(beneficiary.clone()),
        Some(Uint256::from_u64(1_000_000)),
        Some(Uint256::zero()),
    )?;
    assert!(txid != Uint256::zero());
    assert!(redeemid.is_some());
    let redeemid = redeemid.unwrap();

    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let receipts = machine.runtime_env.get_all_receipt_logs();
    let last_receipt = receipts[receipts.len() - 1].clone();
    assert!(last_receipt.succeeded());
    assert_eq!(last_receipt.get_request_id(), redeemid);

    let second_to_last = receipts[receipts.len() - 2].clone();
    assert!(second_to_last.succeeded());
    assert_eq!(second_to_last.get_request_id(), txid);

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}

#[test]
fn test_arb_statistics() {
    assert!(_test_arb_stats().is_ok());
}

fn _test_arb_stats() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let arbstats = ArbStatistics::_new(false);

    let (arb_blocknum, num_accounts, storage, _arbgas, txs, contracts) =
        arbstats._get_stats(&mut machine)?;

    assert_eq!(arb_blocknum, Uint256::from_u64(0));
    assert_eq!(num_accounts, Uint256::from_u64(22));
    assert_eq!(storage, Uint256::from_u64(0));
    // assert_eq!(_arbgas, Uint256::from_u64(1_490_972));  // disable this because it will vary over versions
    assert_eq!(txs, Uint256::from_u64(0));
    assert_eq!(contracts, Uint256::from_u64(19));
    Ok(())
}
