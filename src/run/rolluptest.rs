/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::evm::abi::_ArbOwner;
use crate::run::{load_from_file, runtime_env::RuntimeEnvironment};
use crate::uint256::Uint256;
use ethers_signers::Signer;
use std::path::Path;

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

pub fn _do_rollup_tracker_ops() {
    let rt_env = RuntimeEnvironment::_new_with_owner(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let owner = Uint256::from_bytes(wallet.address().as_bytes());
    let arbowner = _ArbOwner::_new(&wallet, false);
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
