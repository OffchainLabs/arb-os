/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Value;
use crate::run::{
    _bytestack_from_bytes, load_from_file, run_from_file, Machine, RuntimeEnvironment,
};
use crate::uint256::Uint256;
use num_bigint::{BigUint, RandBigInt};
use rlp::RlpStream;
use std::convert::TryInto;
use std::path::Path;

#[test]
fn test_arraytest() {
    let path = Path::new("builtin/arraytest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{:?}", e);
        }
    }
}

#[test]
fn test_kvstest() {
    let path = Path::new("builtin/kvstest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_storage_map() {
    let path = Path::new("stdlib/storageMapTest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_queuetest() {
    let path = Path::new("stdlib/queuetest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_globaltest() {
    let path = Path::new("builtin/globaltest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_pqtest() {
    let path = Path::new("stdlib/priorityqtest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_bytearray() {
    let path = Path::new("stdlib/bytearraytest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_map() {
    let path = Path::new("builtin/maptest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_keccak() {
    let path = Path::new("stdlib/keccaktest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_bls() {
    let path = Path::new("stdlib/blstest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_sha256() {
    let path = Path::new("stdlib/sha256test.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_fixedpoint() {
    let path = Path::new("stdlib/fixedpointtest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_ripemd160() {
    let path = Path::new("stdlib/ripemd160test.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_biguint() {
    let path = Path::new("stdlib/biguinttest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_rlp() {
    let mut ui = Uint256::one();
    for _i in 0..100 {
        test_rlp_uint(ui.clone(), ui.rlp_encode());
        let ui2 = ui.div(&Uint256::from_usize(2048)).unwrap(); // a valid address
        test_rlp_uint(ui2.clone(), ui2.rlp_encode());
        ui = ui
            .mul(&Uint256::from_usize(19482103))
            .add(&Uint256::from_usize(91));
    }
    let ui = Uint256::from_usize(4313412);
    test_rlp_uint(ui.clone(), ui.rlp_encode());

    let mut byte_testvecs = vec![
        vec![0u8],
        vec![13u8],
        vec![243u8],
        "Hello".as_bytes().to_vec(),
        "The quick brown fox jumped over the lazy dog."
            .as_bytes()
            .to_vec(),
        vec![77u8; 692],
    ];
    let mut new_test_vec: Vec<u8> = Vec::new();
    for i in 0u64..2198 {
        new_test_vec.push(((73 * i) % 256).try_into().unwrap());
    }
    byte_testvecs.push(new_test_vec.clone());
    for testvec in byte_testvecs {
        let res = rlp::encode(&testvec);
        test_rlp_bytearray(testvec, res);
    }

    let list3_testvecs = vec![
        (Uint256::zero(), vec![243u8], Uint256::one()),
        (
            Uint256::from_usize(9831498),
            vec![3u8],
            Uint256::from_usize(4313412),
        ),
        (
            Uint256::from_usize(9831498),
            new_test_vec,
            Uint256::from_usize(4313412),
        ),
    ];
    for testvec in list3_testvecs {
        let res = encode_list3(testvec.clone());
        test_rlp_list3(testvec, res);
    }
}

#[cfg(test)]
fn encode_list3(testvec: (Uint256, Vec<u8>, Uint256)) -> Vec<u8> {
    let mut stream = RlpStream::new_list(3);
    stream
        .append(&testvec.0.to_bytes_minimal())
        .append(&testvec.1)
        .append(&testvec.2.to_bytes_minimal());
    stream.out()
}

#[cfg(test)]
fn test_rlp_uint(ui: Uint256, correct_result: Vec<u8>) {
    let path = Path::new("stdlib/rlptest.mexe");
    let res = run_from_file(
        path,
        vec![Value::Int(Uint256::zero()), Value::Int(ui)],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], _bytestack_from_bytes(&correct_result));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[cfg(test)]
fn test_rlp_bytearray(input: Vec<u8>, correct_result: Vec<u8>) {
    let path = Path::new("stdlib/rlptest.mexe");
    let res = run_from_file(
        path,
        vec![Value::Int(Uint256::one()), _bytestack_from_bytes(&input)],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], _bytestack_from_bytes(&correct_result));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[cfg(test)]
fn test_rlp_list3(testvec: (Uint256, Vec<u8>, Uint256), correct_result: Vec<u8>) {
    let path = Path::new("stdlib/rlptest.mexe");
    let res = run_from_file(
        path,
        vec![
            Value::Int(Uint256::from_usize(2)),
            Value::new_tuple(vec![
                Value::Int(testvec.0),
                _bytestack_from_bytes(&testvec.1),
                Value::Int(testvec.2),
            ]),
        ],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], _bytestack_from_bytes(&correct_result));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_codeload() {
    let path = Path::new("minitests/codeloadtest.mexe");
    let res = run_from_file(
        path,
        vec![],
        RuntimeEnvironment::new(Uint256::from_usize(1111), None),
        false,
    );
    match res {
        Ok(res) => {
            assert_eq!(res[0], Value::Int(Uint256::zero()));
        }
        Err(e) => {
            panic!("{}\n{}", e.0, e.1);
        }
    }
}

#[test]
fn test_direct_deploy_add() {
    crate::evm::evm_direct_deploy_add(None, false);
}

#[test]
fn test_sha256_precompile() {
    crate::evm::evm_eval_sha256(None, false);
}

#[test]
fn test_ecpairing_precompile() {
    crate::evm::_evm_ecpairing_precompile(None, false);
}

#[test]
fn test_ripemd160_precompile() {
    crate::evm::_evm_eval_ripemd160(None, false);
}

#[test]
fn test_deploy_buddy_contract() {
    crate::evm::evm_deploy_buddy_contract(None, false);
}

#[test]
fn test_non_eip155_signed_tx() {
    crate::evm::evm_deploy_using_non_eip159_signature(None, false).unwrap();
}

#[test]
fn test_direct_deploy_and_call_add() {
    let _log = crate::evm::evm_direct_deploy_and_call_add(None, false);
}

#[test]
fn test_call_from_contract() {
    let _log = crate::evm::_evm_test_contract_call(None, false);
}

#[test]
fn test_direct_deploy_and_compressed_call_add() {
    let _log = crate::evm::evm_direct_deploy_and_compressed_call_add(None, false);
}

#[test]
fn test_payment_in_constructor() {
    crate::evm::_evm_test_payment_in_constructor(None, false);
}

#[test]
fn test_arbsys() {
    let _log = crate::evm::evm_test_arbsys(None, false);
}

#[test]
fn test_arbsys_direct() {
    crate::evm::evm_test_arbsys_direct(None, false).unwrap();
}

#[test]
fn test_arbowner() {
    crate::evm::_evm_test_arbowner(None, false).unwrap();
}

#[test]
fn test_rate_control() {
    crate::evm::_evm_test_rate_control(None, false).unwrap();
}

#[test]
fn test_function_table_access() {
    crate::evm::evm_test_function_table_access(None, false).unwrap();
}

#[test]
fn test_evm_add_code() {
    crate::evm::_basic_evm_add_test(None, false).unwrap();
}

#[test]
fn test_same_address_deploy() {
    crate::evm::_evm_test_same_address_deploy(None, false);
}

#[test]
pub fn test_crosscontract_call_with_constructors() {
    match crate::evm::evm_xcontract_call_with_constructors(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_gas_charging_underfunded() {
    match crate::evm::_evm_run_with_gas_charging(
        None,
        Some((
            Uint256::_from_gwei(1000000),
            Uint256::from_u64(100),
            Uint256::zero(),
        )),
        false,
        false,
    ) {
        Ok(result) => assert_eq!(result, false),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_gas_charging_fully_funded() {
    match crate::evm::_evm_run_with_gas_charging(
        None,
        Some((Uint256::one(), Uint256::from_u64(100), Uint256::zero())),
        false,
        false,
    ) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_tx_with_deposit() {
    match crate::evm::_evm_tx_with_deposit(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_create_opcode() {
    match crate::evm::evm_test_create(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_crosscontract_call_using_batch() {
    match crate::evm::evm_xcontract_call_using_batch(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_crosscontract_call_sequencer_fast_path() {
    match crate::evm::_evm_xcontract_call_using_sequencer_batch(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_crosscontract_call_sequencer_slow_path() {
    match crate::evm::_evm_xcontract_call_sequencer_slow_path(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_crosscontract_call_sequencer_reordering() {
    match crate::evm::_evm_xcontract_call_sequencer_reordering(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

pub fn _test_crosscontract_call_using_compressed_batch() {
    match crate::evm::_evm_xcontract_call_using_compressed_batch(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
fn test_payment_to_empty_address() {
    crate::evm::evm_payment_to_empty_address(None, false);
}

#[test]
fn test_underfunded_nested_call() {
    assert!(crate::evm::_underfunded_nested_call_test(None, false).is_ok());
}

#[test]
fn test_bls_registry() {
    crate::evm::bls::_evm_test_bls_registry(None, false);
}

#[test]
fn test_rollup_tracker() {
    crate::run::rolluptest::_test_rollup_tracker();
}

fn test_call_to_precompile5(
    machine: &mut Machine,
    sender_addr: &Uint256,
    b: BigUint,
    e: BigUint,
    m: BigUint,
) -> Result<BigUint, ethabi::Error> {
    let b_bytes = b.to_bytes_be();
    let e_bytes = e.to_bytes_be();
    let m_bytes = m.to_bytes_be();
    let mut calldata = Uint256::from_usize(b_bytes.len()).to_bytes_be();
    calldata.extend(Uint256::from_usize(e_bytes.len()).to_bytes_be());
    calldata.extend(Uint256::from_usize(m_bytes.len()).to_bytes_be());
    calldata.extend(b_bytes);
    calldata.extend(e_bytes);
    calldata.extend(m_bytes);

    let txid = machine.runtime_env.insert_tx_message(
        sender_addr.clone(),
        Uint256::from_u64(1_000_000_000),
        Uint256::zero(),
        Uint256::from_u64(5),
        Uint256::zero(),
        &calldata,
        false,
    );
    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let _gas_used = machine.run(None);
    let logs = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(logs.len(), num_logs_before + 1);
    let receipt = logs[num_logs_before].clone();

    assert_eq!(receipt.get_request_id(), txid);
    assert!(receipt.succeeded());

    Ok(BigUint::from_bytes_be(&receipt.get_return_data()))
}

#[test]
fn test_precompile5_small() {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();
    let my_addr = Uint256::from_usize(1025);

    match test_call_to_precompile5(
        &mut machine,
        &my_addr,
        BigUint::from(2u64),
        BigUint::from(4u64),
        BigUint::from(9u64),
    ) {
        Ok(bi) => {
            assert_eq!(bi, BigUint::from(7u64));
        }
        Err(e) => {
            panic!("{}", e);
        }
    }
}

#[test]
fn test_precompile5_big() {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();
    let my_addr = Uint256::from_usize(1025);

    let mut rng = rand::thread_rng();
    let b: BigUint = rng.gen_biguint(128);
    let e: BigUint = rng.gen_biguint(32);
    let m: BigUint = rng.gen_biguint(128);
    match test_call_to_precompile5(&mut machine, &my_addr, b.clone(), e.clone(), m.clone()) {
        Ok(actual) => {
            let expected = b.modpow(&e, &m);
            println!("actual   {}", actual);
            println!("expected {}", expected);
            assert_eq!(actual, expected);
        }
        Err(e) => {
            panic!("{}", e);
        }
    }
}
