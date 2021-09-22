/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::miniconstants::init_constant_table;
use crate::compile::DebugInfo;
use crate::console::Color;
use crate::evm::abi::ArbSys;
use crate::evm::preinstalled_contracts::{_ArbAggregator, _ArbOwner, _try_upgrade};
use crate::evm::test_contract_path2;
use crate::evm::{preinstalled_contracts::_ArbInfo, test_contract_path, AbiForContract};
use crate::mavm::Buffer;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Value};
use crate::run::runtime_env::remap_l1_sender_address;
use crate::run::RuntimeEnvironment;
use crate::run::{_bytestack_from_bytes, load_from_file, run, run_from_file, Machine};
use crate::run::{load_from_file_and_env_ret_file_info_table, MachineState};
use crate::uint256::Uint256;
use crate::upload::CodeUploader;
use ethers_signers::Signer;
use num_bigint::{BigUint, RandBigInt};
use rlp::RlpStream;
use std::convert::TryInto;
use std::option::Option::None;
use std::path::Path;

mod integration;

fn test_from_file_with_args_and_return(
    path: &Path,
    args: Vec<Value>,
    ret: Value,
    coverage_filename: Option<String>,
) {
    let res = run_from_file(path, args, coverage_filename, false);
    match res {
        Ok(res) => match &res[0] {
            Value::Buffer(_) if res[0] != ret => panic!("{}", &res[0].pretty_print(Color::RED)),
            _ => {
                if res[0] != ret {
                    println!("  - expected {}", ret.pretty_print(Color::RED));
                    println!("  - executed {}", res[0].pretty_print(Color::RED));
                    panic!("Unexpected result from test");
                }
            }
        },
        Err((error, trace)) => {
            println!("{}", error);
            panic!("{:?}", trace);
        }
    }
}

fn test_from_file(path: &Path, expected_return: Value) {
    test_from_file_with_args_and_return(
        path,
        vec![],
        expected_return,
        Some({
            let mut file = path.to_str().unwrap().to_string();
            let length = file.len();
            file.truncate(length - 5);
            file.replace("/", "-")
        }),
    );
}

/// Runs a .mexe file, seeing if any string-encoded error messages are reported
fn test_for_error_string(path: &Path) {
    test_from_file(path, Value::Buffer(Buffer::new_empty()))
}

/// Runs a .mexe file, seeing if any integer-encoded error messages are reported
fn test_for_numeric_error_code(path: &Path) {
    test_from_file(path, Value::Int(Uint256::from_usize(0)))
}

#[test]
fn test_arraytest() {
    test_for_error_string(Path::new("builtin/arraytest.mexe"));
}

#[test]
fn test_kvstest() {
    test_for_error_string(Path::new("builtin/kvstest.mexe"));
}

#[test]
fn test_address_set() {
    test_for_numeric_error_code(Path::new("stdlib/addressSetTest.mexe"));
}

#[test]
fn test_storage_map() {
    test_for_numeric_error_code(Path::new("stdlib/storageMapTest.mexe"));
}

#[test]
fn test_queuetest() {
    test_for_numeric_error_code(Path::new("stdlib/queuetest.mexe"));
}

#[test]
fn test_globaltest() {
    test_for_numeric_error_code(Path::new("builtin/globaltest.mexe"));
}

#[test]
fn test_pqtest() {
    test_for_numeric_error_code(Path::new("stdlib/priorityqtest.mexe"));
}

#[test]
fn test_bytearray() {
    test_for_numeric_error_code(Path::new("stdlib/bytearraytest.mexe"));
}

#[test]
fn test_map() {
    test_for_error_string(Path::new("builtin/maptest.mexe"));
}

#[test]
fn test_keccak() {
    test_for_numeric_error_code(Path::new("stdlib/keccaktest.mexe"));
}

#[test]
fn test_bls() {
    test_for_error_string(Path::new("stdlib/blstest.mexe"));
}

#[test]
fn test_sha256() {
    test_for_numeric_error_code(Path::new("stdlib/sha256test.mexe"));
}

#[test]
fn test_fixedpoint() {
    test_for_numeric_error_code(Path::new("stdlib/fixedpointtest.mexe"));
}

#[test]
fn test_ripemd160() {
    test_for_numeric_error_code(Path::new("stdlib/ripemd160test.mexe"));
}

#[test]
fn test_biguint() {
    test_for_numeric_error_code(Path::new("stdlib/biguinttest.mexe"));
}

#[test]
fn test_expanding_int_array() {
    test_for_numeric_error_code(Path::new("stdlib/expandingIntArrayTest.mexe"));
}

#[test]
fn test_rlp() {
    let mut ui = Uint256::one();
    for _i in 0..100 {
        test_rlp_uint(ui.clone(), ui.rlp_encode(), None);
        let ui2 = ui.div(&Uint256::from_usize(2048)).unwrap(); // a valid address
        test_rlp_uint(ui2.clone(), ui2.rlp_encode(), None);
        ui = ui
            .mul(&Uint256::from_usize(19482103))
            .add(&Uint256::from_usize(91));
    }
    let ui = Uint256::from_usize(4313412);
    test_rlp_uint(ui.clone(), ui.rlp_encode(), None);

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
    for (i, testvec) in byte_testvecs.iter().enumerate() {
        let res = rlp::encode(&*testvec);
        test_rlp_bytearray(testvec.to_vec(), res, Some(format!("rlptest_bv_{}", i)));
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
    for (i, testvec) in list3_testvecs.iter().enumerate() {
        let res = encode_list3(testvec.clone());
        test_rlp_list3(testvec.clone(), res, Some(format!("rlptest_ls_{}", i)));
    }
}

fn encode_list3(testvec: (Uint256, Vec<u8>, Uint256)) -> Vec<u8> {
    let mut stream = RlpStream::new_list(3);
    stream
        .append(&testvec.0.to_bytes_minimal())
        .append(&testvec.1)
        .append(&testvec.2.to_bytes_minimal());
    stream.out()
}

fn test_rlp_uint(ui: Uint256, correct_result: Vec<u8>, coverage_filename: Option<String>) {
    test_from_file_with_args_and_return(
        Path::new("stdlib/rlptest.mexe"),
        vec![Value::Int(Uint256::zero()), Value::Int(ui)],
        _bytestack_from_bytes(&correct_result),
        coverage_filename,
    );
}

fn test_rlp_bytearray(input: Vec<u8>, correct_result: Vec<u8>, coverage_filename: Option<String>) {
    test_from_file_with_args_and_return(
        Path::new("stdlib/rlptest.mexe"),
        vec![Value::Int(Uint256::one()), _bytestack_from_bytes(&input)],
        _bytestack_from_bytes(&correct_result),
        coverage_filename,
    );
}

fn test_rlp_list3(
    testvec: (Uint256, Vec<u8>, Uint256),
    correct_result: Vec<u8>,
    coverage_file: Option<String>,
) {
    test_from_file_with_args_and_return(
        Path::new("stdlib/rlptest.mexe"),
        vec![
            Value::Int(Uint256::from_usize(2)),
            Value::new_tuple(vec![
                Value::Int(testvec.0),
                _bytestack_from_bytes(&testvec.1),
                Value::Int(testvec.2),
            ]),
        ],
        _bytestack_from_bytes(&correct_result),
        coverage_file,
    );
}

#[test]
fn test_codeload() {
    test_for_numeric_error_code(Path::new("minitests/codeloadtest.mexe"));
}

#[test]
fn test_closures() {
    test_for_error_string(Path::new("minitests/simple-closure.mexe"));
    test_for_error_string(Path::new("minitests/closure.mexe"));
}

#[test]
fn test_direct_deploy_add() {
    crate::evm::evm_direct_deploy_add(None, false);
}

#[test]
fn test_extcodesize_of_constructor() {
    crate::evm::evm_test_extcodesize_of_constructor(None);
}

#[test]
fn test_sha256_precompile() {
    crate::evm::evm_eval_sha256(None, false);
}

#[test]
fn test_ecpairing_precompile() {
    crate::evm::evm_ecpairing_precompile(None, false);
}

#[test]
fn test_ripemd160_precompile() {
    crate::evm::evm_eval_ripemd160(None, false);
}

/*  Disabled this test because the format it uses is no longer supported. Aggregator testing
    will still exercise this functionality.
#[test]
fn test_non_eip155_signed_tx() {
    crate::evm::evm_deploy_using_non_eip159_signature(None, false).unwrap();
}
*/

#[test]
fn test_direct_deploy_and_call_add() {
    let _log = crate::evm::evm_direct_deploy_and_call_add(None, false);
}

#[test]
fn test_call_from_contract() {
    let _log = crate::evm::evm_test_contract_call(None, false);
}

#[test]
fn test_direct_deploy_and_compressed_call_add() {
    let _log = crate::evm::evm_direct_deploy_and_compressed_call_add(None, false);
}

#[test]
fn test_payment_in_constructor() {
    crate::evm::evm_test_payment_in_constructor(None, false);
}

#[test]
fn test_block_num_consistency() {
    let _ = crate::evm::evm_block_num_consistency_test(false).unwrap();
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
fn test_rate_control() {
    let _ = crate::evm::preinstalled_contracts::evm_test_rate_control(None, false);
}

#[test]
fn test_function_table_access() {
    crate::evm::evm_test_function_table_access(None, false).unwrap();
}

#[test]
fn test_l2_to_l1_call() {
    crate::evm::evm_test_callback(None, false).unwrap();
}

#[test]
fn test_evm_add_code() {
    crate::evm::basic_evm_add_test(None, false).unwrap();
}

#[test]
pub fn test_tx_with_deposit() {
    match crate::evm::evm_tx_with_deposit(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_crosscontract_call_with_constructors() {
    match crate::evm::evm_xcontract_call_with_constructors(None, false, false) {
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
pub fn test_crosscontract_call_using_compressed_batch() {
    match crate::evm::evm_xcontract_call_using_batch(None, false, false) {
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
    assert!(crate::evm::underfunded_nested_call_test(None, false).is_ok());
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
        None,
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
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);
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

    machine.write_coverage("test_precompile5_small".to_string());
}

#[test]
fn test_precompile5_big() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);
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

    machine.write_coverage("test_precompile5_big".to_string());
}

#[test]
fn reinterpret_register() {
    let mut old_machine = load_from_file(Path::new("upgradetests/regcopy_old.mexe"));
    let _ = run(&mut old_machine, vec![], false, None);
    let mut new_machine = load_from_file(Path::new("upgradetests/regcopy_new.mexe"));
    run(&mut new_machine, vec![old_machine.register], false, None).unwrap();
    assert_eq!(
        *new_machine.stack_top().unwrap(),
        Value::Int(Uint256::one())
    );
}

#[test]
fn small_upgrade() {
    let mut machine = load_from_file(Path::new("upgradetests/upgrade1_old.mexe"));
    let uploader = CodeUploader::_new_from_file(Path::new("upgradetests/upgrade1_new.mexe"));
    let code_bytes = uploader._to_flat_vec();
    let msg = Value::new_tuple(vec![
        Value::Int(Uint256::from_usize(code_bytes.len())),
        Value::new_buffer(code_bytes),
    ]);
    machine.start_coverage();
    machine.runtime_env.insert_full_inbox_contents(vec![msg]);
    let _ = run(&mut machine, vec![], false, None);

    //let mut new_machine = load_from_file(Path::new("upgradetests/regcopy_new.mexe"), rt_env);
    //run(&mut new_machine, vec![machine.register], false).unwrap();
    println!("Machine state after: {:?}", machine.state);
    assert_eq!(
        *machine.stack_top().unwrap(),
        Value::Int(Uint256::from_u64(42))
    );
    machine.write_coverage("small_upgrade".to_string());
}

#[test]
fn small_upgrade_auto_remap() {
    let mut machine = load_from_file(Path::new("looptest/upgrade2_old.mexe"));
    let uploader = CodeUploader::_new_from_file(Path::new("looptest/upgrade2_new.mexe"));
    let code_bytes = uploader._to_flat_vec();
    let msg = Value::new_tuple(vec![
        Value::Int(Uint256::from_usize(code_bytes.len())),
        Value::new_buffer(code_bytes),
    ]);
    machine.start_coverage();
    machine.runtime_env.insert_full_inbox_contents(vec![msg]);
    let _ = run(&mut machine, vec![], false, None);

    println!("Machine state after: {:?}", machine.state);
    assert_eq!(
        *machine.stack_top().unwrap(),
        Value::Int(Uint256::from_u64(42))
    );
    machine.write_coverage("small_upgrade_auto_remap".to_string());
}

#[test]
fn test_gasleft_with_delegatecall() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);
    let my_addr = Uint256::from_u64(1025);

    let mut greeter_contract =
        AbiForContract::new_from_file(&test_contract_path2("Delegator", "Greeter")).unwrap();
    if greeter_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy Greeter contract");
    }

    let mut delegator_contract =
        AbiForContract::new_from_file(&test_contract_path2("Delegator", "Delegator")).unwrap();
    if delegator_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy Delegator contract");
    }

    let (receipts, _) = delegator_contract
        .call_function(
            my_addr,
            "testDelegate",
            &[ethabi::Token::Address(greeter_contract.address.to_h160())],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();

    assert_eq!(receipts.len(), 1);
    let logs = receipts[0]._get_evm_logs();
    assert_eq!(logs.len(), 2);
    let gasleft_before = Uint256::from_bytes(&logs[0].data);
    let gasleft_after = Uint256::from_bytes(&logs[1].data);
    assert!(gasleft_before > gasleft_after);

    machine.write_coverage("test_gasleft_with_delegatecall".to_string());
}

#[test]
pub fn test_malformed_upgrade() {
    macro_rules! opcode {
        ($opcode:ident) => {
            Instruction::from_opcode(AVMOpcode::$opcode, DebugInfo::default())
        };
        ($opcode:ident, $immediate:expr) => {
            Instruction::from_opcode_imm(AVMOpcode::$opcode, $immediate, DebugInfo::default())
        };
    }

    let (mut machine, _) = load_from_file_and_env_ret_file_info_table(
        Path::new("arb_os/arbos-upgrade.mexe"),
        RuntimeEnvironment::default(),
    );

    // Create a segment that will make the register's state incompatible with the upgrade,
    // then load the machine. An error should jump to this error handler, which will reveal
    // whether errorHandler_setUpgradeProtector() restored the state of the globals.

    let mut prelude = vec![
        opcode!(ErrCodePoint),
        opcode!(PushInsn, Value::from(AVMOpcode::Halt.to_number())), // Log the state of the register,
        opcode!(PushInsn, Value::from(AVMOpcode::Log.to_number())), //  ensuring the old globals (1937)
        opcode!(PushInsn, Value::from(AVMOpcode::Rpush.to_number())), // was restored by the protector
        opcode!(ErrSet),
        opcode!(ErrPush),
        opcode!(Log),
        opcode!(Rset, Value::from(1937_usize)), // will induce an error during the upgrade
        opcode!(Jump, Value::CodePoint(CodePt::Internal(0))),
    ];

    prelude.reverse(); // the rust emulator requires this

    machine.state = MachineState::Running(CodePt::InSegment(1, prelude.len() - 1));
    machine.code.segments.push(prelude);

    machine.start_coverage();
    machine.run(None);

    // ensure globals are restored
    assert_eq!(machine.runtime_env.logs[1], Value::from(1937_usize));

    // ensure error handler is restored
    assert_eq!(
        machine.runtime_env.logs[0],
        Value::CodePoint(machine.err_codepoint)
    );

    machine.write_coverage("test_malformed_upgrade".to_string());
}

#[test]
pub fn test_if_still_upgradable() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos-upgrade.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    if add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy Add contract");
    }

    let arbowner = _ArbOwner::_new(&wallet, false);
    let arbsys = ArbSys::new(&wallet, false);
    assert_eq!(
        arbsys.arbos_version(&mut machine)?,
        *init_constant_table(Some(Path::new("arb_os/constants.json")))
            .unwrap()
            .get("ArbosVersionNumber")
            .unwrap()
    );
    arbowner._add_chain_owner(&mut machine, remap_l1_sender_address(my_addr), true, false)?;

    let log_value = Value::new_tuple(vec![Value::from(1937_usize)]);

    let mut uploader = CodeUploader::_new(1);
    uploader._serialize_one(&Instruction::from_opcode_imm(
        AVMOpcode::Log,
        log_value.clone(),
        DebugInfo::default(),
    ));

    _try_upgrade(&arbowner, &mut machine, uploader, None)?;

    assert_eq!(machine.runtime_env.logs.last(), Some(&log_value));

    machine.write_coverage("test_if_still_upgradable".to_string());
    Ok(())
}

#[test]
pub fn evm_test_memory_charges() {
    let small_memory_balance = balance_after_memory_usage(1000);
    let large_memory_balance = balance_after_memory_usage(1500);
    assert!(large_memory_balance < small_memory_balance);
}

#[cfg(test)]
fn balance_after_memory_usage(usage: u64) -> Uint256 {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);
    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let contract = match AbiForContract::new_from_file(&test_contract_path("MemoryUsage")) {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, false);
            if let Ok(contract_addr) = result {
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

    machine.runtime_env.insert_eth_deposit_message(
        contract.address.clone(),
        contract.address.clone(),
        Uint256::_from_eth(100),
        false,
    );
    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::_from_eth(100),
        true,
    );
    let _ = machine.run(None);

    let arbowner = _ArbOwner::_new(&wallet, false);
    arbowner
        ._set_fees_enabled(&mut machine, true, true)
        .unwrap();

    let (receipts, _) = contract
        .call_function(
            my_addr.clone(),
            "test",
            &[ethabi::Token::Uint(Uint256::from_u64(usage).to_u256())],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());

    arbowner
        ._set_fees_enabled(&mut machine, false, true)
        .unwrap();

    let arbinfo = _ArbInfo::_new(false);
    let balance = arbinfo
        ._get_balance(&mut machine, &remap_l1_sender_address(my_addr))
        .unwrap();
    machine.write_coverage("balance_after_memory_usage".to_string());
    balance
}

#[test]
fn test_gas_estimation_non_preferred_aggregator() {
    test_gas_estimation(false);
}

#[test]
fn test_gas_estimation_preferred_aggregator() {
    test_gas_estimation(true);
}

#[cfg(test)]
fn test_gas_estimation(use_preferred_aggregator: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);
    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let aggregator = Uint256::from_u64(341348);

    machine.runtime_env.insert_eth_deposit_message(
        Uint256::zero(),
        my_addr.clone(),
        Uint256::_from_eth(10000),
        false,
    );
    machine.runtime_env.insert_eth_deposit_message(
        Uint256::zero(),
        aggregator.clone(),
        Uint256::_from_eth(10000),
        false,
    );
    let _ = machine.run(None);

    let contract = match AbiForContract::new_from_file(&test_contract_path("Add")) {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, false);
            if let Ok(contract_addr) = result {
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

    if use_preferred_aggregator {
        let arbaggregator = _ArbAggregator::_new(false);
        arbaggregator
            ._set_default_aggregator(&mut machine, aggregator.clone(), None)
            .unwrap();
    }

    let arbowner = _ArbOwner::_new(&wallet, false);
    arbowner
        ._set_fees_enabled(&mut machine, true, true)
        .unwrap();

    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);
    let _ = machine.run(None);

    let (gas_estimate, estimate_fee_stats) = contract
        .estimate_gas_for_function_call(
            "add",
            &[
                ethabi::Token::Uint(Uint256::from_u64(1).to_u256()),
                ethabi::Token::Uint(Uint256::from_u64(1).to_u256()),
            ],
            &mut machine,
            Uint256::zero(),
            aggregator.clone(),
            &wallet,
            false,
        )
        .unwrap();

    for i in 0..4 {
        assert_eq!(
            estimate_fee_stats[0][i].mul(&estimate_fee_stats[1][i]),
            estimate_fee_stats[2][i]
        );
    }

    let mut batch = machine.runtime_env.new_batch();
    let _txid = contract
        ._add_function_call_to_compressed_batch(
            &mut batch,
            "add",
            &[
                ethabi::Token::Uint(Uint256::from_u64(1).to_u256()),
                ethabi::Token::Uint(Uint256::from_u64(1).to_u256()),
            ],
            &mut machine,
            Uint256::zero(),
            &wallet,
            Some(gas_estimate.add(&Uint256::from_u64(500))),
        )
        .unwrap();

    let num_receipts_before = machine.runtime_env.get_all_receipt_logs().len();

    machine
        .runtime_env
        .insert_batch_message(aggregator, &*batch);
    let _ = machine.run(None);

    let receipts = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(receipts.len(), num_receipts_before + 1);
    let receipt = receipts[num_receipts_before].clone();
    assert!(receipt.succeeded());
    let fee_stats = receipt._get_fee_stats();
    for i in 0..4 {
        assert_eq!(fee_stats[0][i].mul(&fee_stats[1][i]), fee_stats[2][i]);
    }
    machine.write_coverage("test_gas_estimation".to_string());
}

#[test]
fn test_selfdestruct_in_constructor() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut victim_contract =
        AbiForContract::new_from_file(&test_contract_path("SelfDestructor")).unwrap();
    let _ = victim_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .unwrap();

    let arbinfo = _ArbInfo::_new(false);
    assert!(
        arbinfo
            ._get_code(&mut machine, &victim_contract.address)
            .unwrap()
            .len()
            > 0
    );

    let mut actor_contract =
        AbiForContract::new_from_file(&test_contract_path("ConstructorSD")).unwrap();
    let _ = actor_contract
        .deploy(
            &[
                ethabi::Token::Address(victim_contract.address.to_h160()),
                ethabi::Token::Address(Uint256::zero().to_h160()),
            ],
            &mut machine,
            Uint256::zero(),
            None,
            false,
        )
        .unwrap();

    assert_eq!(
        arbinfo
            ._get_code(&mut machine, &victim_contract.address)
            .unwrap()
            .len(),
        0
    );

    machine.write_coverage("test_selfdestruct_in_constructor".to_string());
}

#[test]
fn test_selfdestruct() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut victim_contract =
        AbiForContract::new_from_file(&test_contract_path("SelfDestructor")).unwrap();
    let _ = victim_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .unwrap();

    let arbinfo = _ArbInfo::_new(false);
    assert!(
        arbinfo
            ._get_code(&mut machine, &victim_contract.address)
            .unwrap()
            .len()
            > 0
    );

    let mut actor_contract =
        AbiForContract::new_from_file(&test_contract_path("Destroyer")).unwrap();
    let _ = actor_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .unwrap();

    let (receipts, _) = actor_contract
        .call_function(
            Uint256::from_u64(132098125),
            "destroy",
            &[
                ethabi::Token::Address(victim_contract.address.to_h160()),
                ethabi::Token::Address(Uint256::zero().to_h160()),
            ],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());

    assert_eq!(
        arbinfo
            ._get_code(&mut machine, &victim_contract.address)
            .unwrap()
            .len(),
        0
    );

    machine.write_coverage("test_selfdestruct".to_string());
}

#[test]
fn test_l1_sender_rewrite() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_u64(80293481);

    let mut contract = AbiForContract::new_from_file(&test_contract_path("BlockNum")).unwrap();
    if let Err(receipt) = contract.deploy(&[], &mut machine, Uint256::zero(), None, false) {
        if !receipt.unwrap().succeeded() {
            panic!("unexpected failure deploying BlockNum contract");
        }
    }

    let (receipts, _) = contract
        .call_function(
            my_addr.clone(),
            "getSender",
            &[],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();

    assert_eq!(receipts.len(), 1);
    let receipt = receipts[0].clone();
    assert!(receipt.succeeded());
    let return_val = Uint256::from_bytes(&receipt.get_return_data());
    assert_eq!(return_val, remap_l1_sender_address(my_addr.clone()));

    let wallet = machine.runtime_env.new_wallet();
    let arbsys = ArbSys::new(&wallet, false);
    assert_eq!(
        arbsys
            .map_l1_sender_contract_address_to_l2_alias(
                &mut machine,
                my_addr.clone(),
                my_addr.clone()
            )
            .unwrap(),
        remap_l1_sender_address(my_addr.clone())
    );

    let (receipts, _) = contract
        .call_function(
            my_addr.clone(),
            "getL1CallerInfo",
            &[],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();
    assert_eq!(receipts.len(), 1);
    let return_data = receipts[0].get_return_data();
    assert_eq!(Uint256::from_bytes(&return_data[0..32]), Uint256::one());
    assert_eq!(Uint256::from_bytes(&return_data[32..64]), my_addr);

    let test_cases = vec![
        ("0", "0"),
        ("81759a874b3", "1111000000000000000000000000081759a885c4"),
        (
            "ffffffffffffffffffffffffffffffffffffffff",
            "1111000000000000000000000000000000001110",
        ),
    ];
    for (pre, post) in test_cases {
        let pre = Uint256::from_string_hex(pre).unwrap();
        let post = Uint256::from_string_hex(post).unwrap();
        let res = arbsys
            .map_l1_sender_contract_address_to_l2_alias(&mut machine, pre.clone(), Uint256::one())
            .unwrap();
        assert_eq!(res, post);
    }
}

#[test]
fn test_recurse_norevert() {
    test_recurse_impl(20, false);
}

#[test]
fn test_recurse_revert() {
    test_recurse_impl(20, true);
}

#[cfg(test)]
fn test_recurse_impl(depth: u64, should_revert: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_u64(80293481);

    let mut contract = AbiForContract::new_from_file(&test_contract_path("BlockNum")).unwrap();
    if let Err(receipt) = contract.deploy(&[], &mut machine, Uint256::zero(), None, false) {
        if !receipt.unwrap().succeeded() {
            panic!("unexpected failure deploying BlockNum contract");
        }
    }

    let (receipts, _) = contract
        .call_function(
            my_addr.clone(),
            "recursiveCall",
            &[
                ethabi::Token::Uint(Uint256::from_u64(depth).to_u256()),
                ethabi::Token::Bool(should_revert),
            ],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();

    assert_eq!(receipts.len(), 1);
    let receipt = receipts[0].clone();
    assert_eq!(
        receipt.get_return_code(),
        if should_revert {
            Uint256::one()
        } else {
            Uint256::zero()
        }
    );
}

#[test]
fn test_barely_out_of_gas() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut contract = AbiForContract::new_from_file(&test_contract_path("BlockNum")).unwrap();
    if let Err(receipt) = contract.deploy(&[], &mut machine, Uint256::zero(), None, false) {
        if !receipt.unwrap().succeeded() {
            panic!("unexpected failure deploying BlockNum contract");
        }
    }

    let mut low_gas = 1;
    let mut hi_gas = 1000000;
    let success_code = Uint256::zero();
    // let out_of_gas_code = Uint256::from_u64(16);

    assert!(call_deplete_gas(&mut machine, &contract, low_gas) != success_code);
    assert!(call_deplete_gas(&mut machine, &contract, hi_gas) == success_code);

    while hi_gas > low_gas + 1 {
        let mid_gas = (hi_gas + low_gas) / 2;
        if call_deplete_gas(&mut machine, &contract, mid_gas) == success_code {
            hi_gas = mid_gas;
        } else {
            low_gas = mid_gas;
        }
    }
}

#[cfg(test)]
fn call_deplete_gas(machine: &mut Machine, contract: &AbiForContract, gas: u64) -> Uint256 {
    let (receipts, _) = contract
        .call_function(
            Uint256::from_u64(80293481),
            "useGasDownTo",
            &[ethabi::Token::Uint(Uint256::from_u64(gas).to_u256())],
            machine,
            Uint256::zero(),
            false,
        )
        .unwrap();

    machine
        .runtime_env
        ._advance_time(Uint256::one(), Some(Uint256::from_u64(15)), false);

    assert_eq!(receipts.len(), 1);
    receipts[0].get_return_code()
}

#[test]
fn test_gas_refunds() {
    let index1 = Uint256::from_u64(73);
    let index2 = Uint256::from_u64(99);
    let zero = Uint256::zero();
    let one = Uint256::one();

    let cost_zeroes = measure_gas_for_alloc_dealloc(vec![&index1, &zero, &index1, &zero, &index1, &zero]);

    let cost_one_alloc = measure_gas_for_alloc_dealloc(vec![&index2, &one, &index1, &zero, &index1, &zero]);
    assert_close(&cost_one_alloc, &cost_zeroes.add(&Uint256::from_u64(200_000)));

    let cost_alloc_dealloc = measure_gas_for_alloc_dealloc(vec![&index2, &one, &index2, &zero, &index1, &zero]);
    assert_close(&cost_zeroes, &cost_alloc_dealloc);

    let cost_alloc_realloc = measure_gas_for_alloc_dealloc(vec![&index2, &one, &index2, &zero, &index2, &one]);
    assert_close(&cost_alloc_realloc, &cost_one_alloc);
}

#[cfg(test)]
fn assert_close(x: &Uint256, y: &Uint256) {
    println!("====== {} {}", x, y);
    if x > y {
        assert!(x < &y.add(&Uint256::from_u64(200)));
    } else {
        assert!(y < &x.add(&Uint256::from_u64(200)));
    }
}

#[cfg(test)]
fn measure_gas_for_alloc_dealloc(args: Vec<&Uint256>) -> Uint256 {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = wallet.address();
    let my_addr256 = Uint256::from_bytes(&my_addr.to_fixed_bytes());

    let mut contract = AbiForContract::new_from_file(&test_contract_path("BlockNum")).unwrap();
    if let Err(receipt) = contract.deploy(&[], &mut machine, Uint256::zero(), None, false) {
        if !receipt.unwrap().succeeded() {
            panic!("unexpected failure deploying BlockNum contract");
        }
    }

    machine.runtime_env.insert_eth_deposit_message(
        Uint256::zero(),
        my_addr256.clone(),
        Uint256::_from_eth(1000),
        false,
    );
    let _ = machine.run(None);

    let arbowner = _ArbOwner::_new(&wallet, false);
    arbowner._set_fees_enabled(&mut machine, true, true).unwrap();
    let _ = machine.run(None);

    let (receipts, _) = contract.call_function_compressed(
        my_addr256.clone(),
        "rewriteStorage",
        &[
            ethabi::Token::Uint(args[0].to_u256()),
            ethabi::Token::Uint(args[1].to_u256()),
            ethabi::Token::Uint(args[2].to_u256()),
            ethabi::Token::Uint(args[3].to_u256()),
            ethabi::Token::Uint(args[4].to_u256()),
            ethabi::Token::Uint(args[5].to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
        &wallet,
        false
    ).unwrap();
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());

    receipts[0].get_gas_used()
}
