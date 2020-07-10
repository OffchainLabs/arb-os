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

use crate::mavm::Value;
use crate::run::{bytes_from_bytestack, bytestack_from_bytes, load_from_file, RuntimeEnvironment};
use crate::uint256::Uint256;
use abi::AbiForContract;
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;
use std::usize;

mod abi;

const EMULATION_FUNCS: [&str; 41] = [
    // If you modify this, be sure to regenerate the EVM jumptable
    "evmOp_stop",
    "evmOp_sha3",
    "evmOp_address",
    "evmOp_balance",
    "evmOp_origin",
    "evmOp_caller",
    "evmOp_callvalue",
    "evmOp_calldataload",
    "evmOp_calldatasize",
    "evmOp_calldatacopy",
    "evmOp_codesize",
    "evmOp_codecopy",
    "evmOp_extcodesize",
    "evmOp_extcodecopy",
    "evmOp_returndatasize",
    "evmOp_returndatacopy",
    "evmOp_timestamp",
    "evmOp_number",
    "evmOp_mload",
    "evmOp_mstore",
    "evmOp_mstore8",
    "evmOp_sload",
    "evmOp_sstore",
    "evmOp_getjumpaddr",
    "evmOp_msize",
    "evmOp_log0",
    "evmOp_log1",
    "evmOp_log2",
    "evmOp_log3",
    "evmOp_log4",
    "evmOp_sloadbytes",
    "evmOp_sstorebytes",
    "evmOp_ssize",
    "evmOp_call",
    "evmOp_callcode",
    "evmOp_return",
    "evmOp_delegatecall",
    "evmOp_staticcall",
    "evmOp_revert",
    "evmOp_txexecgas",
    "evmOp_selfdestruct",
];

pub fn runtime_func_name(slot: usize) -> &'static str {
    EMULATION_FUNCS[slot]
}

pub fn num_runtime_funcs() -> usize {
    EMULATION_FUNCS.len()
}

pub fn make_evm_jumptable_mini(filepath: &Path) -> Result<(), io::Error> {
    let path = Path::new(filepath);
    let display = path.display();

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why.to_string()),
        Ok(file) => file,
    };
    writeln!(file, "// Automatically generated file -- do not edit")?;
    for name in EMULATION_FUNCS.iter() {
        writeln!(file, "import func {}();", name)?;
    }
    writeln!(file)?;
    writeln!(
        file,
        "var evm_jumptable: [{}]func();",
        EMULATION_FUNCS.len()
    )?;
    writeln!(file)?;
    writeln!(file, "public func init_evm_jumptable() {{")?;
    writeln!(file, "    evm_jumptable = evm_jumptable")?;
    for (i, name) in EMULATION_FUNCS.iter().enumerate() {
        writeln!(
            file,
            "        with {{ [{}] = unsafecast<func()>({}) }}",
            i, name
        )?;
    }
    writeln!(file, "        ;")?;
    writeln!(file, "}}")?;
    write!(
        file,
        "\npublic func evm_jumptable_get(idx: uint) -> option<func()>\n{{\n"
    )?;
    write!(
        file,
        "    if (idx >= {}) {{\n        return None<func()>;\n    }} else {{\n",
        EMULATION_FUNCS.len()
    )?;
    writeln!(file, "        return Some(evm_jumptable[idx]);")?;
    write!(file, "    }}\n}}\n")?;
    Ok(())
}

#[derive(Clone)]
pub struct CallInfo<'a> {
    function_name: &'a str,
    args: &'a [ethabi::Token],
    payment: Uint256,
    mutating: bool,
}

/*
pub fn evm_load_and_call_func(
    contract_json_file_name: &str,
    other_contract_names: &[&str],
    contract_name: &str,
    function_name: &str,
    args: &[ethabi::Token],
    payment: Uint256,
    log_to: Option<&Path>,
    mutating: bool,
    debug: bool,
    profile: bool,
) -> Result<Vec<ethabi::Token>, ethabi::Error> {
    Ok(evm_load_and_call_funcs(
        contract_json_file_name,
        other_contract_names,
        contract_name,
        vec![CallInfo {
            function_name,
            args,
            payment,
            mutating,
        }]
        .as_ref(),
        log_to,
        debug,
        profile,
    )?[0]
        .clone())
}
*/

/*
pub fn evm_load_and_call_funcs(
    contract_json_file_name: &str,
    other_contract_names: &[&str],
    contract_name: &str,
    call_infos: &[CallInfo],
    log_to: Option<&Path>,
    debug: bool,
    profile: bool,
) -> Result<Vec<Vec<ethabi::Token>>, ethabi::Error> {
    let dapp_abi = match abi::AbiForDappArbCompiled::new_from_file(contract_json_file_name) {
        Ok(dabi) => dabi,
        Err(e) => {
            panic!("failed to load dapp ABI from file: {:?}", e);
        }
    };
    let mut all_contracts = Vec::new();
    for other_contract_name in other_contract_names {
        match dapp_abi.get_contract(other_contract_name) {
            Some(contract) => {
                all_contracts.push(contract);
            }
            None => {
                panic!("couldn't find contract {}", other_contract_name);
            }
        }
    }
    let this_contract = match dapp_abi.get_contract(contract_name) {
        Some(contract) => {
            all_contracts.push(contract);
            contract
        }
        None => {
            panic!("couldn't find contract {}", contract_name);
        }
    };

    let mut rt_env = RuntimeEnvironment::new();
    for contract in all_contracts {
        contract.insert_upload_message(&mut rt_env);
    }

    let mut call_funcs = Vec::new();
    for call_info in call_infos {
        let this_func = match this_contract.get_function(call_info.function_name) {
            Ok(func) => func,
            Err(e) => {
                panic!(
                    "couldn't find {} function in {} contract: {:?}",
                    call_info.function_name,
                    contract_name,
                    e.to_string()
                );
            }
        };
        call_funcs.push(this_func);

        let calldata = this_func.encode_input(call_info.args).unwrap();
        if call_info.mutating {
            rt_env.insert_txcall_message(
                Uint256::from_usize(1_000_000_000_000),
                Uint256::zero(),
                this_contract.address.clone(),
                call_info.payment.clone(),
                &calldata,
            );
        } else {
            rt_env.insert_nonmutating_call_message(
                this_contract.address.clone(),
                Uint256::from_usize(1_000_000_000_000),
                &calldata,
            );
        }
    }

    if profile {
        crate::run::profile_gen_from_file(Path::new("arb_os/arbos.mexe"), vec![], rt_env.clone());
    }
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);

    let logs = match crate::run::run(&mut machine, vec![], debug) {
        Ok(logs) => logs,
        Err(e) => {
            panic!("run failed: {:?}", e);
        }
    };

    assert_eq!(logs.len(), call_infos.len());
    let mut ret = Vec::new();
    for (i, _) in call_infos.iter().enumerate() {
        if let Value::Tuple(tup) = &logs[i] {
            println!("log number {} received: {:#?}", i, *tup);
            if let Some(result_bytes) = bytes_from_bytestack(tup[2].clone()) {
                if result_bytes.is_empty() {
                    ret.push(vec![]);
                } else {
                    ret.push(call_funcs[i].decode_output(&result_bytes)?);
                }
            } else {
                panic!("log element was not a bytestack");
            }
        } else {
            panic!("log item was not a Tuple");
        }
    }

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
    Ok(ret)
}

pub fn evm_load_add_and_verify(log_to: Option<&Path>, mutating: bool, debug: bool, profile: bool) {
    use std::convert::TryFrom;
    match evm_load_and_call_func(
        "contracts/add/compiled.json",
        vec![].as_ref(),
        "Add",
        "add",
        vec![
            ethabi::Token::Uint(ethabi::Uint::one()),
            ethabi::Token::Uint(ethabi::Uint::one()),
        ]
        .as_ref(),
        Uint256::zero(),
        log_to,
        mutating,
        debug,
        profile,
    ) {
        Ok(tokens) => match tokens[0] {
            Token::Uint(ui) => {
                assert_eq!(ui, ethabi::Uint::try_from(2).unwrap());
            }
            _ => {
                panic!("token was not a uint: {:?}", tokens[0]);
            }
        },
        Err(e) => {
            panic!("error loading and calling Add::add: {:?}", e);
        }
    }
}

pub fn evm_load_fib_and_verify(log_to: Option<&Path>, debug: bool, profile: bool) {
    use std::convert::TryFrom;
    match evm_load_and_call_func(
        "contracts/fibonacci/compiled.json",
        vec![].as_ref(),
        "Fibonacci",
        "doFib",
        vec![ethabi::Token::Uint(ethabi::Uint::try_from(5).unwrap())].as_ref(),
        Uint256::zero(),
        log_to,
        true,
        debug,
        profile,
    ) {
        Ok(tokens) => match tokens[0] {
            Token::Uint(ui) => {
                assert_eq!(ui, ethabi::Uint::try_from(8).unwrap());
            }
            _ => {
                panic!("token was not a uint: {:?}", tokens[0]);
            }
        },
        Err(e) => {
            panic!("error loading and calling Fibonacci::doFib: {:?}", e);
        }
    }
}
*/

pub fn evm_xcontract_call_with_constructors(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let rt_env = RuntimeEnvironment::new();
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine
        .runtime_env
        .insert_eth_deposit_message(Uint256::from_usize(1025), Uint256::from_usize(100000));
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
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    if let Value::Tuple(tup) = &logs[0] {
        assert_eq!(tup[1], Value::Int(Uint256::zero()));
    }

    let (logs, sends) = pc_contract.call_function(
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
    if let Value::Tuple(tup) = &logs[0] {
        assert_eq!(tup[1], Value::Int(Uint256::zero()));
    }

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }

    Ok(true)
}

pub fn evm_direct_deploy_add(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new();
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
    let rt_env = RuntimeEnvironment::new();
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    machine
        .runtime_env
        .insert_eth_deposit_message(Uint256::from_usize(1025), Uint256::from_usize(10000));
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
            if let Value::Tuple(tup) = &logs[0] {
                assert_eq!(tup[1], Value::Int(Uint256::zero()));
                match bytes_from_bytestack(tup[2].clone()) {
                    Some(result_bytes) => {
                        let decoded_result = contract
                            .get_function("getSeqNum")
                            .unwrap()
                            .decode_output(&result_bytes)
                            .unwrap();
                        assert_eq!(
                            decoded_result[0],
                            ethabi::Token::Uint(ethabi::Uint::try_from(3).unwrap())
                        );
                    }
                    None => {
                        panic!("malformed result bytestack");
                    }
                }
            } else {
                panic!("malformed log return");
            }
        }
        Err(e) => {
            panic!(e.to_string());
        }
    }

    let result = contract.call_function(
        "withdrawMyEth",
        vec![].as_ref(),
        &mut machine,
        Uint256::from_usize(5000),
        debug,
    );
    match result {
        Ok((logs, sends)) => {
            assert_eq!(logs.len(), 1);
            if let Value::Tuple(tup) = &logs[0] {
                assert_eq!(tup[1], Value::Int(Uint256::zero()));
            } else {
                panic!("malformed log");
            }
            assert_eq!(sends.len(), 1);
            let mut expected_bytes = Uint256::from_usize(1025).to_bytes_be();
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
    let rt_env = RuntimeEnvironment::new();
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

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
            if let Value::Tuple(tup) = &logs[0] {
                assert_eq!(tup[1], Value::Int(Uint256::zero()));
                match bytes_from_bytestack(tup[2].clone()) {
                    Some(result_bytes) => {
                        let decoded_result = contract
                            .get_function("add")
                            .unwrap()
                            .decode_output(&result_bytes)
                            .unwrap();
                        assert_eq!(
                            decoded_result[0],
                            ethabi::Token::Uint(ethabi::Uint::try_from(2).unwrap())
                        );
                    }
                    None => {
                        panic!("malformed result bytestack");
                    }
                }
            } else {
                panic!("malformed log return");
            }
        }
        Err(e) => {
            panic!(e.to_string());
        }
    }

    if let Some(path) = log_to {
        machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

#[cfg(test)]
pub fn mint_erc20_and_get_balance(debug: bool) {
    let token_addr = Uint256::from_usize(32563);
    let me = Uint256::from_usize(1025);
    let million = Uint256::from_usize(1000000);

    let mut rt_env = RuntimeEnvironment::new();
    rt_env.insert_erc20_deposit_message(token_addr.clone(), me.clone(), million);
    let mut calldata: Vec<u8> = vec![0x70, 0xa0, 0x82, 0x31]; // code for balanceOf method
    calldata.extend(me.to_bytes_be());
    rt_env.insert_tx_message(
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
    println!("first log item: {}", logs[logs.len() - 2]);
    println!("second log item: {}", logs[logs.len() - 1]);
    if let Value::Tuple(tup) = &logs[logs.len() - 2] {
        assert_eq!(tup[1], Value::Int(Uint256::zero()));
    } else {
        panic!("first log item was malformed");
    }
    if let Value::Tuple(tup) = &logs[logs.len() - 1] {
        assert_eq!(tup[1], Value::Int(Uint256::zero()));
    } else {
        panic!("second log item was malformed");
    }
}

pub fn make_logs_for_all_arbos_tests() {
    /*
    evm_load_add_and_verify(
        Some(Path::new("testlogs/evm_load_add_and_verify.aoslog")),
        true,
        false,
        false,
    );
    evm_load_fib_and_verify(
        Some(Path::new("testlogs/evm_load_fib_and_verify.aoslog")),
        false,
        false,
    );
     */
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
    evm_test_arbsys(Some(Path::new("testlogs/evm_test_arbsys.aoslog")), false);
}
