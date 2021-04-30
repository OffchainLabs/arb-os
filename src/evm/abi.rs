/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::evm::test_contract_path;
use crate::mavm::Value;
use crate::run::{ArbosReceipt, Machine};
use crate::uint256::Uint256;
use ethers_core::utils::keccak256;
use ethers_signers::Wallet;
use std::{fs::File, io::Read, path::Path};

pub fn contract_path(folder: &str, contract_name: &str) -> String {
    format!("{}/{}.sol/{}.json", folder, contract_name, contract_name)
}

pub fn builtin_contract_path(contract_name: &str) -> String {
    format!(
        "contracts/artifacts/arbos/builtin/{}.sol/{}.json",
        contract_name, contract_name
    )
}

#[derive(Debug, Clone)]
pub struct AbiForContract {
    pub code_bytes: Vec<u8>,
    pub contract: ethabi::Contract,
    pub address: Uint256,
    name: String,
}

impl AbiForContract {
    pub fn new_from_file(filename: &str) -> Result<Self, ethabi::Error> {
        let path = Path::new(filename);
        let mut file = File::open(path).map_err(|e| ethabi::Error::from(e.to_string()))?;
        let mut s = String::new();
        file.read_to_string(&mut s)
            .map_err(|e| ethabi::Error::from(e.to_string()))?;

        let json_from_file = serde_json::from_str::<serde_json::Value>(&s)
            .map_err(|e| ethabi::Error::from(e.to_string()))?;

        if let serde_json::Value::Object(fields) = json_from_file {
            let json_abi = fields
                .get("abi")
                .ok_or_else(|| ethabi::Error::from("no abi key in json"))?;

            let contract: ethabi::Contract = serde_json::from_value(json_abi.clone())
                .map_err(|e| ethabi::Error::from(e.to_string()))?;

            let name = fields
                .get("contractName")
                .ok_or_else(|| ethabi::Error::from("no name key in json"))?;

            let decoded_insns = {
                let code_str = fields
                    .get("bytecode")
                    .ok_or_else(|| ethabi::Error::from("no code key in json"))?
                    .as_str()
                    .unwrap()
                    .to_string();
                hex::decode(&code_str[2..]).unwrap()
            };

            Ok(AbiForContract {
                code_bytes: decoded_insns.to_vec(),
                contract,
                address: Uint256::zero(),
                name: name.to_string(),
            })
        } else {
            Err(ethabi::Error::from("json file not an array"))
        }
    }

    pub fn deploy(
        &mut self,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        advance_time: Option<Uint256>,
        debug: bool,
    ) -> Result<Uint256, Option<ArbosReceipt>> {
        let initial_logs_len = machine.runtime_env.get_all_receipt_logs().len();
        let augmented_code = if let Some(constructor) = self.contract.constructor() {
            match constructor.encode_input(self.code_bytes.clone(), args) {
                Ok(aug_code) => aug_code,
                Err(e) => {
                    panic!("couldn't encode data for constructor: {:?}", e);
                }
            }
        } else {
            self.code_bytes.clone()
        };

        let request_id = machine.runtime_env.insert_tx_message(
            Uint256::from_u64(1025),
            Uint256::from_usize(1_000_000_000),
            Uint256::zero(),
            Uint256::zero(),
            payment,
            &augmented_code,
            false,
        );

        if let Some(delta_blocks) = advance_time {
            machine
                .runtime_env
                ._advance_time(delta_blocks.clone(), None, true);
        }

        let _gas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        }; // handle this deploy message
        let logs = machine.runtime_env.get_all_receipt_logs();

        if logs.len() != initial_logs_len + 1 {
            println!(
                "deploy: expected 1 new log item, got {}",
                logs.len() - initial_logs_len
            );
            return Err(None);
        }

        let log_item = &logs[logs.len() - 1];
        if !log_item.succeeded() {
            return Err(Some(log_item.clone()));
        }
        if let Value::Tuple(tup2) = log_item.get_request() {
            assert_eq!(tup2[5], Value::Int(request_id));
        } else {
            println!("Malformed ArbOS log item");
            return Err(None);
        }
        let buf = log_item.get_return_data();
        self.address = Uint256::from_bytes(&buf);
        Ok(self.address.clone())
    }

    pub fn bind_interface_to_address(&mut self, addr: Uint256) {
        // assume that self is an interface
        // bind self to a contract at addr, assuming it implements the interface
        // after this, self.call_function etc will work as expected
        self.address = addr;
    }

    pub fn get_function(&self, name: &str) -> Result<&ethabi::Function, ethabi::Error> {
        self.contract.function(name)
    }

    pub fn _generate_calldata_for_function(
        &self,
        func_name: &str,
        args: &[ethabi::Token],
    ) -> Result<Vec<u8>, ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        Ok(this_function.encode_input(args).unwrap())
    }

    pub fn call_function(
        &self,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        debug: bool,
    ) -> Result<(Vec<ArbosReceipt>, Vec<Vec<u8>>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        machine.runtime_env.insert_tx_message(
            sender_addr,
            Uint256::from_usize(1_000_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            &calldata,
            false,
        );

        let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
        let num_sends_before = machine.runtime_env.get_all_sends().len();
        let _arbgas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        };
        let logs = machine.runtime_env.get_all_receipt_logs();
        let sends = machine.runtime_env.get_all_sends();
        Ok((
            logs[num_logs_before..].to_vec(),
            sends[num_sends_before..].to_vec(),
        ))
    }

    pub fn _send_retryable_tx(
        &self,
        sender: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        max_submission_cost: Uint256,
        credit_back_address: Option<Uint256>,
        beneficiary: Option<Uint256>,
        max_gas_immed: Option<Uint256>,
        gas_price_immed: Option<Uint256>,
    ) -> Result<(Uint256, Uint256, Option<Uint256>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let (submit_txid, retryable_txid, maybe_redeemid) =
            machine.runtime_env._insert_retryable_tx_message(
                sender.clone(),
                self.address.clone(),
                payment.clone(),
                payment,
                max_submission_cost,
                credit_back_address.unwrap_or(sender.clone()),
                beneficiary.unwrap_or(sender),
                max_gas_immed.unwrap_or(Uint256::zero()),
                gas_price_immed.unwrap_or(Uint256::zero()),
                &calldata,
            );

        Ok((submit_txid, retryable_txid, maybe_redeemid))
    }

    pub fn _call_function_from_contract(
        &self,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        debug: bool,
    ) -> Result<(Vec<ArbosReceipt>, Vec<Vec<u8>>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        machine.runtime_env.insert_tx_message_from_contract(
            sender_addr,
            Uint256::from_usize(100_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            &calldata,
            false,
        );

        let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
        let num_sends_before = machine.runtime_env.get_all_sends().len();
        let _arbgas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        };
        let logs = machine.runtime_env.get_all_receipt_logs();
        let sends = machine.runtime_env.get_all_sends();
        Ok((
            logs[num_logs_before..].to_vec(),
            sends[num_sends_before..].to_vec(),
        ))
    }

    pub fn _call_function_with_deposit(
        &self,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        debug: bool,
    ) -> Result<(Vec<ArbosReceipt>, Vec<Vec<u8>>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        machine.runtime_env.insert_tx_message(
            sender_addr,
            Uint256::from_usize(100_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            &calldata,
            true,
        );

        let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
        let num_sends_before = machine.runtime_env.get_all_sends().len();
        let _arbgas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        };
        let logs = machine.runtime_env.get_all_receipt_logs();
        let sends = machine.runtime_env.get_all_sends();
        Ok((
            logs[num_logs_before..].to_vec(),
            sends[num_sends_before..].to_vec(),
        ))
    }

    pub fn call_function_compressed(
        &self,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        wallet: &Wallet,
        debug: bool,
    ) -> Result<(Vec<ArbosReceipt>, Vec<Vec<u8>>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let (tx_contents, _tx_id_bytes) =
            machine.runtime_env.make_compressed_and_signed_l2_message(
                Uint256::zero(),
                Uint256::from_usize(100_000_000),
                self.address.clone(),
                payment,
                &calldata,
                wallet,
            );
        machine
            .runtime_env
            .insert_l2_message(sender_addr, &tx_contents, false);

        let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
        let num_sends_before = machine.runtime_env.get_all_sends().len();
        let _arbgas_used = if debug {
            machine.debug(None)
        } else {
            machine.run(None)
        };
        let logs = machine.runtime_env.get_all_receipt_logs();
        let sends = machine.runtime_env.get_all_sends();
        Ok((
            logs[num_logs_before..].to_vec(),
            sends[num_sends_before..].to_vec(),
        ))
    }

    pub fn add_function_call_to_batch(
        &self,
        batch: &mut Vec<u8>,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        wallet: &Wallet,
    ) -> Result<Uint256, ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let tx_id_bytes = machine
            .runtime_env
            ._append_compressed_and_signed_tx_message_to_batch(
                batch,
                Uint256::from_usize(100_000_000),
                Uint256::zero(),
                self.address.clone(),
                payment,
                calldata,
                &wallet,
            );

        Ok((Uint256::from_bytes(&tx_id_bytes)))
    }

    pub fn _add_function_call_to_compressed_batch(
        &self,
        batch: &mut Vec<u8>,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        wallet: &Wallet,
    ) -> Result<Uint256, ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let tx_id_bytes = machine
            .runtime_env
            ._append_compressed_and_signed_tx_message_to_batch(
                batch,
                Uint256::from_usize(100_000_000),
                Uint256::zero(),
                self.address.clone(),
                payment,
                calldata,
                &wallet,
            );

        Ok((Uint256::from_bytes(&tx_id_bytes)))
    }

    pub fn short_signature_for_function(&self, func_name: &str) -> Result<[u8; 4], ethabi::Error> {
        let long_sig = self.contract.function(func_name)?.signature();
        let short_sig = long_sig.split(":").collect::<Vec<&str>>()[0];
        let long_result = keccak256(short_sig.as_bytes());
        Ok([
            long_result[0],
            long_result[1],
            long_result[2],
            long_result[3],
        ])
    }

    pub fn append_to_compression_func_table(
        &self,
        func_table: &mut FunctionTable,
        func_name: &str,
        is_payable: bool,
        gas_limit: Uint256,
    ) -> Result<(), ethabi::Error> {
        func_table.append(
            self.short_signature_for_function(func_name)?,
            is_payable,
            gas_limit,
        );
        Ok(())
    }
    pub fn new_deployed(
        machine: &mut Machine,
        path: &str,
        args: &[ethabi::Token],
        payment: Uint256,
        advance_time: Option<Uint256>,
        debug: bool,
    ) -> Result<AbiForContract, ethabi::Error> {
        match AbiForContract::new_from_file(path) {
            Ok(mut contract) => {
                let result = contract.deploy(args, machine, payment, advance_time, debug);
                if let Ok(contract_addr) = result {
                    assert_ne!(contract_addr, Uint256::zero());
                    Ok(contract)
                } else {
                    Err(ethabi::Error::from("deploy failed"))
                }
            }
            Err(e) => Err(ethabi::Error::from(format!(
                "error loading contract: {:?}",
                e
            ))),
        }
    }
}

pub fn deploy_add(machine: &mut Machine) -> Result<AbiForContract, ethabi::Error> {
    AbiForContract::new_deployed(
        machine,
        &test_contract_path("Add"),
        &[],
        Uint256::zero(),
        None,
        false,
    )
}

#[test]
fn test_function_short_signature_correct() {
    let abi = AbiForContract::new_from_file(&builtin_contract_path("ArbSys")).unwrap();
    let sig = abi.short_signature_for_function("withdrawEth").unwrap();
    assert_eq!(sig, [0x25u8, 0xe1u8, 0x60u8, 0x63u8]);
}

struct FunctionTableItem {
    func_code: [u8; 4],
    is_payable: bool,
    gas_limit: Uint256,
}

pub struct FunctionTable {
    contents: Vec<FunctionTableItem>,
}

impl FunctionTable {
    pub fn new() -> Self {
        FunctionTable { contents: vec![] }
    }

    pub fn append(&mut self, func_code: [u8; 4], is_payable: bool, gas_limit: Uint256) {
        self.contents.push(FunctionTableItem {
            func_code,
            is_payable,
            gas_limit,
        });
    }

    pub fn marshal(&self) -> Vec<u8> {
        let mut ret = vec![];
        ret.extend(Uint256::from_usize(self.contents.len()).rlp_encode());
        for ti in &self.contents {
            ret.extend(ti.func_code.to_vec());
            ret.extend(vec![if ti.is_payable { 1u8 } else { 0u8 }]);
            ret.extend(ti.gas_limit.rlp_encode());
        }
        ret
    }
}
