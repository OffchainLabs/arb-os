/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Value;
use crate::run::{ArbosReceipt, Machine};
use crate::uint256::Uint256;
use ethers_core::utils::keccak256;
use ethers_signers::Signer;
use ethers_signers::Wallet;
use std::{fs::File, io::Read, path::Path};

#[derive(Debug, Clone)]
pub struct AbiForContract {
    code_bytes: Vec<u8>,
    contract: ethabi::Contract,
    pub address: Uint256,
    name: String,
}

impl AbiForContract {
    pub fn new_from_file(filename: &str) -> Result<Self, ethabi::Error> {
        let path = Path::new(filename);
        let mut file = match File::open(path) {
            Ok(f) => f,
            Err(e) => {
                return Err(ethabi::Error::from(e.to_string()));
            }
        };
        let mut s = String::new();
        s = match file.read_to_string(&mut s) {
            Err(why) => {
                return Err(ethabi::Error::from(why.to_string()));
            }
            Ok(_) => s,
        };

        let parse_result: Result<serde_json::Value, serde_json::Error> = serde_json::from_str(&s);
        let json_from_file = match parse_result {
            Ok(v) => v,
            Err(e) => {
                return Err(ethabi::Error::from(e.to_string()));
            }
        };

        if let serde_json::Value::Object(fields) = json_from_file {
            let json_abi = match fields.get("abi") {
                Some(val) => val,
                None => {
                    return Err(ethabi::Error::from("no abi key in json"));
                }
            };

            let contract: ethabi::Contract = match serde_json::from_value(json_abi.clone()) {
                Ok(c) => c,
                Err(e) => {
                    return Err(ethabi::Error::from(e.to_string()));
                }
            };

            let name = match &fields.get("contractName") {
                Some(serde_json::Value::String(s)) => s,
                _ => {
                    return Err(ethabi::Error::from("no name key in json"));
                }
            };

            let decoded_insns = match &fields.get("bytecode") {
                Some(val) => {
                    let code_str = val.as_str().unwrap().to_string();
                    hex::decode(&code_str[2..]).unwrap()
                }
                None => {
                    return Err(ethabi::Error::from("no code key in json"));
                }
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
        deploy_as_buddy: bool,
        debug: bool,
    ) -> Result<Uint256, Option<ArbosReceipt>> {
        let initial_logs_len = machine.runtime_env.get_all_receipt_logs().len();
        let initial_sends_len = machine.runtime_env.get_all_sends().len();
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

        let sender_addr = Uint256::from_usize(1025);
        let request_id = if deploy_as_buddy {
            machine.runtime_env.insert_buddy_deploy_message(
                sender_addr.clone(),
                Uint256::from_usize(1_000_000_000),
                Uint256::zero(),
                payment,
                &augmented_code,
            )
        } else {
            machine.runtime_env.insert_tx_message(
                sender_addr.clone(),
                Uint256::from_usize(1_000_000_000),
                Uint256::zero(),
                Uint256::zero(),
                payment,
                &augmented_code,
            )
        };

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

        if deploy_as_buddy {
            let sends = machine.runtime_env.get_all_sends();
            if sends.len() != initial_sends_len + 1 {
                println!(
                    "deploy: expected 1 new send, got {}",
                    sends.len() - initial_sends_len
                );
                return Err(None);
            }
            if let Value::Tuple(tup) = &sends[sends.len() - 1] {
                if (tup[0] != Value::Int(Uint256::from_usize(5)))
                    || (tup[1] != Value::Int(sender_addr))
                {
                    println!("deploy: incorrect values in send item");
                    return Err(None);
                }
            } else {
                println!("malformed send item");
                return Err(None);
            }
        }

        let log_item = &logs[logs.len() - 1];
        if !log_item.succeeded() {
            return Err(Some(log_item.clone()));
        }
        if let Value::Tuple(tup2) = log_item.get_request() {
            assert_eq!(tup2[4], Value::Int(request_id));
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

    pub fn call_function(
        &self,
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        debug: bool,
    ) -> Result<(Vec<ArbosReceipt>, Vec<Value>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        machine.runtime_env.insert_tx_message(
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            &calldata,
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
    ) -> Result<(Vec<ArbosReceipt>, Vec<Value>), ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let (tx_contents, _tx_id_bytes) =
            machine.runtime_env.make_compressed_and_signed_l2_message(
                Uint256::zero(),
                Uint256::from_usize(1_000_000_000_000),
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
        sender_addr: Uint256,
        func_name: &str,
        args: &[ethabi::Token],
        machine: &mut Machine,
        payment: Uint256,
        wallet: &Wallet,
    ) -> Result<Uint256, ethabi::Error> {
        let this_function = self.contract.function(func_name)?;
        let calldata = this_function.encode_input(args).unwrap();

        let tx_id_bytes = machine.runtime_env.append_signed_tx_message_to_batch(
            batch,
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
            Uint256::zero(),
            self.address.clone(),
            payment,
            calldata,
            &wallet,
        );

        Ok((Uint256::from_bytes(&tx_id_bytes)))
    }

    #[cfg(test)]
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
                Uint256::from_usize(1_000_000_000_000),
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
}

#[test]
fn test_function_short_signature_correct() {
    let abi = AbiForContract::new_from_file("contracts/add/build/contracts/ArbSys.json").unwrap();
    let sig = abi.short_signature_for_function("withdrawEth").unwrap();
    assert_eq!(sig, [0x25u8, 0xe1u8, 0x60u8, 0x63u8]);
}

pub struct ArbSys<'a> {
    pub contract_abi: AbiForContract,
    wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbSys<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file("contracts/add/build/contracts/ArbSys.json").unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(100));
        ArbSys {
            contract_abi,
            wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn _withdraw_eth(
        &self,
        machine: &mut Machine,
        payee_addr: Uint256,
        amount: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "withdrawEth",
            &[ethabi::Token::Address(ethabi::Address::from(
                payee_addr.to_h160(),
            ))],
            machine,
            amount,
            self.wallet,
            self.debug,
        )?;

        if (receipts.len() != 1) {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn get_transaction_count(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("getTransactionCount", machine, addr)
    }

    fn addr_to_uint_tx(
        &self,
        func_name: &str,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            func_name,
            &[ethabi::Token::Address(ethabi::Address::from(
                addr.to_h160(),
            ))],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            println!("ArbOS return code: {}", receipts[0].get_return_code());
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[ethabi::ParamType::Uint(256)],
            &receipts[0].get_return_data(),
        )?;

        match return_vals[0] {
            ethabi::Token::Uint(ui) => Ok(Uint256::from_u256(&ui)),
            _ => panic!(),
        }
    }

    pub fn address_table_register(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("addressTable_register", machine, addr)
    }

    pub fn address_table_lookup(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("addressTable_lookup", machine, addr)
    }

    pub fn address_table_size(&self, machine: &mut Machine) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "addressTable_size",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            println!(
                "addressTable_size revert code {}",
                receipts[0].get_return_code()
            );
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[ethabi::ParamType::Uint(256)],
            &receipts[0].get_return_data(),
        )?;

        match return_vals[0] {
            ethabi::Token::Uint(ui) => Ok(Uint256::from_u256(&ui)),
            _ => panic!(),
        }
    }

    pub fn address_table_lookup_index(
        &self,
        machine: &mut Machine,
        index: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "addressTable_lookupIndex",
            &[ethabi::Token::Uint(ethabi::Uint::from(index.to_u256()))],
            machine,
            Uint256::zero(),
            self.wallet,
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[ethabi::ParamType::Address],
            &receipts[0].get_return_data(),
        )?;

        match return_vals[0] {
            ethabi::Token::Address(addr) => Ok(Uint256::from_bytes(addr.as_bytes())),
            _ => panic!(),
        }
    }

    pub fn _address_table_decompress(
        &self,
        machine: &mut Machine,
        buf: &[u8],
        offset: Uint256,
    ) -> Result<(Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "addressTable_decompress",
            &[
                ethabi::Token::Bytes(buf.to_vec()),
                ethabi::Token::Uint(offset.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[ethabi::ParamType::Address, ethabi::ParamType::Uint(256)],
            &receipts[0].get_return_data(),
        )?;

        match (return_vals[0].clone(), return_vals[1].clone()) {
            (ethabi::Token::Address(addr), ethabi::Token::Uint(ui)) => Ok((
                Uint256::from_bytes(addr.as_bytes()),
                Uint256::from_u256(&ui),
            )),
            _ => panic!(),
        }
    }

    pub fn _address_table_compress(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Vec<u8>, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "addressTable_compress",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals =
            ethabi::decode(&[ethabi::ParamType::Bytes], &receipts[0].get_return_data())?;

        match &return_vals[0] {
            ethabi::Token::Bytes(buf) => Ok(buf.to_vec()),
            _ => panic!(),
        }
    }

    pub fn register_bls_key(
        &self,
        machine: &mut Machine,
        x0: Uint256,
        x1: Uint256,
        y0: Uint256,
        y1: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "registerBlsKey",
            &[
                ethabi::Token::Uint(x0.to_u256()),
                ethabi::Token::Uint(x1.to_u256()),
                ethabi::Token::Uint(y0.to_u256()),
                ethabi::Token::Uint(y1.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }
        Ok(())
    }

    pub fn get_bls_public_key(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<(Uint256, Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getBlsPublicKey",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[
                ethabi::ParamType::Uint(256),
                ethabi::ParamType::Uint(256),
                ethabi::ParamType::Uint(256),
                ethabi::ParamType::Uint(256),
            ],
            &receipts[0].get_return_data(),
        )?;

        match (
            &return_vals[0],
            &return_vals[1],
            &return_vals[2],
            &return_vals[3],
        ) {
            (
                ethabi::Token::Uint(ui0),
                ethabi::Token::Uint(ui1),
                ethabi::Token::Uint(ui2),
                ethabi::Token::Uint(ui3),
            ) => Ok((
                Uint256::from_u256(ui0),
                Uint256::from_u256(ui1),
                Uint256::from_u256(ui2),
                Uint256::from_u256(ui3),
            )),
            _ => panic!(),
        }
    }

    pub fn upload_function_table(
        &self,
        machine: &mut Machine,
        func_table: &FunctionTable,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "uploadFunctionTable",
            &[ethabi::Token::Bytes(func_table.marshal())],
            machine,
            Uint256::zero(),
            self.wallet,
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }

        Ok(())
    }

    pub fn function_table_size(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("functionTableSize", machine, addr)
    }

    pub fn function_table_get(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        index: Uint256,
    ) -> Result<(Uint256, bool, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "functionTableGet",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Uint(index.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            println!(
                "arbsys.function_table_get revert code {}",
                receipts[0].get_return_code()
            );
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[
                ethabi::ParamType::Uint(256),
                ethabi::ParamType::Bool,
                ethabi::ParamType::Uint(256),
            ],
            &receipts[0].get_return_data(),
        )?;

        match (&return_vals[0], &return_vals[1], &return_vals[2]) {
            (
                ethabi::Token::Uint(func_code),
                ethabi::Token::Bool(is_payable),
                ethabi::Token::Uint(gas_limit),
            ) => Ok((
                Uint256::from_u256(func_code),
                *is_payable,
                Uint256::from_u256(gas_limit),
            )),
            _ => panic!(),
        }
    }
}

pub struct ArbAggregator<'a> {
    pub contract_abi: AbiForContract,
    _wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbAggregator<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file("contracts/add/build/contracts/ArbAggregator.json")
                .unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(106));
        ArbAggregator {
            contract_abi,
            _wallet: wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn register_as_aggregator(
        &self,
        machine: &mut Machine,
        fee_per_byte: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "registerAsAggregator",
            &[ethabi::Token::Uint(fee_per_byte.to_u256())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if receipts[0].succeeded() {
            Ok(())
        } else {
            println!(
                "arb_aggregator.register_as_aggregator revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _set_aggregator_fee(
        &self,
        machine: &mut Machine,
        fee_per_byte: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setAggregatorFee",
            &[ethabi::Token::Uint(fee_per_byte.to_u256())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if receipts[0].succeeded() {
            Ok(())
        } else {
            println!(
                "arb_aggregator.set_aggregator_fee revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _get_aggregator_fee(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setAggregatorFee",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            println!(
                "arb_aggregator.set_aggregator_fee revert code {}",
                receipts[0].get_return_code()
            );
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[ethabi::ParamType::Uint(256)],
            &receipts[0].get_return_data(),
        )?;

        match &return_vals[0] {
            ethabi::Token::Uint(fee) => Ok(Uint256::from_u256(fee)),
            _ => panic!(),
        }
    }

    pub fn _withdraw_as_aggregator(&self, machine: &mut Machine) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "withdrawAsAggregator",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if receipts[0].succeeded() {
            Ok(())
        } else {
            println!(
                "arb_aggregator.withdraw_as_aggregator revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _register_as_client(
        &self,
        machine: &mut Machine,
        aggregator_addr: Uint256,
        max_price: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "registerAsClient",
            &[
                ethabi::Token::Uint(aggregator_addr.to_u256()),
                ethabi::Token::Uint(max_price.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if receipts[0].succeeded() {
            Ok(())
        } else {
            println!(
                "arb_aggregator.register_as_client revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _get_client_info(
        &self,
        machine: &mut Machine,
        client_addr: Uint256,
        aggregator_addr: Uint256,
    ) -> Result<(bool, Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getClientInfo",
            &[
                ethabi::Token::Uint(client_addr.to_u256()),
                ethabi::Token::Uint(aggregator_addr.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !receipts[0].succeeded() {
            println!(
                "arb_aggregator.get_client_info revert code {}",
                receipts[0].get_return_code()
            );
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[
                ethabi::ParamType::Bool,
                ethabi::ParamType::Uint(256),
                ethabi::ParamType::Uint(256),
            ],
            &receipts[0].get_return_data(),
        )?;

        match (&return_vals[0], &return_vals[1], &return_vals[2]) {
            (ethabi::Token::Bool(b), ethabi::Token::Uint(u1), ethabi::Token::Uint(u2)) => {
                Ok((*b, Uint256::from_u256(u1), Uint256::from_u256(u2)))
            }
            _ => panic!(),
        }
    }

    pub fn _withdraw_as_client(
        &self,
        machine: &mut Machine,
        aggregator_addr: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "withdrawAsClient",
            &[ethabi::Token::Uint(aggregator_addr.to_u256())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if receipts[0].succeeded() {
            Ok(())
        } else {
            println!(
                "arb_aggregator.withdraw_as_client revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }
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
