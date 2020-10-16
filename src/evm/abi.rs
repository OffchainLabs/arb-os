/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Value;
use crate::run::{ArbosReceipt, Machine};
use crate::uint256::Uint256;
use ethers_signers::{Signer, Wallet};
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
        deploy_as_buddy: bool,
        debug: bool,
    ) -> Option<Uint256> {
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
                Uint256::from_usize(1_000_000_000_000),
                Uint256::zero(),
                Uint256::zero(),
                &augmented_code,
            )
        } else {
            machine.runtime_env.insert_tx_message(
                sender_addr.clone(),
                Uint256::from_usize(1_000_000_000_000),
                Uint256::zero(),
                Uint256::zero(),
                Uint256::zero(),
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
            return None;
        }

        if deploy_as_buddy {
            let sends = machine.runtime_env.get_all_sends();
            if sends.len() != initial_sends_len + 1 {
                println!(
                    "deploy: expected 1 new send, got {}",
                    sends.len() - initial_sends_len
                );
                return None;
            }
            if let Value::Tuple(tup) = &sends[sends.len() - 1] {
                if (tup[0] != Value::Int(Uint256::from_usize(5)))
                    || (tup[1] != Value::Int(sender_addr))
                {
                    println!("deploy: incorrect values in send item");
                    return None;
                }
            } else {
                println!("malformed send item");
                return None;
            }
        }

        let log_item = &logs[logs.len() - 1];
        assert!(log_item.succeeded());
        if let Value::Tuple(tup2) = log_item.get_request() {
            assert_eq!(tup2[4], Value::Int(request_id));
        } else {
            println!("Malformed ArbOS log item");
            return None;
        }
        let buf = log_item.get_return_data();
        self.address = Uint256::from_bytes(&buf);
        Some(self.address.clone())
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
    pub fn add_function_call_to_compressed_batch(
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
            .append_compressed_and_signed_tx_message_to_batch(
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
}

pub struct ArbSys<'a> {
    contract_abi: AbiForContract,
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

    pub fn withdraw_eth(
        &self,
        machine: &mut Machine,
        payee_addr: Uint256,
        amount: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
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
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            func_name,
            &[ethabi::Token::Address(ethabi::Address::from(
                addr.to_h160(),
            ))],
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
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "addressTable_size",
            &[],
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

    pub fn address_table_decompress(
        &self,
        machine: &mut Machine,
        buf: &[u8],
        offset: Uint256,
    ) -> Result<(Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "addressTable_decompress",
            &[
                ethabi::Token::Bytes(buf.to_vec()),
                ethabi::Token::Uint(offset.to_u256()),
            ],
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

    pub fn address_table_compress(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Vec<u8>, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "addressTable_compress",
            &[ethabi::Token::Address(addr.to_h160())],
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
            &[ethabi::ParamType::Bytes],
            &receipts[0].get_return_data(),
        )?;

        match &return_vals[0] {
            ethabi::Token::Bytes(buf) => Ok(buf.to_vec()),
            _ => panic!(),
        }
    }

    pub fn register_bls_key(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Vec<u8>, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "addressTable_compress",
            &[ethabi::Token::Address(addr.to_h160())],
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
            &[ethabi::ParamType::Bytes],
            &receipts[0].get_return_data(),
        )?;

        match &return_vals[0] {
            ethabi::Token::Bytes(buf) => Ok(buf.to_vec()),
            _ => panic!(),
        }
    }
}
