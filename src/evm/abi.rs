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

pub fn builtin_contract_path(contract_name: &str) -> String {
    format!("contracts/arbos/build/contracts/{}.json", contract_name)
}

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
        advance_time: Option<Uint256>,
        address_for_buddy: Option<Uint256>,
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

        let (request_id, sender_addr) = if let Some(buddy_addr) = address_for_buddy.clone() {
            (
                machine.runtime_env.insert_buddy_deploy_message(
                    buddy_addr.clone(),
                    Uint256::from_usize(1_000_000_000),
                    Uint256::zero(),
                    payment,
                    &augmented_code,
                ),
                buddy_addr,
            )
        } else {
            (
                machine.runtime_env.insert_tx_message(
                    Uint256::from_u64(1025),
                    Uint256::from_usize(1_000_000_000_000),
                    Uint256::zero(),
                    Uint256::zero(),
                    payment,
                    &augmented_code,
                    false,
                ),
                Uint256::from_u64(1025),
            )
        };

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

        if address_for_buddy.is_some() {
            let sends = machine.runtime_env.get_all_sends();
            if sends.len() != initial_sends_len + 1 {
                println!(
                    "deploy: expected 1 new send, got {}",
                    sends.len() - initial_sends_len
                );
                return Err(None);
            }
            let the_send = &sends[sends.len() - 1];
            if (the_send[0..32] != Uint256::from_u64(5).to_bytes_be())
                || (the_send[32..64] != sender_addr.to_bytes_be())
            {
                println!("deploy: incorrect values in send item");
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
            Uint256::from_usize(1_000_000_000_000),
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

        machine.runtime_env._insert_tx_message_from_contract(
            sender_addr,
            Uint256::from_usize(1_000_000_000_000),
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
            Uint256::from_usize(1_000_000_000_000),
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
    let abi = AbiForContract::new_from_file(&builtin_contract_path("ArbSys")).unwrap();
    let sig = abi.short_signature_for_function("withdrawEth").unwrap();
    assert_eq!(sig, [0x25u8, 0xe1u8, 0x60u8, 0x63u8]);
}

pub struct ArbSys<'a> {
    pub contract_abi: AbiForContract,
    _wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbSys<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbSys")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(100));
        ArbSys {
            contract_abi,
            _wallet: wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn _arbos_version(&self, machine: &mut Machine) -> Result<Uint256, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "arbOSVersion",
            &[],
            machine,
            Uint256::zero(),
            self._wallet,
            self.debug,
        )?;

        if (receipts.len() != 1) {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[ethabi::ParamType::Uint(256)],
                &receipts[0].get_return_data(),
            )?;
            match return_vals[0] {
                ethabi::Token::Uint(ui) => Ok(Uint256::from_u256(&ui)),
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from("reverted"))
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
            self._wallet,
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
}

pub struct _ArbInfo {
    pub contract_abi: AbiForContract,
    debug: bool,
}

impl _ArbInfo {
    pub fn _new(debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbInfo")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(101));
        _ArbInfo {
            contract_abi,
            debug,
        }
    }

    pub fn _get_balance(
        &self,
        machine: &mut Machine,
        addr: &Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::from_u64(1112),
            "getBalance",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[ethabi::ParamType::Uint(256)],
                &receipts[0].get_return_data(),
            )?;
            match return_vals[0] {
                ethabi::Token::Uint(ui) => Ok(Uint256::from_u256(&ui)),
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }
}

pub struct ArbAddressTable<'a> {
    pub contract_abi: AbiForContract,
    _wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbAddressTable<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbAddressTable")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(102));
        ArbAddressTable {
            contract_abi,
            _wallet: wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
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

    pub fn register(&self, machine: &mut Machine, addr: Uint256) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("register", machine, addr)
    }

    pub fn lookup(&self, machine: &mut Machine, addr: Uint256) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("lookup", machine, addr)
    }

    pub fn size(&self, machine: &mut Machine) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "size",
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

    pub fn lookup_index(
        &self,
        machine: &mut Machine,
        index: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "lookupIndex",
            &[ethabi::Token::Uint(ethabi::Uint::from(index.to_u256()))],
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
            &[ethabi::ParamType::Address],
            &receipts[0].get_return_data(),
        )?;

        match return_vals[0] {
            ethabi::Token::Address(addr) => Ok(Uint256::from_bytes(addr.as_bytes())),
            _ => panic!(),
        }
    }

    pub fn decompress(
        &self,
        machine: &mut Machine,
        buf: &[u8],
        offset: Uint256,
    ) -> Result<(Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "decompress",
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

    pub fn compress(&self, machine: &mut Machine, addr: Uint256) -> Result<Vec<u8>, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "compress",
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
}

pub struct ArbBLS<'a> {
    pub contract_abi: AbiForContract,
    _wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbBLS<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbBLS")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(103));
        ArbBLS {
            contract_abi,
            _wallet: wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn register(
        &self,
        machine: &mut Machine,
        x0: Uint256,
        x1: Uint256,
        y0: Uint256,
        y1: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "register",
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

    pub fn get_public_key(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<(Uint256, Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getPublicKey",
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
}

pub struct ArbFunctionTable<'a> {
    pub contract_abi: AbiForContract,
    wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbFunctionTable<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbFunctionTable")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(104));
        ArbFunctionTable {
            contract_abi,
            wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn upload(
        &self,
        machine: &mut Machine,
        func_table: &FunctionTable,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "upload",
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

    pub fn size(&self, machine: &mut Machine, addr: Uint256) -> Result<Uint256, ethabi::Error> {
        self.addr_to_uint_tx("size", machine, addr)
    }

    pub fn get(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        index: Uint256,
    ) -> Result<(Uint256, bool, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "get",
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
}

pub struct _ArbOwner<'a> {
    pub contract_abi: AbiForContract,
    wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> _ArbOwner<'a> {
    pub fn _new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbOwner")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(107));
        _ArbOwner {
            contract_abi,
            wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn _give_ownership(
        &self,
        machine: &mut Machine,
        new_owner: Uint256,
        old_owner: Option<Uint256>,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            old_owner.unwrap_or(self.my_address.clone()),
            "giveOwnership",
            &[ethabi::Token::Address(new_owner.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _add_to_reserve_funds(
        &self,
        machine: &mut Machine,
        amount: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "addToReserveFunds",
            &[],
            machine,
            amount,
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _get_fee_recipient(&self, machine: &mut Machine) -> Result<Uint256, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getFeeRecipient",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if !receipts[0].succeeded() {
            return Err(ethabi::Error::from("reverted"));
        }

        let return_vals = ethabi::decode(
            &[ethabi::ParamType::Address],
            &receipts[0].get_return_data(),
        )?;

        match &return_vals[0] {
            ethabi::Token::Address(addr) => Ok(Uint256::from_bytes(addr.as_bytes())),
            _ => panic!(),
        }
    }

    pub fn _set_fee_recipient(
        &self,
        machine: &mut Machine,
        recipient: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setFeeRecipient",
            &[ethabi::Token::Address(recipient.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _get_fee_rates(
        &self,
        machine: &mut Machine,
    ) -> Result<(Uint256, Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getFeeRates",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
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
                Uint256::from_u256(&ui0),
                Uint256::from_u256(&ui1),
                Uint256::from_u256(&ui2),
                Uint256::from_u256(&ui3),
            )),
            _ => panic!(),
        }
    }

    pub fn _set_fee_rates(
        &self,
        machine: &mut Machine,
        num1: Uint256,
        denom1: Uint256,
        num2: Uint256,
        denom2: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setFeeRates",
            &[
                ethabi::Token::Uint(num1.to_u256()),
                ethabi::Token::Uint(denom1.to_u256()),
                ethabi::Token::Uint(num2.to_u256()),
                ethabi::Token::Uint(denom2.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _set_blocks_per_send(
        &self,
        machine: &mut Machine,
        blocks_per_send: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setBlocksPerSend",
            &[ethabi::Token::Uint(blocks_per_send.to_u256())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _change_sequencer(
        &self,
        machine: &mut Machine,
        sequencer_addr: Uint256,
        delay_blocks: Uint256,
        delay_seconds: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "changeSequencer",
            &[
                ethabi::Token::Address(sequencer_addr.to_h160()),
                ethabi::Token::Uint(delay_blocks.to_u256()),
                ethabi::Token::Uint(delay_seconds.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _get_fee_maxes(
        &self,
        machine: &mut Machine,
    ) -> Result<(Uint256, Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getFeeMaxes",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
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
                Uint256::from_u256(&ui0),
                Uint256::from_u256(&ui1),
                Uint256::from_u256(&ui2),
                Uint256::from_u256(&ui3),
            )),
            _ => panic!(),
        }
    }

    pub fn _set_fee_maxes(
        &self,
        machine: &mut Machine,
        num1: Uint256,
        denom1: Uint256,
        num2: Uint256,
        denom2: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setFeeMaxes",
            &[
                ethabi::Token::Uint(num1.to_u256()),
                ethabi::Token::Uint(denom1.to_u256()),
                ethabi::Token::Uint(num2.to_u256()),
                ethabi::Token::Uint(denom2.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _start_code_upload(&self, machine: &mut Machine) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            "startCodeUpload",
            &[],
            machine,
            Uint256::zero(),
            self.wallet,
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _continue_code_upload(
        &self,
        machine: &mut Machine,
        marshalled_code: Vec<u8>,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "continueCodeUpload",
            &[ethabi::Token::Bytes(marshalled_code)],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _finish_code_upload_as_arbos_upgrade(
        &self,
        machine: &mut Machine,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "finishCodeUploadAsArbosUpgrade",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Err(ethabi::Error::from(
                "should have reverted but actually succeeded",
            ))
        } else {
            Ok(())
        }
    }

    pub fn _finish_code_upload_as_pluggable(
        &self,
        machine: &mut Machine,
        id: Uint256,
        keep_state: bool,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "finishCodeUploadAsPluggable",
            &[
                ethabi::Token::Uint(id.to_u256()),
                ethabi::Token::Bool(keep_state),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }
}

pub struct ArbosTest {
    pub contract_abi: AbiForContract,
    debug: bool,
}

impl ArbosTest {
    pub fn new(debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbosTest")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(105));
        ArbosTest {
            contract_abi,
            debug,
        }
    }

    pub fn _install_account_and_call(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        balance: Uint256,
        nonce: Uint256,
        code: Vec<u8>,
        storage: Vec<u8>,
        calldata: Vec<u8>,
    ) -> Result<Vec<u8>, ethabi::Error> {
        self.install_account(
            machine,
            addr.clone(),
            balance.clone(),
            nonce.clone(),
            Some(code),
            Some(storage),
        )?;
        self.call(machine, Uint256::zero(), addr.clone(), calldata, balance)?;
        self._get_marshalled_storage(machine, addr)
    }

    pub fn install_account(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        balance: Uint256,
        nonce: Uint256,
        code: Option<Vec<u8>>,
        storage: Option<Vec<u8>>,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "installAccount",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Bool((code == None) && (storage == None)),
                ethabi::Token::Uint(balance.to_u256()),
                ethabi::Token::Uint(nonce.to_u256()),
                ethabi::Token::Bytes(code.unwrap_or(vec![])),
                ethabi::Token::Bytes(storage.unwrap_or(vec![])),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn call(
        &self,
        machine: &mut Machine,
        caller_addr: Uint256,
        callee_addr: Uint256,
        calldata: Vec<u8>,
        callvalue: Uint256,
    ) -> Result<Vec<u8>, ethabi::Error> {
        machine.runtime_env.insert_eth_deposit_message(
            Uint256::zero(),
            caller_addr.clone(),
            callvalue.clone(),
        );
        let _tx_id = machine.runtime_env.insert_tx_message(
            caller_addr,
            Uint256::from_usize(1000000000),
            Uint256::zero(),
            callee_addr,
            callvalue,
            &calldata,
            false,
        );
        let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
        let num_sends_before = machine.runtime_env.get_all_sends().len();
        let _arbgas_used = if self.debug {
            machine.debug(None)
        } else {
            machine.run(None)
        };
        let logs = machine.runtime_env.get_all_receipt_logs();
        let sends = machine.runtime_env.get_all_sends();

        if (logs.len() != num_logs_before + 1) || (sends.len() != num_sends_before) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !logs[num_logs_before].succeeded() {
            println!(
                "arbosTest.run revert code {}",
                logs[num_logs_before].get_return_code()
            );
            return Err(ethabi::Error::from("reverted"));
        }

        Ok(logs[num_logs_before].get_return_data())
    }

    pub fn get_account_info(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<(Uint256, Uint256, Vec<u8>), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getAccountInfo",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            let return_data = receipts[0].get_return_data();
            Ok((
                Uint256::from_bytes(&return_data[0..32]),
                Uint256::from_bytes(&return_data[32..64]),
                return_data[64..].to_vec(),
            ))
        } else {
            println!(
                "arbosTest.run revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _get_marshalled_storage(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Vec<u8>, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getMarshalledStorage",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            return Ok(receipts[0].get_return_data());
        } else {
            println!(
                "arbosTest.run revert code {}",
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
