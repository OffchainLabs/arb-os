use super::*;
use crate::compile::miniconstants::init_constant_table;
use crate::evm::live_code::ArbosTest;
use crate::run::{load_from_file, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use crate::upload::CodeUploader;
use ethers_core::utils::keccak256;
use ethers_signers::{Signer, Wallet};
use std::collections::hash_map::DefaultHasher;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::option::Option::None;
use std::path::Path;

pub struct ArbOwner<'a> {
    pub contract_abi: AbiForContract,
    wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbOwner<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbOwner")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(107));
        ArbOwner {
            contract_abi,
            wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn set_chain_parameter(
        &self,
        machine: &mut Machine,
        param_name: &str,
        value: Uint256,
        force_owner: bool, // force the message to come from address zero, which is an owner
    ) -> Result<(), ethabi::Error> {
        let param_id = param_id_from_name(param_name);
        let (receipts, _sends) = self.contract_abi.call_function_from_contract(
            if force_owner {
                Uint256::zero()
            } else {
                Uint256::from_u64(42894528) // any old address
            },
            "setChainParameter",
            &[
                ethabi::Token::Uint(param_id.to_u256()),
                ethabi::Token::Uint(value.to_u256()),
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
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }

    pub fn set_gas_accounting_params(
        &self,
        machine: &mut Machine,
        speed_limit: Uint256,
        gas_pool_max: Uint256,
        tx_gas_limit: Uint256,
    ) -> Result<(), ethabi::Error> {
        self.set_chain_parameter(machine, "SpeedLimitPerSecond", speed_limit, true)?;
        self.set_chain_parameter(machine, "GasPoolMax", gas_pool_max, true)?;
        self.set_chain_parameter(machine, "TxGasLimit", tx_gas_limit, true)
    }

    pub fn give_ownership(
        &self,
        machine: &mut Machine,
        new_owner: Uint256,
        force_owner: bool,
    ) -> Result<(), ethabi::Error> {
        self.set_chain_parameter(machine, "ChainOwner", new_owner, force_owner)
    }

    pub fn add_to_reserve_funds(
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

    pub fn set_fees_enabled(
        &self,
        machine: &mut Machine,
        enabled: bool,
        force_owner: bool, // force the message to come from address zero, which is an owner
    ) -> Result<(), ethabi::Error> {
        self.set_chain_parameter(
            machine,
            "FeesEnabled",
            if enabled {
                Uint256::one()
            } else {
                Uint256::zero()
            },
            force_owner,
        )
    }

    pub fn get_fee_recipients(
        &self,
        machine: &mut Machine,
    ) -> Result<(Uint256, Uint256), ethabi::Error> {
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
            &[ethabi::ParamType::Address, ethabi::ParamType::Address],
            &receipts[0].get_return_data(),
        )?;

        match (&return_vals[0], &return_vals[1]) {
            (ethabi::Token::Address(addr1), ethabi::Token::Address(addr2)) => Ok((
                Uint256::from_bytes(addr1.as_bytes()),
                Uint256::from_bytes(addr2.as_bytes()),
            )),
            _ => panic!(),
        }
    }

    pub fn set_fee_recipients(
        &self,
        machine: &mut Machine,
        recipient1: Uint256,
        recipient2: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "setFeeRecipient",
            &[
                ethabi::Token::Address(recipient1.to_h160()),
                ethabi::Token::Address(recipient2.to_h160()),
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

    pub fn set_seconds_per_send(
        &self,
        machine: &mut Machine,
        seconds_per_send: Uint256,
    ) -> Result<(), ethabi::Error> {
        self.set_chain_parameter(machine, "SecondsPerSend", seconds_per_send, true)
    }

    pub fn start_code_upload(
        &self,
        machine: &mut Machine,
        last_upgrade_hash: Option<Uint256>,
        with_check: bool,
    ) -> Result<(), ethabi::Error> {
        let arg = &[ethabi::Token::FixedBytes(
            last_upgrade_hash.unwrap_or(Uint256::zero()).to_bytes_be(),
        )];
        let (receipts, _sends) = self.contract_abi.call_function_compressed(
            self.my_address.clone(),
            if with_check {
                "startCodeUploadWithCheck"
            } else {
                "startCodeUpload"
            },
            if with_check { arg } else { &[] },
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

    pub fn continue_code_upload(
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

    pub fn get_uploaded_code_hash(&self, machine: &mut Machine) -> Result<Uint256, ethabi::Error> {
        let (receipts, _) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getUploadedCodeHash",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        Ok(Uint256::from_bytes(
            &receipts[receipts.len() - 1].get_return_data(),
        ))
    }

    pub fn finish_code_upload_as_arbos_upgrade(
        &self,
        machine: &mut Machine,
        new_code_hash: Uint256,
        previous_upgrade_code_hash: Uint256,
    ) -> Result<bool, ethabi::Error> {
        let (receipts, _) = self.contract_abi.call_function(
            self.my_address.clone(),
            "finishCodeUploadAsArbosUpgrade",
            &[
                ethabi::Token::FixedBytes(new_code_hash.to_bytes_be()),
                ethabi::Token::FixedBytes(previous_upgrade_code_hash.to_bytes_be()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        assert_eq!(receipts.len(), 1);

        Ok(receipts[0].succeeded())
    }

    pub fn deploy_contract(
        &self,
        machine: &mut Machine,
        constructor_data: &[u8],
        deemed_address: Uint256,
        deemed_nonce: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "deployContract",
            &[
                ethabi::Token::Bytes(constructor_data.to_vec()),
                ethabi::Token::Address(deemed_address.to_h160()),
                ethabi::Token::Uint(deemed_nonce.to_u256()),
            ],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn set_fair_gas_price_sender(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        set_to: bool,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setFairGasPriceSender",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Bool(set_to),
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

    pub fn is_fair_gas_price_sender(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<bool, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "isFairGasPriceSender",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(!Uint256::from_bytes(&receipts[0].get_return_data()).is_zero())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn get_all_fair_gas_price_senders(
        &self,
        machine: &mut Machine,
    ) -> Result<Vec<Uint256>, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "getAllFairGasPriceSenders",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let ret_data = receipts[0].get_return_data();
            let mut ret = vec![];
            let mut offset = 64;
            while (offset < ret_data.len()) {
                ret.push(Uint256::from_bytes(&ret_data[offset..offset + 32]));
                offset += 32;
            }
            Ok(ret)
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn allow_all_senders(&self, machine: &mut Machine) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "allowAllSenders",
            &[],
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

    pub fn allow_only_owner_to_send(&self, machine: &mut Machine) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "allowOnlyOwnerToSend",
            &[],
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

    pub fn is_allowed_sender(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<bool, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "isAllowedSender",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(!Uint256::from_bytes(&receipts[0].get_return_data()).is_zero())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn add_allowed_sender(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<bool, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "addAllowedSender",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()) != Uint256::zero())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn remove_allowed_sender(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<bool, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "removeAllowedSender",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()) != Uint256::zero())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn get_all_allowed_senders(
        &self,
        machine: &mut Machine,
    ) -> Result<Vec<Uint256>, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "getAllAllowedSenders",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let decoded =
                ethabi::decode(&[ethabi::ParamType::Bytes], &*receipts[0].get_return_data())?;

            match &decoded[0] {
                ethabi::Token::Bytes(ret_data) => {
                    let mut ret = vec![];
                    let mut offset = 0;
                    while (offset < ret_data.len()) {
                        ret.push(Uint256::from_bytes(&ret_data[offset..offset + 32]));
                        offset += 32;
                    }
                    Ok(ret)
                }
                _ => Err(ethabi::Error::from("invalid returndata encoding")),
            }
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn set_l1_gas_price_estimate(
        &self,
        machine: &mut Machine,
        price_in_gwei: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setL1GasPriceEstimate",
            &[ethabi::Token::Uint(price_in_gwei.to_u256())],
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
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }
}

fn param_id_from_name(name: &str) -> Uint256 {
    Uint256::from_bytes(&keccak256(name.as_bytes()))
}

pub struct ArbGasInfo<'a> {
    pub contract_abi: AbiForContract,
    //TODO: Check why this isnt used
    _wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> ArbGasInfo<'a> {
    pub fn new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbGasInfo")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(108));
        ArbGasInfo {
            contract_abi,
            _wallet: wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn get_prices_in_wei_with_aggregator(
        &self,
        machine: &mut Machine,
        aggregator: Uint256,
    ) -> Result<(Uint256, Uint256, Uint256, Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getPricesInWeiWithAggregator",
            &[ethabi::Token::Address(aggregator.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[
                    ethabi::ParamType::Uint(256),
                    ethabi::ParamType::Uint(256),
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
                &return_vals[4],
                &return_vals[5],
            ) {
                (
                    ethabi::Token::Uint(ui0),
                    ethabi::Token::Uint(ui1),
                    ethabi::Token::Uint(ui2),
                    ethabi::Token::Uint(ui3),
                    ethabi::Token::Uint(ui4),
                    ethabi::Token::Uint(ui5),
                ) => Ok((
                    Uint256::from_u256(&ui0),
                    Uint256::from_u256(&ui1),
                    Uint256::from_u256(&ui2),
                    Uint256::from_u256(&ui3),
                    Uint256::from_u256(&ui4),
                    Uint256::from_u256(&ui5),
                )),
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }

    pub fn get_prices_in_arbgas_with_aggregator(
        &self,
        machine: &mut Machine,
        aggregator: Uint256,
    ) -> Result<(Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getPricesInArbGasWithAggregator",
            &[ethabi::Token::Address(aggregator.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[
                    ethabi::ParamType::Uint(256),
                    ethabi::ParamType::Uint(256),
                    ethabi::ParamType::Uint(256),
                ],
                &receipts[0].get_return_data(),
            )?;

            match (&return_vals[0], &return_vals[1], &return_vals[2]) {
                (ethabi::Token::Uint(ui0), ethabi::Token::Uint(ui1), ethabi::Token::Uint(ui2)) => {
                    Ok((
                        Uint256::from_u256(&ui0),
                        Uint256::from_u256(&ui1),
                        Uint256::from_u256(&ui2),
                    ))
                }
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }

    pub fn get_gas_accounting_params(
        &self,
        machine: &mut Machine,
    ) -> Result<(Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getGasAccountingParams",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[
                    ethabi::ParamType::Uint(256),
                    ethabi::ParamType::Uint(256),
                    ethabi::ParamType::Uint(256),
                ],
                &receipts[0].get_return_data(),
            )?;

            match (&return_vals[0], &return_vals[1], &return_vals[2]) {
                (ethabi::Token::Uint(ui0), ethabi::Token::Uint(ui1), ethabi::Token::Uint(ui2)) => {
                    Ok((
                        Uint256::from_u256(&ui0),
                        Uint256::from_u256(&ui1),
                        Uint256::from_u256(&ui2),
                    ))
                }
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }

    pub fn get_l1_gas_price_estimate(
        &self,
        machine: &mut Machine,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            self.my_address.clone(),
            "getL1GasPriceEstimate",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if receipts.len() != 1 {
            return Err(ethabi::Error::from("wrong number of receipts"));
        }

        if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }

    pub fn set_l1_gas_price_estimate(
        &self,
        machine: &mut Machine,
        price_wei: Uint256,
        caller: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            caller,
            "setL1GasPriceEstimate",
            &[ethabi::Token::Uint(price_wei.to_u256())],
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
            Err(ethabi::Error::from(format!(
                "tx failed: {}",
                receipts[0]._get_return_code_text()
            )))
        }
    }
}

pub struct ArbAggregator {
    pub contract_abi: AbiForContract,
    debug: bool,
}

impl ArbAggregator {
    pub fn new(debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbAggregator")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(109));
        ArbAggregator {
            contract_abi,
            debug,
        }
    }

    pub fn get_preferred_aggregator(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<(Uint256, bool), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getPreferredAggregator",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[ethabi::ParamType::Address, ethabi::ParamType::Bool],
                &receipts[0].get_return_data(),
            )?;

            match (&return_vals[0], &return_vals[1]) {
                (ethabi::Token::Address(r1), ethabi::Token::Bool(r2)) => {
                    Ok((Uint256::from_bytes(r1.as_bytes()), *r2))
                }
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn set_preferred_aggregator(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        sender: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            sender,
            "setPreferredAggregator",
            &[ethabi::Token::Address(addr.to_h160())],
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

    pub fn get_default_aggregator(&self, machine: &mut Machine) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getDefaultAggregator",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            let return_vals = ethabi::decode(
                &[ethabi::ParamType::Address],
                &receipts[0].get_return_data(),
            )?;

            match &return_vals[0] {
                ethabi::Token::Address(r1) => Ok(Uint256::from_bytes(r1.as_bytes())),
                _ => panic!(),
            }
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn set_default_aggregator(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        sender: Option<Uint256>, // default sender is address zero
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            if let Some(s) = sender {
                s
            } else {
                Uint256::zero()
            },
            "setDefaultAggregator",
            &[ethabi::Token::Address(addr.to_h160())],
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

    pub fn get_fee_collector(
        &self,
        machine: &mut Machine,
        addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::from_u64(10892),
            "getFeeCollector",
            &[ethabi::Token::Address(addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn set_fee_collector(
        &self,
        machine: &mut Machine,
        agg_addr: Uint256,
        new_collector: Uint256,
        sender: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            sender,
            "setFeeCollector",
            &[
                ethabi::Token::Address(agg_addr.to_h160()),
                ethabi::Token::Address(new_collector.to_h160()),
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

    pub fn get_tx_base_fee(
        &self,
        machine: &mut Machine,
        agg_addr: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "getTxBaseFee",
            &[ethabi::Token::Address(agg_addr.to_h160())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn set_tx_base_fee(
        &self,
        machine: &mut Machine,
        agg_addr: Uint256,
        fee: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setTxBaseFee",
            &[
                ethabi::Token::Address(agg_addr.to_h160()),
                ethabi::Token::Uint(fee.to_u256()),
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
}

pub struct ArbReplayableTx {
    pub contract_abi: AbiForContract,
    debug: bool,
}

impl ArbReplayableTx {
    pub fn new(debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbRetryableTx")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(110));
        ArbReplayableTx {
            contract_abi,
            debug,
        }
    }

    pub fn redeem(&self, machine: &mut Machine, txid: Uint256) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "redeem",
            &[ethabi::Token::FixedBytes(txid.to_bytes_be())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() < 1) || (receipts.len() > 2) || (sends.len() != 0) {
            println!("{} receipts, {} sends", receipts.len(), sends.len());
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[receipts.len() - 1].succeeded() {
            Ok(())
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn get_timeout(
        &self,
        machine: &mut Machine,
        txid: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getTimeout",
            &[ethabi::Token::FixedBytes(txid.to_bytes_be())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn get_keepalive_price(
        &self,
        machine: &mut Machine,
        txid: Uint256,
    ) -> Result<(Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getKeepalivePrice",
            &[ethabi::Token::FixedBytes(txid.to_bytes_be())],
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
            ))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn keepalive(
        &self,
        machine: &mut Machine,
        txid: Uint256,
        payment: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "keepalive",
            &[ethabi::Token::FixedBytes(txid.to_bytes_be())],
            machine,
            payment,
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn get_beneficiary(
        &self,
        machine: &mut Machine,
        txid: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getBeneficiary",
            &[ethabi::Token::FixedBytes(txid.to_bytes_be())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn cancel(
        &self,
        machine: &mut Machine,
        txid: Uint256,
        sender: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            sender,
            "cancel",
            &[ethabi::Token::FixedBytes(txid.to_bytes_be())],
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
}

pub struct ArbStatistics {
    pub contract_abi: AbiForContract,
    debug: bool,
}

impl ArbStatistics {
    pub fn new(debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbStatistics")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(111));
        ArbStatistics {
            contract_abi,
            debug,
        }
    }

    pub fn get_stats(
        &self,
        machine: &mut Machine,
    ) -> Result<(Uint256, Uint256, Uint256, Uint256, Uint256, Uint256), ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(), // send from address zero
            "getStats",
            &[],
            machine,
            Uint256::zero(),
            self.debug,
        )?;

        if (receipts.len() != 1) || (sends.len() != 0) {
            println!("{} receipts, {} sends", receipts.len(), sends.len());
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            let retdata = receipts[0].get_return_data();
            Ok((
                Uint256::from_bytes(&retdata[0..32]),
                Uint256::from_bytes(&retdata[32..64]),
                Uint256::from_bytes(&retdata[64..96]),
                Uint256::from_bytes(&retdata[96..128]),
                Uint256::from_bytes(&retdata[128..160]),
                Uint256::from_bytes(&retdata[160..192]),
            ))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }
}

#[test]
fn test_payment_to_self() {
    let _ = evm_payment_to_self(None, false).unwrap();
}

pub fn evm_payment_to_self(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_u64(1025);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_u64(20000),
    );
    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let arbinfo = ArbInfo::new(false);
    let balance = arbinfo.get_balance(&mut machine, &my_addr)?;
    assert_eq!(balance, Uint256::from_u64(20000));

    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr.clone(),
        Uint256::from_u64(1000000000),
        None,
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

    let new_balance = arbinfo.get_balance(&mut machine, &my_addr)?;
    assert_eq!(new_balance, Uint256::from_u64(20000));

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_payment_to_self".to_string());
    Ok(())
}

#[test]
fn test_upgrade_arbos_to_different_version() {
    test_upgrade_arbos_over_itself_impl().unwrap();
}

fn test_upgrade_arbos_over_itself_impl() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos_before.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    machine.runtime_env.insert_eth_deposit_message(
        Uint256::zero(),
        Uint256::zero(),
        Uint256::_from_eth(100000),
    );
    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::_from_eth(100000),
    );
    let deployer_addr = Uint256::from_u64(1025);
    machine.runtime_env.insert_eth_deposit_message(
        deployer_addr.clone(),
        deployer_addr.clone(),
        Uint256::_from_eth(100000),
    );
    let _ = machine.run(None);

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    if add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy Add contract");
    }

    let arbowner = ArbOwner::new(&wallet, false);

    let arbsys_orig_binding = ArbSys::new(&wallet, false);
    assert_eq!(
        arbsys_orig_binding.arbos_version(&mut machine)?,
        Uint256::from_u64(35),
    );

    arbowner.give_ownership(&mut machine, my_addr, true)?;

    let mexe_path = Path::new("arb_os/arbos-upgrade.mexe");
    let _previous_upgrade_hash =
        try_upgrade(&arbowner, &mut machine, &mexe_path, None, false)?.unwrap();

    let wallet2 = machine.runtime_env.new_wallet();
    let arbsys = ArbSys::new(&wallet2, false);
    let arbos_version = arbsys.arbos_version(&mut machine)?;
    assert_eq!(
        arbos_version,
        *init_constant_table(Some(Path::new("arb_os/constants.json")))
            .unwrap()
            .get("ArbosVersionNumber")
            .unwrap()
    );
    let arbos_version_orig = arbsys_orig_binding.arbos_version(&mut machine)?;
    assert_eq!(arbos_version, arbos_version_orig);

    machine.write_coverage("test_upgrade_arbos_to_different_version".to_string());
    Ok(())
}

fn try_upgrade(
    arbowner: &ArbOwner,
    machine: &mut Machine,
    mexe_path: &Path,
    previous_upgrade_hash: Option<Uint256>,
    with_check: bool,
) -> Result<Option<Uint256>, ethabi::Error> {
    let uploader = CodeUploader::_new_from_file(mexe_path);
    arbowner.start_code_upload(machine, None, with_check)?;

    let mut accum = vec![];
    for buf in uploader.instructions {
        accum.extend(buf);
        if (accum.len() > 3000) {
            arbowner.continue_code_upload(machine, accum)?;
            accum = vec![];
        }
    }
    if (accum.len() > 0) {
        arbowner.continue_code_upload(machine, accum)?;
    }

    let expected_code_hash = arbowner.get_uploaded_code_hash(machine)?;
    Ok(
        if arbowner.finish_code_upload_as_arbos_upgrade(
            machine,
            expected_code_hash.clone(),
            previous_upgrade_hash.unwrap_or(Uint256::zero()),
        )? {
            Some(expected_code_hash)
        } else {
            None
        },
    )
}

#[test]
pub fn test_gas_charging_underfunded() {
    match evm_run_with_gas_charging(None, Uint256::_from_gwei(20), false) {
        Ok(result) => assert_eq!(result, false),
        Err(e) => panic!("error {}", e),
    }
}

#[test]
pub fn test_gas_charging_fully_funded() {
    match evm_run_with_gas_charging(None, Uint256::_from_eth(1000), false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

pub fn evm_run_with_gas_charging(
    log_to: Option<&Path>,
    funding: Uint256,
    debug: bool,
) -> Result<bool, ethabi::Error> {
    // returns Ok(true) if success, Ok(false) if insufficient gas money, Err otherwise
    use std::convert::TryFrom;
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        funding.clone(),
    );
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
    let arbowner = ArbOwner::new(&wallet, false);
    arbowner.set_fees_enabled(&mut machine, true, true)?;
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, false);

    println!("Function call ...");
    let (logs, sends) = pc_contract.call_function(
        my_addr.clone(),
        "deposit",
        &[],
        &mut machine,
        Uint256::_from_eth(1),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);

    if !logs[0].succeeded() {
        if logs[0].get_return_code() == Uint256::from_u64(3) {
            machine.write_coverage(format!("test_gas_charging_{}", funding));
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

    machine.write_coverage(format!("test_gas_charging_{}", funding));
    Ok(true)
}

#[test]
fn test_arbowner() {
    match evm_test_arbowner(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn evm_test_arbowner(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, debug);

    arbowner.give_ownership(&mut machine, my_addr, true)?;

    arbowner.start_code_upload(&mut machine, None, true)?;

    let mcode = vec![0x90u8, 1u8, 0u8, 42u8]; // debugprint(42)
    arbowner.continue_code_upload(&mut machine, mcode)?;

    let expected_code_hash = arbowner.get_uploaded_code_hash(&mut machine)?;
    assert!(arbowner.finish_code_upload_as_arbos_upgrade(
        &mut machine,
        expected_code_hash,
        Uint256::zero(),
    )?);

    arbowner.set_seconds_per_send(&mut machine, Uint256::from_u64(10))?;

    arbowner.set_gas_accounting_params(
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

    machine.write_coverage("test_arbowner".to_string());
    Ok(())
}

#[test]
fn test_arb_fair_gas_price_list() {
    evm_test_arb_fair_gas_price_list();
}

fn evm_test_arb_fair_gas_price_list() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let _my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, false);

    let addr1 = Uint256::from_u64(13853);
    let addr2 = Uint256::from_u64(813915);
    let addr3 = Uint256::from_u64(88);

    assert_eq!(
        arbowner
            .get_all_fair_gas_price_senders(&mut machine)
            .unwrap()
            .len(),
        0
    );
    assert!(!arbowner
        .is_fair_gas_price_sender(&mut machine, addr2.clone())
        .unwrap());
    arbowner
        .set_fair_gas_price_sender(&mut machine, addr1.clone(), true)
        .unwrap();
    assert!(arbowner
        .is_fair_gas_price_sender(&mut machine, addr1.clone())
        .unwrap());
    assert!(!arbowner
        .is_fair_gas_price_sender(&mut machine, addr2.clone())
        .unwrap());
    arbowner
        .set_fair_gas_price_sender(&mut machine, addr3.clone(), true)
        .unwrap();
    assert!(arbowner
        .is_fair_gas_price_sender(&mut machine, addr3.clone())
        .unwrap());

    let lis = arbowner
        .get_all_fair_gas_price_senders(&mut machine)
        .unwrap();
    assert_eq!(lis.len(), 2);
    assert!(
        ((lis[0] == addr1.clone()) && (lis[1] == addr3.clone()))
            || ((lis[0] == addr3.clone()) && (lis[1] == addr1.clone()))
    );

    machine.write_coverage("test_arb_fair_gas_price_list".to_string());
}

#[test]
fn test_run_tx_with_extra_gas() {
    match evm_test_tx_with_extra_gas(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn evm_test_tx_with_extra_gas(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, debug);

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    let constructor_data = if let Some(constructor) = add_contract.contract.constructor() {
        match constructor.encode_input(add_contract.code_bytes.clone(), &[]) {
            Ok(aug_code) => aug_code,
            Err(e) => {
                panic!("couldn't encode data for constructor: {:?}", e);
            }
        }
    } else {
        add_contract.code_bytes.clone()
    };

    let deploy_addr = arbowner.deploy_contract(
        &mut machine,
        &constructor_data,
        Uint256::from_u64(41389147891),
        Uint256::from_u64(417813478913111),
    )?;

    add_contract.bind_interface_to_address(deploy_addr.clone());

    let (receipts, _) = add_contract.call_function(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());
    assert_eq!(
        receipts[0].get_return_data(),
        Uint256::from_u64(2).to_bytes_be()
    );

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_run_tx_with_extra_gas".to_string());
    Ok(())
}

#[test]
fn test_arbgasinfo() {
    match evm_test_arbgasinfo(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn evm_test_arbgasinfo(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, debug);
    let arbgasinfo = ArbGasInfo::new(&wallet, debug);
    let arbaggregator = ArbAggregator::new(debug);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::_from_eth(100),
    );
    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let (l2tx, l1calldata, storage, basegas, conggas, totalgas) =
        arbgasinfo.get_prices_in_wei_with_aggregator(&mut machine, Uint256::zero())?;
    assert!(l2tx.is_zero());
    assert!(l1calldata.is_zero());
    assert!(storage.is_zero());
    assert!(basegas.is_zero());
    assert!(conggas.is_zero());
    assert_eq!(basegas.add(&conggas), totalgas);

    arbowner.set_fees_enabled(&mut machine, true, true)?;
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);

    let (l2tx, l1calldata, storage, basegas, conggas, totalgas) =
        arbgasinfo.get_prices_in_wei_with_aggregator(&mut machine, Uint256::zero())?;
    println!(
        "L2 tx {}, L1 calldata {}, L2 storage {}, base gas {}, congestion gas {}, total gas {}",
        l2tx, l1calldata, storage, basegas, conggas, totalgas
    );
    assert_eq!(l2tx, Uint256::from_u64(690000000000000));
    assert_eq!(l1calldata, Uint256::from_u64(172500000000));
    assert_eq!(storage, Uint256::from_u64(300000000000000));
    assert_eq!(basegas, Uint256::from_u64(1500000000));
    assert!(conggas.is_zero());
    assert_eq!(basegas.add(&conggas), totalgas);

    let (l2tx, l1calldata, storage) =
        arbgasinfo.get_prices_in_arbgas_with_aggregator(&mut machine, Uint256::zero())?;
    println!(
        "L2 tx / ag {}, L1 calldata / ag {}, L2 storage / ag {}",
        l2tx, l1calldata, storage
    );
    assert_eq!(l2tx, Uint256::from_u64(460000));
    assert_eq!(l1calldata, Uint256::from_u64(115));
    assert_eq!(storage, Uint256::from_u64(200000));

    let (speed_limit, gas_pool_max, tx_gas_limit) =
        arbgasinfo.get_gas_accounting_params(&mut machine)?;
    println!(
        "speed limit {}, pool max {}, tx gas limit {}",
        speed_limit, gas_pool_max, tx_gas_limit
    );
    assert_eq!(speed_limit, Uint256::from_u64(400_000));
    assert_eq!(gas_pool_max, Uint256::from_u64(288_000_000));
    assert_eq!(tx_gas_limit, Uint256::from_u64(8_000_000));

    let agg_addr = Uint256::from_u64(777);
    let fee = arbaggregator
        .get_tx_base_fee(&mut machine, agg_addr.clone())
        .unwrap();
    assert_eq!(fee, Uint256::from_u64(4000));

    let new_fee = Uint256::from_u64(8913);
    arbaggregator
        .set_tx_base_fee(&mut machine, agg_addr.clone(), new_fee.clone())
        .unwrap();

    let fee = arbaggregator
        .get_tx_base_fee(&mut machine, agg_addr.clone())
        .unwrap();
    assert_eq!(fee, new_fee);

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_arbgasinfo".to_string());
    Ok(())
}

#[test]
fn test_arbgas_oracle() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, false);
    let arbgasinfo = ArbGasInfo::new(&wallet, false);

    arbowner
        .set_chain_parameter(&mut machine, "GasPriceOracle", my_addr.clone(), true)
        .unwrap();

    let gpest = Uint256::_from_gwei(73);
    arbgasinfo
        .set_l1_gas_price_estimate(&mut machine, gpest.clone(), my_addr.clone())
        .unwrap();

    let gasprice = arbgasinfo.get_l1_gas_price_estimate(&mut machine).unwrap();
    assert_eq!(gpest, gasprice);
    machine.write_coverage("test_arbgas_oracle".to_string());
}

pub fn evm_test_rate_control(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());
    let arbowner = ArbOwner::new(&wallet, debug);

    arbowner.give_ownership(&mut machine, my_addr, true)?;

    let const_table = init_constant_table(Some(Path::new("arb_os/constants.json"))).unwrap();

    let (r1, r2) = arbowner.get_fee_recipients(&mut machine)?;
    assert_eq!(&r1, const_table.get("NetFee_defaultRecipient").unwrap());
    assert_eq!(
        &r2,
        const_table.get("CongestionFee_defaultRecipient").unwrap()
    );

    let new_r1 = r1.add(&Uint256::one());
    let new_r2 = r2.add(&Uint256::one());
    arbowner.set_fee_recipients(&mut machine, new_r1.clone(), new_r2.clone())?;
    let (updated_r1, updated_r2) = arbowner.get_fee_recipients(&mut machine)?;
    assert_eq!(new_r1, updated_r1);
    assert_eq!(new_r2, updated_r2);

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("evm_test_rate_control".to_string());
    Ok(())
}

#[test]
fn test_rollup_tracker() {
    do_rollup_tracker_ops();
}

pub fn do_rollup_tracker_ops() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let owner = Uint256::from_bytes(wallet.address().as_bytes());
    let arbowner = ArbOwner::new(&wallet, false);
    arbowner
        .give_ownership(&mut machine, owner.clone(), true)
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
    let _ = machine.run(None);

    arbowner
        .add_to_reserve_funds(&mut machine, Uint256::_from_eth(1))
        .unwrap();

    insert_create_node(
        &mut machine.runtime_env,
        &Uint256::one(),
        &Uint256::zero(),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );
    insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(2),
        &Uint256::one(),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    insert_claim_node(&mut machine.runtime_env, &Uint256::from_u64(2), &claimer);

    insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(3),
        &Uint256::one(),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(4),
        &Uint256::from_u64(2),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    insert_new_stake(
        &mut machine.runtime_env,
        &Uint256::from_u64(4),
        &claimer,
        None,
    );

    insert_rollup_debug(&mut machine.runtime_env);
    let _ = machine.run(None);

    insert_confirm_node(&mut machine.runtime_env, &Uint256::zero());
    insert_confirm_node(&mut machine.runtime_env, &Uint256::one());
    insert_confirm_node(&mut machine.runtime_env, &Uint256::from_u64(2));
    insert_reject_node(&mut machine.runtime_env, &Uint256::from_u64(3));
    insert_confirm_node(&mut machine.runtime_env, &Uint256::from_u64(4));

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(100), None, true);

    insert_create_node(
        &mut machine.runtime_env,
        &Uint256::from_u64(5),
        &Uint256::from_u64(4),
        None,
        &Uint256::from_u64(10),
        my_addr.clone(),
    );

    let _ = machine.run(None);

    machine.write_coverage("test_rollup_tracker".to_string());
    // There isn't really a result we can check here, so this test just confirms that nothing crashes.
}

pub fn insert_create_node(
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
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf, None, None);
}

pub fn insert_confirm_node(rt_env: &mut RuntimeEnvironment, height_l2: &Uint256) {
    let mut buf = vec![1u8];
    buf.extend(height_l2.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf, None, None);
}

pub fn insert_reject_node(rt_env: &mut RuntimeEnvironment, height_l2: &Uint256) {
    let mut buf = vec![2u8];
    buf.extend(height_l2.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf, None, None);
}

pub fn insert_new_stake(
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
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf, None, None);
}

pub fn insert_claim_node(rt_env: &mut RuntimeEnvironment, height_l2: &Uint256, claimer: &Uint256) {
    let mut buf = vec![4u8];
    buf.extend(height_l2.to_bytes_be());
    buf.extend(claimer.to_bytes_be());
    rt_env.insert_l1_message(8u8, Uint256::zero(), &buf, None, None);
}

pub fn insert_rollup_debug(rt_env: &mut RuntimeEnvironment) {
    rt_env.insert_l1_message(8u8, Uint256::zero(), &[255u8], None, None);
}

#[test]
fn test_arbaggregator() {
    match evm_test_arbaggregator(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{:?}", e),
    }
}

pub fn evm_test_arbaggregator(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbagg = ArbAggregator::new(debug);

    assert_eq!(
        arbagg.get_fee_collector(&mut machine, my_addr.clone())?,
        my_addr.clone()
    );

    let pref_agg = arbagg.get_preferred_aggregator(&mut machine, my_addr.clone())?;
    assert_eq!(pref_agg, (Uint256::zero(), true));

    let new_pref_agg = Uint256::from_u64(4242);
    arbagg.set_preferred_aggregator(&mut machine, new_pref_agg.clone(), my_addr.clone())?;
    let pref_agg = arbagg.get_preferred_aggregator(&mut machine, my_addr.clone())?;
    assert_eq!(pref_agg, (new_pref_agg, false));

    let def_agg = arbagg.get_default_aggregator(&mut machine)?;
    assert_eq!(def_agg, Uint256::zero());

    let new_def_agg = Uint256::from_u64(9696);
    arbagg.set_default_aggregator(&mut machine, new_def_agg.clone(), None)?;
    let def_agg = arbagg.get_default_aggregator(&mut machine)?;
    assert_eq!(def_agg, new_def_agg);

    assert!(arbagg
        .set_default_aggregator(
            &mut machine,
            Uint256::from_u64(12345),
            Some(my_addr.clone())
        )
        .is_err());

    assert_eq!(
        arbagg.get_fee_collector(&mut machine, my_addr.clone())?,
        my_addr.clone()
    );

    let new_collector = Uint256::from_u64(1298031);

    assert!(arbagg
        .set_fee_collector(
            &mut machine,
            my_addr.clone(),
            new_collector.clone(),
            new_collector.clone()
        )
        .is_err());
    assert_eq!(
        arbagg.get_fee_collector(&mut machine, my_addr.clone())?,
        my_addr.clone()
    );

    assert!(arbagg
        .set_fee_collector(
            &mut machine,
            my_addr.clone(),
            new_collector.clone(),
            my_addr.clone()
        )
        .is_ok());
    assert_eq!(
        arbagg.get_fee_collector(&mut machine, my_addr.clone())?,
        new_collector.clone()
    );

    let newer_collector = Uint256::from_u64(589713578913);
    assert!(arbagg
        .set_fee_collector(
            &mut machine,
            my_addr.clone(),
            newer_collector.clone(),
            my_addr.clone()
        )
        .is_err());
    assert!(arbagg
        .set_fee_collector(
            &mut machine,
            my_addr.clone(),
            newer_collector.clone(),
            new_collector.clone()
        )
        .is_ok());
    assert_eq!(
        arbagg.get_fee_collector(&mut machine, my_addr.clone())?,
        newer_collector.clone()
    );

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_arbaggregator".to_string());
    Ok(())
}

#[test]
fn test_retryable() {
    match test_retryable_impl(None, false) {
        Ok(()) => {}
        Err(e) => panic!("{}", e),
    }
}

pub fn test_retryable_impl(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_u64(1234);

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    if add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy Add contract");
    }

    let beneficiary = Uint256::from_u64(9185);

    let (_, txid, _) = add_contract.send_retryable_tx(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
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

    let arb_replayable = ArbReplayableTx::new(debug);
    let timeout = arb_replayable.get_timeout(&mut machine, txid.clone())?;
    assert!(timeout > machine.runtime_env.current_timestamp);

    let (keepalive_price, reprice_time) =
        arb_replayable.get_keepalive_price(&mut machine, txid.clone())?;
    assert_eq!(keepalive_price, Uint256::zero());
    assert!(reprice_time > machine.runtime_env.current_timestamp);

    let keepalive_ret = arb_replayable.keepalive(&mut machine, txid.clone(), keepalive_price)?;

    let new_timeout = arb_replayable.get_timeout(&mut machine, txid.clone())?;
    assert_eq!(keepalive_ret, new_timeout);
    assert!(new_timeout > timeout);

    arb_replayable.redeem(&mut machine, txid.clone())?;

    let new_timeout = arb_replayable.get_timeout(&mut machine, txid.clone())?;
    assert_eq!(new_timeout, Uint256::zero()); // verify that txid has been removed

    // make another one, and have the beneficiary cancel it
    let (_, txid, _) = add_contract.send_retryable_tx(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
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

    let out_beneficiary = arb_replayable.get_beneficiary(&mut machine, txid.clone())?;
    assert_eq!(out_beneficiary, beneficiary);

    arb_replayable.cancel(&mut machine, txid.clone(), beneficiary.clone())?;

    assert_eq!(
        arb_replayable.get_timeout(&mut machine, txid)?,
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

    let (_submitid, txid, maybe_redeemid) = add_contract.send_retryable_tx(
        my_addr.clone(),
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
        &mut machine,
        Uint256::zero(),
        Uint256::_from_eth(100),
        Uint256::zero(),
        None,
        Some(beneficiary.clone()),
        Some(Uint256::from_u64(1_000_000)),
        Some(Uint256::_from_gwei(5)),
    )?;
    assert!(txid != Uint256::zero());
    assert!(maybe_redeemid.is_some());
    let redeemid = maybe_redeemid.unwrap();

    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let receipts = machine.runtime_env.get_all_receipt_logs();
    let last_receipt = receipts[receipts.len() - 1].clone();
    assert_eq!(last_receipt.get_return_code(), Uint256::zero());
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

    machine.write_coverage("test_retryable".to_string());
    Ok(())
}

#[test]
fn test_arb_statistics() {
    assert!(test_arb_stats().is_ok());
}

fn test_arb_stats() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let arbstats = ArbStatistics::new(false);

    let (arb_blocknum, num_accounts, storage, _arbgas, txs, contracts) =
        arbstats.get_stats(&mut machine)?;

    assert_eq!(arb_blocknum, Uint256::from_u64(1));
    assert_eq!(num_accounts, Uint256::from_u64(22));
    assert_eq!(storage, Uint256::from_u64(0));
    // assert_eq!(_arbgas, Uint256::from_u64(1_490_972));  // disable this because it will vary over versions
    assert_eq!(txs, Uint256::from_u64(0));
    assert_eq!(contracts, Uint256::from_u64(20));
    machine.write_coverage("test_arb_statistics".to_string());
    Ok(())
}

#[test]
fn test_allowed_senders() {
    evm_test_allowed_senders();
}

fn evm_test_allowed_senders() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();

    let arbowner = ArbOwner::new(&wallet, false);

    let addr1 = Uint256::from_u64(123489121);
    let addr2 = Uint256::from_u64(1348098777777);

    assert!(arbowner
        .is_allowed_sender(&mut machine, addr1.clone())
        .unwrap());

    assert!(arbowner.allow_only_owner_to_send(&mut machine).is_ok());
    assert!(!arbowner
        .is_allowed_sender(&mut machine, addr1.clone())
        .unwrap());
    assert!(arbowner
        .is_allowed_sender(&mut machine, Uint256::zero())
        .unwrap());

    assert!(arbowner.allow_all_senders(&mut machine).is_ok());
    assert!(arbowner
        .is_allowed_sender(&mut machine, addr1.clone())
        .unwrap());

    assert!(arbowner.allow_only_owner_to_send(&mut machine).is_ok());
    assert!(arbowner
        .add_allowed_sender(&mut machine, addr1.clone())
        .is_ok());
    assert!(arbowner
        .is_allowed_sender(&mut machine, addr1.clone())
        .unwrap());
    assert!(!arbowner
        .is_allowed_sender(&mut machine, addr2.clone())
        .unwrap());

    assert!(arbowner
        .add_allowed_sender(&mut machine, addr2.clone())
        .is_ok());
    let serialized = arbowner.get_all_allowed_senders(&mut machine).unwrap();
    assert_eq!(serialized.len(), 2);
    assert!(
        ((serialized[0] == addr1.clone()) && (serialized[1] == addr2.clone()))
            || ((serialized[0] == addr2.clone()) && (serialized[1] == addr1.clone()))
    );

    assert!(arbowner
        .remove_allowed_sender(&mut machine, addr1.clone())
        .is_ok());
    assert!(!arbowner
        .is_allowed_sender(&mut machine, addr1.clone())
        .unwrap());
    assert!(arbowner
        .is_allowed_sender(&mut machine, addr2.clone())
        .unwrap());

    machine.write_coverage("test_allowed_senders".to_string());
}

#[test]
fn test_eventual_congestion_reject() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_address = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, false);
    let _ = arbowner.set_fees_enabled(&mut machine, true, true).unwrap();
    machine.runtime_env.insert_eth_deposit_message(
        my_address.clone(),
        my_address.clone(),
        Uint256::_from_eth(1000),
    );
    let _ = machine.run(None);
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);

    let arbtest = ArbosTest::new(false);
    for _ in 0..1200 {
        let res_code = match arbtest.burn_arb_gas(
            &mut machine,
            my_address.clone(),
            Uint256::from_u64(2_000_000),
        ) {
            Ok(rc) => rc,
            Err(_) => panic!(),
        };
        if res_code == 2 {
            machine.write_coverage("test_eventual_congestion_reject".to_string());
            return;
        } // we hit congestion, as expected
        assert_eq!(res_code, 0); // we should report success, if haven't hit congestion yet
    }
    assert!(false);
}

#[test]
fn test_congestion_price_adjustment() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_address = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, false);
    let arbgasinfo = ArbGasInfo::new(&wallet, false);
    let _ = arbowner.set_fees_enabled(&mut machine, true, true).unwrap();
    machine.runtime_env.insert_eth_deposit_message(
        my_address.clone(),
        my_address.clone(),
        Uint256::_from_eth(1000),
    );
    let _ = machine.run(None);
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);

    let _randomish_address = Uint256::from_u64(1583913081);
    assert_eq!(
        arbgasinfo
            //._get_prices_in_wei(&mut machine, randomish_address.clone())  preserve this for later integration
            .get_prices_in_wei_with_aggregator(&mut machine, my_address.clone())
            .unwrap()
            .4,
        Uint256::zero()
    );

    let arbtest = ArbosTest::new(false);
    let mut seen_any_congestion = false;
    for _ in 0..1200 {
        let res_code = match arbtest.burn_arb_gas(
            &mut machine,
            my_address.clone(),
            Uint256::from_u64(2_000_000),
        ) {
            Ok(rc) => rc,
            Err(_) => panic!(),
        };
        assert!((!seen_any_congestion && (res_code == 0)) || (res_code == 2)); // success or congestion
        if (res_code == 2) {
            seen_any_congestion = true;
        }
    }
    assert!(seen_any_congestion);

    // the chain should be congested now
    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, false);
    let _ = machine.run(None);

    let prices = arbgasinfo
        //._get_prices_in_wei(&mut machine, randomish_address.clone())  preserve this for later integration
        .get_prices_in_wei_with_aggregator(&mut machine, my_address.clone())
        .unwrap();
    assert!(prices.4 > Uint256::zero());

    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(48), Some(Uint256::from_u64(720)), false);
    let prices2 = arbgasinfo
        //._get_prices_in_wei(&mut machine, randomish_address.clone())  preserve this for later integration
        .get_prices_in_wei_with_aggregator(&mut machine, my_address.clone())
        .unwrap();
    assert_eq!(prices2.4, Uint256::zero());

    machine.write_coverage("test_congestion_price_adjustment".to_string());
}

#[test]
fn test_set_gas_price_estimate() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_address = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = ArbOwner::new(&wallet, false);
    let arbgasinfo = ArbGasInfo::new(&wallet, false);

    machine.runtime_env.insert_eth_deposit_message(
        my_address.clone(),
        my_address.clone(),
        Uint256::_from_eth(1000),
    );
    let _ = machine.run(None);

    arbowner.set_fees_enabled(&mut machine, true, true).unwrap();

    let new_gas_price = Uint256::from_u64(37);
    let new_storage_price = new_gas_price.mul(&Uint256::from_u64(2_000_000_000_000));

    let storage_price = arbgasinfo
        .get_prices_in_wei_with_aggregator(&mut machine, my_address.clone())
        .unwrap()
        .2;
    assert!(storage_price != new_storage_price);

    arbowner
        .set_l1_gas_price_estimate(&mut machine, new_gas_price)
        .unwrap();

    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, false);

    let storage_price = arbgasinfo
        .get_prices_in_wei_with_aggregator(&mut machine, my_address.clone())
        .unwrap()
        .2;
    assert_eq!(storage_price, new_storage_price);

    machine.write_coverage("test_set_gas_price_estimate".to_string());
}

struct Erc2470 {
    //TODO: Figure out why this isnt used
    _addr: Uint256,
    contract_abi: AbiForContract,
}

impl Erc2470 {
    fn new(machine: &mut Machine, ao: &ArbOwner) -> Self {
        let constructor_data = hex::decode("608060405234801561001057600080fd5b50610134806100206000396000f3fe6080604052348015600f57600080fd5b506004361060285760003560e01c80634af63f0214602d575b600080fd5b60cf60048036036040811015604157600080fd5b810190602081018135640100000000811115605b57600080fd5b820183602082011115606c57600080fd5b80359060200191846001830284011164010000000083111715608d57600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600092019190915250929550509135925060eb915050565b604080516001600160a01b039092168252519081900360200190f35b6000818351602085016000f5939250505056fea26469706673582212206b44f8a82cb6b156bfcc3dc6aadd6df4eefd204bc928a4397fd15dacf6d5320564736f6c63430006020033").unwrap();
        let deemed_sender =
            Uint256::from_string_hex("Bb6e024b9cFFACB947A71991E386681B1Cd1477D").unwrap();
        let deemed_nonce = Uint256::zero();

        let deployed_addr = ao
            .deploy_contract(machine, &constructor_data, deemed_sender, deemed_nonce)
            .unwrap();
        let mut contract_abi =
            AbiForContract::new_from_file(&test_contract_path("SingletonFactory")).unwrap();
        contract_abi.bind_interface_to_address(deployed_addr.clone());
        Erc2470 {
            _addr: deployed_addr,
            contract_abi,
        }
    }

    fn deploy(
        &self,
        machine: &mut Machine,
        code: Vec<u8>,
        salt: Uint256,
    ) -> Result<Uint256, ethabi::Error> {
        let (receipts, sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "deploy",
            &[
                ethabi::Token::Bytes(code),
                ethabi::Token::FixedBytes(salt.to_bytes_be()),
            ],
            machine,
            Uint256::zero(),
            false,
        )?;
        if (receipts.len() != 1) || (sends.len() != 0) {
            Err(ethabi::Error::from("wrong number of receipts or sends"))
        } else if receipts[0].succeeded() {
            Ok(Uint256::from_bytes(&receipts[0].get_return_data()))
        } else {
            Err(ethabi::Error::from("reverted"))
        }
    }

    fn deploy_from_file(
        &self,
        machine: &mut Machine,
        code_filename: &str,
        args: &[ethabi::Token],
        salt: Uint256,
    ) -> Result<Option<AbiForContract>, ethabi::Error> {
        let path = Path::new(code_filename);
        let mut file = File::open(path).map_err(|e| ethabi::Error::from(e.to_string()))?;
        let mut s = String::new();
        file.read_to_string(&mut s)
            .map_err(|e| ethabi::Error::from(e.to_string()))?;

        let json_from_file = serde_json::from_str::<serde_json::Value>(&s)
            .map_err(|e| ethabi::Error::from(e.to_string()))?;

        if let serde_json::Value::Object(fields) = json_from_file {
            let decoded_insns = {
                let code_str = fields
                    .get("bytecode")
                    .ok_or_else(|| ethabi::Error::from("no code key in json"))?
                    .as_str()
                    .unwrap()
                    .to_string();
                hex::decode(&code_str[2..]).unwrap()
            };

            let augmented_code = if let Some(constructor) = self.contract_abi.contract.constructor()
            {
                match constructor.encode_input(decoded_insns.clone(), args) {
                    Ok(aug_code) => aug_code,
                    Err(e) => {
                        panic!("couldn't encode data for constructor: {:?}", e);
                    }
                }
            } else {
                decoded_insns.clone()
            };

            let addr = self.deploy(machine, augmented_code, salt)?;
            if (addr == Uint256::zero()) {
                Ok(None)
            } else {
                let mut contract = AbiForContract::new_from_file(code_filename)?;
                contract.bind_interface_to_address(addr);
                Ok(Some(contract))
            }
        } else {
            Err(ethabi::Error::from("json file not an array"))
        }
    }
}

#[test]
fn test_erc2470() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let ao = ArbOwner::new(&wallet, false);

    let _erc = Erc2470::new(&mut machine, &ao);

    let add_contract_filename = test_contract_path("Add");
    let add = _erc
        .deploy_from_file(&mut machine, &add_contract_filename, &[], Uint256::zero())
        .unwrap()
        .unwrap();
    assert_eq!(
        add.address,
        Uint256::from_string_hex("7154b030bfa6f3b6937e57800eec2463e2c5687a").unwrap()
    );

    let (receipts, sends) = add
        .call_function(
            Uint256::zero(),
            "add",
            &[
                ethabi::Token::Uint(Uint256::one().to_u256()),
                ethabi::Token::Uint(Uint256::one().to_u256()),
            ],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();

    assert_eq!(receipts.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(receipts[0].succeeded());
    assert_eq!(
        Uint256::from_bytes(&receipts[0].get_return_data()),
        Uint256::from_u64(2)
    );

    machine.write_coverage("test_erc2470".to_string());
}

#[test]
fn test_create2_target_nonce_nonzero() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let ao = ArbOwner::new(&wallet, false);
    let arbtest = ArbosTest::new(false);

    let target_addr = Uint256::from_string_hex("7154b030bfa6f3b6937e57800eec2463e2c5687a").unwrap();
    assert!(arbtest
        .set_nonce(&mut machine, target_addr, Uint256::one())
        .is_ok());

    let _erc = Erc2470::new(&mut machine, &ao);
    let add_contract_filename = test_contract_path("Add");

    // now a deploy of the add contract should fail, because the deploy address has nonzero nonce
    let res = _erc
        .deploy_from_file(&mut machine, &add_contract_filename, &[], Uint256::zero())
        .unwrap();
    assert!(res.is_none());

    machine.write_coverage("test_create2_target_nonce_nonzero".to_string());
}

#[test]
fn test_eip_3541() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let ao = ArbOwner::new(&wallet, false);

    let _erc = Erc2470::new(&mut machine, &ao);

    // test cases that should fail, per EIP-3541
    for code in &[
        "60ef60005360016000f3",
        "60ef60005360026000f3",
        "60ef60005360036000f3",
        "60ef60005360206000f3",
    ] {
        assert!(_erc
            .deploy(&mut machine, hex::decode(code).unwrap(), Uint256::zero())
            .unwrap()
            .is_zero());
    }

    // test case that should succeed, per EIP-3541
    assert!(!_erc
        .deploy(
            &mut machine,
            hex::decode("60fe60005360016000f3").unwrap(),
            Uint256::zero()
        )
        .unwrap()
        .is_zero());

    machine.write_coverage("test_eip_3541".to_string());
}

#[test]
fn test_pay_eoa_from_contract() {
    evm_pay_eoa_from_contract(None, false);
}

pub fn evm_pay_eoa_from_contract(log_to: Option<&Path>, debug: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let add_contract = match AbiForContract::new_from_file(&test_contract_path("Add")) {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, debug);
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

    let payer = Uint256::from_u64(5386492);
    let recipient = Uint256::from_u64(5771838591);
    machine.runtime_env.insert_eth_deposit_message(
        payer.clone(),
        payer.clone(),
        Uint256::_from_eth(1000),
    );
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let arbinfo = ArbInfo::new(debug);
    let balance_before = arbinfo.get_balance(&mut machine, &recipient).unwrap();
    assert!(balance_before.is_zero());

    let (receipts, _) = add_contract
        .call_function(
            payer,
            "payTo",
            &[ethabi::Token::Address(recipient.to_h160())],
            &mut machine,
            Uint256::one(),
            debug,
        )
        .unwrap();
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());

    let balance_after = arbinfo.get_balance(&mut machine, &recipient).unwrap();
    assert_eq!(balance_after, Uint256::one());

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_pay_eoa_from_contract".to_string());
}

pub struct ArbInfo {
    pub contract_abi: AbiForContract,
    debug: bool,
}

impl ArbInfo {
    pub fn new(debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbInfo")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(101));
        ArbInfo {
            contract_abi,
            debug,
        }
    }

    pub fn get_balance(
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

#[test]
fn test_evm_add_code() {
    basic_evm_add_test(None, false).unwrap();
}

pub fn basic_evm_add_test(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let arbos_test = ArbosTest::new(debug);

    let code = hex::decode("7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0160005500").unwrap();
    let result = arbos_test.install_account_and_call(
        &mut machine,
        Uint256::from_u64(89629813089426890),
        Uint256::zero(),
        Uint256::one(),
        code,
        vec![],
        vec![],
    )?;
    let mut right_answer = vec![0u8; 32];
    right_answer.extend(vec![255u8; 31]);
    right_answer.extend(vec![254u8]);
    assert_eq!(result, right_answer);

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_evm_add_code".to_string());
    Ok(())
}

#[test]
fn test_call_from_contract() {
    let _log = evm_test_contract_call(None, false);
}

pub fn evm_test_contract_call(log_to: Option<&Path>, debug: bool) {
    use std::convert::TryFrom;
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_usize(1025);
    let contract = match AbiForContract::new_from_file(&test_contract_path("Add")) {
        Ok(mut contract) => {
            let result = contract.deploy(&[], &mut machine, Uint256::zero(), None, debug);
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

    for i in 0..4 {
        let result = contract.call_function_from_contract(
            my_addr.clone(),
            "add",
            vec![
                ethabi::Token::Uint(ethabi::Uint::one()),
                ethabi::Token::Uint(Uint256::from_u64(i).to_u256()),
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
                    ethabi::Token::Uint(ethabi::Uint::try_from(1 + i).unwrap())
                );
            }
            Err(e) => {
                panic!("{}", e.to_string());
            }
        }
    }

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_call_from_contract".to_string());
}

#[test]
pub fn test_tx_with_deposit() {
    match evm_tx_with_deposit(None, false, false) {
        Ok(result) => assert_eq!(result, true),
        Err(e) => panic!("error {}", e),
    }
}

pub fn evm_tx_with_deposit(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_usize(1025);

    let mut fib_contract = AbiForContract::new_from_file(&test_contract_path("Fibonacci"))?;
    if fib_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract = AbiForContract::new_from_file(&test_contract_path("PaymentChannel"))?;

    if pc_contract
        .deploy(
            &[ethabi::Token::Address(ethereum_types::H160::from_slice(
                &fib_contract.address.to_bytes_be()[12..],
            ))],
            &mut machine,
            Uint256::zero(),
            None,
            debug,
        )
        .is_err()
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let (logs, sends) = pc_contract.call_function_with_deposit(
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
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_tx_with_deposit".to_string());
    Ok(true)
}

pub fn evm_xcontract_call_using_compressed_batch(
    log_to: Option<&Path>,
    debug: bool,
    _profile: bool,
) -> Result<bool, ethabi::Error> {
    use std::convert::TryFrom;
    let mut rt_env = RuntimeEnvironment::default();

    let wallet = rt_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let mut machine = load_from_file_and_env(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero(true);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_usize(100000),
    );
    machine
        .runtime_env
        ._advance_time(Uint256::from_u64(50), None, true);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // handle this eth deposit message

    let mut fib_contract = AbiForContract::new_from_file(&test_contract_path("Fibonacci"))?;
    if fib_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy Fibonacci contract");
    }

    let mut pc_contract = AbiForContract::new_from_file(&test_contract_path("PaymentChannel"))?;
    if pc_contract
        .deploy(
            &[ethabi::Token::Address(ethereum_types::H160::from_slice(
                &fib_contract.address.to_bytes_be()[12..],
            ))],
            &mut machine,
            Uint256::zero(),
            None,
            debug,
        )
        .is_err()
    {
        panic!("failed to deploy PaymentChannel contract");
    }

    let mut batch = machine.runtime_env.new_batch();
    let tx_id_1 = pc_contract.add_function_call_to_compressed_batch(
        &mut batch,
        "deposit",
        &[],
        &mut machine,
        Uint256::from_usize(10000),
        &wallet,
    )?;
    let tx_id_2 = pc_contract.add_function_call_to_compressed_batch(
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
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("_evm_xcontract_call_using_compressed_batch".to_string());
    Ok(true)
}

#[test]
fn evm_test_constructor_recursion() {
    let _ = test_constructor_recursion().unwrap();
}

pub fn test_constructor_recursion() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_usize(1025);

    let mut ccontract = AbiForContract::new_from_file(&test_contract_path2(
        "ReverterFactory",
        "ConstructorCallback2",
    ))?;
    if ccontract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy ConstructorCallback contract");
    }

    let (receipts, _) = ccontract
        .call_function(
            my_addr.clone(),
            "test",
            &[],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());

    machine.write_coverage("evm_test_constructor_recursion".to_string());
    Ok(())
}

pub fn test_contract_path2(parent_name: &str, contract_name: &str) -> String {
    format!(
        "contracts/artifacts/arbos/test/{}.sol/{}.json",
        parent_name, contract_name
    )
}

pub fn evm_tests() -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut test_contract = AbiForContract::new_from_file(&test_contract_path("EvmTests"))?;
    if test_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy EvmTests contract");
    }

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add"))?;
    if add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy Add contract");
    }

    let (logs, sends) = test_contract.call_function(
        Uint256::zero(),
        "test",
        &[ethabi::Token::Address(add_contract.address.to_h160())],
        &mut machine,
        Uint256::zero(),
        false,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());
    let evm_logs = logs[0]._get_evm_logs();
    if evm_logs.len() != 0 {
        let log_data = &evm_logs[0].data;
        let val0 = Uint256::from_bytes(&log_data[0..32]);
        let val1 = Uint256::from_bytes(&log_data[32..64]);
        println!("Log codes: {} {}", val0, val1);
    }
    assert_eq!(evm_logs.len(), 0);

    // test log0 instruction
    let (logs, sends) = test_contract.call_function(
        Uint256::zero(),
        "makeLog0",
        &[],
        &mut machine,
        Uint256::zero(),
        false,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());
    let evm_logs = logs[0]._get_evm_logs();
    assert_eq!(evm_logs.len(), 1);
    let log_0_data = &evm_logs[0].data;
    assert_eq!(log_0_data.len(), 32);
    assert_eq!(Uint256::from_bytes(log_0_data), Uint256::from_u64(73));

    // test selfdestruct instruction
    let mut sd_contract = AbiForContract::new_from_file(&test_contract_path("SelfDestructor"))?;
    if sd_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .is_err()
    {
        panic!("failed to deploy SelfDestructor contract");
    }
    machine.runtime_env.insert_eth_deposit_message(
        Uint256::zero(),
        sd_contract.address.clone(),
        Uint256::from_u64(777),
    );
    let _ = machine.run(None);

    let (logs, sends) = test_contract.call_function(
        Uint256::zero(),
        "destructTest",
        &[ethabi::Token::Address(sd_contract.address.to_h160())],
        &mut machine,
        Uint256::zero(),
        false,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    machine.write_coverage("evm_tests".to_string());
    Ok(())
}

pub fn evm_block_num_consistency_test(debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_usize(1025);

    let mut bn_contract = AbiForContract::new_from_file(&test_contract_path("BlockNum"))?;
    if bn_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy BlockNum contract");
    }

    let (logs, sends) = bn_contract.call_function(
        my_addr.clone(),
        "getBlock",
        &[],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());
    let get_block_result = Uint256::from_bytes(&logs[0].get_return_data());

    let (logs, sends) = bn_contract.call_function(
        my_addr.clone(),
        "setBlock",
        &[],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    let (logs, sends) = bn_contract.call_function(
        my_addr.clone(),
        "currBlock",
        &[],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());
    let curr_block_result = Uint256::from_bytes(&logs[0].get_return_data());

    assert_eq!(get_block_result, curr_block_result);

    machine.write_coverage("test_block_num_consistency".to_string());
    Ok(())
}

pub fn underfunded_nested_call_test(
    log_to: Option<&Path>,
    debug: bool,
) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut contract = AbiForContract::new_from_file(&test_contract_path("Underfunded"))?;
    if contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy Fibonacci contract");
    }

    let (logs, sends) = contract.call_function(
        Uint256::from_u64(1028),
        "nestedCall",
        &[ethabi::Token::Uint(Uint256::zero().to_u256())],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    let (logs, sends) = contract.call_function(
        Uint256::from_u64(1028),
        "nestedCall",
        &[ethabi::Token::Uint(Uint256::one().to_u256())],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_underfunded_nested_call".to_string());
    Ok(())
}

pub fn evm_test_callback(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut contract = AbiForContract::new_from_file(&test_contract_path("Callback"))?;
    if contract
        .deploy(&[], &mut machine, Uint256::zero(), None, debug)
        .is_err()
    {
        panic!("failed to deploy Callback contract");
    }

    let (logs, sends) = contract.call_function(
        Uint256::from_u64(1028),
        "sendDummies",
        &[],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert_eq!(sends.len(), 0);
    assert!(logs[0].succeeded());
    let evmlogs = logs[0]._get_evm_logs();
    assert_eq!(evmlogs.len(), 3);
    for i in 0..2 {
        assert_eq!(evmlogs[i].addr, contract.address);
        assert_eq!(evmlogs[i].vals[1], Uint256::from_usize(i + 1));
        assert_eq!(
            evmlogs[i].data[0..32],
            Uint256::from_usize(i + 11).to_bytes_be()[0..32]
        );
        assert_eq!(
            evmlogs[i].data[32..64],
            Uint256::from_usize(i + 21).to_bytes_be()[0..32]
        );
    }

    let (logs, _) = contract.call_function(
        Uint256::from_u64(1028),
        "doCallback",
        &[],
        &mut machine,
        Uint256::zero(),
        debug,
    )?;
    assert_eq!(logs.len(), 1);
    assert!(logs[0].succeeded());
    let evmlogs = logs[0]._get_evm_logs();
    assert_eq!(evmlogs.len(), 8);

    println!("{}", evmlogs[2].vals[0]);
    assert_eq!(
        evmlogs[2].vals[0],
        Uint256::from_bytes(
            &hex::decode("5baaa87db386365b5c161be377bc3d8e317e8d98d71a3ca7ed7d555340c8f767")
                .unwrap()
        )
    );
    assert_eq!(evmlogs[2].addr, Uint256::from_u64(100)); // log was emitted by ArbSys
    assert_eq!(evmlogs[2].vals[2], Uint256::zero()); // unique ID = 0
    let batch_number = &evmlogs[2].vals[3];
    assert_eq!(batch_number, &Uint256::zero());
    let index_in_batch = Uint256::from_bytes(&evmlogs[2].data[32..64]);
    assert_eq!(index_in_batch, Uint256::zero());
    let calldata_size = Uint256::from_bytes(&evmlogs[2].data[(7 * 32)..(8 * 32)]);
    assert_eq!(calldata_size, Uint256::from_u64(11));

    assert_eq!(
        evmlogs[6].vals[0],
        Uint256::from_bytes(
            &hex::decode("5baaa87db386365b5c161be377bc3d8e317e8d98d71a3ca7ed7d555340c8f767")
                .unwrap()
        )
    );
    assert_eq!(evmlogs[6].addr, Uint256::from_u64(100)); // log was emitted by ArbSys
    assert_eq!(evmlogs[6].vals[2], Uint256::one()); // unique ID = 1
    let batch_number = &evmlogs[6].vals[3];
    assert_eq!(batch_number, &Uint256::zero());
    let index_in_batch = Uint256::from_bytes(&evmlogs[6].data[32..64]);
    assert_eq!(index_in_batch, Uint256::one());
    let calldata_size = Uint256::from_bytes(&evmlogs[6].data[(7 * 32)..(8 * 32)]);
    assert_eq!(calldata_size, Uint256::from_u64(17));

    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // advance time so that sends are emitted

    let sends = machine.runtime_env.get_all_sends();
    assert_eq!(sends.len(), 2);
    assert_eq!(sends[0][0], 3u8); // send type
    assert_eq!(sends[0][1..33], contract.address.to_bytes_be()[0..32]);
    assert_eq!(sends[0][161..193], [0u8; 32]);
    assert_eq!(sends[0].len(), 204); // 11 bytes of calldata after 193 bytes of fields
    assert_eq!(sends[1][0], 3u8); // send type
    assert_eq!(sends[1][1..33], contract.address.to_bytes_be()[0..32]);
    assert_eq!(sends[1][161..193], [0u8; 32]);
    assert_eq!(sends[1].len(), 210); // 17 bytes of calldata after 193 bytes of fields

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_l2_to_l1_call".to_string());
    Ok(())
}

pub fn evm_test_payment_in_constructor(log_to: Option<&Path>, debug: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

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

    let contract = match AbiForContract::new_from_file(&test_contract_path("Add")) {
        Ok(mut contract) => {
            let result =
                contract.deploy(&vec![], &mut machine, Uint256::from_u64(10000), None, debug);

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

    let result = contract.call_function(
        my_addr.clone(),
        "withdraw5000",
        vec![].as_ref(),
        &mut machine,
        Uint256::zero(),
        debug,
    );
    match result {
        Ok((logs, _sends)) => {
            assert_eq!(logs.len(), 1);
            assert!(logs[0].succeeded());
        }
        Err(e) => {
            panic!("{}", e.to_string());
        }
    }

    machine
        .runtime_env
        ._advance_time(Uint256::one(), None, true);
    let _gas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    }; // make sure the machine notices that time advanced

    let last_send = machine.runtime_env._get_last_send().unwrap();
    assert_eq!(last_send[0], 3u8);
    assert_eq!(last_send[1..33], contract.address.to_bytes_be());
    assert_eq!(last_send[33..65], Uint256::from_u64(1025).to_bytes_be());
    assert_eq!(last_send[161..193], Uint256::from_u64(5000).to_bytes_be());
    assert_eq!(last_send.len(), 193);

    if let Some(path) = log_to {
        let _ = machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("evm_test_payment_in_constructor".to_string());
}

#[test]
fn evm_reverter_factory_test() {
    evm_reverter_factory_test_impl();
}

fn evm_reverter_factory_test_impl() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let _contract = match AbiForContract::new_from_file(&test_contract_path("ReverterFactory")) {
        Ok(mut contract) => {
            let result = contract.deploy(
                &[ethabi::Token::Uint(Uint256::one().to_u256())],
                &mut machine,
                Uint256::zero(),
                None,
                false,
            );
            if let Err(maybe_receipt) = result {
                if maybe_receipt.is_none() {
                    panic!("deploy failed without receipt");
                }
            } else {
                panic!("deploy succeeded but should have failed");
            }
        }
        Err(e) => {
            panic!("error loading contract: {:?}", e);
        }
    };

    machine.write_coverage("evm_reverter_factory_test".to_string());
}

pub fn evm_eval_ripemd160(log_to: Option<&Path>, debug: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_u64(1025);
    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr,
        Uint256::from_u64(1000000000),
        None,
        Uint256::from_u64(3), // ripemd160 precompile
        Uint256::from_u64(0),
        &vec![0x61u8],
        false,
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
            "0000000000000000000000000bdc9d2d256b3ee9daae347be6f4dc835a467ffe"
        )
        .unwrap()
    );

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("test_ripemd160_precompile".to_string());
}

#[test]
fn evm_bad_receipt_revert_test() {
    evm_bad_receipt_revert_test_impl();
}

fn evm_bad_receipt_revert_test_impl() {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let my_addr = Uint256::from_u64(1025);

    let mut add_contract = AbiForContract::new_from_file(&test_contract_path("Add")).unwrap();
    let _ = add_contract
        .deploy(&[], &mut machine, Uint256::zero(), None, false)
        .unwrap();
    let (receipts, _) = add_contract
        .call_function(
            my_addr.clone(),
            "add",
            &[
                ethabi::Token::Uint(Uint256::one().to_u256()),
                ethabi::Token::Uint(Uint256::one().to_u256()),
            ],
            &mut machine,
            Uint256::zero(),
            false,
        )
        .unwrap();
    assert_eq!(receipts.len(), 1);
    assert!(receipts[0].succeeded());
    let _ = machine.run(None);
    let total_receipts_before = machine.runtime_env.get_all_receipt_logs().len();

    let txid = machine.runtime_env.insert_tx_message(
        my_addr,
        Uint256::zero(),
        None,
        add_contract.address,
        Uint256::one(),
        &[],
        true,
    );
    assert!(txid != receipts[0].get_request_id());
    let _ = machine.run(None);

    let receipts = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(receipts.len(), total_receipts_before + 1);
    assert_eq!(
        receipts[total_receipts_before].get_return_code(),
        Uint256::from_u64(10)
    );

    machine.write_coverage("evm_bad_receipt_revert_test".to_string());
}

pub fn evm_ecpairing_precompile(_log_to: Option<&Path>, debug: bool) {
    for (calldata, result) in &[
        // test vectors from geth: https://github.com/ethereum/go-ethereum/blob/2045a2bba3cd2f93fd913c692be146adabd8940c/core/vm/testdata/precompiles/bn256Pairing.json
        ("1c76476f4def4bb94541d57ebba1193381ffa7aa76ada664dd31c16024c43f593034dd2920f673e204fee2811c678745fc819b55d3e9d294e45c9b03a76aef41209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf704bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a416782bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550111e129f1cf1097710d41c4ac70fcdfa5ba2023c6ff1cbeac322de49d1b6df7c2032c61a830e3c17286de9462bf242fca2883585b93870a73853face6a6bf411198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", true),
        ("2eca0c7238bf16e83e7a1e6c5d49540685ff51380f309842a98561558019fc0203d3260361bb8451de5ff5ecd17f010ff22f5c31cdf184e9020b06fa5997db841213d2149b006137fcfb23036606f848d638d576a120ca981b5b1a5f9300b3ee2276cf730cf493cd95d64677bbb75fc42db72513a4c1e387b476d056f80aa75f21ee6226d31426322afcda621464d0611d226783262e21bb3bc86b537e986237096df1f82dff337dd5972e32a8ad43e28a78a96a823ef1cd4debe12b6552ea5f06967a1237ebfeca9aaae0d6d0bab8e28c198c5a339ef8a2407e31cdac516db922160fa257a5fd5b280642ff47b65eca77e626cb685c84fa6d3b6882a283ddd1198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", true),
        ("0f25929bcb43d5a57391564615c9e70a992b10eafa4db109709649cf48c50dd216da2f5cb6be7a0aa72c440c53c9bbdfec6c36c7d515536431b3a865468acbba2e89718ad33c8bed92e210e81d1853435399a271913a6520736a4729cf0d51eb01a9e2ffa2e92599b68e44de5bcf354fa2642bd4f26b259daa6f7ce3ed57aeb314a9a87b789a58af499b314e13c3d65bede56c07ea2d418d6874857b70763713178fb49a2d6cd347dc58973ff49613a20757d0fcc22079f9abd10c3baee245901b9e027bd5cfc2cb5db82d4dc9677ac795ec500ecd47deee3b5da006d6d049b811d7511c78158de484232fc68daf8a45cf217d1c2fae693ff5871e8752d73b21198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", true),
        ("2f2ea0b3da1e8ef11914acf8b2e1b32d99df51f5f4f206fc6b947eae860eddb6068134ddb33dc888ef446b648d72338684d678d2eb2371c61a50734d78da4b7225f83c8b6ab9de74e7da488ef02645c5a16a6652c3c71a15dc37fe3a5dcb7cb122acdedd6308e3bb230d226d16a105295f523a8a02bfc5e8bd2da135ac4c245d065bbad92e7c4e31bf3757f1fe7362a63fbfee50e7dc68da116e67d600d9bf6806d302580dc0661002994e7cd3a7f224e7ddc27802777486bf80f40e4ca3cfdb186bac5188a98c45e6016873d107f5cd131f3a3e339d0375e58bd6219347b008122ae2b09e539e152ec5364e7e2204b03d11d3caa038bfc7cd499f8176aacbee1f39e4e4afc4bc74790a4a028aff2c3d2538731fb755edefd8cb48d6ea589b5e283f150794b6736f670d6a1033f9b46c6f5204f50813eb85c8dc4b59db1c5d39140d97ee4d2b36d99bc49974d18ecca3e7ad51011956051b464d9e27d46cc25e0764bb98575bd466d32db7b15f582b2d5c452b36aa394b789366e5e3ca5aabd415794ab061441e51d01e94640b7e3084a07e02c78cf3103c542bc5b298669f211b88da1679b0b64a63b7e0e7bfe52aae524f73a55be7fe70c7e9bfc94b4cf0da1213d2149b006137fcfb23036606f848d638d576a120ca981b5b1a5f9300b3ee2276cf730cf493cd95d64677bbb75fc42db72513a4c1e387b476d056f80aa75f21ee6226d31426322afcda621464d0611d226783262e21bb3bc86b537e986237096df1f82dff337dd5972e32a8ad43e28a78a96a823ef1cd4debe12b6552ea5f", true),
        ("20a754d2071d4d53903e3b31a7e98ad6882d58aec240ef981fdf0a9d22c5926a29c853fcea789887315916bbeb89ca37edb355b4f980c9a12a94f30deeed30211213d2149b006137fcfb23036606f848d638d576a120ca981b5b1a5f9300b3ee2276cf730cf493cd95d64677bbb75fc42db72513a4c1e387b476d056f80aa75f21ee6226d31426322afcda621464d0611d226783262e21bb3bc86b537e986237096df1f82dff337dd5972e32a8ad43e28a78a96a823ef1cd4debe12b6552ea5f1abb4a25eb9379ae96c84fff9f0540abcfc0a0d11aeda02d4f37e4baf74cb0c11073b3ff2cdbb38755f8691ea59e9606696b3ff278acfc098fa8226470d03869217cee0a9ad79a4493b5253e2e4e3a39fc2df38419f230d341f60cb064a0ac290a3d76f140db8418ba512272381446eb73958670f00cf46f1d9e64cba057b53c26f64a8ec70387a13e41430ed3ee4a7db2059cc5fc13c067194bcc0cb49a98552fd72bd9edb657346127da132e5b82ab908f5816c826acb499e22f2412d1a2d70f25929bcb43d5a57391564615c9e70a992b10eafa4db109709649cf48c50dd2198a1f162a73261f112401aa2db79c7dab1533c9935c77290a6ce3b191f2318d198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", true),
        ("1c76476f4def4bb94541d57ebba1193381ffa7aa76ada664dd31c16024c43f593034dd2920f673e204fee2811c678745fc819b55d3e9d294e45c9b03a76aef41209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf704bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a416782bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550111e129f1cf1097710d41c4ac70fcdfa5ba2023c6ff1cbeac322de49d1b6df7c103188585e2364128fe25c70558f1560f4f9350baf3959e603cc91486e110936198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", false),
        ("", true),
        ("00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", false),
        ("00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d", true),
        ("00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", true),
        ("105456a333e6d636854f987ea7bb713dfd0ae8371a72aea313ae0c32c0bf10160cf031d41b41557f3e7e3ba0c51bebe5da8e6ecd855ec50fc87efcdeac168bcc0476be093a6d2b4bbf907172049874af11e1b6267606e00804d3ff0037ec57fd3010c68cb50161b7d1d96bb71edfec9880171954e56871abf3d93cc94d745fa114c059d74e5b6c4ec14ae5864ebe23a71781d86c29fb8fb6cce94f70d3de7a2101b33461f39d9e887dbb100f170a2345dde3c07e256d1dfa2b657ba5cd030427000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000021a2c3013d2ea92e13c800cde68ef56a294b883f6ac35d25f587c09b1b3c635f7290158a80cd3d66530f74dc94c94adb88f5cdb481acca997b6e60071f08a115f2f997f3dbd66a7afe07fe7862ce239edba9e05c5afff7f8a1259c9733b2dfbb929d1691530ca701b4a106054688728c9972c8512e9789e9567aae23e302ccd75", true),
        ("00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d", true),
        ("00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", true),
        ("105456a333e6d636854f987ea7bb713dfd0ae8371a72aea313ae0c32c0bf10160cf031d41b41557f3e7e3ba0c51bebe5da8e6ecd855ec50fc87efcdeac168bcc0476be093a6d2b4bbf907172049874af11e1b6267606e00804d3ff0037ec57fd3010c68cb50161b7d1d96bb71edfec9880171954e56871abf3d93cc94d745fa114c059d74e5b6c4ec14ae5864ebe23a71781d86c29fb8fb6cce94f70d3de7a2101b33461f39d9e887dbb100f170a2345dde3c07e256d1dfa2b657ba5cd030427000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000021a2c3013d2ea92e13c800cde68ef56a294b883f6ac35d25f587c09b1b3c635f7290158a80cd3d66530f74dc94c94adb88f5cdb481acca997b6e60071f08a115f2f997f3dbd66a7afe07fe7862ce239edba9e05c5afff7f8a1259c9733b2dfbb929d1691530ca701b4a106054688728c9972c8512e9789e9567aae23e302ccd75", true),
    ] {
        evm_ecpairing_precompile_test_one(calldata, *result, debug);
    }
}

fn evm_ecpairing_precompile_test_one(calldata: &str, result: bool, debug: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let mut hasher = DefaultHasher::new();
    calldata.hash(&mut hasher);
    let input_hash = hasher.finish();

    let my_addr = Uint256::from_u64(1025);
    let calldata = hex::decode(calldata).unwrap();
    assert_eq!(calldata.len() % (6 * 32), 0);

    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr,
        Uint256::from_u64(1000000000),
        None,
        Uint256::from_u64(8), // ecpairing precompile
        Uint256::from_u64(0),
        &calldata,
        false,
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
    assert_eq!(return_uint == Uint256::one(), result);

    machine.write_coverage(format!("test_ecpairing_precompile-{:x}", input_hash));

    //if let Some(path) = log_to {
    //    machine.runtime_env.recorder.to_file(path, machine.get_total_gas_usage().to_u64().unwrap()).unwrap();
    //}
}
