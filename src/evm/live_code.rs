use crate::evm::{builtin_contract_path, AbiForContract};
use crate::run::Machine;
use crate::uint256::Uint256;

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
            false,
        );
        let _tx_id = machine.runtime_env.insert_tx_message(
            caller_addr,
            Uint256::from_usize(1_000_000_000),
            None,
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

        if (logs.len() != num_logs_before + 2) || (sends.len() != num_sends_before) {
            return Err(ethabi::Error::from("wrong number of receipts or sends"));
        }
        if !logs[num_logs_before + 1].succeeded() {
            println!(
                "arbosTest.run revert code {}",
                logs[num_logs_before + 1].get_return_code()
            );
            return Err(ethabi::Error::from("reverted"));
        }

        Ok(logs[num_logs_before + 1].get_return_data())
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
        let _ = machine.runtime_env.get_seq_num(&Uint256::zero(), true);
        let _ = machine.runtime_env.get_seq_num(&Uint256::zero(), true);
        self.call(machine, Uint256::zero(), addr.clone(), calldata, balance)?;
        self._get_marshalled_storage(machine, addr)
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
            return Ok(receipts[0].get_return_data()[64..].to_vec());
        } else {
            println!(
                "arbosTest.run revert code {}",
                receipts[0].get_return_code()
            );
            Err(ethabi::Error::from("reverted"))
        }
    }

    pub fn _burn_arb_gas(
        &self,
        machine: &mut Machine,
        sender_addr: Uint256,
        amount: Uint256,
    ) -> Result<u64, ethabi::Error> {
        let (receipts, _) = self.contract_abi.call_function(
            sender_addr, // send from address zero
            "burnArbGas",
            &[ethabi::Token::Uint(amount.to_u256())],
            machine,
            Uint256::zero(),
            self.debug,
        )?;
        assert_eq!(receipts.len(), 1);
        Ok(receipts[0].get_return_code().to_u64().unwrap())
    }

    pub fn _set_nonce(
        &self,
        machine: &mut Machine,
        addr: Uint256,
        new_nonce: Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setNonce",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Uint(new_nonce.to_u256()),
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

    pub fn _set_balance(
        &self,
        machine: &mut Machine,
        addr: &Uint256,
        new_balance: &Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setBalance",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Uint(new_balance.to_u256()),
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

    pub fn _set_code(
        &self,
        machine: &mut Machine,
        addr: &Uint256,
        code: Vec<u8>,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setCode",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Bytes(code),
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

    pub fn _set_state(
        &self,
        machine: &mut Machine,
        addr: &Uint256,
        state: Vec<u8>,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "setState",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Bytes(state),
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

    pub fn _store(
        &self,
        machine: &mut Machine,
        addr: &Uint256,
        key: &Uint256,
        val: &Uint256,
    ) -> Result<(), ethabi::Error> {
        let (receipts, _sends) = self.contract_abi.call_function(
            Uint256::zero(),
            "store",
            &[
                ethabi::Token::Address(addr.to_h160()),
                ethabi::Token::Uint(key.to_u256()),
                ethabi::Token::Uint(val.to_u256()),
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
