/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

use crate::evm::abi::{builtin_contract_path, AbiForContract};
use crate::run::{load_from_file, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use ethers_signers::{Signer, Wallet};
use std::path::Path;

pub struct _ArbBLS<'a> {
    pub contract_abi: AbiForContract,
    _wallet: &'a Wallet,
    my_address: Uint256,
    debug: bool,
}

impl<'a> _ArbBLS<'a> {
    pub fn _new(wallet: &'a Wallet, debug: bool) -> Self {
        let mut contract_abi =
            AbiForContract::new_from_file(&builtin_contract_path("ArbBLS")).unwrap();
        contract_abi.bind_interface_to_address(Uint256::from_u64(103));
        _ArbBLS {
            contract_abi,
            _wallet: wallet,
            my_address: Uint256::from_bytes(wallet.address().as_bytes()),
            debug,
        }
    }

    pub fn _register(
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

    pub fn _get_public_key(
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

pub fn _evm_test_bls_registry(log_to: Option<&Path>, debug: bool) {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arb_bls = _ArbBLS::_new(&wallet, debug);

    assert!(arb_bls._get_public_key(&mut machine, my_addr.clone()).is_err());

    let expected = [
        Uint256::from_u64(13),
        Uint256::from_u64(26),
        Uint256::from_u64(39),
        Uint256::from_u64(52),
    ];
    assert!(arb_bls
        ._register(
            &mut machine,
            expected[0].clone(),
            expected[1].clone(),
            expected[2].clone(),
            expected[3].clone(),
        )
        .is_ok());

    let (res0, res1, res2, res3) = match arb_bls._get_public_key(&mut machine, my_addr.clone()) {
        Ok(res) => res,
        Err(_) => panic!(),
    };
    assert_eq!(res0, expected[0]);
    assert_eq!(res1, expected[1]);
    assert_eq!(res2, expected[2]);
    assert_eq!(res3, expected[3]);

    if let Some(path) = log_to {
        let _ = machine.runtime_env.recorder.to_file(path).unwrap();
    }
}

use bn::{pairing, AffineG1, AffineG2, Fq, Fq2, Group, Gt, G1, G2};

pub struct BLSPublicKey {
    //TODO: add fields
}

pub struct BLSPrivateKey {
    //TODO: add fields
}

pub struct BLSSignature {
    //TODO: add fields
}

pub struct BLSAggregateSignature {
    //TODO: add fields
}

pub fn generate_bls_key_pair() -> (BLSPublicKey, BLSPrivateKey) {
    let random_generator = rand::thread_rng();
    //TODO: implement this
    (BLSPublicKey{}, BLSPrivateKey{})  // temporary, for typechecking
}

impl BLSPrivateKey {
    pub fn sign_message(&self, message: &[u8]) -> BLSSignature {
        //TODO: implement this
        BLSSignature{}  // temporary, for typechecking
    }
}

impl BLSPublicKey {
    pub fn to_bytes(&self) -> Vec<u8> {
        //TODO: implement this
        vec![]   // temporary, for typechecking
    }
}

impl BLSSignature {
    pub fn to_bytes(&self) -> Vec<u8> {
        //TODO: implement this
        vec![]   // temporary, for typechecking
    }
}

impl BLSAggregateSignature {
    pub fn new(sigs: Vec<BLSSignature>, messages: Vec<&[u8]>) -> Self {
        //TODO: implement this
        BLSAggregateSignature{}   // temporary, for typechecking
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        //TODO: implement this
        vec![]     // temporary, for typechecking
    }
}