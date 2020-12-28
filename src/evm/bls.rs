/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

use crate::evm::abi::{builtin_contract_path, AbiForContract};
use crate::run::{load_from_file, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use ethers_signers::{Signer, Wallet};
use std::path::Path;
use crypto::digest::Digest;
use crypto::sha2::Sha256;
use num_bigint::BigUint;
use num_bigint::ToBigUint;
use num_integer::Integer;
use std::cmp::Ordering;

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



fn hash_to_point(
    domain: &[u8],
    msg: &[u8]
    ) -> () {

    let (u0, u1) = hashToField(domain, msg);
    let p0 = map_to_g1(&u0);
    let p1 = map_to_g1(&u1);

//     let x = asm(p0.0, p0.1, p1.0, p1.1) uint { ecadd }; // check order
//     let y = asm() uint { };



//     return Some(struct { x: x, y: y });
}



fn map_to_g1(_x: &BigUint) -> (Option<(BigUint)>, Option<(BigUint)>) {
        let field_order = BigUint::parse_bytes(b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47", 16).unwrap();
        // sqrt(-3)
    let z0 = BigUint::parse_bytes(b"0000000000000000b3c4d79d41a91759a9e4c7e359b6b89eaec68e62effffffd", 16).unwrap();

// (sqrt(-3) - 1)  / 2
    let z1 = BigUint::parse_bytes(b"000000000000000059e26bcea0d48bacd4f263f1acdb5c4f5763473177fffffe", 16).unwrap();

        if(_x.cmp(&field_order) != Ordering::Less) {
            return (None,None)
        } 

        let sqrtX = sqrt(_x);

        let mut found = false;

        if sqrtX.is_some() {
        	found = true;
        }

        let mut a0 = (_x * _x).mod_floor(&field_order);
        a0 = (a0 + &ToBigUint::to_biguint(&4).unwrap()).mod_floor(&field_order);
        let mut a1 = (_x * z0).mod_floor(&field_order);
        let mut a2 = (&a0 * &a1).mod_floor(&field_order);
        a2 = inverse(&a2);
        a1 = (&a1 * &a1).mod_floor(&field_order);
        a1 = (&a1 * &a2).mod_floor(&field_order);
        
        a1 = (_x * &a1).mod_floor(&field_order);
        
        let mut x = (z1 + (&field_order-&a1)).mod_floor(&field_order);

        a1 = (&x * &x).mod_floor(&field_order);
        a1 = (&a1 * &x).mod_floor(&field_order);
        a1 = (&a1 + &ToBigUint::to_biguint(&3).unwrap()).mod_floor(&field_order);

        let mut sqrt_a1 = sqrt(&a1);

         if let Some(sqa1) = sqrt_a1 {
        	if (found) {
                a1 = sqa1;
    	    } else {
     	    	a1 = &field_order - sqa1;
     	    }
            return (Some(x),Some(a1));
        }

        x = (x + &ToBigUint::to_biguint(&1).unwrap()).mod_floor(&field_order);  
        x = &field_order - &x;

        a1 = (&x * &x).mod_floor(&field_order);
        a1 = (&a1 * &x).mod_floor(&field_order);
        a1 = (&a1 + &ToBigUint::to_biguint(&3).unwrap()).mod_floor(&field_order);


        sqrt_a1 = sqrt(&a1);

         if let Some(sqa1) = sqrt_a1 {
        	if (found) {
                a1 = sqa1;
    	    } else {
     	    	a1 = &field_order - sqa1;
     	    }
            return (Some(x),Some(a1));
        }


        x = (&a0 * &a0).mod_floor(&field_order);
        x = (&x * &x).mod_floor(&field_order);
        x = (&x * &a2).mod_floor(&field_order);
        x = (&x * &a2).mod_floor(&field_order);
        x = (&x + &ToBigUint::to_biguint(&1).unwrap()).mod_floor(&field_order);

        a1 = (&x * &x).mod_floor(&field_order);
        a1 = (&a1 * &x).mod_floor(&field_order);
        a1 = (&a1 + &ToBigUint::to_biguint(&3).unwrap()).mod_floor(&field_order);

        sqrt_a1 = sqrt(&a1);

         if let Some(sqa1) = sqrt_a1 {
        	if (found) {
                a1 = sqa1;
    	    } else {
     	    	a1 = &field_order - sqa1;
     	    }
            return (Some(x),Some(a1));
        }

        (None, None)


}


fn sqrt(xx: &BigUint) -> Option<BigUint> {
        let field_order = BigUint::parse_bytes(b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47", 16).unwrap();
        let field_orderplus1_div4 = BigUint::parse_bytes(b"0xc19139cb84c680a6e14116da060561765e05aa45a1c72a34f082305b61f3f52", 16).unwrap();
        let x = xx.modpow(&field_orderplus1_div4,&field_order);

        let square = x.modpow(&ToBigUint::to_biguint(&2).unwrap(),&field_order);
    if(&square == xx) {
        Some(x)
    } else {
        None
    }
}

fn inverse(xx: &BigUint) -> BigUint {
        let field_order = BigUint::parse_bytes(b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47", 16).unwrap();
        let field_ordermin2 = BigUint::parse_bytes(b"0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45", 16).unwrap();
        xx.modpow(&field_ordermin2,&field_order)
}

fn hashToField(
    domain: &[u8],
    msg: &[u8]
    ) ->(BigUint,BigUint){
        let _msg = expandMsgTo96(domain, msg);
    
    let field_order = BigUint::parse_bytes(b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47", 16).unwrap();
    let a = BigUint::from_bytes_be(&_msg[0..48]).mod_floor(&field_order);
    let b = BigUint::from_bytes_be(&_msg[48..96]).mod_floor(&field_order);
    (a,b)


    }

pub fn expandMsgTo96(
    domain: &[u8],
    msg: &[u8],
)->Vec<u8>{
        if (domain.len() > 32) {
        panic!(); //todo fix error handling
        }

    let t0 = msg.len();
    let t1 = domain.len();
    let mut out = vec![0; 96];
    let len0 = 64 + t0 + t1 + 4;
    let mut in0 = vec![0; len0];
    
    // zero pad
    let mut off = 64;
    // msg
    in0[off..off+t0].clone_from_slice(msg);
    off += t0;

    in0[off] = 0;
    off +=1;
    in0[off] = 96;
    off +=1;
    in0[off] = 0;
    off +=1;

    in0[off..off+t1].clone_from_slice(domain);
    off += t1;
    in0[off] = t1 as u8;

    let mut hasher = Sha256::new();
    hasher.input(&in0);
    let mut b0 = vec![0; 32];
     hasher.result(&mut b0);

    let len1 = 32 + 1 + domain.len() + 1;

    let mut in1 = vec![0; len1];
    in1[0..32].clone_from_slice(&b0);
    off = 32;

    in1[off] = 1;
    off+=1;
    in1[off..off+t1].clone_from_slice(domain);
    off += t1;
    in1[off] = t1 as u8;

    hasher.reset();
    hasher.input(&in1);
    let mut bi = vec![0; 32];
    hasher.result(&mut bi);

    out[0..32].clone_from_slice(&bi);

    let mut t: Vec<u8> = b0.iter().zip(bi.iter()).map(|(&l, &r)| l ^ r).collect(); 

    in1[0..32].clone_from_slice(&t);

    off = 32;
    in1[off] = 2;
    off += 1;

    in1[off..off+t1].clone_from_slice(domain);
    off = off + t1;
    in1[off] = t1 as u8;

    hasher.reset();
    hasher.input(&in1);
    hasher.result(&mut bi);

    out[32..64].clone_from_slice(&bi);

    t = b0.iter().zip(bi.iter()).map(|(&l, &r)| l ^ r).collect(); 

    in1[0..32].clone_from_slice(&t);

    off = 32;
    in1[off] = 3;
    off = off + 1;
    
    in1[off..off+t1].clone_from_slice(domain);
    off = off + t1;
    in1[off] = t1 as u8;

    hasher.reset();
    hasher.input(&in1);
    hasher.result(&mut bi);
    out[64..96].clone_from_slice(&bi);
    out
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
    pub fn to_four_uints(&self) -> (Uint256, Uint256, Uint256, Uint256) {
        //TODO: implement this
        (Uint256::zero(), Uint256::zero(), Uint256::zero(), Uint256::zero())  // temporary, for typechecking
    }

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

#[test]
pub fn test_bls_signed_batch() {
    _evm_test_bls_signed_batch(None, false).unwrap();
}

pub fn _evm_test_bls_signed_batch(
    log_to: Option<&Path>,
    debug: bool,
) -> Result<(), ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111));

    let alice_wallet = rt_env.new_wallet();
    let alice_addr = Uint256::from_bytes(alice_wallet.address().as_bytes());
    let bob_wallet = rt_env.new_wallet();
    let bob_addr = Uint256::from_bytes(bob_wallet.address().as_bytes());

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    // generate and register BLS keys for Alice and Bob
    let (alice_public_key, alice_private_key) = generate_bls_key_pair();
    let (bob_public_key, bob_private_key) = generate_bls_key_pair();
    let alice_arb_bls = _ArbBLS::_new(&alice_wallet, debug);
    let bob_arb_bls = _ArbBLS::_new(&bob_wallet, debug);
    let a4 = alice_public_key.to_four_uints();
    alice_arb_bls._register(&mut machine, a4.0, a4.1, a4.2, a4.3)?;
    let b4 = bob_public_key.to_four_uints();
    bob_arb_bls._register(&mut machine, b4.0, b4.1, b4.2, b4.3)?;

    Ok(())
}