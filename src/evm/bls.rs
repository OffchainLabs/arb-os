/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

use crate::evm::abi::{builtin_contract_path, AbiForContract};
use crate::run::{load_from_file, Machine, RuntimeEnvironment};
use crate::uint256::Uint256;
use crypto::digest::Digest;
use crypto::sha2::Sha256;
use ethers_signers::{Signer, Wallet};
use num_bigint::BigUint;
use num_bigint::ToBigUint;
use num_bigint::RandBigInt;
use num_integer::Integer;
use std::cmp::Ordering;
use parity_bn::{AffineG1, AffineG2, Fq, Fr, Group, G1, G2};
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
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arb_bls = _ArbBLS::_new(&wallet, debug);

    assert!(arb_bls
        ._get_public_key(&mut machine, my_addr.clone())
        .is_err());

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



pub fn hash_to_point(
    domain: &[u8],
    msg: &[u8]
    ) -> Option<G1> {

    let (u0, u1) = hash_to_field(domain, msg);
    if let Some((px_bi, py_bi)) = map_to_g1(&u0) {
        if let Some((qx_bi, qy_bi)) = map_to_g1(&u1) {


            let px = Fq::from_slice(&px_bi.to_bytes_be()).unwrap();
            let py = Fq::from_slice(&py_bi.to_bytes_be()).unwrap();
            let qx = Fq::from_slice(&qx_bi.to_bytes_be()).unwrap();
            let qy = Fq::from_slice(&qy_bi.to_bytes_be()).unwrap();

            let p = if px == Fq::zero() && py == Fq::zero() {
                G1::zero()
            } else {
                AffineG1::new(px, py).unwrap().into()
            };
            let q = if qx == Fq::zero() && qy == Fq::zero() {
                G1::zero()
            } else {
                AffineG1::new(qx, qy).unwrap().into()
            };

            let ret = p + q ;
            let mut out_buf_0 = vec![0u8; 32];
            ret.x().to_big_endian(&mut out_buf_0).unwrap();
            let mut out_buf_1 = vec![0u8; 32];
            ret.y().to_big_endian(&mut out_buf_1).unwrap();
            println!("{:?}", out_buf_0);
            println!("{:?}", out_buf_1);
            return Some(ret) 
        }
    }
    None
}


fn map_to_g1(_x: &BigUint) -> Option<(BigUint, BigUint)> {

    let field_order = BigUint::parse_bytes(
        b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47",
        16,
    )
    .unwrap();
    // sqrt(-3)
    let z0 = BigUint::parse_bytes(
        b"0000000000000000b3c4d79d41a91759a9e4c7e359b6b89eaec68e62effffffd",
        16,
    )
    .unwrap();
    // (sqrt(-3) - 1)  / 2
    let z1 = BigUint::parse_bytes(
        b"000000000000000059e26bcea0d48bacd4f263f1acdb5c4f5763473177fffffe",
        16,
    )
    .unwrap();

    if(_x.cmp(&field_order) != Ordering::Less) {
        return None 
    } 

            
    let mut found = false;
    let sqrt_x = sqrt(_x);
    if sqrt_x.is_some() {
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
        return Some((x,a1));
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
        return Some((x,a1));
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
        return Some((x,a1));
    }

    None
}

fn sqrt(xx: &BigUint) -> Option<BigUint> {
    let field_order = BigUint::parse_bytes(
        b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47",
        16,
    )
    .unwrap();
    let field_orderplus1_div4 = BigUint::parse_bytes(
        b"c19139cb84c680a6e14116da060561765e05aa45a1c72a34f082305b61f3f52",
        16,
    )
    .unwrap();
    let x = xx.modpow(&field_orderplus1_div4, &field_order);

    let square = (&x * &x).mod_floor(&field_order);
    if(square.cmp(xx) == Ordering::Equal){
        Some(x)
    } else {
        None
    }
}

fn inverse(xx: &BigUint) -> BigUint {
    let field_order = BigUint::parse_bytes(
        b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47",
        16,
    )
    .unwrap();
    let field_ordermin2 = BigUint::parse_bytes(
        b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45",
        16,
    )
    .unwrap();
    xx.modpow(&field_ordermin2, &field_order)
}

fn hash_to_field(domain: &[u8], msg: &[u8]) -> (BigUint, BigUint) {
    let _msg = expand_msg_to_96(domain, msg);

    let field_order = BigUint::parse_bytes(
        b"30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47",
        16,
    )
    .unwrap();
    let a = BigUint::from_bytes_be(&_msg[0..48]).mod_floor(&field_order);
    let b = BigUint::from_bytes_be(&_msg[48..96]).mod_floor(&field_order);
    (a, b)
}

pub fn expand_msg_to_96(domain: &[u8], msg: &[u8]) -> Vec<u8> {
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
    in0[off..off + t0].clone_from_slice(msg);
    off += t0;

    in0[off] = 0;
    off += 1;
    in0[off] = 96;
    off += 1;
    in0[off] = 0;
    off += 1;

    in0[off..off + t1].clone_from_slice(domain);
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
    off += 1;
    in1[off..off + t1].clone_from_slice(domain);
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

    in1[off..off + t1].clone_from_slice(domain);
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

    in1[off..off + t1].clone_from_slice(domain);
    off = off + t1;
    in1[off] = t1 as u8;

    hasher.reset();
    hasher.input(&in1);
    hasher.result(&mut bi);
    out[64..96].clone_from_slice(&bi);
    out
}

pub struct BLSPublicKey {
    g2p:    AffineG2,
}

pub struct BLSPrivateKey {
    s:      Fr
}

pub struct BLSSignature {
    g1p:    AffineG1,
}

pub struct BLSAggregateSignature {
    g1p:    AffineG1,
}

pub fn generate_bls_key_pair() -> (BLSPublicKey, BLSPrivateKey) {
    // can't use built in FR::random() due to versioning mismatch of rand
    let subgroup_order =  BigUint::parse_bytes(b"30644E72E131A029B85045B68181585D2833E84879B9709143E1F593F0000001",16).unwrap();
    let mut rng = rand::thread_rng();
    let s_bi: BigUint = rng.gen_biguint(256).mod_floor(&subgroup_order);
    let s_str = s_bi.to_str_radix(10);
    let s = Fr::from_str(&s_str).unwrap();
    let p_j = (G2::one() * s); // check base point
    let p_a = AffineG2::from_jacobian(p_j).unwrap();
    (BLSPublicKey {g2p: p_a}, BLSPrivateKey {s: s}) 
}

// hardcode domain?
impl BLSPrivateKey {
    pub fn sign_message(&self, domain: &[u8], message: &[u8]) -> BLSSignature {
        let h = hash_to_point(domain, message);
        // ignores error that should never arise that would return None for h. Handle and Return Option<BLSSignature> instead?
        let sigma = h.unwrap() * self.s;
        BLSSignature {g1p: AffineG1::from_jacobian(sigma).unwrap()}
    }
}

impl BLSPublicKey {
    pub fn to_four_uints(&self) -> (Uint256, Uint256, Uint256, Uint256) {
        let mut out_buf_0 = vec![0u8; 32];
        let mut out_buf_1 = vec![0u8; 32];
        let mut out_buf_2 = vec![0u8; 32];
        let mut out_buf_3 = vec![0u8; 32];

        self.g2p.x().real().to_big_endian(&mut out_buf_0).unwrap();
        self.g2p.x().imaginary().to_big_endian(&mut out_buf_1).unwrap();
        self.g2p.y().real().to_big_endian(&mut out_buf_2).unwrap();
        self.g2p.y().imaginary().to_big_endian(&mut out_buf_3).unwrap();

        (
            Uint256::from_bytes(&out_buf_0),
            Uint256::from_bytes(&out_buf_1),
            Uint256::from_bytes(&out_buf_2),
            Uint256::from_bytes(&out_buf_3),

        )
         
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = vec![0u8; 128];
        self.g2p.x().real().to_big_endian(&mut out[0..32]).unwrap();
        self.g2p.x().imaginary().to_big_endian(&mut out[32..64]).unwrap();
        self.g2p.y().real().to_big_endian(&mut out[64..96]).unwrap();
        self.g2p.y().imaginary().to_big_endian(&mut out[96..128]).unwrap();
        out
    }

}

impl BLSSignature {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = vec![0u8; 64];
        self.g1p.x().to_big_endian(&mut out[0..32]).unwrap();
        self.g1p.y().to_big_endian(&mut out[32..64]).unwrap();
        out
    }
}

impl BLSAggregateSignature {
    pub fn new(sigs: Vec<BLSSignature>) -> Self {
        let mut sum = G1::zero();
        for sig in &sigs {  
            sum = (sum + G1::from(sig.g1p));
        }   
        BLSAggregateSignature{g1p: AffineG1::from_jacobian(sum).unwrap()}
        

    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = vec![0u8; 64];
        self.g1p.x().to_big_endian(&mut out[0..32]).unwrap();
        self.g1p.y().to_big_endian(&mut out[32..64]).unwrap();
        out
    }
}

#[test]
pub fn test_bls_signed_batch() {
    _evm_test_bls_signed_batch(None, false).unwrap();
}

pub fn _evm_test_bls_signed_batch(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);

    let alice_wallet = rt_env.new_wallet();
    let alice_addr = Uint256::from_bytes(alice_wallet.address().as_bytes());
    let bob_wallet = rt_env.new_wallet();
    let bob_addr = Uint256::from_bytes(bob_wallet.address().as_bytes());

    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let mut add_contract = AbiForContract::new_from_file(&builtin_contract_path("Add"))?;
    if add_contract.deploy(&[], &mut machine, Uint256::zero(), None, None, debug).is_err() {
        panic!("failed to deploy Add contract");
    }

    // generate and register BLS keys for Alice and Bob
    let domain = vec![0u8; 32]; // TODO change
    let (alice_public_key, alice_private_key) = generate_bls_key_pair();
    let (bob_public_key, bob_private_key) = generate_bls_key_pair();
    let alice_arb_bls = _ArbBLS::_new(&alice_wallet, debug);
    let bob_arb_bls = _ArbBLS::_new(&bob_wallet, debug);
    let a4 = alice_public_key.to_four_uints();
    alice_arb_bls._register(&mut machine, a4.0, a4.1, a4.2, a4.3)?;
    let b4 = bob_public_key.to_four_uints();
    bob_arb_bls._register(&mut machine, b4.0, b4.1, b4.2, b4.3)?;

    let calldata = add_contract.generate_calldata_for_function(
        "add",
        &[
            ethabi::Token::Uint(Uint256::one().to_u256()),
            ethabi::Token::Uint(Uint256::one().to_u256()),
        ],
    )?;
    let alice_compressed_tx = machine.runtime_env._make_compressed_tx_for_bls(
        &alice_addr,
        Uint256::zero(),
        Uint256::from_u64(100000000),
        add_contract.address.clone(),
        Uint256::zero(),
        &calldata,
    );
    let bob_compressed_tx = machine.runtime_env._make_compressed_tx_for_bls(
        &alice_addr,
        Uint256::zero(),
        Uint256::from_u64(100000000),
        add_contract.address.clone(),
        Uint256::zero(),
        &calldata,
    );

    let alice_sig = alice_private_key.sign_message(&domain, &alice_compressed_tx);
    let bob_sig = bob_private_key.sign_message(&domain, &bob_compressed_tx);

    let aggregated_sig = BLSAggregateSignature::new(
        vec![alice_sig, bob_sig],
    );

    machine.runtime_env._insert_bls_batch(
        &[&alice_addr, &bob_addr],
        &[alice_compressed_tx, bob_compressed_tx],
        &aggregated_sig.to_bytes(),
        &Uint256::from_u64(1749),
    );

    let num_logs_before = machine.runtime_env.get_all_receipt_logs().len();
    let _arbgas_used = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };
    let logs = machine.runtime_env.get_all_receipt_logs();
    assert_eq!(logs.len(), num_logs_before + 2);
    assert!(logs[logs.len() - 2].succeeded());
    assert!(logs[logs.len() - 1].succeeded());

    Ok(())
}
