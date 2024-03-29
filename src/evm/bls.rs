/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

/*
use crate::compile::miniconstants::init_constant_table;
use crate::uint256::Uint256;
use crypto::digest::Digest;
use crypto::sha2::Sha256;
use num_bigint::BigUint;
use num_bigint::RandBigInt;
use num_bigint::ToBigUint;
use num_integer::Integer;
use parity_bn::{AffineG1, AffineG2, Fq, Fr, Group, G1, G2};
use std::cmp::Ordering;
use std::path::Path;


#[test]
fn test_bls_registry() {
    evm_test_bls_registry(None, false);
}

pub fn evm_test_bls_registry(log_to: Option<&Path>, debug: bool) {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero(true);

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arb_bls = ArbBLS::new(&wallet, debug);

    assert!(arb_bls
        .get_public_key(&mut machine, remap_l1_sender_address(my_addr.clone()))
        .is_err());

    let expected = [
        Uint256::from_u64(13),
        Uint256::from_u64(26),
        Uint256::from_u64(39),
        Uint256::from_u64(52),
    ];
    assert!(arb_bls
        .register(
            &mut machine,
            expected[0].clone(),
            expected[1].clone(),
            expected[2].clone(),
            expected[3].clone(),
            false,
        )
        .is_ok());

    let (res0, res1, res2, res3) =
        match arb_bls.get_public_key(&mut machine, remap_l1_sender_address(my_addr.clone())) {
            Ok(res) => res,
            Err(_) => panic!(),
        };
    assert_eq!(res0, expected[0]);
    assert_eq!(res1, expected[1]);
    assert_eq!(res2, expected[2]);
    assert_eq!(res3, expected[3]);

    if let Some(path) = log_to {
        let _ = machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    machine.write_coverage("evm_test_bls_registry".to_string());
}

fn to_32_bytes_be(bi: &BigUint) -> Vec<u8> {
    let bytes = bi.to_bytes_be();
    let len = bytes.len();
    if len > 32 {
        panic!(); // altenratively, retrun error cal like all 0 vector?
    } else if len < 32 {
        let mut out = vec![0u8; 32];
        out[32 - len..32].clone_from_slice(&bytes);
        return out;
    }
    bytes
}

pub fn hash_to_point(domain: &Uint256, msg: &[u8]) -> Option<G1> {
    let (u0, u1) = hash_to_field(domain, msg);
    if let Some((px_bi, py_bi)) = map_to_g1(&u0) {
        if let Some((qx_bi, qy_bi)) = map_to_g1(&u1) {
            let px = Fq::from_slice(&to_32_bytes_be(&px_bi)).unwrap();
            let py = Fq::from_slice(&to_32_bytes_be(&py_bi)).unwrap();
            let qx = Fq::from_slice(&to_32_bytes_be(&qx_bi)).unwrap();
            let qy = Fq::from_slice(&to_32_bytes_be(&qy_bi)).unwrap();

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

            let ret = p + q;
            let mut out_buf_0 = vec![0u8; 32];
            ret.x().to_big_endian(&mut out_buf_0).unwrap();
            let mut out_buf_1 = vec![0u8; 32];
            ret.y().to_big_endian(&mut out_buf_1).unwrap();
            return Some(ret);
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
    // _sqrt(-3)
    let z0 = BigUint::parse_bytes(
        b"0000000000000000b3c4d79d41a91759a9e4c7e359b6b89eaec68e62effffffd",
        16,
    )
    .unwrap();
    // (_sqrt(-3) - 1)  / 2
    let z1 = BigUint::parse_bytes(
        b"000000000000000059e26bcea0d48bacd4f263f1acdb5c4f5763473177fffffe",
        16,
    )
    .unwrap();

    if (_x.cmp(&field_order) != Ordering::Less) {
        return None;
    }

    let found_first_sqrt = sqrt(_x).is_some();

    let mut a0 = (_x * _x).mod_floor(&field_order);
    a0 = (a0 + &ToBigUint::to_biguint(&4).unwrap()).mod_floor(&field_order);
    let mut a1 = (_x * z0).mod_floor(&field_order);
    let mut a2 = (&a0 * &a1).mod_floor(&field_order);
    a2 = inverse(&a2);
    a1 = (&a1 * &a1).mod_floor(&field_order);
    a1 = (&a1 * &a2).mod_floor(&field_order);

    a1 = (_x * &a1).mod_floor(&field_order);

    let mut x = (z1 + (&field_order - &a1)).mod_floor(&field_order);

    a1 = (&x * &x).mod_floor(&field_order);
    a1 = (&a1 * &x).mod_floor(&field_order);
    a1 = (&a1 + &ToBigUint::to_biguint(&3).unwrap()).mod_floor(&field_order);

    if let Some(sqa1) = sqrt(&a1) {
        a1 = if (found_first_sqrt) {
            sqa1
        } else {
            &field_order - sqa1
        };
        return Some((x, a1));
    }

    x = (x + &ToBigUint::to_biguint(&1).unwrap()).mod_floor(&field_order);
    x = &field_order - &x;

    a1 = (&x * &x).mod_floor(&field_order);
    a1 = (&a1 * &x).mod_floor(&field_order);
    a1 = (&a1 + &ToBigUint::to_biguint(&3).unwrap()).mod_floor(&field_order);

    if let Some(sqa1) = sqrt(&a1) {
        a1 = if (found_first_sqrt) {
            sqa1
        } else {
            &field_order - sqa1
        };
        return Some((x, a1));
    }

    x = (&a0 * &a0).mod_floor(&field_order);
    x = (&x * &x).mod_floor(&field_order);
    x = (&x * &a2).mod_floor(&field_order);
    x = (&x * &a2).mod_floor(&field_order);
    x = (&x + &ToBigUint::to_biguint(&1).unwrap()).mod_floor(&field_order);

    a1 = (&x * &x).mod_floor(&field_order);
    a1 = (&a1 * &x).mod_floor(&field_order);
    a1 = (&a1 + &ToBigUint::to_biguint(&3).unwrap()).mod_floor(&field_order);

    if let Some(sqa1) = sqrt(&a1) {
        a1 = if (found_first_sqrt) {
            sqa1
        } else {
            &field_order - sqa1
        };
        Some((x, a1))
    } else {
        None
    }
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
    if (square.cmp(xx) == Ordering::Equal) {
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

fn hash_to_field(domain: &Uint256, msg: &[u8]) -> (BigUint, BigUint) {
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

pub fn expand_msg_to_96(domain: &Uint256, msg: &[u8]) -> Vec<u8> {
    let t0 = msg.len();
    let mut out = vec![0; 96];
    let len0 = 64 + t0 + 32 + 4;
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

    in0[off..off + 32].clone_from_slice(&domain.to_bytes_be());
    off += 32;
    in0[off] = 32u8;

    let mut hasher = Sha256::new();
    hasher.input(&in0);
    let mut b0 = vec![0; 32];
    hasher.result(&mut b0);

    let len1 = 32 + 1 + 32 + 1;

    let mut in1 = vec![0; len1];
    in1[0..32].clone_from_slice(&b0);
    off = 32;

    in1[off] = 1;
    off += 1;
    in1[off..off + 32].clone_from_slice(&domain.to_bytes_be());
    off += 32;
    in1[off] = 32u8;

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

    in1[off..off + 32].clone_from_slice(&domain.to_bytes_be());
    off = off + 32;
    in1[off] = 32u8;

    hasher.reset();
    hasher.input(&in1);
    hasher.result(&mut bi);

    out[32..64].clone_from_slice(&bi);

    t = b0.iter().zip(bi.iter()).map(|(&l, &r)| l ^ r).collect();

    in1[0..32].clone_from_slice(&t);

    off = 32;
    in1[off] = 3;
    off = off + 1;

    in1[off..off + 32].clone_from_slice(&domain.to_bytes_be());
    off = off + 32;
    in1[off] = 32u8;

    hasher.reset();
    hasher.input(&in1);
    hasher.result(&mut bi);
    out[64..96].clone_from_slice(&bi);
    out
}

pub struct BLSPublicKey {
    g2p: AffineG2,
}

pub struct BLSPrivateKey {
    s: Fr,
}

pub struct BLSSignature {
    g1p: AffineG1,
}

pub struct BLSAggregateSignature {
    g1p: AffineG1,
}

pub fn generate_bls_key_pair() -> (BLSPublicKey, BLSPrivateKey) {
    // can't use built in FR::random() due to versioning mismatch of rand
    let subgroup_order = BigUint::parse_bytes(
        b"30644E72E131A029B85045B68181585D2833E84879B9709143E1F593F0000001",
        16,
    )
    .unwrap();
    let mut rng = rand::thread_rng();
    let s_bi: BigUint = rng.gen_biguint(256).mod_floor(&subgroup_order);
    let s_str = s_bi.to_str_radix(10);
    let s = Fr::from_str(&s_str).unwrap();
    let p_j = (G2::one() * s); // check base point
    let p_a = AffineG2::from_jacobian(p_j).unwrap();
    (BLSPublicKey { g2p: p_a }, BLSPrivateKey { s })
}

fn domain_for_sender(sender: Uint256) -> Uint256 {
    Uint256::avm_hash2(
        init_constant_table(Some(Path::new("arb_os/constants.json")))
            .unwrap()
            .get("BLSSignatureDomainBase")
            .unwrap(),
        &sender,
    )
}

impl BLSPrivateKey {
    pub fn sign_message(&self, sender_address: Uint256, message: &[u8]) -> BLSSignature {
        let h = hash_to_point(&domain_for_sender(sender_address), message);
        // ignores error that should never arise that would return None for h. Handle and Return Option<BLSSignature> instead?
        let sigma = h.unwrap() * self.s;
        BLSSignature {
            g1p: AffineG1::from_jacobian(sigma).unwrap(),
        }
    }
}

impl BLSPublicKey {
    pub fn to_four_uints(&self) -> (Uint256, Uint256, Uint256, Uint256) {
        let mut out_buf_0 = vec![0u8; 32];
        let mut out_buf_1 = vec![0u8; 32];
        let mut out_buf_2 = vec![0u8; 32];
        let mut out_buf_3 = vec![0u8; 32];

        self.g2p.x().real().to_big_endian(&mut out_buf_0).unwrap();
        self.g2p
            .x()
            .imaginary()
            .to_big_endian(&mut out_buf_1)
            .unwrap();
        self.g2p.y().real().to_big_endian(&mut out_buf_2).unwrap();
        self.g2p
            .y()
            .imaginary()
            .to_big_endian(&mut out_buf_3)
            .unwrap();

        (
            Uint256::from_bytes(&out_buf_0),
            Uint256::from_bytes(&out_buf_1),
            Uint256::from_bytes(&out_buf_2),
            Uint256::from_bytes(&out_buf_3),
        )
    }
}

impl BLSAggregateSignature {
    pub fn new(sigs: Vec<BLSSignature>) -> Self {
        let mut sum = G1::zero();
        for sig in &sigs {
            sum = (sum + G1::from(sig.g1p));
        }
        BLSAggregateSignature {
            g1p: AffineG1::from_jacobian(sum).unwrap(),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = vec![0u8; 64];
        self.g1p.x().to_big_endian(&mut out[0..32]).unwrap();
        self.g1p.y().to_big_endian(&mut out[32..64]).unwrap();
        out
    }
}
 */
