/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use ethereum_types::{H160, U256};
use ethers_core::utils::{keccak256, hash_message};
use num_bigint::{BigInt, BigUint, Sign, ToBigInt};
use num_traits::cast::ToPrimitive;
use num_traits::identities::{One, Zero};
use num_traits::pow::Pow;
use num_traits::sign::Signed;
use num_traits::CheckedSub;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Sub};

#[derive(Debug, Clone, PartialEq, Eq, Ord, Hash)]
pub struct Uint256 {
    val: BigUint,
}

impl Uint256 {
    pub fn from_u64(x: u64) -> Self {
        Uint256 {
            val: BigUint::from(x),
        }
    }

    pub fn from_usize(x: usize) -> Self {
        Uint256 {
            val: BigUint::from(x),
        }
    }

    pub fn from_bool(b: bool) -> Self {
        if b {
            Uint256::one()
        } else {
            Uint256::zero()
        }
    }

    pub fn from_string(s: &str) -> Option<Self> {
        match BigUint::parse_bytes(s.as_bytes(), 10) {
            Some(bui) => match Uint256::trim(&bui) {
                (val, true) => Some(Uint256 { val }),
                (_, false) => None,
            },
            None => None,
        }
    }

    pub fn from_string_hex(s: &str) -> Option<Self> {
        match BigUint::parse_bytes(s.as_bytes(), 16) {
            Some(bui) => match Uint256::trim(&bui) {
                (val, true) => Some(Uint256 { val }),
                (_, false) => None,
            },
            None => None,
        }
    }

    pub fn from_signed_string(s: &str) -> Option<Self> {
        let s = s.replace("s", "");
        match BigInt::parse_bytes(s.as_bytes(), 10) {
            Some(bi) => match Uint256::trim(&Uint256::bigint_to_biguint(bi)) {
                (val, true) => Some(Uint256 { val }),
                (_, false) => None,
            },
            None => None,
        }
    }

    pub fn from_bytes(b: &[u8]) -> Self {
        Uint256 {
            val: BigUint::from_bytes_be(b),
        }
    }

    pub fn from_u256(x: &U256) -> Self {
        let mut b: Vec<u8> = vec![0u8; 32];
        x.to_big_endian(&mut b);
        Uint256::from_bytes(&b)
    }

    pub fn to_usize(&self) -> Option<usize> {
        self.val.to_usize()
    }

    pub fn to_bytes_be(&self) -> Vec<u8> {
        // always returns 32 bytes
        let raw = self.val.to_bytes_be();
        if raw.len() < 32 {
            let mut ret = vec![0u8; 32 - raw.len()];
            ret.extend(raw);
            ret
        } else {
            raw
        }
    }

    pub fn to_h160(&self) -> H160 {
        H160::from_slice(&self.to_bytes_minimal())
    }

    pub fn to_u256(&self) -> U256 {
        U256::from_big_endian(&self.to_bytes_minimal())
    }

    pub fn trim_to_u64(&self) -> u64 {
        self.val
            .clone()
            .bitand(BigUint::from(0xffffffffffffffffu64))
            .to_u64()
            .unwrap()
    }

    pub fn to_bytes_minimal(&self) -> Vec<u8> {
        if self.is_zero() {
            vec![]
        } else {
            self.val.to_bytes_be()
        }
    }

    #[cfg(test)]
    pub fn rlp_encode(&self) -> Vec<u8> {
        // RLP encode the minimal byte representation of self
        if (self.is_zero()) {
            vec![0x80u8]
        } else {
            rlp::encode(&self.to_bytes_minimal())
        }
    }

    pub fn zero() -> Self {
        Uint256 {
            val: BigUint::zero(),
        }
    }

    pub fn one() -> Self {
        Uint256 {
            val: BigUint::one(),
        }
    }

    pub fn _max_neg_int() -> Self {
        Uint256::one().exp(&Uint256::from_usize(255))
    }

    pub fn is_zero(&self) -> bool {
        self.val == BigUint::zero()
    }

    pub fn unary_minus(&self) -> Option<Self> {
        let s = self.to_signed();
        if s == BigInt::new(Sign::Minus, vec![0, 0, 0, 0, 0, 0, 0, 0x8000_0000]) {
            None
        } else {
            Some(Uint256 {
                val: Uint256::bigint_to_biguint(s.neg()),
            })
        }
    }

    pub fn bitwise_neg(&self) -> Self {
        let all_ones = BigUint::new(vec![0xffff_ffff; 8]);
        let val = self.val.clone().bitxor(all_ones);
        Uint256 { val }
    }

    pub fn bitwise_and(&self, other: &Self) -> Self {
        let val = self.val.clone().bitand(other.val.clone());
        Uint256 { val }
    }

    pub fn bitwise_or(&self, other: &Self) -> Self {
        let val = self.val.clone().bitor(other.val.clone());
        Uint256 { val }
    }

    pub fn bitwise_xor(&self, other: &Self) -> Self {
        let val = self.val.clone().bitxor(other.val.clone());
        Uint256 { val }
    }

    pub fn add(&self, other: &Self) -> Self {
        let res = self.val.clone().add(&other.val);
        let (val, _) = Uint256::trim(&res);
        Uint256 { val }
    }

    pub fn sub(&self, other: &Self) -> Option<Self> {
        let res = self.val.clone().checked_sub(&other.val)?;
        let (val, _) = Uint256::trim(&res);
        Some(Uint256 { val })
    }

    pub fn unchecked_sub(&self, other: &Self) -> Self {
        // subtraction mod 2**256
        if self < other {
            other.sub(self).unwrap().unary_minus().unwrap()
        } else {
            self.sub(other).unwrap()
        }
    }

    pub fn mul(&self, other: &Self) -> Self {
        let res = self.val.clone().mul(&other.val);
        let (val, _) = Uint256::trim(&res);
        Uint256 { val }
    }

    pub fn div(&self, other: &Self) -> Option<Self> {
        if other.val.is_zero() {
            None
        } else {
            let val = self.val.clone().div(&other.val);
            Some(Uint256 { val })
        }
    }

    pub fn modulo(&self, other: &Self) -> Option<Self> {
        if other.val.is_zero() {
            None
        } else {
            let val = self.val.clone().rem(&other.val);
            Some(Uint256 { val })
        }
    }

    pub fn sdiv(&self, other: &Self) -> Option<Self> {
        if other.val.is_zero() {
            None
        } else {
            let val = self.to_signed().div(&other.to_signed());
            Some(Uint256 {
                val: Uint256::bigint_to_biguint(val),
            })
        }
    }

    pub fn smodulo(&self, other: &Self) -> Option<Self> {
        //TODO: verify that semantics match AVM
        if other.val.is_zero() {
            None
        } else {
            let val = self.to_signed().rem(&other.to_signed());
            Some(Uint256 {
                val: Uint256::bigint_to_biguint(val),
            })
        }
    }

    pub fn add_mod(&self, denom: &Self, modulus: &Self) -> Option<Self> {
        if modulus.val.is_zero() {
            None
        } else {
            let val = self
                .val
                .clone()
                .add(denom.val.clone())
                .rem(modulus.val.clone());
            Some(Uint256 { val })
        }
    }

    pub fn mul_mod(&self, denom: &Self, modulus: &Self) -> Option<Self> {
        if modulus.val.is_zero() {
            None
        } else {
            let val = self
                .val
                .clone()
                .mul(denom.val.clone())
                .rem(modulus.val.clone());
            Some(Uint256 { val })
        }
    }

    pub fn exp(&self, other: &Self) -> Self {
        Uint256 {
            val: Uint256::trim(&self.val.pow(&other.val)).0,
        }
    }

    pub fn s_less_than(&self, other: &Self) -> bool {
        self.to_signed() < other.to_signed()
    }

    fn trim(bui: &BigUint) -> (BigUint, bool) {
        if bui.bits() <= 256 {
            (bui.clone(), true)
        } else {
            let mask = BigUint::new(vec![0xffff_ffff; 8]);
            (bui.bitand(mask), false)
        }
    }

    fn to_signed(&self) -> BigInt {
        if self.val < BigUint::new(vec![0, 0, 0, 0, 0, 0, 0, 0x8000_0000]) {
            self.val.to_bigint().unwrap()
        } else {
            let unshifted = self.val.to_bigint().unwrap();
            let shift = BigInt::new(Sign::Plus, vec![0, 0, 0, 0, 0, 0, 0, 0, 1]);
            shift.sub(unshifted)
        }
    }

    fn bigint_to_biguint(bi: BigInt) -> BigUint {
        if bi.is_negative() {
            bi.add(BigInt::new(Sign::Plus, vec![0, 0, 0, 0, 0, 0, 0, 0, 1]))
                .to_biguint()
                .unwrap()
        } else {
            bi.to_biguint().unwrap()
        }
    }

    pub fn avm_hash(&self) -> Self {
        let bytes_buf = self.to_bytes_be();
        let hash_result = keccak256(&bytes_buf); // keccak
        Uint256::from_bytes(&hash_result)
    }

    pub fn avm_hash2(v1: &Self, v2: &Self) -> Self {
        let mut bytes1 = v1.to_bytes_be();
        let bytes2 = v2.to_bytes_be();
        bytes1.extend(bytes2);
        let hash_result = keccak256(&bytes1);
        Uint256::from_bytes(&hash_result)
    }
}

impl PartialOrd for Uint256 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.val.cmp(&other.val))
    }
}

impl fmt::Display for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cutoff: usize = 1 << 32;
        if self.val < BigUint::from(cutoff) {
            write!(f, "{}", self.val)
        } else {
            write!(f, "{:#x}", self.val)
        }
    }
}

impl Serialize for Uint256 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = String::new();
        s.push_str(&self.val.to_str_radix(16));
        s.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Uint256 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(Uint256 {
            val: BigUint::parse_bytes(s.as_bytes(), 16).unwrap(),
        })
    }
}

#[test]
fn test_keccak_vs_messagehash() {
    let mut extended: Vec<u8> = "\x19Ethereum Signed Message:\n32".as_bytes().to_vec();
    panic!("{:x?}", extended);
    let the_string = [0u8;32];
    extended.extend(&the_string);
    assert_eq!(hash_message(the_string).as_bytes(), keccak256(&extended));
}