/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::{zero_hash, Buffer};
use crate::uint256::Uint256;

use rand::Rng;
use std::convert::TryFrom;

fn hash_buffer(buf: &[u8], pack: bool) -> Uint256 {
    if buf.len() == 0 {
        return zero_hash(32);
    }
    if buf.len() == 32 {
        return Uint256::from_bytes(buf).avm_hash();
    }
    let len = buf.len();
    let h2 = hash_buffer(&buf[len / 2..len], false);
    if h2 == zero_hash(32) && pack {
        return hash_buffer(&buf[0..len / 2], true);
    }
    let h1 = hash_buffer(&buf[0..len / 2], false);
    Uint256::avm_hash2(&h1, &h2)
}

fn hash_buffer2(vec: Vec<u8>) -> Uint256 {
    let mut buf = Buffer::empty0();
    for (i, &el) in vec.iter().enumerate() {
        buf = buf.set_byte(i, el);
    }
    buf.avm_hash()
}

#[test]
fn test_hash_test() {
    let mut buf: Vec<u8> = Vec::new();
    buf.resize(64, 0);
    for i in 0..64 {
        buf[i] = u8::try_from(i).unwrap();
    }
    let u1 = Uint256::from_bytes(&buf[0..32]).avm_hash();
    let u2 = Uint256::from_bytes(&buf[32..64]).avm_hash();
    assert_eq!(Uint256::avm_hash2(&u1, &u2), hash_buffer2(buf.to_vec()));

    let mut rng = rand::thread_rng();
    buf.resize(8 * 1024, 0);
    for i in 0..8 * 1024 {
        buf[i] = rng.gen();
    }
    assert_eq!(hash_buffer(&buf, true), hash_buffer2(buf.to_vec()));
}
