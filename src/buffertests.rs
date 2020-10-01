/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::uint256::Uint256;
use crate::mavm::Buffer;

use std::convert::TryFrom;

fn hash_buffer(vec: Vec<u8>) -> Uint256 {
    let mut buf = Buffer::empty0();
    for i in 0..vec.len() {
        buf = buf.set_byte(i, vec[i]);
    }
    return buf.avm_hash();
}

#[test]
fn test_hash_test() {
    let mut buf : Vec<u8> = Vec::new();
    buf.resize(64, 0);
    for i in 0..64 {
        buf[i] = u8::try_from(i).unwrap();
    }
    let u1 = Uint256::from_bytes(&buf[0..32]).avm_hash();
    let u2 = Uint256::from_bytes(&buf[32..64]).avm_hash();
    if Uint256::avm_hash2(&u1, &u2) != hash_buffer(buf.to_vec()) {
        panic!("hash mismatch");
    }
}


