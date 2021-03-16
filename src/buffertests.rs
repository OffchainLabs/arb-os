/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

/*
use crate::mavm::{zero_hash, Buffer};
use crate::uint256::Uint256;

use rand::Rng;
use std::convert::TryFrom;

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
*/

