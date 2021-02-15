/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Buffer;
use crate::uint256::Uint256;

use rand::Rng;
use std::convert::TryFrom;

pub fn zero_hash(sz: u8) -> Uint256 {
    if sz == 32 {
        return Uint256::zero().avm_hash();
    }
    let h1 = zero_hash(sz/2);
    Uint256::avm_hash2(&h1, &h1)
}

fn hash_buffer_aux(buf: &[u8], pack: bool) -> (Uint256, bool) {
    if buf.len() == 0 {
        return (zero_hash(32), true);
    }
    if buf.len() == 32 {
        let res = Uint256::from_bytes(buf).avm_hash();
        let zero = res == zero_hash(32);
        return (res, zero)
    }
    let len = buf.len();
    let (h2, zero2) = hash_buffer_aux(&buf[len / 2..len], false);
    if zero2 && pack {
        // println!("zero2 pack {}", len);
        return hash_buffer_aux(&buf[0..len / 2], pack);
    }
    let (h1, zero1) = hash_buffer_aux(&buf[0..len / 2], false);
    // println!("normal pack {} zero1 {} zero2 {} {}", pack, zero1, zero2, len);
    (Uint256::avm_hash2(&h1, &h2), zero2 && zero1)
}

fn hash_full_buffer(buf: &[u8]) -> Uint256 {
    if buf.len() == 0 {
        return zero_hash(32);
    }
    if buf.len() == 32 {
        return Uint256::from_bytes(buf).avm_hash();
    }
    let len = buf.len();
    let h1 = hash_full_buffer(&buf[0..len / 2]);
    let h2 = hash_full_buffer(&buf[len / 2..len]);
    Uint256::avm_hash2(&h1, &h2)
}

fn hash_buffer(buf: &[u8]) -> Uint256 {
    let (res, _) = hash_buffer_aux(buf, true);
    res
}

fn hash_buffer2(vec: Vec<u8>) -> Uint256 {
    let mut buf = Buffer::empty0();
    for (i, &el) in vec.iter().enumerate() {
        buf = buf.set_byte(i, el);
    }
    buf.hash().hash
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
    assert_eq!(hash_buffer(&buf), hash_buffer2(buf.to_vec()));

    let mut buf: Vec<u8> = Vec::new();
    buf.resize(8 * 1024, 0);
    assert_eq!(hash_buffer(&buf), zero_hash(32));

    // Test single value buffers
    for i in 0..1024 {
        let mut buf: Vec<u8> = Vec::new();
        buf.resize(32 * 1024, 0);
        buf[i*32] = 123;
        let b = Buffer::empty0();
        let b = b.set_byte(i*32, 123);
        if i >= 512 {
            assert_eq!(hash_buffer(&buf), hash_full_buffer(&buf));
        }
        assert_eq!(hash_buffer(&buf), b.hash().hash);
    }

}
