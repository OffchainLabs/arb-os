/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Buffer;
use merkletree::hash::Algorithm;
use std::hash::Hasher;
use crypto::sha3::{Sha3, Sha3Mode};
use crypto::digest::Digest;
use bytes::Bytes;
use merkletree::merkle::MerkleTree;
use merkletree::store::VecStore;

pub struct KeccakAlgorithm(Sha3);

impl KeccakAlgorithm {
    pub fn new() -> KeccakAlgorithm {
        KeccakAlgorithm(Sha3::new(Sha3Mode::Keccak256))
    }
}

impl Default for KeccakAlgorithm {
    fn default() -> KeccakAlgorithm {
        KeccakAlgorithm::new()
    }
}

impl Hasher for KeccakAlgorithm {
    #[inline]
    fn write(&mut self, msg: &[u8]) {
        self.0.input(msg)
    }

    #[inline]
    fn finish(&self) -> u64 {
        unimplemented!()
    }
}

impl Algorithm<[u8; 32]> for KeccakAlgorithm {
    #[inline]
    fn hash(&mut self) -> [u8; 32] {
        let mut h = [0u8; 32];
        self.0.result(&mut h);
        h
    }

    #[inline]
    fn reset(&mut self) {
        self.0.reset();
    }
}

fn hash_buffer(buf: Vec<u8>) -> Uint256 {
    let words = Bytes::from(buf);
    let t: MerkleTree<[u8; 32], KeccakAlgorithm, VecStore<_>> =
      MerkleTree::try_from_iter(words.chunks(32).map(|buf| {
        let mut res = [0u8; 32];
        for x in 0..buf.len() {
            res[x] = buf[x]
        }
        // Perhaps need to hash once here
        Ok(res)
      })).unwrap();
    return Uint256::from_bytes(&t.root());
}

#[test]
fn test_hash_test() {
    let buf = [1,2,3,4,5,6,7,8,9,10];
    println!("hashing: {}", hash_buffer(buf.clone()));
    panic!("???");
}


