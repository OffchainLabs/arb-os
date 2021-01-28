/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Instruction;
use std::path::Path;
use crate::link::LinkedProgram;
use std::fs::File;
use std::io::Read;
use std::convert::TryInto;
use crate::run::{RuntimeEnvironment, load_from_file};
use crate::uint256::Uint256;
use crate::evm::abi::{_ArbOwner, ArbSys};
use ethers_signers::Signer;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CodeUploader {
    serialized: Vec<u8>,
    full_batches: Vec<Vec<u8>>,
    num_so_far: usize,
    num_total: usize,
}

impl CodeUploader {
    pub fn _new(num_total: usize) -> Self {
        Self {
            serialized: vec![],
            full_batches: vec![],
            num_so_far: 0,
            num_total,
        }
    }

    pub fn _new_from_file(path: &Path) -> Self {
        let mut file = match File::open(&path) {
            Err(why) => panic!("couldn't open {}: {:?}", path.display(), why),
            Ok(file) => file,
        };

        let mut s = String::new();
        s = match file.read_to_string(&mut s) {
            Err(why) => panic!("couldn't read {}: {:?}", path.display(), why),
            Ok(_) => s,
        };

        let parse_result: Result<LinkedProgram, serde_json::Error> = serde_json::from_str(&s);
        match parse_result {
            Ok(prog) => {
                let code_len = prog.code.len();
                let mut ret = CodeUploader::_new(code_len);
                for i in 0..prog.code.len() {
                    ret._serialize_one(&prog.code[code_len-1-i]);
                }
                ret._finish_batch();
                ret
            },
            Err(_) => { panic!(); }
        }
    }

    pub fn _push_byte(&mut self, b: u8) {
        self.serialized.push(b);
    }

    pub fn _push_bytes(&mut self, b: &[u8]) {
        self.serialized.extend(b);
    }

    pub fn _serialize_one(&mut self, insn: &Instruction) {
        insn._upload(self);
        self.num_so_far = self.num_so_far + 1;
        if self.serialized.len() > 3000 {
            self._finish_batch();
        }
    }

    pub fn _translate_pc(&self, pc: usize) -> usize {
        self.num_total - pc
    }

    fn _finish_batch(&mut self) {
        if self.serialized.len() > 0 {
            self.full_batches.push(self.serialized.clone());
            self.serialized = vec![];
        }
    }

    pub fn _finalize(&mut self) -> Vec<Vec<u8>> {
        self._finish_batch();
        self.full_batches.clone()
    }

    pub fn _serialize(&mut self) -> Vec<u8> {
        let mut ret = vec![];
        ret.extend(&(self.num_total as u32).to_be_bytes());
        for buf in self._finalize() {
            ret.extend(&(buf.len() as u32).to_be_bytes());
            ret.extend(buf);
        }
        ret
    }

    pub fn _deserialize(buf: Vec<u8>) -> Self {
        let num_total = u32::from_be_bytes(buf[0..4].try_into().unwrap()) as usize;
        let mut offset = 4;
        let mut full_batches = vec![];
        while offset < buf.len() {
            let batch_size = u32::from_be_bytes(buf[offset..offset+4].try_into().unwrap()) as usize;
            offset = offset + 4;
            full_batches.push(buf[offset..offset+batch_size].to_vec());
            offset = offset + batch_size;
        }
        Self {
            num_total,
            num_so_far: num_total,
            full_batches,
            serialized: vec![],
        }
    }
}

#[test]
fn test_code_upload_prep() {
    let uploader = CodeUploader::_new_from_file(Path::new("arb_os/arbos.mexe"));
    assert!(uploader.num_total > 5000);
    assert_eq!(uploader.num_total, uploader.num_so_far);
    let reconstituted = CodeUploader::_deserialize(uploader.clone()._serialize());
    assert_eq!(uploader, reconstituted);
}

#[test]
fn test_upgrade_arbos_over_itself() {
    _test_upgrade_arbos_over_itself_impl().unwrap();
}

fn _test_upgrade_arbos_over_itself_impl() -> Result<(), ethabi::Error> {
    let rt_env = RuntimeEnvironment::new(Uint256::from_usize(1111), None);
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"), rt_env);
    machine.start_at_zero();

    let wallet = machine.runtime_env.new_wallet();
    let my_addr = Uint256::from_bytes(wallet.address().as_bytes());

    let arbowner = _ArbOwner::_new(&wallet, false);

    arbowner._give_ownership(&mut machine, my_addr, Some(Uint256::zero()))?;

    let uploader = CodeUploader::_new_from_file(Path::new("arb_os/arbos.mexe"));

    arbowner._start_code_upload(&mut machine)?;

    for buf in uploader.full_batches {
        arbowner._continue_code_upload(&mut machine, buf)?;
    }

    arbowner._finish_code_upload_as_arbos_upgrade(&mut machine)?;

    let wallet2 = machine.runtime_env.new_wallet();
    let arbsys = ArbSys::new(&wallet2, false);
    let arbos_version = arbsys._arbos_version(&mut machine)?;
    let arbsys_orig = ArbSys::new(&wallet, false);
    let arbos_version_orig = arbsys_orig._arbos_version(&mut machine)?;
    assert_eq!(arbos_version, arbos_version_orig);

    Ok(())
}
