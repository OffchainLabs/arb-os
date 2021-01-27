/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::Instruction;
use std::path::Path;
use crate::link::LinkedProgram;
use std::fs::File;
use std::io::Read;
use std::convert::TryInto;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CodeUploader {
    serialized: Vec<u8>,
    full_batches: Vec<Vec<u8>>,
    num_so_far: usize,
    num_total: usize,
}

impl CodeUploader {
    pub fn new(num_total: usize) -> Self {
        Self {
            serialized: vec![],
            full_batches: vec![],
            num_so_far: 0,
            num_total,
        }
    }

    pub fn new_from_file(path: &Path) -> Self {
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
                let mut ret = CodeUploader::new(code_len);
                for i in 0..prog.code.len() {
                    ret.serialize_one(&prog.code[code_len-1-i]);
                }
                ret.finish_batch();
                ret
            },
            Err(_) => { panic!(); }
        }
    }

    pub fn push_byte(&mut self, b: u8) {
        self.serialized.push(b);
    }

    pub fn push_bytes(&mut self, b: &[u8]) {
        self.serialized.extend(b);
    }

    pub fn serialize_one(&mut self, insn: &Instruction) {
        insn.upload(self);
        self.num_so_far = self.num_so_far + 1;
        if self.serialized.len() > 3000 {
            self.finish_batch();
        }
    }

    pub fn translate_pc(&self, pc: usize) -> usize {
        self.num_total - pc
    }

    fn finish_batch(&mut self) {
        if self.serialized.len() > 0 {
            self.full_batches.push(self.serialized.clone());
            self.serialized = vec![];
        }
    }

    pub fn finalize(&mut self) -> Vec<Vec<u8>> {
        self.finish_batch();
        self.full_batches.clone()
    }

    pub fn serialize(&mut self) -> Vec<u8> {
        let mut ret = vec![];
        ret.extend(&(self.num_total as u32).to_be_bytes());
        for buf in self.finalize() {
            ret.extend(&(buf.len() as u32).to_be_bytes());
            ret.extend(buf);
        }
        ret
    }

    pub fn deserialize(buf: Vec<u8>) -> Self {
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
    let uploader = CodeUploader::new_from_file(Path::new("arb_os/arbos.mexe"));
    assert!(uploader.num_total > 5000);
    assert_eq!(uploader.num_total, uploader.num_so_far);
    let reconstituted = CodeUploader::deserialize(uploader.clone().serialize());
    assert_eq!(uploader, reconstituted);
}
