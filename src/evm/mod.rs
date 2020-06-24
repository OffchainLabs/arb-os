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

use crate::compile::{CompileError, CompiledProgram, Type};
use crate::link::{link, postlink_compile, ImportedFunc, LinkedProgram};
use crate::mavm::{Instruction, Label, LabelGenerator, Opcode, Value};
use crate::run::{bytes_from_bytestack, load_from_file, RuntimeEnvironment};
use crate::stringtable::StringTable;
use crate::uint256::Uint256;
use ethabi::Token;
use serde::Serialize;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;
use std::usize;

mod abi;

pub fn compile_evm_file(path: &Path, debug: bool) -> Result<Vec<LinkedProgram>, CompileError> {
    match evm_json_from_file(path) {
        Ok(evm_json) => compile_from_json(evm_json, debug),
        Err(e) => {
            println!("Error reading in EVM file: {:?}", e);
            Err(CompileError::new(
                "error parsing compiled EVM file".to_string(),
                None,
            ))
        }
    }
}

fn evm_json_from_file(path: &Path) -> Result<serde_json::Value, serde_json::Error> {
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {:?}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    s = match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {:?}", display, why),
        Ok(_) => s,
    };

    return serde_json::from_str(&s);
}

#[derive(Serialize)]
pub struct CompiledEvmContract {
    address: Uint256,
    storage: HashMap<Uint256, Uint256>,
    code: Vec<Instruction>,
    evm_pcs: Vec<usize>,
}

impl CompiledEvmContract {
    fn get_storage_info_struct(&self) -> Value {
        let mut ret = Value::none();
        for (offset, val) in &self.storage {
            ret = Value::Tuple(vec![
                ret,
                Value::Int(offset.clone()),
                Value::Int(val.clone()),
            ]);
        }
        ret
    }
}

pub fn compile_from_json(
    evm_json: serde_json::Value,
    debug: bool,
) -> Result<Vec<LinkedProgram>, CompileError> {
    if let serde_json::Value::Array(contracts) = evm_json {
        let mut linked_contracts = Vec::new();
        let mut label_gen = LabelGenerator::new();
        for contract in contracts {
            if let serde_json::Value::Object(items) = contract {
                let (compiled_contract, lg) = compile_from_evm_contract(items, label_gen)?;
                let storage_info_struct = compiled_contract.get_storage_info_struct();
                let linked_contract = postlink_compile(
                    link(
                        &[CompiledProgram::new(
                            compiled_contract.code,
                            vec![],
                            vec![],
                            0,
                            None,
                        )],
                        true,
                        Some(storage_info_struct),
                        false,
                    )
                    .unwrap(), // BUGBUG--this should handle errors gracefully, not just panic
                    true,
                    compiled_contract.evm_pcs,
                    debug,
                )
                .unwrap(); //BUGBUG--this should handle errors gracefully, not just panic
                linked_contracts.push(linked_contract);
                label_gen = lg;
            } else {
                return Err(CompileError::new(
                    "unexpected contents in EVM json file".to_string(),
                    None,
                ));
            }
        }
        Ok(linked_contracts)
    } else {
        Err(CompileError::new(
            "unexpected contents in EVM json file".to_string(),
            None,
        ))
    }
}

pub fn compile_from_evm_contract(
    contract: serde_json::map::Map<String, serde_json::Value>,
    mut label_gen: LabelGenerator,
) -> Result<(CompiledEvmContract, LabelGenerator), CompileError> {
    let code_str = if let serde_json::Value::String(s) = &contract["code"] {
        s
    } else {
        return Err(CompileError::new(
            "bad code string in EVM json file".to_string(),
            None,
        ));
    };
    let mut code = Vec::new();
    let mut evm_func_map = HashMap::new();
    for (idx, name) in EMULATION_FUNCS.iter().enumerate() {
        evm_func_map.insert(*name, Label::Runtime(idx));
    }
    let decoded_insns = hex::decode(&code_str[2..]).unwrap();

    // strip cbor info
    let cbor_length = u16::from_be_bytes(
        decoded_insns[decoded_insns.len() - 2..]
            .try_into()
            .expect("unexpected u16 parsing error"),
    );
    let cbor_length = cbor_length as usize;
    let decoded_insns = &decoded_insns[..(decoded_insns.len() - cbor_length - 2)];

    let mut evm_pcs = Vec::new();
    let mut i = 0;
    while i < decoded_insns.len() {
        let insn = decoded_insns[i];
        if (insn >= 0x60) && (insn <= 0x7f) {
            let nbytes = usize::from(insn) - 0x5f;
            code = compile_push_insn(
                &decoded_insns[(i + 1)..(usize::from(insn) + i + 2 - 0x60)],
                code,
            );
            i += (nbytes + 1);
        } else {
            match compile_evm_insn(insn, i, code, label_gen, &evm_func_map) {
                Some((c, lg, maybe_pc)) => {
                    code = c;
                    label_gen = lg;
                    if let Some(pc) = maybe_pc {
                        evm_pcs.push(pc);
                    }
                    i += 1;
                }
                None => {
                    return Err(CompileError::new(
                        "unsupported instruction in EVM code".to_string(),
                        None,
                    ));
                }
            }
        }
    }

    Ok((
        CompiledEvmContract {
            address: if let serde_json::Value::String(s) = &contract["address"] {
                Uint256::from_string_hex(&s[2..]).unwrap()
            } else {
                return Err(CompileError::new(
                    "invalid address format in EVM json file".to_string(),
                    None,
                ));
            },
            storage: if let serde_json::Value::Object(m) = &contract["storage"] {
                let mut hm = HashMap::new();
                for (k, v) in m {
                    hm.insert(
                        Uint256::from_string_hex(&k[2..]).unwrap(),
                        if let serde_json::Value::String(s) = v {
                            Uint256::from_string_hex(&s[2..]).unwrap()
                        } else {
                            return Err(CompileError::new(
                                "invalid storage value format in EVM json file".to_string(),
                                None,
                            ));
                        },
                    );
                }
                hm
            } else {
                return Err(CompileError::new(
                    "invalid storage format in EVM json file".to_string(),
                    None,
                ));
            },
            code,
            evm_pcs,
        },
        label_gen,
    ))
}

pub fn compile_evm_insn(
    evm_insn: u8,
    evm_pc: usize,
    mut code: Vec<Instruction>,
    label_gen: LabelGenerator,
    evm_func_map: &HashMap<&str, Label>,
) -> Option<(Vec<Instruction>, LabelGenerator, Option<usize>)> {
    match evm_insn {
        0x00 => evm_emulate(code, label_gen, evm_func_map, "evmOp_stop"), // STOP
        0x01 => { // ADD
            code.push(Instruction::from_opcode(Opcode::Plus, None));
            Some((code, label_gen, None))
        }
        0x02 => { // MUL
            code.push(Instruction::from_opcode(Opcode::Mul, None));
            Some((code, label_gen, None))
        }
        0x03 => { // SUB
            code.push(Instruction::from_opcode(Opcode::Minus, None));
            Some((code, label_gen, None))
        }
        0x04 => { // DIV
            // structure is complex because of nonstandard semantics of DIV in EVM
            // EVM DIV returns zero if denominator is zero
            let (mid_label, lg) = label_gen.next();
            let (end_label, lg) = lg.next();
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::zero()), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));
            code.push(Instruction::from_opcode(Opcode::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Div, None));
            code.push(Instruction::from_opcode(Opcode::Label(end_label), None));
            Some((code, lg, None))
        }
        0x05 => { // SDIV
             // structure is complex because of nonstandard semantics of SDIV in EVM
            // EVM SDIV returns zero if denominator is zero
            // EVM SDIV returns MaxNegInt if numerator is MaxNegInt and denominator is -1
            let max_neg_int = Value::Int(Uint256::max_neg_int());
            let minus_one = Value::Int(Uint256::one().unary_minus().unwrap());
            let (mid_label, lg) = label_gen.next();
            let (mid2_label, lg) = lg.next();
            let (end_label, lg) = lg.next();
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid_label), None));

            // case: denominator == 0
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::zero()), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));

            code.push(Instruction::from_opcode(Opcode::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Dup0, None));
            code.push(Instruction::from_opcode_imm(Opcode::NotEqual, max_neg_int, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid2_label), None));
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            code.push(Instruction::from_opcode_imm(Opcode::NotEqual, minus_one, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid2_label), None));

            // case: numerator == MaxNegInt  &&  denominator == -1
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));


            code.push(Instruction::from_opcode(Opcode::Label(mid2_label), None));
            // general case
            code.push(Instruction::from_opcode(Opcode::Div, None));

            code.push(Instruction::from_opcode(Opcode::Label(end_label), None));
            Some((code, lg, None))
        }
        0x06 => { // MOD
            // structure is complex because of nonstandard semantics of MOD in EVM
            // EVM MOD returns zero if modulus is zero
            let (mid_label, lg) = label_gen.next();
            let (end_label, lg) = lg.next();
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::zero()), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));
            code.push(Instruction::from_opcode(Opcode::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Mod, None));
            code.push(Instruction::from_opcode(Opcode::Label(end_label), None));
            Some((code, lg, None))
        }
        0x07 => { // SMOD
            // structure is complex because of nonstandard semantics of SMOD in EVM
            // EVM SMOD returns zero if modulus is zero
            let (mid_label, lg) = label_gen.next();
            let (end_label, lg) = lg.next();
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::zero()), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));
            code.push(Instruction::from_opcode(Opcode::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Smod, None));
            code.push(Instruction::from_opcode(Opcode::Label(end_label), None));
            Some((code, lg, None))
        }
        0x08 => { // ADDMOD
            // structure is complex because of nonstandard semantics of ADDMOD in EVM
            // EVM ADDMOD returns zero if modulus is zero
            let (mid_label, lg) = label_gen.next();
            let (end_label, lg) = lg.next();
            code.push(Instruction::from_opcode(Opcode::Dup2, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::zero()), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));
            code.push(Instruction::from_opcode(Opcode::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::AddMod, None));
            code.push(Instruction::from_opcode(Opcode::Label(end_label), None));
            Some((code, lg, None))
        }
        0x09 => { // MULMOD
            // structure is complex because of nonstandard semantics of MULMOD in EVM
            // EVM MULMOD returns zero if modulus is zero
            let (mid_label, lg) = label_gen.next();
            let (end_label, lg) = lg.next();
            code.push(Instruction::from_opcode(Opcode::Dup2, None));
            code.push(Instruction::from_opcode_imm(Opcode::Cjump, Value::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::zero()), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(end_label), None));
            code.push(Instruction::from_opcode(Opcode::Label(mid_label), None));
            code.push(Instruction::from_opcode(Opcode::MulMod, None));
            code.push(Instruction::from_opcode(Opcode::Label(end_label), None));
            Some((code, lg, None))
        }
        0x0a => { // EXP
            code.push(Instruction::from_opcode(Opcode::Exp, None));
            Some((code, label_gen, None))
        }
        0x0b => { // SIGNEXTEND
            code.push(Instruction::from_opcode(Opcode::SignExtend, None));
            Some((code, label_gen, None))
        }
        // 0x0c - 0x0f unused
        0x10 => { // LT
            code.push(Instruction::from_opcode(Opcode::LessThan, None));
            Some((code, label_gen, None))
        }
        0x11 => { // GT
            code.push(Instruction::from_opcode(Opcode::GreaterThan, None));
            Some((code, label_gen, None))
        }
        0x12 => { // SLT
            code.push(Instruction::from_opcode(Opcode::SLessThan, None));
            Some((code, label_gen, None))
        }
        0x13 => { // SGT
            code.push(Instruction::from_opcode(Opcode::SGreaterThan, None));
            Some((code, label_gen, None))
        }
        0x14 => { // EQ
            code.push(Instruction::from_opcode(Opcode::Equal, None));
            Some((code, label_gen, None))
        }
        0x15 => { // ISZERO
            code.push(Instruction::from_opcode(Opcode::IsZero, None));
            Some((code, label_gen, None))
        }
        0x16 => { // AND
            code.push(Instruction::from_opcode(Opcode::BitwiseAnd, None));
            Some((code, label_gen, None))
        }
        0x17 => { // OR
            code.push(Instruction::from_opcode(Opcode::BitwiseOr, None));
            Some((code, label_gen, None))
        }
        0x18 => { // XOR
            code.push(Instruction::from_opcode(Opcode::BitwiseXor, None));
            Some((code, label_gen, None))
        }
        0x19 => { // NOT
            code.push(Instruction::from_opcode(Opcode::BitwiseNeg, None));
            Some((code, label_gen, None))
        }
        0x1a => { // BYTE
            code.push(Instruction::from_opcode(Opcode::Byte, None));
            Some((code, label_gen, None))
        }
        0x1b => { // SHL
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Exp, Value::Int(Uint256::from_usize(2)), None));
            code.push(Instruction::from_opcode(Opcode::Mul, None));
            Some((code, label_gen, None))
        }
        0x1c => { // SHR
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Exp, Value::Int(Uint256::from_usize(2)), None));
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::Div, None));
            Some((code, label_gen, None))
        }
        0x1d => { // SAR
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Exp, Value::Int(Uint256::from_usize(2)), None));
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::Sdiv, None));
            Some((code, label_gen, None))
        }
        // 0x1e - 0x1f unused
        0x20 => evm_emulate(code, label_gen, evm_func_map, "evmOp_sha3"), // SHA3
        // 0x21-0x2f unused
        0x30 => evm_emulate(code, label_gen, evm_func_map, "evmOp_address"), // ADDRESS
        0x31 => evm_emulate(code, label_gen, evm_func_map, "evmOp_balance"),// BALANCE
        0x32 => evm_emulate(code, label_gen, evm_func_map, "evmOp_origin"), // ORIGIN
        0x33 => evm_emulate(code, label_gen, evm_func_map, "evmOp_caller"), // CALLER
        0x34 => evm_emulate(code, label_gen, evm_func_map, "evmOp_callvalue"), // CALLVALUE
        0x35 => evm_emulate(code, label_gen, evm_func_map, "evmOp_calldataload"), // CALLDATALOAD
        0x36 => evm_emulate(code, label_gen, evm_func_map, "evmOp_calldatasize"), // CALLDATASIZE
        0x37 => evm_emulate(code, label_gen, evm_func_map, "evmOp_calldatacopy"), // CALLDATACOPY
        0x38 => evm_emulate(code, label_gen, evm_func_map, "evmOp_codesize"), // CODESIZE
        0x39 => evm_emulate(code, label_gen, evm_func_map, "evmOp_codecopy"), // CODECOPY
        0x3a => { // GASPRICE
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::one()), None));
            Some((code, label_gen, None))
        }
        0x3b => evm_emulate(code, label_gen, evm_func_map, "evmOp_extcodesize"), // EXTCODESIZE 
        0x3c => evm_emulate(code, label_gen, evm_func_map, "evmOp_extcodecopy"), // EXTCODECOPY  
        0x3d => evm_emulate(code, label_gen, evm_func_map, "evmOp_returndatasize"), // RETURNDATASIZE 
        0x3e => evm_emulate(code, label_gen, evm_func_map, "evmOp_returndatacopy"), // RETURNDATACOPY
        // 0x3f unused
        0x40 => None, // BLOCKHASH
        0x41 => None, // COINBASE
        0x42 => evm_emulate(code, label_gen, evm_func_map, "evmOp_timestamp"), // TIMESTAMP 
        0x43 => evm_emulate(code, label_gen, evm_func_map, "evmOp_number"), // NUMBER
        0x44 => None, // DIFFICULTY
        0x45 => { // GASLIMIT
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::from_usize(10_000_000_000)), None));
            Some((code, label_gen, None))
        }
        // 0x46-0x4f unused
        0x50 => { // POP
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            Some((code, label_gen, None))
        }
        0x51 => evm_emulate(code, label_gen, evm_func_map, "evmOp_mload"), // MLOAD
        0x52 => evm_emulate(code, label_gen, evm_func_map, "evmOp_mstore"), // MSTORE
        0x53 => evm_emulate(code, label_gen, evm_func_map, "evmOp_mstore8"), // MSTORE8
        0x54 => evm_emulate(code, label_gen, evm_func_map, "evmOp_sload"), // SLOAD
        0x55 => evm_emulate(code, label_gen, evm_func_map, "evmOp_sstore"), // SSTORE
        0x56 => { // JUMP
            let (c, lg, mpc) = evm_emulate(code, label_gen, evm_func_map, "evmOp_getjumpaddr")?;
            code = c;
            code.push(Instruction::from_opcode(Opcode::Jump, None));
            Some((code, lg, mpc))
        }
        0x57 => {  // JUMPI
            let (not_taken_label, lg) = label_gen.next();
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::IsZero, None));
            code.push(Instruction::from_opcode_imm(
                Opcode::Cjump,
                Value::Label(not_taken_label),
                None
            ));
            let (c, lg, mpc) = evm_emulate(code, lg, evm_func_map, "evmOp_getjumpaddr")?;
            code = c;
            code.push(Instruction::from_opcode(Opcode::Jump, None));
            code.push(Instruction::from_opcode(Opcode::Label(not_taken_label), None));
            Some((code, lg, mpc))
        }
        0x58 => None, // GETPC
        0x59 => evm_emulate(code, label_gen, evm_func_map, "evmOp_msize"), // MSIZE
        0x5a => { // GAS
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::from_usize(9_999_999_999)), None));
            Some((code, label_gen, None))
        }
        0x5b => { // JUMPDEST
            code.push(Instruction::from_opcode(Opcode::Label(Label::Evm(evm_pc)), None));
            Some((code, label_gen, Some(evm_pc)))
        }
        // 0x5c - 0x5f unused
        0x60 | // PUSHn instructions -- will call another function to handle these
        0x61 |
        0x62 |
        0x63 |
        0x64 |
        0x65 |
        0x66 |
        0x67 |
        0x68 |
        0x69 |
        0x6a |
        0x6b |
        0x6c |
        0x6d |
        0x6e |
        0x6f |
        0x70 |
        0x71 |
        0x72 |
        0x73 |
        0x74 |
        0x75 |
        0x76 |
        0x77 |
        0x78 |
        0x79 |
        0x7a |
        0x7b |
        0x7c |
        0x7d |
        0x7e |
        0x7f => { panic!("called evm_compile_insn with push-type instruction"); }
        // DUP instructions follow; note that DUPn on EVM corresponds to dup(n-1) on AVM
        0x80 => { // DUP1  
            code.push(Instruction::from_opcode(Opcode::Dup0, None));
            Some((code, label_gen, None))
        }
        0x81 => { // DUP2 
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            Some((code, label_gen, None))
        }
        0x82 => { // DUP3  
            code.push(Instruction::from_opcode(Opcode::Dup2, None));
            Some((code, label_gen, None))
        }
        0x83 => Some((gen_dupn(code, 3), label_gen, None)),
        0x84 => Some((gen_dupn(code, 4), label_gen, None)),
        0x85 => Some((gen_dupn(code, 5), label_gen, None)),
        0x86 => Some((gen_dupn(code, 6), label_gen, None)),
        0x87 => Some((gen_dupn(code, 7), label_gen, None)),
        0x88 => Some((gen_dupn(code, 8), label_gen, None)),
        0x89 => Some((gen_dupn(code, 9), label_gen, None)),
        0x8a => Some((gen_dupn(code, 10), label_gen, None)),
        0x8b => Some((gen_dupn(code, 11), label_gen, None)),
        0x8c => Some((gen_dupn(code, 12), label_gen, None)),
        0x8d => Some((gen_dupn(code, 13), label_gen, None)),
        0x8e => Some((gen_dupn(code, 14), label_gen, None)),
        0x8f => Some((gen_dupn(code, 15), label_gen, None)),
        0x90 => { // SWAP1  
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            Some((code, label_gen, None))
        }
        0x91 => { // SWAP2  
            code.push(Instruction::from_opcode(Opcode::Swap2, None));
            Some((code, label_gen, None))
        }
        0x92 => Some((gen_swapn(code, 3), label_gen, None)),
        0x93 => Some((gen_swapn(code, 4), label_gen, None)),
        0x94 => Some((gen_swapn(code, 5), label_gen, None)),
        0x95 => Some((gen_swapn(code, 6), label_gen, None)),
        0x96 => Some((gen_swapn(code, 7), label_gen, None)),
        0x97 => Some((gen_swapn(code, 8), label_gen, None)),
        0x98 => Some((gen_swapn(code, 9), label_gen, None)),
        0x99 => Some((gen_swapn(code, 10), label_gen, None)),
        0x9a => Some((gen_swapn(code, 11), label_gen, None)),
        0x9b => Some((gen_swapn(code, 12), label_gen, None)),
        0x9c => Some((gen_swapn(code, 13), label_gen, None)),
        0x9d => Some((gen_swapn(code, 14), label_gen, None)),
        0x9e => Some((gen_swapn(code, 15), label_gen, None)),
        0x9f => Some((gen_swapn(code, 16), label_gen, None)),
        0xa0 => evm_emulate(code, label_gen, evm_func_map, "evmOp_log0"), // LOG0
        0xa1 => evm_emulate(code, label_gen, evm_func_map, "evmOp_log1"), // LOG1
        0xa2 => evm_emulate(code, label_gen, evm_func_map, "evmOp_log2"), // LOG2
        0xa3 => evm_emulate(code, label_gen, evm_func_map, "evmOp_log3"), // LOG3 
        0xa4 => evm_emulate(code, label_gen, evm_func_map, "evmOp_log4"), // LOG4 
        // 0xa4-0xaf unused
        // 0xb0-0xba unused, reserved for EIP 615
        // 0xbb-0xe0 unused
        0xe1 => evm_emulate(code, label_gen, evm_func_map, "evmOp_sloadbytes"), // SLOADBYTES 
        0xe2 => evm_emulate(code, label_gen, evm_func_map, "evmOp_sstorebytes"), // SSTOREBYTES 
        0xe3 => evm_emulate(code, label_gen, evm_func_map, "evmOp_ssize"), // SSIZE
        // 0xe4-0xef unused
        0xf0 => None, // CREATE
        0xf1 => evm_emulate(code, label_gen, evm_func_map, "evmOp_call"), // CALL 
        0xf2 => evm_emulate(code, label_gen, evm_func_map, "evmOp_callcode"), // CALLCODE 
        0xf3 => evm_emulate(code, label_gen, evm_func_map, "evmOp_return"), // RETURN 
        0xf4 => evm_emulate(code, label_gen, evm_func_map, "evmOp_delegatecall"), // DELEGATECALL
        0xf5 => None, // CREATE2
        // 0xf6-0xf9 unused
        0xfa => evm_emulate(code, label_gen, evm_func_map, "evmOp_staticcall"), // STATICCALL
        0xfb => evm_emulate(code, label_gen, evm_func_map, "evmOp_revert"), // REVERT 
        0xfc => evm_emulate(code, label_gen, evm_func_map, "evmOp_txexecgas"), // TXEXECGAS
        0xfd => evm_emulate(code, label_gen, evm_func_map, "evmOp_revert"), // REVERT
        0xfe => { // INVALID  
            code.push(Instruction::from_opcode(Opcode::Panic, None));
            Some((code, label_gen, None))
        }
        0xff => evm_emulate(code, label_gen, evm_func_map, "evmOp_selfdestruct"), // SELFDESTRUCT
        _ => { panic!("invalid EVM instruction {}", evm_insn); }
    }
}

fn gen_dupn(mut code: Vec<Instruction>, n: usize) -> Vec<Instruction> {
    for _i in 2..n {
        code.push(Instruction::from_opcode(Opcode::AuxPush, None));
    }
    code.push(Instruction::from_opcode(Opcode::Dup2, None));
    for _i in 2..n {
        code.push(Instruction::from_opcode(Opcode::AuxPop, None));
        code.push(Instruction::from_opcode(Opcode::Swap1, None));
    }
    code
}

fn gen_swapn(mut code: Vec<Instruction>, n: usize) -> Vec<Instruction> {
    for _i in 2..n {
        code.push(Instruction::from_opcode(Opcode::Swap1, None));
        code.push(Instruction::from_opcode(Opcode::AuxPush, None));
    }
    code.push(Instruction::from_opcode(Opcode::Swap2, None));
    code.push(Instruction::from_opcode(Opcode::Swap1, None));
    for _i in 2..n {
        code.push(Instruction::from_opcode(Opcode::AuxPop, None));
    }
    code
}

fn evm_emulate(
    mut code: Vec<Instruction>,
    label_gen: LabelGenerator,
    evm_func_map: &HashMap<&str, Label>,
    name: &str,
) -> Option<(Vec<Instruction>, LabelGenerator, Option<usize>)> {
    match evm_func_map.get(name) {
        Some(func_label) => {
            let (ret_label, lg) = label_gen.next();
            code.push(Instruction::from_opcode_imm(
                Opcode::Noop,
                Value::Label(ret_label),
                None,
            ));
            code.push(Instruction::from_opcode_imm(
                Opcode::Jump,
                Value::Label(*func_label),
                None,
            ));
            code.push(Instruction::from_opcode(Opcode::Label(ret_label), None));
            Some((code, lg, None))
        }
        None => {
            panic!("nonexistent evm emulation func: {}", name);
        }
    }
}

const EMULATION_FUNCS: [&str; 41] = [
    // If you modify this, be sure to regenerate the EVM jumptable
    "evmOp_stop",
    "evmOp_sha3",
    "evmOp_address",
    "evmOp_balance",
    "evmOp_origin",
    "evmOp_caller",
    "evmOp_callvalue",
    "evmOp_calldataload",
    "evmOp_calldatasize",
    "evmOp_calldatacopy",
    "evmOp_codesize",
    "evmOp_codecopy",
    "evmOp_extcodesize",
    "evmOp_extcodecopy",
    "evmOp_returndatasize",
    "evmOp_returndatacopy",
    "evmOp_timestamp",
    "evmOp_number",
    "evmOp_mload",
    "evmOp_mstore",
    "evmOp_mstore8",
    "evmOp_sload",
    "evmOp_sstore",
    "evmOp_getjumpaddr",
    "evmOp_msize",
    "evmOp_log0",
    "evmOp_log1",
    "evmOp_log2",
    "evmOp_log3",
    "evmOp_log4",
    "evmOp_sloadbytes",
    "evmOp_sstorebytes",
    "evmOp_ssize",
    "evmOp_call",
    "evmOp_callcode",
    "evmOp_return",
    "evmOp_delegatecall",
    "evmOp_staticcall",
    "evmOp_revert",
    "evmOp_txexecgas",
    "evmOp_selfdestruct",
];

pub fn runtime_func_name(slot: usize) -> &'static str {
    EMULATION_FUNCS[slot]
}

pub fn num_runtime_funcs() -> usize {
    EMULATION_FUNCS.len()
}

fn compile_push_insn(data: &[u8], mut code: Vec<Instruction>) -> Vec<Instruction> {
    let mut val = Uint256::zero();
    for d in data {
        val = val
            .mul(&Uint256::from_usize(256))
            .add(&Uint256::from_usize(usize::from(*d)));
    }
    code.push(Instruction::from_opcode_imm(
        Opcode::Noop,
        Value::Int(val),
        None,
    ));
    code
}

#[allow(dead_code)]
fn imported_funcs_for_evm() -> (Vec<ImportedFunc>, StringTable) {
    let mut imp_funcs = Vec::new();
    let mut string_table = StringTable::new();
    for name in EMULATION_FUNCS.iter() {
        string_table.get(name.to_string());
    }
    for (i, name) in EMULATION_FUNCS.iter().enumerate() {
        imp_funcs.push(ImportedFunc::new(
            i,
            string_table.get(name.to_string()),
            &string_table,
            vec![],
            Type::Void,
            true,
        ));
    }
    (imp_funcs, string_table)
}

pub fn make_evm_jumptable_mini(filepath: &Path) -> Result<(), io::Error> {
    let path = Path::new(filepath);
    let display = path.display();

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why.to_string()),
        Ok(file) => file,
    };
    writeln!(file, "// Automatically generated file -- do not edit")?;
    for name in EMULATION_FUNCS.iter() {
        writeln!(file, "import func {}();", name)?;
    }
    writeln!(file, "")?;
    writeln!(
        file,
        "var evm_jumptable: [{}]func();",
        EMULATION_FUNCS.len()
    )?;
    writeln!(file, "")?;
    writeln!(file, "public func init_evm_jumptable() {{")?;
    writeln!(file, "    evm_jumptable = evm_jumptable")?;
    for (i, name) in EMULATION_FUNCS.iter().enumerate() {
        writeln!(
            file,
            "        with {{ [{}] = unsafecast<func()>({}) }}",
            i, name
        )?;
    }
    writeln!(file, "        ;")?;
    writeln!(file, "}}")?;
    write!(
        file,
        "\npublic func evm_jumptable_get(idx: uint) -> option<func()>\n{{\n"
    )?;
    write!(
        file,
        "    if (idx >= {}) {{\n        return None<func()>;\n    }} else {{\n",
        EMULATION_FUNCS.len()
    )?;
    write!(file, "        return Some(evm_jumptable[idx]);\n")?;
    write!(file, "    }}\n}}\n")?;
    Ok(())
}

#[derive(Clone)]
pub struct CallInfo<'a> {
    function_name: &'a str,
    args: &'a [ethabi::Token],
    payment: Uint256,
}

pub fn evm_load_and_call_func(
    contract_json_file_name: &str,
    other_contract_names: &[&str],
    contract_name: &str,
    function_name: &str,
    args: &[ethabi::Token],
    payment: Uint256,
    debug: bool,
) -> Result<Vec<ethabi::Token>, ethabi::Error> {
    Ok(evm_load_and_call_funcs(
        contract_json_file_name,
        other_contract_names,
        contract_name,
        vec![CallInfo {
            function_name,
            args,
            payment,
        }]
        .as_ref(),
        debug,
    )?[0]
        .clone())
}

pub fn evm_load_and_call_funcs(
    contract_json_file_name: &str,
    other_contract_names: &[&str],
    contract_name: &str,
    call_infos: &[CallInfo],
    debug: bool,
) -> Result<Vec<Vec<ethabi::Token>>, ethabi::Error> {
    let dapp_abi = match abi::AbiForDapp::new_from_file(contract_json_file_name) {
        Ok(dabi) => dabi,
        Err(_) => {
            panic!("failed to load dapp ABI from file");
        }
    };
    let mut all_contracts = Vec::new();
    for other_contract_name in other_contract_names {
        match dapp_abi.get_contract(other_contract_name) {
            Some(contract) => {
                all_contracts.push(contract);
            }
            None => {
                panic!("couldn't find contract {}", other_contract_name);
            }
        }
    }
    let this_contract = match dapp_abi.get_contract(contract_name) {
        Some(contract) => {
            all_contracts.push(contract);
            contract
        }
        None => {
            panic!("couldn't find contract {}", contract_name);
        }
    };

    let mut rt_env = RuntimeEnvironment::new();
    for contract in all_contracts {
        contract.insert_upload_message(&mut rt_env);
    }

    let mut call_funcs = Vec::new();
    for call_info in call_infos {
        let this_func = match this_contract.get_function(call_info.function_name) {
            Ok(func) => func,
            Err(e) => {
                panic!(
                    "couldn't find {} function in {} contract: {:?}",
                    call_info.function_name,
                    contract_name,
                    e.to_string()
                );
            }
        };
        call_funcs.push(this_func);

        let calldata = this_func.encode_input(call_info.args).unwrap();
        rt_env.insert_txcall_message(
            this_contract.address.clone(),
            call_info.payment.clone(),
            &calldata,
        );
    }

    let mut machine = load_from_file(Path::new("arbruntime/runtime.mexe"), rt_env);

    let logs = match crate::run::run(&mut machine, vec![], debug) {
        Ok(logs) => logs,
        Err(e) => {
            panic!("run failed: {:?}", e);
        }
    };

    assert_eq!(logs.len(), call_infos.len());
    let mut ret = Vec::new();
    for (i, _) in call_infos.iter().enumerate() {
        if let Value::Tuple(tup) = &logs[i] {
            println!("log number {} received: {:#?}", i, tup);
            if let Some(result_bytes) = bytes_from_bytestack(tup[2].clone()) {
                if result_bytes.len() == 0 {
                    ret.push(vec![]);
                } else {
                    ret.push(call_funcs[i].decode_output(&result_bytes)?);
                }
            } else {
                panic!("log element was not a bytestack");
            }
        } else {
            panic!("log item was not a Tuple");
        }
    }
    Ok(ret)
}

pub fn evm_load_add_and_verify(debug: bool) {
    use std::convert::TryFrom;
    match evm_load_and_call_func(
        "contracts/add/compiled.json",
        vec![].as_ref(),
        "Add",
        "add",
        vec![
            ethabi::Token::Uint(ethabi::Uint::one()),
            ethabi::Token::Uint(ethabi::Uint::one()),
        ]
        .as_ref(),
        Uint256::zero(),
        debug,
    ) {
        Ok(tokens) => match tokens[0] {
            Token::Uint(ui) => {
                assert_eq!(ui, ethabi::Uint::try_from(2).unwrap());
            }
            _ => {
                panic!("token was not a uint: {:?}", tokens[0]);
            }
        },
        Err(e) => {
            panic!("error loading and calling Add::add: {:?}", e);
        }
    }
}

pub fn evm_load_fib_and_verify(debug: bool) {
    use std::convert::TryFrom;
    match evm_load_and_call_func(
        "contracts/fibonacci/compiled.json",
        vec![].as_ref(),
        "Fibonacci",
        "doFib",
        vec![ethabi::Token::Uint(ethabi::Uint::try_from(5).unwrap())].as_ref(),
        Uint256::zero(),
        debug,
    ) {
        Ok(tokens) => match tokens[0] {
            Token::Uint(ui) => {
                assert_eq!(ui, ethabi::Uint::try_from(8).unwrap());
            }
            _ => {
                panic!("token was not a uint: {:?}", tokens[0]);
            }
        },
        Err(e) => {
            panic!("error loading and calling Fibonacci::doFib: {:?}", e);
        }
    }
}

pub fn evm_xcontract_call_and_verify(debug: bool) {
    use std::convert::TryFrom;
    match evm_load_and_call_funcs(
        "contracts/fibonacci/compiled.json",
        vec!["Fibonacci"].as_ref(),
        "PaymentChannel",
        vec![
           CallInfo {
                function_name: "deposit",
                args: vec![].as_ref(),
                payment: Uint256::from_usize(10000),
            },
            CallInfo {
                function_name: "transferFib",
                args: vec![
                    ethabi::Token::Address(ethabi::Address::from_low_u64_be(5000)),
                    ethabi::Token::Uint(ethabi::Uint::try_from(1).unwrap()),
                ]
                .as_ref(),
                payment: Uint256::zero(),
            },
        ]
        .as_ref(),
        debug,
    ) {
        Ok(tokens) => {
            assert_eq!(tokens.len(), 2);
        }
        Err(e) => {
            panic!("error loading and calling PaymentChannel::deposit and ::transferFib: {:?}", e);
        }
    }
}
