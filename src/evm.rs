use std::usize;
use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::collections::HashMap;
use crate::compile::{CompiledProgram, CompileError, SourceFileMap};
use crate::uint256::Uint256;
use crate::mavm::{Instruction, Opcode, Value, Label, LabelGenerator};
use crate::link::{link, ImportedFunc};
use crate::stringtable::StringTable;
use crate::build_builtins::BuiltinArray;


pub fn compile_evm_file<'a>(path: &Path) -> Result<CompiledProgram, CompileError<'a>> {
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
     
    let parse_result: Result<serde_json::Value, serde_json::Error> = serde_json::from_str(&s);
    match parse_result {
        Ok(evm_json) => compile_from_json(evm_json),
        Err(e) => {
            println!("Error reading in EVM file: {:?}", e);
            Err(CompileError::new("error parsing compiled EVM file", None))
        }
    }
}

pub struct CompiledEvmContract {
    address: Uint256,
    storage: HashMap<Uint256, Uint256>,
    insns: Vec<Instruction>,
}

impl CompiledEvmContract {
    fn get_info_datastruct(&self) -> Value {
        let num_storages = self.storage.len();
        let mut storages_array = BuiltinArray::new(num_storages, Value::none());
        for (i, (mem_addr, mem_val)) in self.storage.iter().enumerate() {
            storages_array.set(i, Value::Tuple(vec![Value::Int(mem_addr.clone()), Value::Int(mem_val.clone())]));
        }
        Value::Tuple(vec![
            Value::Int(self.address.clone()),
            storages_array.to_value(),
        ])
    }
}

pub fn compile_from_json<'a>(evm_json: serde_json::Value) -> Result<CompiledProgram, CompileError<'a>> {
    if let serde_json::Value::Array(contracts) = evm_json {
        let mut compiled_contracts = Vec::new();
        let mut label_gen = LabelGenerator::new();
        for contract in contracts {
            if let serde_json::Value::Object(items) = contract {
                let (compiled_contract, lg) = compile_from_evm_contract(items, label_gen)?;
                compiled_contracts.push(compiled_contract);
                label_gen = lg;
            } else {
                return Err(CompileError::new("unexpected contents in EVM json file", None));
            }
        }
        evm_link(compiled_contracts)
    } else {
        Err(CompileError::new("unexpected contents in EVM json file", None))
    }
}

pub fn compile_from_evm_contract<'a>(
    contract: serde_json::map::Map<String, serde_json::Value>,
    mut label_gen: LabelGenerator,
) -> Result<(CompiledEvmContract, LabelGenerator), CompileError<'a>> {
    let code_str = if let serde_json::Value::String(s) = &contract["code"] {
        s
    } else {
        return Err(CompileError::new("bad code string in EVM json file", None));
    };
    let mut code = Vec::new();
    let mut evm_func_map = HashMap::new();
    for (idx, name) in EMULATION_FUNCS.iter().enumerate() {
        evm_func_map.insert(*name, Label::External(idx));
    }
    let decoded_insns = hex::decode(&code_str[2..]).unwrap();
    let mut i = 0;
    while i < decoded_insns.len() {
        let insn = decoded_insns[i];
        if (insn >= 0x60) && (insn <= 0x7f) {
            let nbytes = usize::from(insn)-0x5f;
            code = compile_push_insn(&decoded_insns[(i+1)..(usize::from(insn)+i+2-0x60)], code);
            i += (nbytes+1);
        } else {
            match compile_evm_insn(insn, code, label_gen, &evm_func_map) {
                Some((c, lg)) => {
                    code = c;
                    label_gen = lg;
                    i += 1;
                }
                None => { 
                    return Err(CompileError::new("unsupported instruction in EVM code", None));
                }
            }
        }
    }
    
    Ok((
        CompiledEvmContract {
            address: 
                if let serde_json::Value::String(s) = &contract["address"] {
                    Uint256::from_string_hex(&s[2..]).unwrap()
                } else {
                    return Err(CompileError::new("invalid address format in EVM json file", None));
                },
            storage: 
                if let serde_json::Value::Object(m) = &contract["storage"] {
                    let mut hm = HashMap::new();
                    for (k,v) in m {
                        hm.insert(
                            Uint256::from_string_hex(&k[2..]).unwrap(), 
                            if let serde_json::Value::String(s) = v {
                                Uint256::from_string_hex(&s[2..]).unwrap()
                            } else {
                                return Err(CompileError::new("invalid storage value format in EVM json file", None));
                            }
                        );
                    }
                    hm
                } else {
                    return Err(CompileError::new("invalid storage format in EVM json file", None));
                },
            insns: code,
        }, 
        label_gen
    ))
}

pub fn compile_evm_insn(
    evm_insn: u8, mut code: Vec<Instruction>, 
    label_gen: LabelGenerator,
    evm_func_map: &HashMap<&str, Label>,
) -> Option<(Vec<Instruction>, LabelGenerator)> {
    println!("insn {:2x}", evm_insn);
    match evm_insn {
        0x00 => evm_emulate(code, label_gen, evm_func_map,  "stop"), // STOP
        0x01 => { // ADD
            code.push(Instruction::from_opcode(Opcode::Plus, None));
            Some((code, label_gen))
        }
        0x02 => { // MUL
            code.push(Instruction::from_opcode(Opcode::Mul, None));
            Some((code, label_gen))
        }  
        0x03 => { // SUB
            code.push(Instruction::from_opcode(Opcode::Minus, None));
            Some((code, label_gen))
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
            Some((code, lg))
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
            Some((code, lg))
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
            Some((code, lg))
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
            Some((code, lg))
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
            Some((code, lg))
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
            Some((code, lg)) 
        }  
        0x0a => { // EXP
            code.push(Instruction::from_opcode(Opcode::Exp, None));
            Some((code, label_gen))
        }      
        0x0b => { // SIGNEXTEND
            code.push(Instruction::from_opcode(Opcode::SignExtend, None));
            Some((code, label_gen))
        }      
        // 0x0c - 0x0f unused
        0x10 => { // LT
            code.push(Instruction::from_opcode(Opcode::LessThan, None));
            Some((code, label_gen))
        }  
        0x11 => { // GT
            code.push(Instruction::from_opcode(Opcode::GreaterThan, None));
            Some((code, label_gen))
        }    
        0x12 => { // SLT
            code.push(Instruction::from_opcode(Opcode::SLessThan, None));
            Some((code, label_gen))
        }               
        0x13 => { // SGT
            code.push(Instruction::from_opcode(Opcode::SGreaterThan, None));
            Some((code, label_gen))
        }    
        0x14 => { // EQ
            code.push(Instruction::from_opcode(Opcode::Equal, None));
            Some((code, label_gen))
        } 
        0x15 => { // ISZERO
            code.push(Instruction::from_opcode(Opcode::Not, None));
            Some((code, label_gen))
        }   
        0x16 => { // AND
            code.push(Instruction::from_opcode(Opcode::BitwiseAnd, None));
            Some((code, label_gen))
        } 
        0x17 => { // OR
            code.push(Instruction::from_opcode(Opcode::BitwiseOr, None));
            Some((code, label_gen))
        }   
        0x18 => { // XOR
            code.push(Instruction::from_opcode(Opcode::BitwiseXor, None));
            Some((code, label_gen))
        }   
        0x19 => { // NOT
            code.push(Instruction::from_opcode(Opcode::BitwiseNeg, None));
            Some((code, label_gen))
        }             
        0x1a => { // BYTE
            code.push(Instruction::from_opcode(Opcode::Byte, None));
            Some((code, label_gen))
        }   
        0x1b => { // SHL
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Exp, Value::Int(Uint256::from_usize(2)), None));
            code.push(Instruction::from_opcode(Opcode::Mul, None));
            Some((code, label_gen))
        } 
        0x1c => { // SHR
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Exp, Value::Int(Uint256::from_usize(2)), None));
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::Div, None));
            Some((code, label_gen))
        }
        0x1d => { // SAR
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode_imm(Opcode::Exp, Value::Int(Uint256::from_usize(2)), None));
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::Sdiv, None));
            Some((code, label_gen))
        }
        // 0x1e - 0x1f unused
        0x20 => evm_emulate(code, label_gen, evm_func_map,  "sha3"), // SHA3
        // 0x21-0x2f unused
        0x30 => evm_emulate(code, label_gen, evm_func_map,  "address"), // ADDRESS
        0x31 => evm_emulate(code, label_gen, evm_func_map,  "balance"),// BALANCE
        0x32 => evm_emulate(code, label_gen, evm_func_map,  "origin"), // ORIGIN
        0x33 => evm_emulate(code, label_gen, evm_func_map,  "caller"), // CALLER
        0x34 => evm_emulate(code, label_gen, evm_func_map,  "callvalue"), // CALLVALUE
        0x35 => evm_emulate(code, label_gen, evm_func_map,  "calldataload"), // CALLDATALOAD
        0x36 => evm_emulate(code, label_gen, evm_func_map,  "calldatasize"), // CALLDATASIZE
        0x37 => evm_emulate(code, label_gen, evm_func_map,  "calldatacopy"), // CALLDATACOPY
        0x38 => evm_emulate(code, label_gen, evm_func_map,  "codesize"), // CODESIZE
        0x39 => evm_emulate(code, label_gen, evm_func_map,  "codecopy"), // CODECOPY
        0x3a => { // GASPRICE
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::one()), None));
            Some((code, label_gen))
        }   
        0x3b => evm_emulate(code, label_gen, evm_func_map,  "extcodesize"), // EXTCODESIZE 
        0x3c => evm_emulate(code, label_gen, evm_func_map,  "extcodecopy"), // EXTCODECOPY  
        0x3d => evm_emulate(code, label_gen, evm_func_map,  "returndatasize"), // RETURNDATASIZE 
        0x3e => evm_emulate(code, label_gen, evm_func_map,  "returndatacopy"), // RETURNDATACOPY
        // 0x3f unused
        0x40 => None, // BLOCKHASH
        0x41 => None, // COINBASE
        0x42 => evm_emulate(code, label_gen, evm_func_map,  "timestamp"), // TIMESTAMP 
        0x43 => evm_emulate(code, label_gen, evm_func_map,  "number"), // NUMBER
        0x44 => None, // DIFFICULTY
        0x45 => { // GASLIMIT
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::from_usize(10_000_000_000)), None));
            Some((code, label_gen))
        }   
        // 0x46-0x4f unused
        0x50 => { // POP
            code.push(Instruction::from_opcode(Opcode::Pop, None));
            Some((code, label_gen))
        }   
        0x51 => evm_emulate(code, label_gen, evm_func_map,  "mload"), // MLOAD
        0x52 => evm_emulate(code, label_gen, evm_func_map,  "mstore"), // MSTORE
        0x53 => evm_emulate(code, label_gen, evm_func_map,  "mstore8"), // MSTORE8
        0x54 => evm_emulate(code, label_gen, evm_func_map,  "sload"), // SLOAD
        0x55 => evm_emulate(code, label_gen, evm_func_map,  "sstore"), // SSTORE
        0x56 => { // JUMP
            let (c, lg) = evm_emulate(code, label_gen, evm_func_map, "getjumpaddr")?;
            code = c;
            code.push(Instruction::from_opcode(Opcode::Jump, None));
            Some((code, lg))
        }
        0x57 => {  // JUMPI
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            let (c, lg) = evm_emulate(code, label_gen, evm_func_map, "getjumpaddr")?;
            code = c;
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            code.push(Instruction::from_opcode(Opcode::Cjump, None));
            Some((code, lg))
        }
        0x58 => None, // GETPC
        0x59 => evm_emulate(code, label_gen, evm_func_map,  "msize"), // MSIZE
        0x5a => { // GAS
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(Uint256::from_usize(9_999_999_999)), None));
            Some((code, label_gen))
        }  
        0x5b => { // JUMPDEST
            Some((code, label_gen))
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
            Some((code, label_gen))
        }   
        0x81 => { // DUP2 
            code.push(Instruction::from_opcode(Opcode::Dup1, None));
            Some((code, label_gen))
        }   
        0x82 => { // DUP3  
            code.push(Instruction::from_opcode(Opcode::Dup2, None));
            Some((code, label_gen))
        }  
        0x83 => Some((gen_dupn(code, 3), label_gen)),
        0x84 => Some((gen_dupn(code, 4), label_gen)),
        0x85 => Some((gen_dupn(code, 5), label_gen)),
        0x86 => Some((gen_dupn(code, 6), label_gen)),
        0x87 => Some((gen_dupn(code, 7), label_gen)),
        0x88 => Some((gen_dupn(code, 8), label_gen)),
        0x89 => Some((gen_dupn(code, 9), label_gen)),
        0x8a => Some((gen_dupn(code, 10), label_gen)),
        0x8b => Some((gen_dupn(code, 11), label_gen)),
        0x8c => Some((gen_dupn(code, 12), label_gen)),
        0x8d => Some((gen_dupn(code, 13), label_gen)),
        0x8e => Some((gen_dupn(code, 14), label_gen)),
        0x8f => Some((gen_dupn(code, 15), label_gen)),
        0x90 => { // SWAP1  
            code.push(Instruction::from_opcode(Opcode::Swap1, None));
            Some((code, label_gen))
        }   
        0x91 => { // SWAP2  
            code.push(Instruction::from_opcode(Opcode::Swap2, None));
            Some((code, label_gen))
        }  
        0x92 => Some((gen_swapn(code, 3), label_gen)), 
        0x93 => Some((gen_swapn(code, 4), label_gen)), 
        0x94 => Some((gen_swapn(code, 5), label_gen)), 
        0x95 => Some((gen_swapn(code, 6), label_gen)), 
        0x96 => Some((gen_swapn(code, 7), label_gen)), 
        0x97 => Some((gen_swapn(code, 8), label_gen)), 
        0x98 => Some((gen_swapn(code, 9), label_gen)), 
        0x99 => Some((gen_swapn(code, 10), label_gen)), 
        0x9a => Some((gen_swapn(code, 11), label_gen)), 
        0x9b => Some((gen_swapn(code, 12), label_gen)), 
        0x9c => Some((gen_swapn(code, 13), label_gen)), 
        0x9d => Some((gen_swapn(code, 14), label_gen)), 
        0x9e => Some((gen_swapn(code, 15), label_gen)), 
        0x9f => Some((gen_swapn(code, 16), label_gen)), 
        0xa0 => evm_emulate(code, label_gen, evm_func_map,  "log0"), // LOG0
        0xa1 => evm_emulate(code, label_gen, evm_func_map,  "log1"), // LOG1
        0xa2 => evm_emulate(code, label_gen, evm_func_map,  "log2"), // LOG2
        0xa3 => evm_emulate(code, label_gen, evm_func_map,  "log3"), // LOG3 
        0xa4 => evm_emulate(code, label_gen, evm_func_map,  "log4"), // LOG4 
        // 0xa5-0xaf unused
        // 0xb0-0xba unused, reserved for EIP 615
        // 0xbb-0xe0 unused
        0xe1 => evm_emulate(code, label_gen, evm_func_map,  "sloadbytes"), // SLOADBYTES 
        0xe2 => evm_emulate(code, label_gen, evm_func_map,  "sstorebytes"), // SSTOREBYTES 
        0xe3 => evm_emulate(code, label_gen, evm_func_map,  "ssize"), // SSIZE
        // 0xe4-0xef unused
        0xf0 => None, // CREATE
        0xf1 => evm_emulate(code, label_gen, evm_func_map,  "call"), // CALL 
        0xf2 => evm_emulate(code, label_gen, evm_func_map,  "callcode"), // CALLCODE 
        0xf3 => evm_emulate(code, label_gen, evm_func_map,  "return"), // RETURN 
        0xf4 => evm_emulate(code, label_gen, evm_func_map,  "delegatecall"), // DELEGATECALL
        0xf5 => None, // CREATE2
        // 0xf6-0xf9 unused
        0xfa => evm_emulate(code, label_gen, evm_func_map,  "staticcall"), // STATICCALL
        0xfb => evm_emulate(code, label_gen, evm_func_map,  "revert"), // REVERT 
        0xfc => evm_emulate(code, label_gen, evm_func_map,  "txexecgas"), // TXEXECGAS
        0xfd => evm_emulate(code, label_gen, evm_func_map,  "revert"), // REVERT
        0xfe => { // INVALID  
            code.push(Instruction::from_opcode(Opcode::Panic, None));
            Some((code, label_gen))
        }   
        0xff => evm_emulate(code, label_gen, evm_func_map,  "selfdestruct"), // SELFDESTRUCT
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
    name: &str
) -> Option<(Vec<Instruction>, LabelGenerator)> {
    match evm_func_map.get(name) {
        Some(func_label) => {
            let (ret_label, lg) = label_gen.next();
            code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Label(ret_label), None));
            code.push(Instruction::from_opcode_imm(Opcode::Jump, Value::Label(*func_label), None));
            code.push(Instruction::from_opcode(Opcode::Label(ret_label), None));
            Some((code, lg))
        }
        None => { panic!("nonexistent evm emulation func: {}", name); }
    }
}

const EMULATION_FUNCS: [&str; 41] = [
    "stop",
    "sha3",
    "address",
    "balance",
    "origin",
    "caller",
    "callvalue",
    "calldataload",
    "calldatasize",
    "calldatacopy",
    "codesize",
    "codecopy",
    "extcodesize",
    "extcodecopy",
    "returndatasize",
    "returndatacopy",
    "timestamp",
    "number",
    "mload",
    "mstore",
    "mstore8",
    "sload",
    "sstore",
    "getjumpaddr",
    "msize",
    "log0",
    "log1",
    "log2",
    "log3",
    "log4",
    "sloadbytes",
    "sstorebytes",
    "ssize",
    "call",
    "callcode",
    "return",
    "delegatecall",
    "staticcall",
    "revert",
    "txexecgas",
    "selfdestruct",
];

fn compile_push_insn(data: &[u8], mut code: Vec<Instruction>) -> Vec<Instruction> {
    println!("insn {:2x}", 0x5f+data.len());
    let mut val = Uint256::zero();
    for d in data {
        val = val.mul(&Uint256::from_usize(256)).add(&Uint256::from_usize(usize::from(*d)));
    }
    code.push(Instruction::from_opcode_imm(Opcode::Noop, Value::Int(val), None));
    code
}

fn evm_link(contracts: Vec<CompiledEvmContract>) -> Result<CompiledProgram, CompileError<'static>> {
    let (imports, _string_table) = imported_funcs_for_evm();
    let num_contracts = contracts.len();
    let mut summary_array = BuiltinArray::new(num_contracts, Value::none());
    let mut comp_progs = Vec::new();
    for (i, contract) in contracts.iter().enumerate() {
        summary_array.set(i, contract.get_info_datastruct());
        comp_progs.push(CompiledProgram {
            code: contract.insns.clone(),
            exported_funcs: vec![],
            imported_funcs: imports.clone(),
            global_num_limit: 0,
            source_file_map: SourceFileMap::new_empty(),
        });
    }
    link(&comp_progs)
}

fn imported_funcs_for_evm() -> (Vec<ImportedFunc>, StringTable<'static>) {
    let mut imp_funcs = Vec::new();
    let mut string_table = StringTable::new();
    for name in EMULATION_FUNCS.iter() {
        string_table.get(name);
    }
    for (i, name) in EMULATION_FUNCS.iter().enumerate() {
        imp_funcs.push(ImportedFunc::new(i, string_table.get(name), &string_table));
    }
    (imp_funcs, string_table)
}
