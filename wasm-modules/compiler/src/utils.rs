use crate::mavm::{
    AVMOpcode, CodePt, Instruction, Label, Value,
};
use crate::uint256::Uint256;

use ethers_core::utils::keccak256;
use parity_wasm::elements::Instruction::*;
use parity_wasm::elements::*;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone)]
struct Control {
    target: usize,
    rets: usize,
    level: usize,
    else_label: usize,
    is_ite: bool,
    is_loop: bool,
}

#[wasm_bindgen]
extern "C" {
    fn read_buffer(idx: i32) -> i32;
    fn setlen(idx: i32);
    fn getlen() -> i32;
    fn write_buffer(idx: i32, c: i32);
    fn usegas(gas: i32);
    fn wextra(idx: i32, c: i32);
}

const LEVEL: usize = 5;

fn block_len(bt: &BlockType) -> usize {
    match *bt {
        BlockType::Value(_) => 1,
        BlockType::NoResult => 0,
    }
}

fn get_func_type(m: &Module, sig: u32) -> &FunctionType {
    match m.type_section().unwrap().types()[sig as usize] {
        Type::Function(ref t) => t,
    }
}

fn num_func_returns(ft: &FunctionType) -> usize {
    ft.results().len()
}

pub fn simple_op(op: AVMOpcode) -> Instruction {
    Instruction::from_opcode(op)
}

/*
fn debug_op(str: String) -> Instruction {
    Instruction::from_opcode(AVMOpcode::Noop)
}
*/

fn immed_op(op: AVMOpcode, v: Value) -> Instruction {
    Instruction::from_opcode_imm(op, v)
}

fn mk_label(idx: usize) -> Instruction {
    Instruction::from_opcode_imm(AVMOpcode::Label, Value::Label(Label::Evm(idx)))
}

fn mk_func_label(idx: usize) -> Instruction {
    Instruction::from_opcode_imm(AVMOpcode::Label, Value::Label(Label::WasmFunc(idx)))
}

fn get_from_table(res: &mut Vec<Instruction>, idx: Value) {
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(1)));
    res.push(push_value(idx));
    for _i in 0..LEVEL {
        // Stack: idx, table
        res.push(simple_op(AVMOpcode::Dup0)); // idx, idx, table
        res.push(immed_op(AVMOpcode::BitwiseAnd, int_from_usize(7))); // idx (mod), idx, table
        res.push(simple_op(AVMOpcode::Swap1)); // idx, idx (mod), table
        res.push(immed_op(AVMOpcode::ShiftRight, int_from_usize(3))); // idx, idx (mod), table
        res.push(simple_op(AVMOpcode::Swap2)); // table, idx (mod), idx
        res.push(simple_op(AVMOpcode::Swap1)); // idx (mod), table, idx
        res.push(simple_op(AVMOpcode::Tget)); // table, idx
        res.push(simple_op(AVMOpcode::Swap1)); // idx, table
    }
    res.push(simple_op(AVMOpcode::Pop));
}

fn get_return_from_table(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::AuxPush));
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(1)));
    res.push(simple_op(AVMOpcode::AuxPop));
    for _i in 0..LEVEL {
        // Stack: idx, table
        res.push(simple_op(AVMOpcode::Dup0)); // idx, idx, table
        res.push(immed_op(AVMOpcode::BitwiseAnd, int_from_usize(7))); // idx (mod), idx, table
        res.push(simple_op(AVMOpcode::Swap1)); // idx, idx (mod), table
        res.push(immed_op(AVMOpcode::ShiftRight, int_from_usize(3))); // idx, idx (mod), table
        res.push(simple_op(AVMOpcode::Swap2)); // table, idx (mod), idx
        res.push(simple_op(AVMOpcode::Swap1)); // idx (mod), table, idx
        res.push(simple_op(AVMOpcode::Tget)); // table, idx
        res.push(simple_op(AVMOpcode::Swap1)); // idx, table
    }
    res.push(simple_op(AVMOpcode::Pop));
}

fn set_jump_table(res: &mut Vec<Instruction>) {
    // idx, codept
    res.push(simple_op(AVMOpcode::AuxPush)); // codept ;; idx
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(7)));
    res.push(simple_op(AVMOpcode::AuxPop));
    for _i in 0..LEVEL {
        // Stack: idx, table, codept
        res.push(simple_op(AVMOpcode::Dup0)); // idx, idx, table, codept
        res.push(immed_op(AVMOpcode::BitwiseAnd, int_from_usize(7))); // idx (mod), idx, table
        res.push(simple_op(AVMOpcode::Swap1)); // idx, idx (mod), table
        res.push(immed_op(AVMOpcode::ShiftRight, int_from_usize(3))); // idx, idx (mod), table
        res.push(simple_op(AVMOpcode::Swap2)); // table, idx (mod), idx
        res.push(simple_op(AVMOpcode::Swap1)); // idx (mod), table, idx
                                               // push idx and table to aux staxk
        res.push(simple_op(AVMOpcode::Dup0));
        res.push(simple_op(AVMOpcode::AuxPush)); // ;; idx (mod)
        res.push(simple_op(AVMOpcode::Dup1));
        res.push(simple_op(AVMOpcode::AuxPush)); // ;; table, idx (mod)
        res.push(simple_op(AVMOpcode::Tget)); // table, idx
        res.push(simple_op(AVMOpcode::Swap1)); // idx, table
    }
    res.push(simple_op(AVMOpcode::Pop));
    res.push(simple_op(AVMOpcode::Pop)); // codept
                                         // need to consume table, idx from aux stack
    for _i in 0..LEVEL {
        res.push(simple_op(AVMOpcode::AuxPop)); // table, codept ;; idx (mod)
        res.push(simple_op(AVMOpcode::AuxPop)); // idx, table, codept
        res.push(simple_op(AVMOpcode::Tset)); // table
    }
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(7)));
    res.push(simple_op(AVMOpcode::Rset));
}

fn cjump(res: &mut Vec<Instruction>, idx: usize) {
    get_from_table(res, Value::Label(Label::Evm(idx)));
    res.push(simple_op(AVMOpcode::Cjump));
}

fn jump(res: &mut Vec<Instruction>, idx: usize) {
    get_from_table(res, Value::Label(Label::Evm(idx)));
    res.push(simple_op(AVMOpcode::Jump));
}

fn call_cjump(res: &mut Vec<Instruction>, idx: u32) {
    get_from_table(res, Value::Label(Label::WasmFunc(idx as usize)));
    res.push(simple_op(AVMOpcode::Cjump));
}

fn call_jump(res: &mut Vec<Instruction>, idx: u32) {
    get_from_table(res, Value::Label(Label::WasmFunc(idx as usize)));
    res.push(simple_op(AVMOpcode::Jump));
}

fn get_frame() -> Instruction {
    Instruction::from_opcode_imm(AVMOpcode::Xget, Value::Int(Uint256::from_usize(0)))
}

fn get_return_pc() -> Instruction {
    Instruction::from_opcode_imm(AVMOpcode::Xget, Value::Int(Uint256::from_usize(1)))
}

fn push_value(v: Value) -> Instruction {
    Instruction::from_opcode_imm(AVMOpcode::Noop, v)
}

fn set_frame() -> Instruction {
    Instruction::from_opcode_imm(AVMOpcode::Xset, Value::Int(Uint256::from_usize(0)))
}

fn get64_from_buffer(loc: usize) -> Instruction {
    Instruction::from_opcode_imm(
        AVMOpcode::GetBuffer64,
        Value::Int(Uint256::from_usize(loc * 8)),
    )
}

fn set64_from_buffer(loc: usize) -> Instruction {
    Instruction::from_opcode_imm(
        AVMOpcode::SetBuffer64,
        Value::Int(Uint256::from_usize(loc * 8)),
    )
}

fn adjust_stack(res: &mut Vec<Instruction>, diff: usize, num: usize) {
    // res.push(debug_op(format!("adjust remove {} save {}", diff, num)));
    if diff == 0 {
        return;
    }
    for _i in 0..num {
        res.push(simple_op(AVMOpcode::AuxPush));
    }
    for _i in 0..diff {
        res.push(simple_op(AVMOpcode::Pop));
    }
    for _i in 0..num {
        res.push(simple_op(AVMOpcode::AuxPop));
    }
}

fn get_memory(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(0)));
}

fn set_memory(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(0)));
    res.push(simple_op(AVMOpcode::Rset)); // value, address
}

fn get_buffer(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(2)));
}

fn set_buffer(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(2)));
    res.push(simple_op(AVMOpcode::Rset));
}

fn get_buffer_len(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    // res.push(debug_op("Getting buffer length".to_string()));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(3)));
}

fn set_buffer_len(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    // res.push(debug_op("Setting buffer length".to_string()));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(3)));
    res.push(simple_op(AVMOpcode::Rset));
}

fn get_gas_left(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    // res.push(debug_op("Getting gas".to_string()));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(4)));
}

fn set_gas_left(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rpush));
    // res.push(debug_op("Setting buffer length".to_string()));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(4)));
    res.push(simple_op(AVMOpcode::Rset));
}

// load byte, save address
fn load_byte(res: &mut Vec<Instruction>, offset: usize) {
    // value, address
    res.push(simple_op(AVMOpcode::Swap1)); // address, value
    res.push(simple_op(AVMOpcode::Dup0)); // address, address, value
    get_memory(res);
    res.push(simple_op(AVMOpcode::Swap1)); // address, buffer, address, value
    res.push(immed_op(
        AVMOpcode::Add,
        Value::Int(Uint256::from_usize(offset)),
    )); // address, buffer, address, value
    res.push(simple_op(AVMOpcode::GetBuffer8)); // value2, address, value
    res.push(simple_op(AVMOpcode::Swap1)); // address, value2, value
    res.push(simple_op(AVMOpcode::Swap2)); // value, value2, address
}

fn store_byte(res: &mut Vec<Instruction>, offset: usize) {
    // value, address
    res.push(simple_op(AVMOpcode::Dup1)); // address, value, address
    res.push(simple_op(AVMOpcode::Dup1)); // value, address, value, address

    get_memory(res);
    res.push(simple_op(AVMOpcode::Swap2)); // address, value, buffer, value, address
    res.push(immed_op(
        AVMOpcode::Add,
        Value::Int(Uint256::from_usize(offset)),
    )); // address, value, buffer, value, address
    res.push(simple_op(AVMOpcode::SetBuffer8)); // buffer, value, address
    set_memory(res);
}

fn generate_store(res: &mut Vec<Instruction>, offset: u32, memory_offset: usize, num: usize) {
    res.push(simple_op(AVMOpcode::Dup1));
    res.push(immed_op(
        AVMOpcode::Add,
        Value::Int(Uint256::from_usize(offset as usize + num - 1)),
    )); // address, value, buffer, value, address
    get_memory(res);
    res.push(get64_from_buffer(0));
    res.push(immed_op(
        AVMOpcode::Mul,
        Value::Int(Uint256::from_usize(1 << 16)),
    ));
    res.push(simple_op(AVMOpcode::GreaterThan));
    res.push(simple_op(AVMOpcode::IsZero));
    cjump(res, 1);

    let offset = memory_offset + (offset as usize);
    for i in 0..num {
        store_byte(res, offset + i);
        res.push(immed_op(
            AVMOpcode::ShiftRight,
            Value::Int(Uint256::from_usize(8)),
        ));
    }
    res.push(simple_op(AVMOpcode::Pop));
    res.push(simple_op(AVMOpcode::Pop));
}

fn generate_load(res: &mut Vec<Instruction>, offset: u32, memory_offset: usize, num: usize) {
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::Add,
        Value::Int(Uint256::from_usize(offset as usize + num - 1)),
    )); // address, value, buffer, value, address
    get_memory(res);
    res.push(get64_from_buffer(0));
    res.push(immed_op(
        AVMOpcode::Mul,
        Value::Int(Uint256::from_usize(1 << 16)),
    ));
    res.push(simple_op(AVMOpcode::GreaterThan));
    res.push(simple_op(AVMOpcode::IsZero));
    cjump(res, 1);

    let offset = memory_offset + (offset as usize);
    res.push(immed_op(
        AVMOpcode::Noop,
        Value::Int(Uint256::from_usize(0)),
    ));
    for i in 0..num {
        res.push(immed_op(
            AVMOpcode::ShiftLeft,
            Value::Int(Uint256::from_usize(8)),
        ));
        load_byte(res, offset + num - 1 - i);
        res.push(simple_op(AVMOpcode::BitwiseOr));
    }
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(simple_op(AVMOpcode::Pop));
}

fn signed_op32_swap(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::SignExtend,
        Value::Int(Uint256::from_usize(3)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::SignExtend,
        Value::Int(Uint256::from_usize(3)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffff)),
    ));
}

fn op32_swap(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffff)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffff)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffff)),
    ));
}

fn op32_unary(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffff)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffff)),
    ));
}

fn op32(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(simple_op(AVMOpcode::Swap1));
    op32_swap(res, op);
}

fn signed_op32(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(simple_op(AVMOpcode::Swap1));
    signed_op32_swap(res, op);
}

fn op64(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(simple_op(AVMOpcode::Swap1));
    op64_swap(res, op);
}

fn signed_op64_swap(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::SignExtend,
        Value::Int(Uint256::from_usize(7)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::SignExtend,
        Value::Int(Uint256::from_usize(7)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(0xffffffffffffffff)),
    ));
}

fn signed_op64(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(simple_op(AVMOpcode::Swap1));
    signed_op64_swap(res, op);
}

fn op64_swap(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(0xffffffffffffffff)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(0xffffffffffffffff)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(0xffffffffffffffff)),
    ));
}

fn op64_unary(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(0xffffffffffffffff)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(0xffffffffffffffff)),
    ));
}

fn make_rotl(res: &mut Vec<Instruction>, num: usize) {
    res.push(simple_op(AVMOpcode::ShiftLeft));
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(num)),
    ));
    res.push(simple_op(AVMOpcode::BitwiseOr));
}

fn make_rotr(res: &mut Vec<Instruction>, num: usize) {
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::ShiftLeft,
        Value::Int(Uint256::from_usize(64)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(simple_op(AVMOpcode::ShiftRight));
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftLeft,
        Value::Int(Uint256::from_usize(num)),
    ));
    res.push(simple_op(AVMOpcode::BitwiseOr));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(64)),
    ));
}

fn make_popcnt(res: &mut Vec<Instruction>) {
    let m1 = 0x5555555555555555u64;
    let m2 = 0x3333333333333333u64;
    let m4 = 0x0F0F0F0F0F0F0F0Fu64;
    let h01 = 0x0101010101010101u64;

    // x -= (x >> 1) & m1;
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(1)),
    ));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(m1)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(simple_op(AVMOpcode::Sub));
    // x = (x & m2) + ((x >> 2) & m2);
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(2)),
    ));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(m2)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(m2)),
    ));
    res.push(simple_op(AVMOpcode::Add));
    // x = (x + (x >> 4)) & m4;
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_u64(4)),
    ));
    res.push(simple_op(AVMOpcode::Add));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_u64(m4)),
    ));
    // x = (x * h01) >> 56;
    res.push(immed_op(AVMOpcode::Mul, Value::Int(Uint256::from_u64(h01))));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(56)),
    ));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xff)),
    ));
}

fn make_clz(res: &mut Vec<Instruction>, num: usize) {
    res.push(immed_op(
        AVMOpcode::AuxPush,
        Value::Int(Uint256::from_usize(1)),
    ));
    res.push(immed_op(
        AVMOpcode::Noop,
        Value::Int(Uint256::from_usize(0)),
    ));
    // inc -- count, value
    for i in 0..num {
        res.push(simple_op(AVMOpcode::Swap1)); // inc -- value, count
        res.push(simple_op(AVMOpcode::Dup0)); // inc -- value, value, count
        res.push(immed_op(
            AVMOpcode::BitwiseAnd,
            Value::Int(Uint256::from_usize(1 << (num - i - 1))),
        )); // inc -- not(result), value, count
        res.push(simple_op(AVMOpcode::IsZero)); // inc -- result, value, count
        res.push(simple_op(AVMOpcode::AuxPop)); // inc, result, value, count
        res.push(simple_op(AVMOpcode::BitwiseAnd)); // inc, value, count
        res.push(simple_op(AVMOpcode::Dup0)); // inc, inc, value, count
        res.push(simple_op(AVMOpcode::AuxPush)); // inc -- inc, value, count
        res.push(simple_op(AVMOpcode::Swap1)); // inc -- value, inc, count
        res.push(simple_op(AVMOpcode::Swap2)); // inc -- count, inc, value
        res.push(simple_op(AVMOpcode::Add)); // inc -- count, value
    }
    res.push(simple_op(AVMOpcode::AuxPop)); // inc, count, value
    res.push(simple_op(AVMOpcode::Pop)); // count, value
    res.push(simple_op(AVMOpcode::Swap1)); // value, count
    res.push(simple_op(AVMOpcode::Pop)); // count
}

fn make_ctz(res: &mut Vec<Instruction>, num: usize) {
    res.push(immed_op(
        AVMOpcode::AuxPush,
        Value::Int(Uint256::from_usize(1)),
    ));
    res.push(immed_op(
        AVMOpcode::Noop,
        Value::Int(Uint256::from_usize(0)),
    ));
    // inc -- count, value
    for i in 0..num {
        res.push(simple_op(AVMOpcode::Swap1)); // inc -- value, count
        res.push(simple_op(AVMOpcode::Dup0)); // inc -- value, value, count
        res.push(immed_op(
            AVMOpcode::BitwiseAnd,
            Value::Int(Uint256::from_usize(1 << i)),
        )); // inc -- not(result), value, count
        res.push(simple_op(AVMOpcode::IsZero)); // inc -- result, value, count
        res.push(simple_op(AVMOpcode::AuxPop)); // inc, result, value, count
        res.push(simple_op(AVMOpcode::BitwiseAnd)); // inc, value, count
        res.push(simple_op(AVMOpcode::Dup0)); // inc, inc, value, count
        res.push(simple_op(AVMOpcode::AuxPush)); // inc -- inc, value, count
        res.push(simple_op(AVMOpcode::Swap1)); // inc -- value, inc, count
        res.push(simple_op(AVMOpcode::Swap2)); // inc -- count, inc, value
        res.push(simple_op(AVMOpcode::Add)); // inc -- count, value
    }
    res.push(simple_op(AVMOpcode::AuxPop)); // inc, count, value
    res.push(simple_op(AVMOpcode::Pop)); // count, value
    res.push(simple_op(AVMOpcode::Swap1)); // value, count
    res.push(simple_op(AVMOpcode::Pop)); // count
}

fn trap_zero_division(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(simple_op(AVMOpcode::IsZero));
    cjump(res, 1);
}

fn trap_zero_division_s32(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftLeft,
        Value::Int(Uint256::from_usize(32)),
    ));
    res.push(simple_op(AVMOpcode::Dup2));
    res.push(simple_op(AVMOpcode::BitwiseOr));
    res.push(push_value(Value::Int(
        Uint256::from_string_hex("ffffffff80000000").unwrap(),
    )));
    res.push(simple_op(AVMOpcode::Equal));
    cjump(res, 1);
}

fn trap_zero_division_s64(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftLeft,
        Value::Int(Uint256::from_usize(64)),
    ));
    res.push(simple_op(AVMOpcode::Dup2));
    res.push(simple_op(AVMOpcode::BitwiseOr));
    res.push(push_value(Value::Int(
        Uint256::from_string_hex("ffffffffffffffff8000000000000000").unwrap(),
    )));
    res.push(simple_op(AVMOpcode::Equal));
    cjump(res, 1);
}

fn is_func(e: &External) -> bool {
    match *e {
        External::Function(_idx) => true,
        _ => false,
    }
}

fn get_num_imports(m: &Module) -> u32 {
    match m.import_section() {
        None => 0,
        Some(sec) => {
            let arr = sec.entries();
            arr.iter().filter(|&x| is_func(x.external())).count() as u32
        }
    }
}

fn find_func_type(m: &Module, num: u32) -> &FunctionType {
    // maybe it is import
    if num < get_num_imports(m) {
        let arr = m.import_section().unwrap().entries();
        let idx = match *arr
            .iter()
            .filter(|&x| is_func(x.external()))
            .collect::<Vec<&ImportEntry>>()[num as usize]
            .external()
        {
            External::Function(idx) => idx,
            _ => 0,
        };
        get_func_type(m, idx)
    }
    // find it from sig section
    else {
        get_func_type(
            m,
            m.function_section().unwrap().entries()[(num - get_num_imports(m)) as usize].type_ref(),
        )
    }
}

fn type_code(t: &ValueType) -> u8 {
    match *t {
        ValueType::I32 => 0,
        ValueType::I64 => 1,
        ValueType::F32 => 2,
        ValueType::F64 => 3,
    }
}

fn hash_ftype(ft: &FunctionType) -> Uint256 {
    let mut data = Vec::new();

    for t in ft.params() {
        data.push(type_code(t));
    }

    data.push(0xff);
    for t in ft.results() {
        data.push(type_code(t));
    }

    let hash_result = keccak256(&data);
    Uint256::from_bytes(&hash_result)
}

fn handle_function(
    res: &mut Vec<Instruction>,
    m: &Module,
    func: &FuncBody,
    idx: usize,
    mut label: usize,
    calli: usize,
    memory_offset: usize,
    max_memory: usize,
) -> usize {
    let sig = m.function_section().unwrap().entries()[idx].type_ref();
    let ftype = get_func_type(m, sig);

    // println!("func start label {}", label);

    // let mut res: Vec<Instruction> = Vec::new();
    let mut stack: Vec<Control> = Vec::new();
    let mut ptr: usize = 0;
    let mut bptr: usize = 0;

    // let mut avm_gas: Vec<usize> = Vec::new();

    // Construct the function top level frame
    let end_label = label + 1;
    label = label + 2;
    bptr = bptr + 1;
    let rets = num_func_returns(ftype);

    let def = Control {
        level: ptr + rets,
        rets: rets,
        target: end_label,
        else_label: 0,
        is_ite: false,
        is_loop: false,
    };

    stack.push(def.clone());

    /*
    println!(
        "Got function with {:?} ops, {:?} locals, {} params, {} rets",
        func.code().elements().len(),
        count_locals(func),
        ftype.params().len(),
        rets
    );*/

    let mut unreachable = false;

    for (_idx_inf, op) in func.code().elements().iter().enumerate() {
        /*
        println!(
            "handling ptr {} frames {}; {:?} ... label {} len {}",
            ptr,
            stack.len(),
            op,
            label,
            res.len()
        );
        */
        usegas(1);
        // res.push(debug_op(format!("{:?} level {} func {} idx {}", *op, ptr, idx, idx_inf)));
        if unreachable {
            if *op == End {
                if stack.len() == 0 {
                    break;
                }
                let c: Control = stack.pop().unwrap();
                bptr = bptr - 1;
                if c.is_ite {
                    if c.else_label != 0 {
                        res.push(mk_label(c.else_label));
                    }
                }
                if !c.is_loop {
                    res.push(mk_label(c.target));
                }
                if c.level != ptr {
                    // println!("End block mismatch {} != {} rets {}", ptr, c.level, c.rets);
                    // panic!("End block mismatch");
                }
                ptr = c.level;
                unreachable = false;
                continue;
            } else if *op == Else {
                let mut c: Control = stack.pop().unwrap();
                ptr = c.level - c.rets;
                jump(res, c.target);
                res.push(mk_label(c.else_label));
                c.else_label = 0;
                stack.push(c);
                unreachable = false;
                continue;
            } else {
                continue;
            }
        }

        match &*op {
            Nop => res.push(simple_op(AVMOpcode::Noop)),
            Unreachable => {
                // res.push(simple_op(AVMOpcode::Panic));
                jump(res, 1);
                if stack.len() == 0 {
                    break;
                }
                let c: &Control = &stack[stack.len() - 1];
                ptr = c.level;
                unreachable = true;
            }
            Block(bt) => {
                let end_label = label;
                label = label + 1;
                bptr = bptr + 1;
                let rets = block_len(&bt);
                stack.push(Control {
                    level: ptr + rets,
                    rets: rets,
                    target: end_label,
                    ..def
                });
            }
            If(bt) => {
                ptr = ptr - 1;
                bptr = bptr + 1;
                let else_label = label;
                let end_label = label + 1;
                let rets = block_len(&bt);
                /*
                eprintln!(
                    "Level if {} rets {} end label {} else label {}",
                    ptr, rets, end_label, else_label
                );
                */
                stack.push(Control {
                    level: ptr + rets,
                    rets: rets,
                    target: end_label,
                    else_label,
                    is_ite: true,
                    ..def
                });
                label = label + 2;
                res.push(simple_op(AVMOpcode::IsZero));
                cjump(res, else_label);
            }
            Else => {
                let mut c: Control = stack.pop().unwrap();
                /*
                eprintln!(
                    "Level else {} end label {} else label {} rets {}",
                    c.level, c.target, c.else_label, c.rets
                );*/
                ptr = c.level - c.rets;
                jump(res, c.target);
                res.push(mk_label(c.else_label));
                c.else_label = 0;
                stack.push(c);
            }
            End => {
                if stack.len() == 0 {
                    break;
                }
                let c: Control = stack.pop().unwrap();
                bptr = bptr - 1;
                if c.is_ite {
                    if c.else_label != 0 {
                        res.push(mk_label(c.else_label));
                    }
                }
                if !c.is_loop {
                    res.push(mk_label(c.target));
                }
                if c.level != ptr {
                    // eprintln!("End block mismatch {} != {} rets {}", ptr, c.level, c.rets);
                    // panic!("End block mismatch");
                }
                ptr = c.level;
            }
            Loop(bt) => {
                let start_label = label;
                label = label + 1;
                bptr = bptr + 1;
                let rets = block_len(&bt);
                stack.push(Control {
                    level: ptr + rets,
                    rets: rets,
                    target: start_label,
                    is_loop: true,
                    ..def
                });
                res.push(mk_label(start_label));
            }
            Drop => {
                ptr = ptr - 1;
                res.push(simple_op(AVMOpcode::Pop));
            }
            Br(x) => {
                let c = &stack[stack.len() - (*x as usize) - 1];
                if !c.is_loop {
                    adjust_stack(res, ptr - c.level, c.rets);
                } else {
                    adjust_stack(res, ptr - (c.level - c.rets), 0);
                }
                unreachable = true;
                jump(res, c.target);
            }
            BrIf(x) => {
                let c = &stack[stack.len() - (*x as usize) - 1];
                let continue_label = label;
                let end_label = label + 1;
                label = label + 2;
                cjump(res, continue_label);
                jump(res, end_label);
                res.push(mk_label(continue_label));
                if !c.is_loop {
                    adjust_stack(res, ptr - c.level - 1, c.rets);
                } else {
                    adjust_stack(res, ptr - (c.level - c.rets) - 1, 0);
                }
                jump(res, c.target);
                res.push(mk_label(end_label));
                ptr = ptr - 1;
            }
            GetLocal(x) => {
                // First get stack frame
                res.push(get_frame());
                // Then get the local from the buffer
                res.push(get64_from_buffer(*x as usize));
                ptr = ptr + 1;
            }
            SetLocal(x) => {
                // First get stack frame
                res.push(get_frame());
                // reorder stack
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer(*x as usize));
                // store frame
                res.push(set_frame());
                ptr = ptr - 1;
            }
            TeeLocal(x) => {
                res.push(simple_op(AVMOpcode::Dup0));
                // First get stack frame
                res.push(get_frame());
                // reorder stack
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer(*x as usize));
                // store frame
                res.push(set_frame());
            }
            I32Const(x) => {
                res.push(push_value(Value::Int(Uint256::from_u64(
                    (*x as u64) & 0xffffffff,
                ))));
                ptr = ptr + 1;
            }
            I64Const(x) => {
                res.push(push_value(Value::Int(Uint256::from_u64(*x as u64))));
                ptr = ptr + 1;
            }
            // Just keep the expression stack
            Call(x) => {
                let ftype = find_func_type(m, *x);
                let return_label = label;
                label = label + 1;
                // push new frame to aux stack

                res.push(simple_op(AVMOpcode::NewBuffer));
                res.push(push_value(Value::Label(Label::Evm(return_label))));
                res.push(push_value(Value::new_tuple(vec![
                    Value::new_buffer(vec![]),
                    int_from_usize(0),
                ])));
                res.push(immed_op(AVMOpcode::Tset, int_from_usize(1)));
                res.push(immed_op(AVMOpcode::Tset, int_from_usize(0)));
                res.push(simple_op(AVMOpcode::AuxPush));
                // Push args to frame
                for i in 0..ftype.params().len() {
                    res.push(get_frame());
                    res.push(simple_op(AVMOpcode::Swap1));
                    res.push(set64_from_buffer(ftype.params().len() - 1 - i));
                    res.push(set_frame());
                }
                call_jump(res, *x);
                res.push(mk_label(return_label));
                // Pop stack frame
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Pop));
                ptr = ptr - ftype.params().len() + ftype.results().len();
            }
            CallIndirect(x, _) => {
                let ftype = get_func_type(m, *x);
                let return_label = label;
                label = label + 1;
                // Save func ptr
                res.push(simple_op(AVMOpcode::AuxPush));
                // push new frame to aux stack
                res.push(simple_op(AVMOpcode::NewBuffer));
                res.push(push_value(Value::Label(Label::Evm(return_label))));
                res.push(push_value(Value::new_tuple(vec![
                    Value::new_buffer(vec![]),
                    int_from_usize(0),
                ])));
                res.push(immed_op(AVMOpcode::Tset, int_from_usize(1)));
                res.push(immed_op(AVMOpcode::Tset, int_from_usize(0)));
                res.push(simple_op(AVMOpcode::AuxPush));

                // Push args to frame
                for i in 0..ftype.params().len() {
                    res.push(get_frame());
                    res.push(simple_op(AVMOpcode::Swap1));
                    res.push(set64_from_buffer(ftype.params().len() - 1 - i));
                    res.push(set_frame());
                }
                // Codepoints and hashes are in some kind of a table
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(simple_op(AVMOpcode::AuxPush));
                res.push(push_value(Value::Int(hash_ftype(&ftype))));
                res.push(simple_op(AVMOpcode::Swap1));
                call_jump(res, calli as u32);
                res.push(mk_label(return_label));
                // Pop stack frame
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Pop));
                ptr = ptr - ftype.params().len() + num_func_returns(ftype) - 1;
            }
            Return => {
                let c = &stack[0];
                // println!("return {} level {} rets {}", ptr, c.level, c.rets);
                adjust_stack(res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                jump(res, c.target);
                unreachable = true;
            }
            Select => {
                let else_label = label;
                cjump(res, else_label);
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(mk_label(else_label));
                res.push(simple_op(AVMOpcode::Pop));

                label = label + 2;
                ptr = ptr - 2;
            }
            BrTable(data) => {
                let tab = &data.table;
                let def = data.default;
                let len = tab.len();
                for (i, num) in tab.iter().enumerate() {
                    let c = &stack[stack.len() - (*num as usize) - 1];
                    // println!("Frame {:?}", c);
                    res.push(simple_op(AVMOpcode::Dup0));
                    res.push(immed_op(
                        AVMOpcode::Equal,
                        Value::Int(Uint256::from_usize(i)),
                    ));
                    res.push(simple_op(AVMOpcode::IsZero));
                    cjump(res, label + i);
                    res.push(simple_op(AVMOpcode::Pop));
                    if !c.is_loop {
                        adjust_stack(res, ptr - c.level - 1, c.rets);
                    } else {
                        adjust_stack(res, ptr - (c.level - c.rets) - 1, 0);
                    }
                    jump(res, c.target);
                    res.push(mk_label(label + i));
                }
                let c = &stack[stack.len() - (def as usize) - 1];
                res.push(simple_op(AVMOpcode::Pop));
                if !c.is_loop {
                    adjust_stack(res, ptr - c.level - 1, c.rets);
                } else {
                    adjust_stack(res, ptr - (c.level - c.rets) - 1, 0);
                }
                // adjust_stack(res, ptr - c.level - 1, c.rets);
                jump(res, c.target);

                unreachable = true;
                // ptr = ptr - 1 - c.rets;
                ptr = 0;
                label = label + len + 2;
            }
            GetGlobal(x) => {
                ptr = ptr + 1;
                get_memory(res);
                res.push(get64_from_buffer((*x + 1) as usize));
            }
            SetGlobal(x) => {
                ptr = ptr - 1;
                get_memory(res);
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer((*x + 1) as usize));
                set_memory(res);
            }

            I64Store(_, offset) => {
                ptr = ptr - 2;
                generate_store(res, *offset, memory_offset, 8);
            }

            I32Store(_, offset) | I64Store32(_, offset) => {
                ptr = ptr - 2;
                generate_store(res, *offset, memory_offset, 4);
            }

            I32Store16(_, offset) | I64Store16(_, offset) => {
                ptr = ptr - 2;
                generate_store(res, *offset, memory_offset, 2);
            }

            I32Store8(_, offset) | I64Store8(_, offset) => {
                ptr = ptr - 2;
                generate_store(res, *offset, memory_offset, 1);
            }

            I64Load(_, offset) => {
                generate_load(res, *offset, memory_offset, 8);
            }

            I32Load(_, offset) => {
                generate_load(res, *offset, memory_offset, 4);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
            }

            I64Load32U(_, offset) => {
                generate_load(res, *offset, memory_offset, 4);
            }

            I32Load16U(_, offset) | I64Load16U(_, offset) => {
                generate_load(res, *offset, memory_offset, 2);
            }

            I32Load8U(_, offset) | I64Load8U(_, offset) => {
                generate_load(res, *offset, memory_offset, 1);
            }

            I64Load32S(_, offset) => {
                generate_load(res, *offset, memory_offset, 4);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(31)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_u64(0xffffffff00000000)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
            }

            I64Load16S(_, offset) => {
                generate_load(res, *offset, memory_offset, 2);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(15)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_u64(0xffffffffffff0000)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
            }

            I32Load16S(_, offset) => {
                generate_load(res, *offset, memory_offset, 2);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(15)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_u64(0xffffffffffff0000)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
            }

            I64Load8S(_, offset) => {
                generate_load(res, *offset, memory_offset, 1);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(7)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_u64(0xffffffffffffff00)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
            }

            I32Load8S(_, offset) => {
                generate_load(res, *offset, memory_offset, 1);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(7)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_u64(0xfffffffffffff00)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
            }

            I32Add => {
                op32_swap(res, AVMOpcode::Add);
                ptr = ptr - 1;
            }
            I32Sub => {
                signed_op32_swap(res, AVMOpcode::Sub);
                ptr = ptr - 1;
            }
            I32Eq => {
                op32_swap(res, AVMOpcode::Equal);
                ptr = ptr - 1;
            }
            I32GtU => {
                op32_swap(res, AVMOpcode::GreaterThan);
                ptr = ptr - 1;
            }
            I32GtS => {
                signed_op32_swap(res, AVMOpcode::SGreaterThan);
                ptr = ptr - 1;
            }
            I32LtU => {
                op32_swap(res, AVMOpcode::LessThan);
                ptr = ptr - 1;
            }
            I32LtS => {
                signed_op32_swap(res, AVMOpcode::SLessThan);
                ptr = ptr - 1;
            }
            I32GeU => {
                op32_swap(res, AVMOpcode::LessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32GeS => {
                signed_op32_swap(res, AVMOpcode::SLessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32Ne => {
                op32_swap(res, AVMOpcode::Equal);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32LeU => {
                op32_swap(res, AVMOpcode::GreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32LeS => {
                signed_op32_swap(res, AVMOpcode::SGreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32Eqz => {
                op32_unary(res, AVMOpcode::IsZero);
            }

            I32Mul => {
                op32_swap(res, AVMOpcode::Mul);
                ptr = ptr - 1;
            }
            I32DivU => {
                trap_zero_division(res);
                op32_swap(res, AVMOpcode::Div);
                ptr = ptr - 1;
            }
            I32DivS => {
                trap_zero_division(res);
                trap_zero_division_s32(res);
                signed_op32_swap(res, AVMOpcode::Sdiv);
                ptr = ptr - 1;
            }
            I32RemU => {
                trap_zero_division(res);
                op32_swap(res, AVMOpcode::Mod);
                ptr = ptr - 1;
            }
            I32RemS => {
                trap_zero_division(res);
                signed_op32_swap(res, AVMOpcode::Smod);
                ptr = ptr - 1;
            }
            I32And => {
                op32_swap(res, AVMOpcode::BitwiseAnd);
                ptr = ptr - 1;
            }
            I32Or => {
                op32_swap(res, AVMOpcode::BitwiseOr);
                ptr = ptr - 1;
            }
            I32Xor => {
                op32_swap(res, AVMOpcode::BitwiseXor);
                ptr = ptr - 1;
            }
            I32Shl => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x1f)),
                ));
                op32(res, AVMOpcode::ShiftLeft);
                ptr = ptr - 1;
            }
            I32ShrU => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x1f)),
                ));
                op32(res, AVMOpcode::ShiftRight);
                ptr = ptr - 1;
            }
            I32ShrS => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x1f)),
                ));
                signed_op32(res, AVMOpcode::ShiftArith);
                ptr = ptr - 1;
            }

            I32Rotl => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x1f)),
                ));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                res.push(simple_op(AVMOpcode::Swap1));
                make_rotl(res, 32);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                ptr = ptr - 1;
            }
            I32Rotr => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x1f)),
                ));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                res.push(simple_op(AVMOpcode::Swap1));
                make_rotr(res, 32);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                ptr = ptr - 1;
            }

            I32Popcnt => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                make_popcnt(res);
            }
            I32Clz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                make_clz(res, 32);
            }
            I32Ctz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                make_ctz(res, 32);
            }

            I64Add => {
                op64_swap(res, AVMOpcode::Add);
                ptr = ptr - 1;
            }
            I64Sub => {
                signed_op64_swap(res, AVMOpcode::Sub);
                ptr = ptr - 1;
            }
            I64Eq => {
                op64_swap(res, AVMOpcode::Equal);
                ptr = ptr - 1;
            }
            I64GtU => {
                op64_swap(res, AVMOpcode::GreaterThan);
                ptr = ptr - 1;
            }
            I64GtS => {
                signed_op64_swap(res, AVMOpcode::SGreaterThan);
                ptr = ptr - 1;
            }
            I64LtU => {
                op64_swap(res, AVMOpcode::LessThan);
                ptr = ptr - 1;
            }
            I64LtS => {
                signed_op64_swap(res, AVMOpcode::SLessThan);
                ptr = ptr - 1;
            }
            I64GeU => {
                op64_swap(res, AVMOpcode::LessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64GeS => {
                signed_op64_swap(res, AVMOpcode::SLessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64Ne => {
                op64_swap(res, AVMOpcode::Equal);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64LeU => {
                op64_swap(res, AVMOpcode::GreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64LeS => {
                signed_op64_swap(res, AVMOpcode::SGreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64Eqz => {
                op64_unary(res, AVMOpcode::IsZero);
            }

            I64Mul => {
                op64_swap(res, AVMOpcode::Mul);
                ptr = ptr - 1;
            }
            I64DivU => {
                trap_zero_division(res);
                op64_swap(res, AVMOpcode::Div);
                ptr = ptr - 1;
            }
            I64DivS => {
                trap_zero_division(res);
                trap_zero_division_s64(res);
                signed_op64_swap(res, AVMOpcode::Sdiv);
                ptr = ptr - 1;
            }
            I64RemU => {
                trap_zero_division(res);
                op64_swap(res, AVMOpcode::Mod);
                ptr = ptr - 1;
            }
            I64RemS => {
                trap_zero_division(res);
                signed_op64_swap(res, AVMOpcode::Smod);
                ptr = ptr - 1;
            }
            I64And => {
                op64_swap(res, AVMOpcode::BitwiseAnd);
                ptr = ptr - 1;
            }
            I64Or => {
                op64_swap(res, AVMOpcode::BitwiseOr);
                ptr = ptr - 1;
            }
            I64Xor => {
                op64_swap(res, AVMOpcode::BitwiseXor);
                ptr = ptr - 1;
            }
            I64Shl => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x3f)),
                ));
                op64(res, AVMOpcode::ShiftLeft);
                ptr = ptr - 1;
            }
            I64ShrU => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x3f)),
                ));
                op64(res, AVMOpcode::ShiftRight);
                ptr = ptr - 1;
            }
            I64ShrS => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x3f)),
                ));
                signed_op64(res, AVMOpcode::ShiftArith);
                ptr = ptr - 1;
            }

            I64Rotl => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x3f)),
                ));
                make_rotl(res, 64);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
                ptr = ptr - 1;
            }
            I64Rotr => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0x3f)),
                ));
                make_rotr(res, 64);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
                ptr = ptr - 1;
            }

            I64Popcnt => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
                make_popcnt(res);
            }
            I64Clz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
                make_clz(res, 64);
            }
            I64Ctz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
                make_ctz(res, 64);
            }

            I32WrapI64 => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
            }
            I64ExtendUI32 => {
                res.push(simple_op(AVMOpcode::Noop));
            }
            I64ExtendSI32 => {
                res.push(immed_op(
                    AVMOpcode::SignExtend,
                    Value::Int(Uint256::from_usize(3)),
                ));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_u64(0xffffffffffffffff)),
                ));
            }

            CurrentMemory(_) => {
                ptr = ptr + 1;
                get_memory(res);
                res.push(get64_from_buffer(0));
            }

            GrowMemory(_) => {
                let end_label = label;
                let ok_label = label + 1;
                get_memory(res);
                res.push(get64_from_buffer(0));
                res.push(simple_op(AVMOpcode::Dup0)); // old value to return (except need to handle error)
                res.push(simple_op(AVMOpcode::Swap2));
                res.push(simple_op(AVMOpcode::Add));
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::GreaterThan,
                    Value::Int(Uint256::from_usize(max_memory + 1)),
                ));
                cjump(res, ok_label);
                res.push(simple_op(AVMOpcode::Pop));
                res.push(simple_op(AVMOpcode::Pop));
                res.push(push_value(Value::Int(Uint256::from_usize(0xffffffff)))); // -1 when error
                jump(res, end_label);
                res.push(mk_label(ok_label));
                get_memory(res);
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer(0));
                set_memory(res);
                res.push(mk_label(end_label));
                label = label + 2;
            }
            _ => {}
            /*
            i => {
                panic!("Unknown opcode {:?}", i);
            }*/
        }

    }

    // Function return
    res.push(mk_label(end_label));
    res.push(get_return_pc());
    get_return_from_table(res);
    res.push(simple_op(AVMOpcode::Jump));

    return label;
}

pub fn clear_labels(arr: Vec<Instruction>) -> Vec<Instruction> {
    let mut res = vec![];
    for inst in arr.iter() {
        match inst.opcode {
            AVMOpcode::Label => res.push(simple_op(AVMOpcode::Noop)),
            _ => res.push(inst.clone()),
        }
    }
    res
}

fn table_to_tuple(tab: &[usize], prefix: usize, shift: usize, level: usize, limit: usize) -> Value {
    if prefix > limit {
        return int_from_usize(0);
    }
    if level == 0 {
        let mut v = vec![];
        v.reserve(8);
        for i in 0..8 {
            let idx = prefix + (i << shift);
            let ptr = if idx < tab.len() { tab[idx] } else { 0 };
            // We are adding one instruction to the beginning
            v.push(Value::CodePoint(CodePt::Internal(ptr)));
        }
        return Value::new_tuple(v);
    }
    let mut v = vec![];
    v.reserve(8);
    for i in 0..8 {
        let prefix = prefix + (i << shift);
        v.push(table_to_tuple(tab, prefix, shift + 3, level - 1, limit));
    }
    return Value::new_tuple(v);
}

fn table_to_tuple2(
    tab: &[Value],
    prefix: usize,
    shift: usize,
    level: usize,
    limit: usize,
) -> Value {
    if prefix > limit {
        return int_from_usize(0);
    }
    if level == 0 {
        let mut v = vec![];
        for i in 0..8 {
            let idx = prefix + (i << shift);
            if idx < tab.len() {
                v.push(tab[idx].clone())
            } else {
                v.push(int_from_usize(0))
            }
        }
        return Value::new_tuple(v);
    }
    let mut v = vec![];
    for i in 0..8 {
        let prefix = prefix + (i << shift);
        v.push(table_to_tuple2(tab, prefix, shift + 3, level - 1, limit));
    }
    return Value::new_tuple(v);
}

pub fn make_table(tab: &[Value]) -> Value {
    table_to_tuple2(tab, 0, 0, LEVEL - 1, tab.len())
}

fn value_replace_labels(v: &Value, label_map: &HashMap<Label, Value>) -> Result<Value, Label> {
    match v {
        Value::HashOnly(_, _) => Ok(v.clone()),
        Value::Int(_) => Ok(v.clone()),
        Value::CodePoint(_) => Ok(v.clone()),
        Value::Buffer(_) => Ok(v.clone()),
        Value::Label(label) => {
            let maybe_pc = label_map.get(&label);
            match maybe_pc {
                Some(pc) => Ok(pc.clone()),
                None => Err(label.clone()),
            }
        }
        Value::WasmCodePoint(val, code) => Ok(Value::WasmCodePoint(
            Box::new(value_replace_labels(val, label_map)?),
            code.clone(),
        )),
        Value::Tuple(tup) => {
            let mut new_vec = Vec::new();
            for v in tup.iter() {
                new_vec.push(value_replace_labels(v, label_map)?);
            }
            Ok(Value::new_tuple(new_vec))
        }
    }
}

fn inst_replace_labels(
    inst: &Instruction,
    label_map: &HashMap<Label, Value>,
) -> Result<Instruction, Label> {
    match &inst.immediate {
        Some(val) => Ok(Instruction::from_opcode_imm(
            inst.opcode,
            value_replace_labels(&val, label_map)?,
        )),
        None => Ok(inst.clone()),
    }
}

pub fn has_label(inst: &Instruction) -> bool {
    inst.opcode == AVMOpcode::Label
}

pub fn get_inst(inst: &Instruction) -> u8 {
    if inst.opcode == AVMOpcode::Label {
        AVMOpcode::Noop as u8
    } else {
        inst.opcode as u8
    }
}

pub fn get_immed(inst: &Instruction) -> &Option<Value> {
    if inst.opcode == AVMOpcode::Label {
        &None
    } else {
        &inst.immediate
    }
}

pub fn resolve_labels(arr: &Vec<Instruction>) -> (Vec<Instruction>, usize) {
    let mut labels = HashMap::new();
    let mut tab = vec![];
    usegas(10000);
    for (idx, inst) in arr.iter().enumerate() {
        match inst.opcode {
            AVMOpcode::Label => match inst.immediate {
                Some(Value::Label(Label::Evm(num))) => {
                    tab.push(idx);
                    labels.insert(Label::Evm(num), int_from_usize(tab.len() - 1));
                }
                Some(Value::Label(Label::WasmFunc(num))) => {
                    tab.push(idx);
                    labels.insert(Label::WasmFunc(num), int_from_usize(tab.len() - 1));
                }
                _ => {}
            },
            _ => {}
        }
    }
    usegas(10000);
    let mut res = vec![];
    res.reserve(arr.len());
    for inst in arr.iter() {
        // handle error
        usegas(1);
        res.push(inst_replace_labels(inst, &labels).unwrap());
        // res.push(inst.clone());
    }
    // let res = arr.iter().map(|inst| inst_replace_labels(inst, &labels).unwrap()).collect();
    usegas(10000);
    // println!("Labels {}", tab.len());
    // (res, table_to_tuple(&tab, 0, 0, LEVEL - 1, tab.len()))
    (res, tab.len())
}

fn init_value(_m: &Module, expr: &InitExpr) -> u64 {
    // eprintln!("init {:?}", expr);
    match expr.code()[0] {
        I32Const(a) => (a as u64) & 0xffffffff,
        F32Const(a) => a as u64,
        I64Const(a) => a as u64,
        F64Const(a) => a as u64,
        _ => 0,
    }
}

fn int_from_usize(a: usize) -> Value {
    Value::Int(Uint256::from_usize(a))
}

fn int_from_u32(a: u32) -> Value {
    Value::Int(Uint256::from_u64(a as u64))
}

fn find_function(m: &Module, name: &str) -> Option<u32> {
    match m.export_section() {
        None => None,
        Some(sec) => {
            for e in sec.entries() {
                // println!("Export {}: {:?}", e.field(), e.internal());
                if e.field() == name {
                    if let Internal::Function(arg) = *e.internal() {
                        return Some(arg);
                    }
                }
            }
            None
        }
    }
}

fn find_memory_size(m: &Module) -> u32 {
    match m.memory_section() {
        None => 0,
        Some(sec) => {
            for e in sec.entries() {
                return e.limits().initial();
            }
            0
        }
    }
}

fn find_memory_max(m: &Module) -> u32 {
    match m.memory_section() {
        None => 0,
        Some(sec) => {
            for e in sec.entries() {
                match e.limits().maximum() {
                    None => return 0x10000,
                    Some(max) => return max,
                }
            }
            0
        }
    }
}

fn get_func_imports(m: &Module) -> Vec<&ImportEntry> {
    match m.import_section() {
        None => Vec::new(),
        Some(sec) => {
            let arr = sec.entries();
            arr.iter()
                .filter(|&x| is_func(x.external()))
                .collect::<Vec<&ImportEntry>>()
        }
    }
}

fn empty_tuple() -> Value {
    Value::new_tuple(vec![
        int_from_usize(0),
        int_from_usize(0),
        int_from_usize(0),
        int_from_usize(0),
        int_from_usize(0),
        int_from_usize(0),
        int_from_usize(0),
        int_from_usize(0),
    ])
}

fn empty_table(res: &mut Vec<Instruction>, n: usize) {
    res.push(push_value(empty_tuple()));
    for _i in 1..n {
        for _i in 0..8 {
            res.push(simple_op(AVMOpcode::Dup0));
        }
        res.push(push_value(empty_tuple()));
        for i in 0..8 {
            res.push(immed_op(AVMOpcode::Tset, int_from_usize(i)));
        }
        res.push(simple_op(AVMOpcode::Swap1));
        res.push(simple_op(AVMOpcode::Pop));
    }
}

pub fn process_wasm(buffer: &[u8]) -> Vec<Instruction> {
    let mut init = vec![];
    init.reserve(1000000);

    // These might become replaced
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));

    // Save register
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(simple_op(AVMOpcode::AuxPush));

    empty_table(&mut init, LEVEL);

    init.push(simple_op(AVMOpcode::NewBuffer));
    init.push(push_value(int_from_usize(1000000000)));

    // Initialize register
    init.push(push_value(Value::new_tuple(vec![
        int_from_usize(0), // memory
        int_from_usize(0), // call table
        int_from_usize(0), // IO buffer
        int_from_usize(0), // IO len
        int_from_usize(0), // gas left
        int_from_usize(0), // Immed
        int_from_usize(0), // Code point
        int_from_usize(0), // Generated jump table
    ])));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(4))); // gas left
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(0))); // memory
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(7))); // generated table
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(5))); // immed
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(1))); // call table
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(2))); // IO buffer
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(3))); // IO len
    init.push(simple_op(AVMOpcode::Rset));

    init.push(simple_op(AVMOpcode::ErrCodePoint));
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(6)));
    init.push(simple_op(AVMOpcode::Rset));

    process_wasm_inner(buffer, &mut init, &vec![2], &"test".to_string(), true);

    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(7)));
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(6)));
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(4)));
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(2)));
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(3)));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Pop));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Rset));
    init.push(simple_op(AVMOpcode::Halt));

    init
}

fn process_test(
    buffer: &[u8],
    test_args: &[u64],
    entry: &String,
    set_memory: bool,
) -> Vec<Instruction> {
    let mut init = vec![];

    // These might become replaced
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));

    // Save register
    init.push(simple_op(AVMOpcode::Rpush));
    init.push(simple_op(AVMOpcode::AuxPush));

    // Initialize register
    init.push(push_value(Value::new_tuple(vec![
        Value::new_buffer(vec![]), // memory
        int_from_usize(0),         // call table
        Value::new_buffer(vec![]), // IO buffer
        int_from_usize(0),         // IO len
        int_from_usize(100000),    // gas left
        int_from_usize(0),         // Immed
        int_from_usize(0),         // Instruction
    ])));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(0)));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(1)));
    init.push(simple_op(AVMOpcode::Rset));

    process_wasm_inner(buffer, &mut init, test_args, entry, set_memory);

    // return value is in expression stack?

    init.push(simple_op(AVMOpcode::Rpush));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(0)));
    // Pop frame?
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Pop));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Rset));
    init.push(simple_op(AVMOpcode::Noop));

    init
}

fn process_wasm_inner(
    buffer: &[u8],
    init: &mut Vec<Instruction>,
    test_args: &[u64],
    entry: &String,
    init_memory: bool,
) {
    let module = parity_wasm::deserialize_buffer::<Module>(buffer).unwrap();
    assert!(module.code_section().is_some());

    let imports = get_func_imports(&module);

    let code_section = module.code_section().unwrap(); // Part of the module with functions code

    let f_count = code_section.bodies().len() + imports.len();
    let max_memory = find_memory_max(&module) as usize;

    // Construct initial memory with globals
    let memory_offset = if init_memory {
        init.push(simple_op(AVMOpcode::NewBuffer));

        let mem_size = find_memory_size(&module);

        init.push(push_value(int_from_u32(mem_size)));
        init.push(set64_from_buffer(0));
        let mut globals = 1;
        if let Some(sec) = module.global_section() {
            for g in sec.entries().iter() {
                init.push(push_value(Value::Int(Uint256::from_u64(init_value(
                    &module,
                    g.init_expr(),
                )))));
                init.push(set64_from_buffer(globals));
                globals = globals + 1;
            }
        }

        // Add initial memory segments
        let memory_offset = globals * 8;
        if let Some(sec) = module.data_section() {
            for seg in sec.entries().iter() {
                let offset = match seg.offset() {
                    Some(a) => init_value(&module, a) as usize,
                    None => 0,
                };
                for (i, bt) in seg.value().iter().enumerate() {
                    init.push(push_value(int_from_usize(*bt as usize)));
                    init.push(immed_op(
                        AVMOpcode::SetBuffer8,
                        int_from_usize(memory_offset + offset + i),
                    ));
                }
            }
        }

        set_memory(init);
        memory_offset
    } else {
        let mut globals = 1;
        if let Some(sec) = module.global_section() {
            globals += sec.entries().len();
        }

        // Add initial memory segments
        let memory_offset = globals * 8;
        memory_offset
    };

    // Put initial frame to aux stack
    init.push(push_value(Value::Label(Label::WasmFunc(f_count + 1))));
    init.push(push_value(Value::new_tuple(vec![
        Value::new_buffer(vec![]),
        int_from_usize(0),
    ])));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(1)));
    init.push(simple_op(AVMOpcode::AuxPush));

    // Add test arguments to the frame
    for (i, arg) in test_args.iter().enumerate() {
        init.push(get_frame());
        init.push(push_value(Value::Int(Uint256::from_u64(*arg))));
        init.push(set64_from_buffer(i));
        init.push(set_frame());
    }

    // Here we should have jump to the correct function
    if let Some(f) = find_function(&module, entry) {
        call_jump(init, f);
    }

    let mut label = 2;
    let calli = f_count;

    for (idx, f) in code_section.bodies().iter().enumerate() {
        // function return will be in the stack
        init.push(mk_func_label(idx + imports.len()));
        let n_label = handle_function(
            init,
            &module,
            f,
            idx,
            label,
            calli,
            memory_offset,
            max_memory,
        );
        label = n_label;
    }

    usegas(10000);

    for (idx, f) in get_func_imports(&module).iter().enumerate() {
        init.push(mk_func_label(idx));
        if f.field().contains("read") {
            // init.push(debug_op("Read".to_string()));
            // Get buffer
            get_buffer(init);
            // Get param
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(simple_op(AVMOpcode::GetBuffer8));
        }
        if f.field().contains("write") {
            // init.push(debug_op("Write".to_string()));
            // Get buffer
            get_buffer(init);
            // Get params
            init.push(get_frame());
            init.push(get64_from_buffer(1));
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(simple_op(AVMOpcode::SetBuffer8));
            set_buffer(init);
        }
        if f.field().contains("getlen") {
            get_buffer_len(init);
        }
        if f.field().contains("setlen") {
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            set_buffer_len(init);
        }
        if f.field().contains("usegas") {
            let ok_label = label;
            label = label + 1;
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            get_gas_left(init);
            init.push(simple_op(AVMOpcode::GreaterThan));
            cjump(init, ok_label);

            init.push(push_value(int_from_usize(0)));
            set_gas_left(init);
            call_jump(init, (f_count + 1) as u32);

            init.push(mk_label(ok_label));
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            get_gas_left(init);
            init.push(simple_op(AVMOpcode::Sub));
            set_gas_left(init);
        }
        if f.field().contains("rvec") {
            let start_label = label;
            let end_label = label + 1;
            label = label + 2;

            // init.push(debug_op("Rvec".to_string()));
            // init counter
            init.push(get_frame());
            init.push(push_value(int_from_usize(0)));
            init.push(set64_from_buffer(3));
            init.push(set_frame());

            init.push(mk_label(start_label));
            // check if loop ends
            init.push(get_frame());
            init.push(get64_from_buffer(3));
            init.push(get_frame());
            init.push(get64_from_buffer(2));
            init.push(simple_op(AVMOpcode::LessThan));
            cjump(init, end_label);

            // init.push(debug_op("Rvec loop".to_string()));
            // Read from IO
            get_buffer(init);
            init.push(get_frame());
            init.push(get64_from_buffer(1)); // offset
            init.push(simple_op(AVMOpcode::GetBuffer8));
            // Write to memory
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // pointer
            init.push(simple_op(AVMOpcode::Swap1));
            generate_store(init, 0, memory_offset, 1);

            // increment counters
            init.push(get_frame());
            init.push(get64_from_buffer(1));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(1));
            init.push(set_frame());

            init.push(get_frame());
            init.push(get64_from_buffer(3));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(3));
            init.push(set_frame());

            // also increment memory pointer
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(0));
            init.push(set_frame());

            jump(init, start_label);

            init.push(mk_label(end_label));
        }
        if f.field().contains("wvec") {
            let start_label = label;
            let end_label = label + 1;
            label = label + 2;

            // init.push(debug_op("Wvec".to_string()));
            // init counter
            init.push(get_frame());
            init.push(push_value(int_from_usize(0)));
            init.push(set64_from_buffer(3));
            init.push(set_frame());

            init.push(mk_label(start_label));
            // check if loop ends
            init.push(get_frame());
            init.push(get64_from_buffer(3));
            init.push(get_frame());
            init.push(get64_from_buffer(2));
            init.push(simple_op(AVMOpcode::LessThan));
            cjump(init, end_label);

            // init.push(debug_op("Wvec loop".to_string()));
            // Read from memory
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // pointer
            generate_load(init, 0, memory_offset, 1);
            // Write to IO
            get_buffer(init);
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(get_frame());
            init.push(get64_from_buffer(1)); // offset
            init.push(simple_op(AVMOpcode::SetBuffer8));
            set_buffer(init);

            // increment counters
            init.push(get_frame());
            init.push(get64_from_buffer(1));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(1));
            init.push(set_frame());

            init.push(get_frame());
            init.push(get64_from_buffer(3));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(3));
            init.push(set_frame());

            // also increment memory pointer
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(0));
            init.push(set_frame());

            jump(init, start_label);

            init.push(mk_label(end_label));
        }
        if f.field().contains("tuple2buffer") {
            // inside frame: ptr, idx1, idx2, len; counter
            let start_label = label;
            let end_label = label + 1;
            label = label + 2;

            // init.push(debug_op("tuple2buffer".to_string()));
            // init counter
            init.push(get_frame());
            init.push(push_value(int_from_usize(0)));
            init.push(set64_from_buffer(4));
            init.push(set_frame());

            init.push(mk_label(start_label));
            // check if loop ends
            init.push(get_frame());
            init.push(get64_from_buffer(4));
            init.push(get_frame());
            init.push(get64_from_buffer(3));
            init.push(simple_op(AVMOpcode::LessThan));
            cjump(init, end_label);

            // init.push(debug_op("tuple2buffer loop".to_string()));
            // Read from tuple
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(5))); // tuple, buffer
            init.push(get_frame());
            init.push(get64_from_buffer(1)); // idx, tuple
            init.push(simple_op(AVMOpcode::Tget)); // tuple2
            init.push(get_frame());
            init.push(get64_from_buffer(2)); // idx2, tuple2
            init.push(simple_op(AVMOpcode::Tget)); // buffer
            init.push(get_frame());
            init.push(get64_from_buffer(4)); // offset, buffer
            init.push(simple_op(AVMOpcode::GetBuffer8)); // byte
                                                         // Write to memory
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // pointer
            init.push(simple_op(AVMOpcode::Swap1));
            generate_store(init, 0, memory_offset, 1);

            // increment counters
            init.push(get_frame());
            init.push(get64_from_buffer(4));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(4));
            init.push(set_frame());

            // also increment memory pointer
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(push_value(int_from_usize(1)));
            init.push(simple_op(AVMOpcode::Add));
            init.push(get_frame());
            init.push(simple_op(AVMOpcode::Swap1));
            init.push(set64_from_buffer(0));
            init.push(set_frame());

            jump(init, start_label);

            init.push(mk_label(end_label));
        }
        if f.field().contains("tuplebytes") {
            // init.push(debug_op("tuplebytes".to_string()));
            get_memory(init); // buffer
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(5))); // tuple, buffer
            init.push(get_frame());
            init.push(get64_from_buffer(1)); // idx, tuple, buffer
                                             // init.push(simple_op(AVMOpcode::DebugPrint));
            init.push(simple_op(AVMOpcode::Tget)); // int, buffer
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // address, int, buffer
            init.push(immed_op(
                AVMOpcode::Add,
                Value::Int(Uint256::from_usize(memory_offset)),
            )); // address, int, buffer
            init.push(simple_op(AVMOpcode::SetBuffer256)); // buffer
            set_memory(init);
        }
        if f.field().contains("tuple2bytes") {
            // init.push(debug_op("tuple2bytes".to_string()));
            get_memory(init); // buffer
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(5))); // tuple, buffer
            init.push(get_frame());
            init.push(get64_from_buffer(1)); // idx, tuple, buffer
            init.push(simple_op(AVMOpcode::Tget)); // tuple2, buffer
            init.push(get_frame());
            init.push(get64_from_buffer(2)); // idx2, tuple2, buffer
            init.push(simple_op(AVMOpcode::Tget)); // int, buffer
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // address, int, buffer
            init.push(immed_op(
                AVMOpcode::Add,
                Value::Int(Uint256::from_usize(memory_offset)),
            )); // address, int, buffer
            init.push(simple_op(AVMOpcode::SetBuffer256)); // buffer
            set_memory(init);
        }
        if f.field().contains("uintimmed") {
            get_memory(init); // buffer
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // address, buffer
            init.push(immed_op(
                AVMOpcode::Add,
                Value::Int(Uint256::from_usize(memory_offset)),
            )); // address, buffer
            init.push(simple_op(AVMOpcode::GetBuffer256));
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tset, int_from_usize(5)));
            init.push(simple_op(AVMOpcode::Rset));
        }
        if f.field().contains("specialimmed") {
            init.push(push_value(Value::new_tuple(vec![
                Value::new_buffer(vec![]), // frame
                int_from_usize(0),         // call table
            ])));
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tset, int_from_usize(5)));
            init.push(simple_op(AVMOpcode::Rset));
        }
        if f.field().contains("globalimmed") {
            init.push(push_value(empty_tuple()));
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tset, int_from_usize(5)));
            init.push(simple_op(AVMOpcode::Rset));
        }
        if f.field().contains("pushinst") {
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(6))); // codepoint
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // insn, codepoint
            init.push(simple_op(AVMOpcode::PushInsn));
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tset, int_from_usize(6)));
            init.push(simple_op(AVMOpcode::Rset));
        }
        if f.field().contains("pushimmed") {
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(6))); // codepoint
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(5))); // immed, codepoint
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // insn, immed, codepoint
            init.push(simple_op(AVMOpcode::PushInsnImm));
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tset, int_from_usize(6)));
            init.push(simple_op(AVMOpcode::Rset));
        }
        if f.field().contains("cptable") {
            init.push(simple_op(AVMOpcode::Rpush));
            init.push(immed_op(AVMOpcode::Tget, int_from_usize(6))); // codepoint
            init.push(get_frame());
            init.push(get64_from_buffer(0)); // idx, codept
            set_jump_table(init);
        }

        // Return from function
        init.push(get_return_pc());
        get_return_from_table(init);
        init.push(simple_op(AVMOpcode::Jump));
    }

    // Indirect calls
    init.push(mk_func_label(f_count));
    if let Some(sec) = module.elements_section() {
        for seg in sec.entries().iter() {
            let offset = match seg.offset() {
                None => 0,
                Some(init) => init_value(&module, init) as usize,
            };
            for (idx, f_idx) in seg.members().iter().enumerate() {
                let next_label = label;
                label = label + 1;
                // Function index in module
                let ftype = find_func_type(&module, *f_idx);
                init.push(simple_op(AVMOpcode::Dup0));
                init.push(push_value(Value::Int(Uint256::from_usize(idx + offset))));
                init.push(simple_op(AVMOpcode::Equal));
                init.push(simple_op(AVMOpcode::IsZero));
                cjump(init, next_label);
                // We will call this function now or fail
                init.push(simple_op(AVMOpcode::Pop));
                init.push(push_value(Value::Int(hash_ftype(&ftype))));
                init.push(simple_op(AVMOpcode::Equal));
                call_cjump(init, *f_idx as u32);
                init.push(push_value(Value::Int(
                    Uint256::from_string_hex(
                        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
                    )
                    .unwrap(),
                )));
                init.push(simple_op(AVMOpcode::Panic));
                init.push(mk_label(next_label));
            }
        }
    }

    // Error handling
    init.push(mk_label(1));
    init.push(push_value(Value::Int(
        Uint256::from_string_hex("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
            .unwrap(),
    )));
    init.push(simple_op(AVMOpcode::Panic));

    // Cleaning up
    init.push(mk_func_label(f_count + 1));
}

