use crate::compile::DebugInfo;
use crate::mavm::{
    AVMOpcode, Buffer, CodePt, Instruction, Label, /*LabelGenerator,*/ Opcode, Value,
};
use crate::uint256::Uint256;
use ethers_core::utils::keccak256;
use parity_wasm::elements::Instruction::*;
use parity_wasm::elements::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Control {
    target: usize,
    rets: usize,
    level: usize,
    else_label: usize,
    is_ite: bool,
    is_loop: bool,
}

const LEVEL: usize = 5;

fn block_len(bt: &BlockType) -> usize {
    match *bt {
        BlockType::Value(_) => 1,
        BlockType::NoResult => 0,
    }
}

fn count_locals(func: &FuncBody) -> usize {
    func.locals()
        .iter()
        .fold(0, |sum, x| sum + x.count() as usize)
}

fn get_func_type(m: &Module, sig: u32) -> &FunctionType {
    match m.type_section().unwrap().types()[sig as usize] {
        Type::Function(ref t) => t,
    }
}

fn num_func_returns(ft: &FunctionType) -> usize {
    ft.results().len()
}

fn simple_op(op: AVMOpcode) -> Instruction {
    Instruction::from_opcode(Opcode::AVMOpcode(op), DebugInfo::from(None))
}

fn debug_op(str: String) -> Instruction {
    Instruction::debug(str)
}

fn immed_op(op: AVMOpcode, v: Value) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(op), v, DebugInfo::from(None))
}

fn mk_label(idx: usize) -> Instruction {
    Instruction::from_opcode(Opcode::Label(Label::Evm(idx)), DebugInfo::from(None))
}

fn mk_func_label(idx: usize) -> Instruction {
    Instruction::from_opcode(Opcode::Label(Label::WasmFunc(idx)), DebugInfo::from(None))
}

fn get_from_table(res: &mut Vec<Instruction>, idx: Value) {
    res.push(simple_op(AVMOpcode::Rget));
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
    res.push(simple_op(AVMOpcode::Rget));
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
    Instruction::from_opcode_imm(
        Opcode::AVMOpcode(AVMOpcode::Xget),
        Value::Int(Uint256::from_usize(0)),
        DebugInfo::from(None),
    )
}

fn get_return_pc() -> Instruction {
    Instruction::from_opcode_imm(
        Opcode::AVMOpcode(AVMOpcode::Xget),
        Value::Int(Uint256::from_usize(1)),
        DebugInfo::from(None),
    )
}

fn push_frame(v: Value) -> Instruction {
    Instruction::from_opcode_imm(
        Opcode::AVMOpcode(AVMOpcode::AuxPush),
        v,
        DebugInfo::from(None),
    )
}

fn push_value(v: Value) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Noop), v, DebugInfo::from(None))
}

fn set_frame() -> Instruction {
    Instruction::from_opcode_imm(
        Opcode::AVMOpcode(AVMOpcode::Xset),
        Value::Int(Uint256::from_usize(0)),
        DebugInfo::from(None),
    )
}

fn get64_from_buffer(loc: usize) -> Instruction {
    Instruction::from_opcode_imm(
        Opcode::AVMOpcode(AVMOpcode::GetBuffer64),
        Value::Int(Uint256::from_usize(loc * 8)),
        DebugInfo::from(None),
    )
}

fn set64_from_buffer(loc: usize) -> Instruction {
    Instruction::from_opcode_imm(
        Opcode::AVMOpcode(AVMOpcode::SetBuffer64),
        Value::Int(Uint256::from_usize(loc * 8)),
        DebugInfo::from(None),
    )
}

fn adjust_stack(res: &mut Vec<Instruction>, diff: usize, num: usize) {
    res.push(debug_op(format!("adjust remove {} save {}", diff, num)));
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
    res.push(simple_op(AVMOpcode::Rget));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(0)));
}

fn set_memory(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rget));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(0)));
    res.push(simple_op(AVMOpcode::Rset)); // value, address
}

fn get_buffer(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rget));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(2)));
}

fn set_buffer(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rget));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(2)));
    res.push(simple_op(AVMOpcode::Rset));
}

fn get_buffer_len(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rget));
    res.push(debug_op("Getting buffer length".to_string()));
    res.push(immed_op(AVMOpcode::Tget, int_from_usize(3)));
}

fn set_buffer_len(res: &mut Vec<Instruction>) {
    res.push(simple_op(AVMOpcode::Rget));
    res.push(debug_op("Setting buffer length".to_string()));
    res.push(immed_op(AVMOpcode::Tset, int_from_usize(3)));
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
        AVMOpcode::Plus,
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
        AVMOpcode::Plus,
        Value::Int(Uint256::from_usize(offset)),
    )); // address, value, buffer, value, address
    res.push(simple_op(AVMOpcode::SetBuffer8)); // buffer, value, address
    set_memory(res);
}

fn generate_store(res: &mut Vec<Instruction>, offset: u32, memory_offset: usize, num: usize) {
    res.push(simple_op(AVMOpcode::Dup1));
    res.push(immed_op(
        AVMOpcode::Plus,
        Value::Int(Uint256::from_usize(offset as usize + num - 1)),
    )); // address, value, buffer, value, address
    get_memory(res);
    res.push(get64_from_buffer(0));
    res.push(immed_op(
        AVMOpcode::Mul,
        Value::Int(Uint256::from_usize(1 << 16)),
    ));
    res.push(simple_op(AVMOpcode::LessThan));
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
        AVMOpcode::Plus,
        Value::Int(Uint256::from_usize(offset as usize + num - 1)),
    )); // address, value, buffer, value, address
    get_memory(res);
    res.push(get64_from_buffer(0));
    res.push(immed_op(
        AVMOpcode::Mul,
        Value::Int(Uint256::from_usize(1 << 16)),
    ));
    res.push(simple_op(AVMOpcode::LessThan));
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
        Value::Int(Uint256::from_usize(0xffffffffffffffff)),
    ));
}

fn op64_swap(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffffffffffff)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffffffffffff)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffffffffffff)),
    ));
}

fn op64_unary(res: &mut Vec<Instruction>, op: AVMOpcode) {
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffffffffffff)),
    ));
    res.push(simple_op(op));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(0xffffffffffffffff)),
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
    let m1 = 0x5555555555555555;
    let m2 = 0x3333333333333333;
    let m4 = 0x0F0F0F0F0F0F0F0F;
    let h01 = 0x0101010101010101;

    // x -= (x >> 1) & m1;
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(1)),
    ));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(m1)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(simple_op(AVMOpcode::Minus));
    // x = (x & m2) + ((x >> 2) & m2);
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(2)),
    ));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(m2)),
    ));
    res.push(simple_op(AVMOpcode::Swap1));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(m2)),
    ));
    res.push(simple_op(AVMOpcode::Plus));
    // x = (x + (x >> 4)) & m4;
    res.push(simple_op(AVMOpcode::Dup0));
    res.push(immed_op(
        AVMOpcode::ShiftRight,
        Value::Int(Uint256::from_usize(4)),
    ));
    res.push(simple_op(AVMOpcode::Plus));
    res.push(immed_op(
        AVMOpcode::BitwiseAnd,
        Value::Int(Uint256::from_usize(m4)),
    ));
    // x = (x * h01) >> 56;
    res.push(immed_op(
        AVMOpcode::Mul,
        Value::Int(Uint256::from_usize(h01)),
    ));
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
        res.push(simple_op(AVMOpcode::Plus)); // inc -- count, value
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
        res.push(simple_op(AVMOpcode::Plus)); // inc -- count, value
    }
    res.push(simple_op(AVMOpcode::AuxPop)); // inc, count, value
    res.push(simple_op(AVMOpcode::Pop)); // count, value
    res.push(simple_op(AVMOpcode::Swap1)); // value, count
    res.push(simple_op(AVMOpcode::Pop)); // count
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

fn get_avm_gas(insn: &Instruction) -> usize {
    match insn.opcode {
        Opcode::AVMOpcode(AVMOpcode::Plus) => 3,
        Opcode::AVMOpcode(AVMOpcode::Mul) => 3,
        Opcode::AVMOpcode(AVMOpcode::Minus) => 3,
        Opcode::AVMOpcode(AVMOpcode::Div) => 4,
        Opcode::AVMOpcode(AVMOpcode::Sdiv) => 7,
        Opcode::AVMOpcode(AVMOpcode::Mod) => 4,
        Opcode::AVMOpcode(AVMOpcode::Smod) => 7,
        Opcode::AVMOpcode(AVMOpcode::AddMod) => 4,
        Opcode::AVMOpcode(AVMOpcode::MulMod) => 4,
        Opcode::AVMOpcode(AVMOpcode::Exp) => 25,
        Opcode::AVMOpcode(AVMOpcode::SignExtend) => 7,
        Opcode::AVMOpcode(AVMOpcode::LessThan) => 2,
        Opcode::AVMOpcode(AVMOpcode::GreaterThan) => 2,
        Opcode::AVMOpcode(AVMOpcode::SLessThan) => 2,
        Opcode::AVMOpcode(AVMOpcode::SGreaterThan) => 2,
        Opcode::AVMOpcode(AVMOpcode::Equal) => 2,
        Opcode::AVMOpcode(AVMOpcode::IsZero) => 1,
        Opcode::AVMOpcode(AVMOpcode::BitwiseAnd) => 2,
        Opcode::AVMOpcode(AVMOpcode::BitwiseOr) => 2,
        Opcode::AVMOpcode(AVMOpcode::BitwiseXor) => 2,
        Opcode::AVMOpcode(AVMOpcode::BitwiseNeg) => 1,
        Opcode::AVMOpcode(AVMOpcode::Byte) => 4,
        Opcode::AVMOpcode(AVMOpcode::ShiftLeft) => 4,
        Opcode::AVMOpcode(AVMOpcode::ShiftRight) => 4,
        Opcode::AVMOpcode(AVMOpcode::ShiftArith) => 4,
        Opcode::AVMOpcode(AVMOpcode::Hash) => 7,
        Opcode::AVMOpcode(AVMOpcode::Type) => 3,
        Opcode::AVMOpcode(AVMOpcode::Hash2) => 8,
        Opcode::AVMOpcode(AVMOpcode::Keccakf) => 600,
        Opcode::AVMOpcode(AVMOpcode::Sha256f) => 250,
        Opcode::AVMOpcode(AVMOpcode::Ripemd160f) => 250, //TODO: measure and update this
        Opcode::AVMOpcode(AVMOpcode::Pop) => 1,
        Opcode::AVMOpcode(AVMOpcode::PushStatic) => 1,
        Opcode::AVMOpcode(AVMOpcode::Rget) => 1,
        Opcode::AVMOpcode(AVMOpcode::Rset) => 2,
        Opcode::AVMOpcode(AVMOpcode::Jump) => 4,
        Opcode::AVMOpcode(AVMOpcode::Cjump) => 4,
        Opcode::AVMOpcode(AVMOpcode::StackEmpty) => 2,
        Opcode::AVMOpcode(AVMOpcode::GetPC) => 1,
        Opcode::AVMOpcode(AVMOpcode::AuxPush) => 1,
        Opcode::AVMOpcode(AVMOpcode::AuxPop) => 1,
        Opcode::AVMOpcode(AVMOpcode::AuxStackEmpty) => 2,
        Opcode::AVMOpcode(AVMOpcode::Noop) => 1,
        Opcode::AVMOpcode(AVMOpcode::ErrPush) => 1,
        Opcode::AVMOpcode(AVMOpcode::ErrSet) => 1,
        Opcode::AVMOpcode(AVMOpcode::Dup0) => 1,
        Opcode::AVMOpcode(AVMOpcode::Dup1) => 1,
        Opcode::AVMOpcode(AVMOpcode::Dup2) => 1,
        Opcode::AVMOpcode(AVMOpcode::Swap1) => 1,
        Opcode::AVMOpcode(AVMOpcode::Swap2) => 1,
        Opcode::AVMOpcode(AVMOpcode::Tget) => 2,
        Opcode::AVMOpcode(AVMOpcode::Tset) => 40,
        Opcode::AVMOpcode(AVMOpcode::Tlen) => 2,
        Opcode::AVMOpcode(AVMOpcode::Xget) => 3,
        Opcode::AVMOpcode(AVMOpcode::Xset) => 41,
        Opcode::AVMOpcode(AVMOpcode::Breakpoint) => 100,
        Opcode::AVMOpcode(AVMOpcode::Log) => 100,
        Opcode::AVMOpcode(AVMOpcode::Send) => 100,
        Opcode::AVMOpcode(AVMOpcode::InboxPeek) => 40,
        Opcode::AVMOpcode(AVMOpcode::Inbox) => 40,
        Opcode::AVMOpcode(AVMOpcode::Panic) => 5,
        Opcode::AVMOpcode(AVMOpcode::Halt) => 10,
        Opcode::AVMOpcode(AVMOpcode::ErrCodePoint) => 25,
        Opcode::AVMOpcode(AVMOpcode::PushInsn) => 25,
        Opcode::AVMOpcode(AVMOpcode::PushInsnImm) => 25,
        Opcode::AVMOpcode(AVMOpcode::OpenInsn) => 25,
        Opcode::AVMOpcode(AVMOpcode::DebugPrint) => 1,
        Opcode::AVMOpcode(AVMOpcode::GetGas) => 1,
        Opcode::AVMOpcode(AVMOpcode::SetGas) => 0,
        Opcode::AVMOpcode(AVMOpcode::EcRecover) => 20_000,
        Opcode::AVMOpcode(AVMOpcode::EcAdd) => 3500,
        Opcode::AVMOpcode(AVMOpcode::EcMul) => 82_000,
        // Opcode::AVMOpcode(AVMOpcode::EcPairing) => self.gas_for_pairing(),
        Opcode::AVMOpcode(AVMOpcode::Sideload) => 10,
        Opcode::AVMOpcode(AVMOpcode::NewBuffer) => 1,
        Opcode::AVMOpcode(AVMOpcode::GetBuffer8) => 10,
        Opcode::AVMOpcode(AVMOpcode::GetBuffer64) => 10,
        Opcode::AVMOpcode(AVMOpcode::GetBuffer256) => 10,
        Opcode::AVMOpcode(AVMOpcode::SetBuffer8) => 100,
        Opcode::AVMOpcode(AVMOpcode::SetBuffer64) => 100,
        Opcode::AVMOpcode(AVMOpcode::SetBuffer256) => 100,
        _ => 0,
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
    m: &Module,
    func: &FuncBody,
    idx: usize,
    mut label: usize,
    calli: usize,
    memory_offset: usize,
    max_memory: usize,
) -> (Vec<Instruction>, usize, Vec<usize>) {
    let sig = m.function_section().unwrap().entries()[idx].type_ref();
    let ftype = get_func_type(m, sig);

    println!("func start label {}", label);

    let mut res: Vec<Instruction> = Vec::new();
    let mut stack: Vec<Control> = Vec::new();
    let mut ptr: usize = 0;
    let mut bptr: usize = 0;

    let mut avm_gas: Vec<usize> = Vec::new();

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

    eprintln!(
        "Got function with {:?} ops, {:?} locals, {} params, {} rets",
        func.code().elements().len(),
        count_locals(func),
        ftype.params().len(),
        rets
    );

    for (idx_inf, op) in func.code().elements().iter().enumerate() {
        eprintln!(
            "handling ptr {} frames {}; {:?} ... label {} len {}",
            ptr,
            stack.len(),
            op,
            label,
            res.len()
        );
        let cur_len = res.len();
        res.push(debug_op(format!("{:?} level {} func {} idx {}", *op, ptr, idx, idx_inf)));
        match &*op {
            Nop => res.push(simple_op(AVMOpcode::Noop)),
            Unreachable => {
                res.push(simple_op(AVMOpcode::Panic));
                if stack.len() == 0 {
                    break;
                }
                let c: &Control = &stack[stack.len()-1];
                ptr = c.level;
            },
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
                eprintln!(
                    "Level if {} rets {} end label {} else label {}",
                    ptr, rets, end_label, else_label
                );
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
                cjump(&mut res, else_label);
            }
            Else => {
                let mut c: Control = stack.pop().unwrap();
                eprintln!(
                    "Level else {} end label {} else label {} rets {}",
                    c.level, c.target, c.else_label, c.rets
                );
                ptr = c.level - c.rets;
                jump(&mut res, c.target);
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
                    eprintln!("End block mismatch {} != {} rets {}", ptr, c.level, c.rets);
                    panic!("End block mismatch {}");
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
                eprintln!("Debug br {:?} {}", c, c.level);
                adjust_stack(&mut res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                jump(&mut res, c.target);
            }
            BrIf(x) => {
                let c = &stack[stack.len() - (*x as usize) - 1];
                res.push(debug_op(format!("Debug brif {:?} ptr {} next level: {} + {}", c, ptr, c.level, c.rets)));
                eprintln!("Debug brif {:?} ptr {} next level: {} + {}", c, ptr, c.level, c.rets);
                let continue_label = label;
                let end_label = label + 1;
                label = label + 2;
                cjump(&mut res, continue_label);
                jump(&mut res, end_label);
                res.push(mk_label(continue_label));
                adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                jump(&mut res, c.target);
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
                res.push(push_value(Value::Int(Uint256::from_usize(*x as usize))));
                ptr = ptr + 1;
            }
            I64Const(x) => {
                res.push(push_value(Value::Int(Uint256::from_usize(*x as usize))));
                ptr = ptr + 1;
            }
            // Just keep the expression stack
            Call(x) => {
                let ftype = find_func_type(m, *x);
                println!(
                    "calling {} with type {:?} return label {}",
                    x,
                    ftype,
                    label + 1
                );
                let return_label = label;
                label = label + 1;
                // push new frame to aux stack
                res.push(push_frame(Value::new_tuple(vec![
                    Value::new_buffer(vec![]),
                    Value::Label(Label::Evm(return_label)),
                ])));
                // Push args to frame
                for i in 0..ftype.params().len() {
                    res.push(get_frame());
                    res.push(simple_op(AVMOpcode::Swap1));
                    res.push(set64_from_buffer(ftype.params().len() - 1 - i));
                    res.push(set_frame());
                }
                call_jump(&mut res, *x);
                res.push(mk_label(return_label));
                // Pop stack frame
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Pop));
                ptr = ptr - ftype.params().len() + ftype.results().len();
            }
            CallIndirect(x, _) => {
                let ftype = get_func_type(m, *x);
                println!(
                    "call indirect {} with type {:?} return label {} hash {}",
                    x,
                    ftype,
                    label + 1,
                    hash_ftype(&ftype)
                );
                let return_label = label;
                label = label + 1;
                // Save func ptr
                res.push(simple_op(AVMOpcode::AuxPush));
                // push new frame to aux stack
                res.push(push_frame(Value::new_tuple(vec![
                    Value::new_buffer(vec![]),
                    Value::Label(Label::Evm(return_label)),
                ])));
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
                call_jump(&mut res, calli as u32);
                res.push(mk_label(return_label));
                // Pop stack frame
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Pop));
                ptr = ptr - ftype.params().len() + num_func_returns(ftype) - 1;
            }
            Return => {
                let c = &stack[0];
                println!("return {} level {} rets {}", ptr, c.level, c.rets);
                adjust_stack(&mut res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                jump(&mut res, c.target);
            }
            Select => {
                let else_label = label;
                cjump(&mut res, else_label);
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
                    res.push(simple_op(AVMOpcode::Dup0));
                    res.push(immed_op(
                        AVMOpcode::Equal,
                        Value::Int(Uint256::from_usize(i)),
                    ));
                    res.push(simple_op(AVMOpcode::IsZero));
                    cjump(&mut res, label + i);
                    res.push(simple_op(AVMOpcode::Pop));
                    adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                    jump(&mut res, c.target);
                    res.push(mk_label(label + i));
                }
                let c = &stack[stack.len() - (def as usize) - 1];
                res.push(simple_op(AVMOpcode::Pop));
                adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                jump(&mut res, c.target);

                ptr = ptr - 1 - c.rets;
                label = label + len + 2;
            }

            GetGlobal(x) => {
                ptr = ptr + 1;
                get_memory(&mut res);
                res.push(get64_from_buffer((*x + 1) as usize));
            }
            SetGlobal(x) => {
                ptr = ptr - 1;
                get_memory(&mut res);
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer((*x + 1) as usize));
                set_memory(&mut res);
            }

            I64Store(_, offset) => {
                ptr = ptr - 2;
                generate_store(&mut res, *offset, memory_offset, 8);
            }

            I32Store(_, offset) | I64Store32(_, offset) => {
                ptr = ptr - 2;
                generate_store(&mut res, *offset, memory_offset, 4);
            }

            I32Store16(_, offset) | I64Store16(_, offset) => {
                ptr = ptr - 2;
                generate_store(&mut res, *offset, memory_offset, 2);
            }

            I32Store8(_, offset) | I64Store8(_, offset) => {
                ptr = ptr - 2;
                generate_store(&mut res, *offset, memory_offset, 1);
            }

            I64Load(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 8);
            }

            I32Load(_, offset) | I64Load32U(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 4);
            }

            I32Load16U(_, offset) | I64Load16U(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 2);
            }

            I32Load8U(_, offset) | I64Load8U(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 1);
            }

            I64Load32S(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 4);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(31)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_usize(0xfffffff00000000)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
            }

            I64Load16S(_, offset) | I32Load16S(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 2);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(15)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_usize(0xfffffffffff0000)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
            }

            I64Load8S(_, offset) | I32Load8S(_, offset) => {
                generate_load(&mut res, *offset, memory_offset, 1);
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::ShiftRight,
                    Value::Int(Uint256::from_usize(7)),
                ));
                res.push(immed_op(
                    AVMOpcode::Mul,
                    Value::Int(Uint256::from_usize(0xfffffffffffff00)),
                ));
                res.push(simple_op(AVMOpcode::BitwiseOr));
            }

            I32Add => {
                op32_swap(&mut res, AVMOpcode::Plus);
                ptr = ptr - 1;
            }
            I32Sub => {
                signed_op32_swap(&mut res, AVMOpcode::Minus);
                ptr = ptr - 1;
            }
            I32Eq => {
                op32_swap(&mut res, AVMOpcode::Equal);
                ptr = ptr - 1;
            }
            I32GtU => {
                op32_swap(&mut res, AVMOpcode::GreaterThan);
                ptr = ptr - 1;
            }
            I32GtS => {
                signed_op32_swap(&mut res, AVMOpcode::SGreaterThan);
                ptr = ptr - 1;
            }
            I32LtU => {
                op32_swap(&mut res, AVMOpcode::LessThan);
                ptr = ptr - 1;
            }
            I32LtS => {
                signed_op32_swap(&mut res, AVMOpcode::SLessThan);
                ptr = ptr - 1;
            }
            I32GeU => {
                op32_swap(&mut res, AVMOpcode::LessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32GeS => {
                signed_op32_swap(&mut res, AVMOpcode::SLessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32Ne => {
                op32_swap(&mut res, AVMOpcode::Equal);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32LeU => {
                op32_swap(&mut res, AVMOpcode::GreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32LeS => {
                signed_op32_swap(&mut res, AVMOpcode::SGreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I32Eqz => {
                op32_unary(&mut res, AVMOpcode::IsZero);
            }

            I32Mul => {
                op32_swap(&mut res, AVMOpcode::Mul);
                ptr = ptr - 1;
            }
            I32DivU => {
                op32_swap(&mut res, AVMOpcode::Div);
                ptr = ptr - 1;
            }
            I32DivS => {
                signed_op32_swap(&mut res, AVMOpcode::Sdiv);
                ptr = ptr - 1;
            }
            I32RemU => {
                op32_swap(&mut res, AVMOpcode::Mod);
                ptr = ptr - 1;
            }
            I32RemS => {
                signed_op32_swap(&mut res, AVMOpcode::Smod);
                ptr = ptr - 1;
            }
            I32And => {
                op32_swap(&mut res, AVMOpcode::BitwiseAnd);
                ptr = ptr - 1;
            }
            I32Or => {
                op32_swap(&mut res, AVMOpcode::BitwiseOr);
                ptr = ptr - 1;
            }
            I32Xor => {
                op32_swap(&mut res, AVMOpcode::BitwiseXor);
                ptr = ptr - 1;
            }
            I32Shl => {
                op32(&mut res, AVMOpcode::ShiftLeft);
                ptr = ptr - 1;
            }
            I32ShrU => {
                op32(&mut res, AVMOpcode::ShiftRight);
                ptr = ptr - 1;
            }
            I32ShrS => {
                op32(&mut res, AVMOpcode::ShiftArith);
                ptr = ptr - 1;
            }

            I32Rotl => {
                make_rotl(&mut res, 32);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                ptr = ptr - 1;
            }
            I32Rotr => {
                make_rotr(&mut res, 32);
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
                make_popcnt(&mut res);
            }
            I32Clz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                make_clz(&mut res, 32);
            }
            I32Ctz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffff)),
                ));
                make_ctz(&mut res, 32);
            }

            I64Add => {
                op64_swap(&mut res, AVMOpcode::Plus);
                ptr = ptr - 1;
            }
            I64Sub => {
                signed_op64_swap(&mut res, AVMOpcode::Minus);
                ptr = ptr - 1;
            }
            I64Eq => {
                op64_swap(&mut res, AVMOpcode::Equal);
                ptr = ptr - 1;
            }
            I64GtU => {
                op64_swap(&mut res, AVMOpcode::GreaterThan);
                ptr = ptr - 1;
            }
            I64GtS => {
                signed_op64_swap(&mut res, AVMOpcode::SGreaterThan);
                ptr = ptr - 1;
            }
            I64LtU => {
                op64_swap(&mut res, AVMOpcode::LessThan);
                ptr = ptr - 1;
            }
            I64LtS => {
                signed_op64_swap(&mut res, AVMOpcode::SLessThan);
                ptr = ptr - 1;
            }
            I64GeU => {
                op64_swap(&mut res, AVMOpcode::LessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64GeS => {
                signed_op64_swap(&mut res, AVMOpcode::SLessThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64Ne => {
                op64_swap(&mut res, AVMOpcode::Equal);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64LeU => {
                op64_swap(&mut res, AVMOpcode::GreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64LeS => {
                signed_op64_swap(&mut res, AVMOpcode::SGreaterThan);
                res.push(simple_op(AVMOpcode::IsZero));
                ptr = ptr - 1;
            }
            I64Eqz => {
                op64_unary(&mut res, AVMOpcode::IsZero);
            }

            I64Mul => {
                op64_swap(&mut res, AVMOpcode::Mul);
                ptr = ptr - 1;
            }
            I64DivU => {
                op64_swap(&mut res, AVMOpcode::Div);
                ptr = ptr - 1;
            }
            I64DivS => {
                signed_op64_swap(&mut res, AVMOpcode::Sdiv);
                ptr = ptr - 1;
            }
            I64RemU => {
                op64_swap(&mut res, AVMOpcode::Mod);
                ptr = ptr - 1;
            }
            I64RemS => {
                signed_op64_swap(&mut res, AVMOpcode::Smod);
                ptr = ptr - 1;
            }
            I64And => {
                op64_swap(&mut res, AVMOpcode::BitwiseAnd);
                ptr = ptr - 1;
            }
            I64Or => {
                op64_swap(&mut res, AVMOpcode::BitwiseOr);
                ptr = ptr - 1;
            }
            I64Xor => {
                op64_swap(&mut res, AVMOpcode::BitwiseXor);
                ptr = ptr - 1;
            }
            I64Shl => {
                op64(&mut res, AVMOpcode::ShiftLeft);
                ptr = ptr - 1;
            }
            I64ShrU => {
                op64(&mut res, AVMOpcode::ShiftRight);
                ptr = ptr - 1;
            }
            I64ShrS => {
                op64(&mut res, AVMOpcode::ShiftArith);
                ptr = ptr - 1;
            }

            I64Rotl => {
                make_rotl(&mut res, 64);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffffffffffff)),
                ));
                ptr = ptr - 1;
            }
            I64Rotr => {
                make_rotr(&mut res, 64);
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffffffffffff)),
                ));
                ptr = ptr - 1;
            }

            I64Popcnt => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffffffffffff)),
                ));
                make_popcnt(&mut res);
            }
            I64Clz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffffffffffff)),
                ));
                make_clz(&mut res, 64);
            }
            I64Ctz => {
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffffffffffff)),
                ));
                make_ctz(&mut res, 64);
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
                    Value::Int(Uint256::from_usize(7)),
                ));
                res.push(immed_op(
                    AVMOpcode::BitwiseAnd,
                    Value::Int(Uint256::from_usize(0xffffffffffffffff)),
                ));
            }

            CurrentMemory(_) => {
                ptr = ptr + 1;
                get_memory(&mut res);
                res.push(get64_from_buffer(0));
            }

            GrowMemory(_) => {
                let end_label = label;
                let ok_label = label + 1;
                get_memory(&mut res);
                res.push(get64_from_buffer(0));
                res.push(simple_op(AVMOpcode::Dup0)); // old value to return (except need to handle error)
                res.push(simple_op(AVMOpcode::Swap2));
                res.push(simple_op(AVMOpcode::Plus));
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(immed_op(
                    AVMOpcode::GreaterThan,
                    Value::Int(Uint256::from_usize(max_memory)),
                ));
                cjump(&mut res, ok_label);
                res.push(simple_op(AVMOpcode::Pop));
                res.push(simple_op(AVMOpcode::Pop));
                res.push(push_value(Value::Int(Uint256::from_usize(0xffffffff)))); // -1 when error
                res.push(mk_label(ok_label));
                get_memory(&mut res);
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer(0));
                set_memory(&mut res);
                res.push(mk_label(end_label));
                label = label + 2;
            }
            i => {
                panic!("Unknown opcode {:?}", i);
            }
        }

        let mut gas_acc = 0;
        for i in cur_len..res.len() {
            gas_acc += get_avm_gas(&res[i])
        }
        avm_gas.push(gas_acc);
    }

    // Function return
    res.push(mk_label(end_label));
    res.push(get_return_pc());
    get_return_from_table(&mut res);
    res.push(simple_op(AVMOpcode::Jump));

    return (res, label, avm_gas);
}

pub fn clear_labels(arr: Vec<Instruction>) -> Vec<Instruction> {
    let mut res = vec![];
    for inst in arr.iter() {
        match inst.opcode {
            Opcode::Label(_) => res.push(simple_op(AVMOpcode::Noop)),
            _ => res.push(inst.clone()),
        }
    }
    res
}

fn table_to_tuple(tab: &[usize], prefix: usize, shift: usize, level: usize) -> Value {
    if level == 0 {
        let mut v = vec![];
        for i in 0..8 {
            let idx = prefix + (i << shift);
            let ptr = if idx < tab.len() { tab[idx] } else { 0 };
            // We are adding one instruction to the beginning
            v.push(Value::CodePoint(CodePt::Internal(ptr)));
        }
        return Value::new_tuple(v);
    }
    let mut v = vec![];
    for i in 0..8 {
        let prefix = prefix + (i << shift);
        v.push(table_to_tuple(tab, prefix, shift + 3, level - 1));
    }
    return Value::new_tuple(v);
}

fn table_to_tuple2(tab: &[Value], prefix: usize, shift: usize, level: usize) -> Value {
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
        v.push(table_to_tuple2(tab, prefix, shift + 3, level - 1));
    }
    return Value::new_tuple(v);
}

pub fn make_table(tab: &[Value]) -> Value {
    table_to_tuple2(tab, 0, 0, LEVEL - 1)
}

fn value_replace_labels(v: Value, label_map: &HashMap<Label, Value>) -> Result<Value, Label> {
    match v {
        Value::Int(_) => Ok(v),
        Value::CodePoint(_) => Ok(v),
        Value::Buffer(_) => Ok(v),
        Value::Label(label) => {
            let maybe_pc = label_map.get(&label);
            match maybe_pc {
                Some(pc) => Ok(pc.clone()),
                None => Err(label),
            }
        }
        Value::WasmCodePoint(val, code) => Ok(Value::WasmCodePoint(
            Box::new(value_replace_labels(*val, label_map)?),
            code.clone(),
        )),
        Value::Tuple(tup) => {
            let mut new_vec = Vec::new();
            for v in tup.iter() {
                let val = v.clone();
                new_vec.push(value_replace_labels(val, label_map)?);
            }
            Ok(Value::new_tuple(new_vec))
        }
    }
}

fn inst_replace_labels(
    inst: Instruction,
    label_map: &HashMap<Label, Value>,
) -> Result<Instruction, Label> {
    match inst.immediate {
        Some(val) => Ok(Instruction::from_opcode_imm(
            inst.opcode,
            value_replace_labels(val, label_map)?,
            inst.debug_info,
        )),
        None => Ok(inst),
    }
}

pub fn has_label(inst: &Instruction) -> bool {
    match inst.opcode {
        Opcode::Label(Label::Evm(_)) => true,
        Opcode::Label(Label::WasmFunc(_)) => true,
        _ => false,
    }
}

pub fn has_immed(inst: &Instruction) -> bool {
    match inst.immediate {
        Some(_) => true,
        None => false,
    }
}

pub fn get_immed(inst: &Instruction) -> u64 {
    match &inst.immediate {
        Some(Value::Int(a)) => a.trim_to_u64(),
        _ => 0,
    }
}

pub fn get_inst(inst: &Instruction) -> u8 {
    match inst.opcode {
        Opcode::AVMOpcode(op) => op as u8,
        _ => 0,
    }
}

pub fn resolve_labels(arr: Vec<Instruction>) -> (Vec<Instruction>, Value) {
    let mut labels = HashMap::new();
    let mut tab = vec![];
    for (idx, inst) in arr.iter().enumerate() {
        match inst.opcode {
            Opcode::Label(Label::Evm(num)) => {
                tab.push(idx);
                println!("Found label {} -> {}", num, int_from_usize(tab.len() - 1));
                labels.insert(Label::Evm(num), int_from_usize(tab.len() - 1));
            }
            Opcode::Label(Label::WasmFunc(num)) => {
                tab.push(idx);
                println!(
                    "Found func label {} -> {}",
                    num,
                    int_from_usize(tab.len() - 1)
                );
                labels.insert(Label::WasmFunc(num), int_from_usize(tab.len() - 1));
            }
            _ => {}
        }
    }
    let mut res = vec![];
    for inst in arr.iter() {
        // handle error
        res.push(inst_replace_labels(inst.clone(), &labels).unwrap());
    }
    println!("Labels {}", tab.len());
    (res, table_to_tuple(&tab, 0, 0, LEVEL - 1))
}

fn init_value(_m: &Module, expr: &InitExpr) -> usize {
    eprintln!("init {:?}", expr);
    match expr.code()[0] {
        I32Const(a) => a as usize,
        F32Const(a) => a as usize,
        I64Const(a) => a as usize,
        F64Const(a) => a as usize,
        _ => 0,
    }
}

fn int_from_usize(a: usize) -> Value {
    Value::Int(Uint256::from_usize(a))
}

fn find_function(m: &Module, name: &str) -> Option<u32> {
    match m.export_section() {
        None => None,
        Some(sec) => {
            for e in sec.entries() {
                println!("Export {}: {:?}", e.field(), e.internal());
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

pub fn get_answer64(answer: wasmtime::Func) -> i32 {
    let answer = answer.get0::<i32>().unwrap();

    let result = answer().unwrap();
    result as i32
}

pub fn get_answer32(answer: wasmtime::Func, param: i64) -> i32 {
    let answer = answer.get1::<i64, i32>().unwrap();

    let result = answer(param as i64).unwrap();
    result
}

/*
pub fn run_jit(buffer: &[u8], data: &[u8]) -> i64 {
    use std::cell::RefCell;
    use std::rc::Rc;
    use wasmtime::*;
    let engine = Engine::default();
    let store = Store::new(&engine);

    let module = Module::from_binary(&engine, &buffer).unwrap();

    let buf = Buffer::new(data.to_vec());

    let cell = Rc::new(RefCell::new(buf));
    let cell2 = cell.clone();

    let len = Rc::new(RefCell::new(4));
    let len2 = len.clone();

    //    cell2.replace_with(|buf| buf.set_byte(2, 111));

    let read_func = Func::wrap(&store, move |offset: i32, a: i32| {
        let ret = cell.borrow().read_byte(offset as usize) as i32;
        // println!("read {} {} {}", a, offset, ret);
        ret
    });

    let write_func = Func::wrap(&store, move |offset: i32, a: i32, v: i32| {
        // println!("write {} {} {}", offset, v, a);
        cell2.replace_with(|buf| buf.set_byte(offset as usize, v as u8));
    });

    let len_func = Func::wrap(&store, move || len.borrow().clone() as i32);

    let set_len_func = Func::wrap(&store, move |nlen: i32| len2.replace_with(|_| nlen));

    let error_func = Func::wrap(&store, || {
        panic!("Unknown import");
    });

    let mut imports = vec![];

    for f in module.imports() {
        match (f.ty(), f.name()) {
            (ExternType::Func(_), Some(name)) => {
                if name.contains("read") {
                    imports.push(read_func.clone().into())
                } else if name.contains("write") {
                    imports.push(write_func.clone().into())
                } else if name.contains("len") {
                    imports.push(len_func.clone().into())
                } else if name.contains("set") {
                    imports.push(set_len_func.clone().into())
                } else {
                    imports.push(error_func.clone().into())
                }
            }
            (ExternType::Func(_), None) => imports.push(error_func.clone().into()),
            _ => {}
        }
    }

    let instance = Instance::new(&store, &module, &imports).unwrap();

    let res = match instance.get_func("test") {
        Some(f) => get_answer64(f) as i64,
        None => 0,
    };
    /*
    match instance.get_func("main") {
        Some(f) => return get_answer32(f, param) as i64,
        None => {},
    };*/
    return res;
}
*/

pub struct JitWasm {
    instance: wasmtime::Instance,
    cell: std::rc::Rc<std::cell::RefCell<Buffer>>,
    len_cell: std::rc::Rc<std::cell::RefCell<i32>>,
}

use std::fmt;

impl fmt::Debug for JitWasm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JitWasm")
    }
}

impl JitWasm {
    pub fn new(buffer: &[u8]) -> Self {
        use std::cell::RefCell;
        use std::rc::Rc;
        use wasmtime::*;
        let engine = Engine::default();
        let store = Store::new(&engine);

        let module = Module::from_binary(&engine, &buffer).unwrap();

        let buf = Buffer::new(vec![]);

        let cell = Rc::new(RefCell::new(buf));
        let cell1 = cell.clone();
        let cell2 = cell.clone();

        let len_cell = Rc::new(RefCell::new(4));
        let len1 = len_cell.clone();
        let len2 = len_cell.clone();

        let read_func = Func::wrap(&store, move |offset: i32| {
            let ret = cell1.borrow().read_byte(offset as usize) as i32;
            ret
        });

        let write_func = Func::wrap(&store, move |offset: i32, v: i32| {
            cell2.replace_with(|buf| buf.set_byte(offset as usize, v as u8));
        });

        let len_func = Func::wrap(&store, move || len1.borrow().clone() as i32);

        let set_len_func = Func::wrap(&store, move |nlen: i32| { len2.replace_with(|_| nlen); });

        let error_func = Func::wrap(&store, || {
            panic!("Unknown import");
        });

        let mut imports = vec![];

        for f in module.imports() {
            match (f.ty(), f.name()) {
                (ExternType::Func(_), Some(name)) => {
                    if name.contains("read") {
                        imports.push(read_func.clone().into())
                    } else if name.contains("write") {
                        imports.push(write_func.clone().into())
                    } else if name.contains("getlen") {
                        imports.push(len_func.clone().into())
                    } else if name.contains("setlen") {
                        imports.push(set_len_func.clone().into())
                    } else {
                        imports.push(error_func.clone().into())
                    }
                }
                (ExternType::Func(_), None) => imports.push(error_func.clone().into()),
                _ => {}
            }
        }

        let instance = Instance::new(&store, &module, &imports).unwrap();

        return JitWasm {
            instance,
            cell,
            len_cell,
        };
    }

    pub fn run(&self, buf: Buffer, len: usize) -> (Buffer, usize) {
        self.cell.replace_with(|_buf| buf);
        self.len_cell.replace_with(|_len| len as i32);

        let res = match self.instance.get_func("test") {
            Some(f) => get_answer64(f) as i64,
            None => 0,
        };

        (
            self.cell.borrow().clone(),
            self.len_cell.borrow().clone() as usize,
        )
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

pub fn process_wasm(buffer: &[u8]) -> Vec<Instruction> {
    let module = parity_wasm::deserialize_buffer::<Module>(buffer).unwrap();
    assert!(module.code_section().is_some());

    let imports = get_func_imports(&module);

    let code_section = module.code_section().unwrap(); // Part of the module with functions code

    println!(
        "Function count in wasm file: {}, {} imports",
        code_section.bodies().len(),
        imports.len()
    );
    let f_count = code_section.bodies().len() + imports.len();
    let mut init = vec![];
    let max_memory = 1 << 20;

    // These might become replaced
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));
    init.push(simple_op(AVMOpcode::Noop));

    // Save register
    init.push(simple_op(AVMOpcode::Rget));
    init.push(simple_op(AVMOpcode::AuxPush));

    // Initialize register
    // init.push(immed_op(AVMOpcode::Rset, Value::new_tuple(vec![Value::new_buffer(vec![]), int_from_usize(0)])));
    init.push(push_value(Value::new_tuple(vec![
        Value::new_buffer(vec![]),
        int_from_usize(0),
        Value::new_buffer(vec![123, 234, 12]),
        int_from_usize(3),
    ])));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(1)));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(2)));
    init.push(immed_op(AVMOpcode::Tset, int_from_usize(3)));
    init.push(simple_op(AVMOpcode::Rset));

    // Construct initial memory with globals
    init.push(simple_op(AVMOpcode::NewBuffer));
    init.push(push_value(int_from_usize(1024)));
    init.push(set64_from_buffer(0));
    let mut globals = 1;
    if let Some(sec) = module.global_section() {
        for g in sec.entries().iter() {
            init.push(push_value(Value::Int(Uint256::from_usize(init_value(
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
                Some(a) => init_value(&module, a),
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

    set_memory(&mut init);

    // Put initial frame to aux stack
    init.push(push_frame(Value::new_tuple(vec![
        Value::new_buffer(vec![]),
        Value::Label(Label::WasmFunc(f_count + 1)),
    ])));

    // Add test argument to the frame
    init.push(get_frame());
    init.push(push_value(Value::Int(Uint256::from_usize(2))));
    init.push(set64_from_buffer(0));
    init.push(set_frame());

    // Here we should have jump to the correct function
    if let Some(f) = find_function(&module, "test") {
        call_jump(&mut init, f);
    }

    let mut label = 2;
    let calli = f_count;

    for (idx, f) in code_section.bodies().iter().enumerate() {
        // function return will be in the stack
        init.push(mk_func_label(idx + imports.len()));
        let (mut res, n_label, avm_gas) =
            handle_function(&module, f, idx, label, calli, memory_offset, max_memory);
        init.append(&mut res);
        label = n_label;
        println!("Gas {:?}", avm_gas);
    }

    for (idx, f) in get_func_imports(&module).iter().enumerate() {
        init.push(mk_func_label(idx));
        if f.field().contains("read") {
            init.push(debug_op("Read".to_string()));
            // Get buffer
            get_buffer(&mut init);
            // Get param
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(simple_op(AVMOpcode::GetBuffer8));
        }
        if f.field().contains("write") {
            init.push(debug_op("Write".to_string()));
            // Get buffer
            get_buffer(&mut init);
            // Get params
            init.push(get_frame());
            init.push(get64_from_buffer(1));
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            init.push(simple_op(AVMOpcode::SetBuffer8));
            set_buffer(&mut init);
        }
        if f.field().contains("getlen") {
            get_buffer_len(&mut init);
        }
        if f.field().contains("setlen") {
            init.push(get_frame());
            init.push(get64_from_buffer(0));
            set_buffer_len(&mut init);
        }
        // Return from function
        init.push(get_return_pc());
        get_return_from_table(&mut init);
        init.push(simple_op(AVMOpcode::Jump));
    }

    // Error handling
    init.push(mk_label(1));
    init.push(simple_op(AVMOpcode::Panic));
    // Indirect calls
    init.push(mk_func_label(f_count));
    if let Some(sec) = module.elements_section() {
        for seg in sec.entries().iter() {
            let offset = match seg.offset() {
                None => 0,
                Some(init) => init_value(&module, init),
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
                cjump(&mut init, next_label);
                // We will call this function now or fail
                init.push(simple_op(AVMOpcode::Pop));
                init.push(push_value(Value::Int(hash_ftype(&ftype))));
                init.push(simple_op(AVMOpcode::Equal));
                call_cjump(&mut init, *f_idx as u32);
                init.push(simple_op(AVMOpcode::Panic));
                init.push(mk_label(next_label));
            }
        }
    }

    // Cleaning up
    init.push(mk_func_label(f_count + 1));
    init.push(debug_op("Cleaning up".to_string()));
    init.push(simple_op(AVMOpcode::Rget));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(2)));
    init.push(simple_op(AVMOpcode::Rget));
    init.push(immed_op(AVMOpcode::Tget, int_from_usize(3)));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Pop));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Rset));
    init.push(simple_op(AVMOpcode::Noop));

    init
}

use std::fs::File;
use std::io::Write;

pub fn load(buffer: &[u8], param: &[u8]) -> Vec<Instruction> {
    let init = process_wasm(buffer);
    let mut op_buf = vec![];
    let mut immed_buf = vec![];
    let mut has_immed_buf = vec![];
    let mut has_label_buf = vec![];
    for inst in init.iter() {
        op_buf.push(get_inst(inst));
        if has_immed(inst) {
            has_immed_buf.push(1u8)
        } else {
            has_immed_buf.push(0u8)
        }
        if has_label(inst) {
            has_label_buf.push(1u8)
        } else {
            has_label_buf.push(0u8)
        }
        immed_buf.push(get_immed(inst));
    }

    let (res, tab) = resolve_labels(init);
    // println!("Table {}", tab);
    let res = clear_labels(res);
    // println!("Code {}", );
    let str = serde_json::to_string(&res).unwrap();
    let mut file = File::create("foo.json").unwrap();
    file.write_all(&str.as_bytes());
    let mut file2 = File::create("labels.json").unwrap();
    file2.write_all(serde_json::to_string(&has_label_buf).unwrap().as_bytes());

    let mut a = vec![];
    a.push(push_value(int_from_usize(param.len())));
    a.push(push_value(Value::new_buffer(param.to_vec())));
    a.push(push_value(tab));
    for i in 3..res.len() {
        a.push(res[i].clone());
    }
    a
}
