
use parity_wasm::elements::*;
use parity_wasm::elements::Instruction::*;
use crate::mavm::{AVMOpcode, CodePt, Instruction, Label, /*LabelGenerator,*/ Opcode, Value};
use crate::compile::{DebugInfo};
use crate::uint256::Uint256;
use std::collections::HashMap;
use ethers_core::utils::keccak256;

#[derive(Debug, Clone)]
struct Control {
    target : usize,
    rets : usize,
    level : usize,
    else_label : usize,
    is_ite : bool,
    is_loop : bool,
}

fn block_len(bt : &BlockType) -> usize {
    match *bt {
        BlockType::Value(_) => 1,
        BlockType::NoResult => 0,
    }
}

fn count_locals(func : &FuncBody) -> usize {
    func.locals().iter().fold(0, |sum, x| sum + x.count() as usize)
}

fn get_func_type(m : &Module, sig : u32) -> &FunctionType {
    match m.type_section().unwrap().types()[sig as usize] {
        Type::Function(ref t) => t
    }
}

fn num_func_returns(ft : &FunctionType) -> usize {
    ft.results().len()
}

fn simple_op(op : AVMOpcode) -> Instruction {
    Instruction::from_opcode(Opcode::AVMOpcode(op), DebugInfo::from(None))
}

fn immed_op(op : AVMOpcode, v: Value) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(op), v, DebugInfo::from(None))
}

fn mk_label(idx: usize) -> Instruction {
    Instruction::from_opcode(Opcode::Label(Label::Evm(idx)), DebugInfo::from(None))
}

fn mk_func_label(idx: usize) -> Instruction {
    Instruction::from_opcode(Opcode::Label(Label::WasmFunc(idx)), DebugInfo::from(None))
}

fn cjump(idx: usize) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Cjump), Value::Label(Label::Evm(idx)), DebugInfo::from(None))
}

fn jump(idx: usize) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Jump), Value::Label(Label::Evm(idx)), DebugInfo::from(None))
}

fn call_jump(idx: u32) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Jump), Value::Label(Label::WasmFunc(idx as usize)), DebugInfo::from(None))
}

fn call_cjump(idx: u32) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Cjump), Value::Label(Label::WasmFunc(idx as usize)), DebugInfo::from(None))
}

fn get_frame() -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Xget), Value::Int(Uint256::from_usize(0)), DebugInfo::from(None))
}

fn get_return_pc() -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Xget), Value::Int(Uint256::from_usize(1)), DebugInfo::from(None))
}

fn push_frame(v: Value) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::AuxPush), v, DebugInfo::from(None))
}

fn push_value(v: Value) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Noop), v, DebugInfo::from(None))
}

fn set_frame() -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Xset), Value::Int(Uint256::from_usize(0)), DebugInfo::from(None))
}

fn get64_from_buffer(loc: usize) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::GetBuffer64), Value::Int(Uint256::from_usize(loc*8)), DebugInfo::from(None))
}

fn set64_from_buffer(loc: usize) -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::SetBuffer64), Value::Int(Uint256::from_usize(loc*8)), DebugInfo::from(None))
}

fn adjust_stack(res : &mut Vec<Instruction>, diff : usize, num : usize) {
    println!("adjust remove {} save {}", diff, num);
    if diff == 0 { return; }
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

// load byte, save address
fn load_byte(res: &mut Vec<Instruction>, offset: usize) {
    // value, address
    res.push(simple_op(AVMOpcode::Swap1)); // address, value
    res.push(simple_op(AVMOpcode::Dup0)); // address, address, value
    res.push(simple_op(AVMOpcode::Rget)); // buffer, address, address, value
    res.push(simple_op(AVMOpcode::Swap1)); // address, buffer, address, value
    res.push(immed_op(AVMOpcode::Plus, Value::Int(Uint256::from_usize(offset)))); // address, buffer, address, value
    res.push(simple_op(AVMOpcode::GetBuffer8)); // value2, address, value
    res.push(simple_op(AVMOpcode::Swap1)); // address, value2, value
    res.push(simple_op(AVMOpcode::Swap2)); // value, value2, address
}

// In stack? address (top), value
fn store_byte(res: &mut Vec<Instruction>, offset: usize) {
    // value, address
    res.push(simple_op(AVMOpcode::Dup1)); // address, value, address
    res.push(simple_op(AVMOpcode::Dup1)); // value, address, value, address

    res.push(simple_op(AVMOpcode::Rget)); // buffer, value, address, value, address
    res.push(simple_op(AVMOpcode::Swap2)); // address, value, buffer, value, address
    res.push(immed_op(AVMOpcode::Plus, Value::Int(Uint256::from_usize(offset)))); // address, value, buffer, value, address
    res.push(simple_op(AVMOpcode::SetBuffer8)); // buffer, value, address
    res.push(simple_op(AVMOpcode::Rset)); // value, address
}

fn is_func(e : &External) -> bool {
    match *e {
        External::Function(_idx) => {
            true
        },
        _ => false
    }
}

fn get_num_imports(m : &Module) -> u32 {
    match m.import_section() {
        None => 0,
        Some(sec) => {
            let arr = sec.entries();
            arr.iter().filter(|&x| is_func(x.external())).count() as u32
        }
    }
}

fn find_func_type(m : &Module, num : u32) -> &FunctionType {
    // maybe it is import
    if num < get_num_imports(m) {
        let arr = m.import_section().unwrap().entries();
        let idx = match *arr.iter().filter(|&x| is_func(x.external())).collect::<Vec<&ImportEntry>>()[num as usize].external() {
            External::Function(idx) => idx,
            _ => 0,
        };
        get_func_type(m, idx)
    }
    // find it from sig section
    else {
        get_func_type(m, m.function_section().unwrap().entries()[(num - get_num_imports(m)) as usize].type_ref())
    }
}

fn type_code(t : &ValueType) -> u8 {
    match *t {
        ValueType::I32 => 0,
        ValueType::I64 => 1,
        ValueType::F32 => 2,
        ValueType::F64 => 3,
    }
}

fn hash_ftype(ft : &FunctionType) -> Uint256 {
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

fn handle_function(m : &Module, func : &FuncBody, idx : usize, mut label : usize, calli: usize, memory_offset: usize) -> (Vec<Instruction>, usize) {
    let sig = m.function_section().unwrap().entries()[idx].type_ref();
    let ftype = get_func_type(m, sig);
    
    // println!("{:?}", func.code().elements());
    
    // let num_imports = get_num_imports(m);

    let mut res : Vec<Instruction> = Vec::new();
    let mut stack : Vec<Control> = Vec::new();
    let mut ptr : usize = count_locals(func) + ftype.params().len();
    let mut bptr : usize = 0;

    // Construct the function top level frame
    let end_label = label+1;
    label = label + 2;
    bptr = bptr + 1;
    let rets = num_func_returns(ftype);

    let def = Control {level: ptr+rets, rets: rets, target: end_label, else_label: 0, is_ite: false, is_loop: false};

    stack.push(def.clone());

    eprintln!("Got function with {:?} ops, {:?} locals, {} params, {} rets",
        func.code().elements().len(), count_locals(func), ftype.params().len(), rets);

    for op in func.code().elements().iter() {
        eprintln!("handling {} / {}; {:?} ... label {}", ptr, stack.len(), op, label);
        match &*op {
            Unreachable => res.push(simple_op(AVMOpcode::Panic)),
            Nop => res.push(simple_op(AVMOpcode::Noop)),
            Block(bt) => {
                let end_label = label;
                label = label + 1;
                bptr = bptr + 1;
                let rets = block_len(&bt);
                stack.push(Control {level: ptr+rets, rets: rets, target: end_label, .. def});
            },
            GetLocal(x) => {
                // First get stack frame
                res.push(get_frame());
                // Then get the local from the buffer
                res.push(get64_from_buffer(*x as usize));
                ptr = ptr + 1;
            },
            SetLocal(x) => {
                // First get stack frame
                res.push(get_frame());
                // reorder stack
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer(*x as usize));
                // store frame
                res.push(set_frame());
                ptr = ptr - 1;
            },
            TeeLocal(x) => {
                res.push(simple_op(AVMOpcode::Dup0));
                // First get stack frame
                res.push(get_frame());
                // reorder stack
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer(*x as usize));
                // store frame
                res.push(set_frame());
                ptr = ptr - 1;
            },
            I32Add => {
                res.push(simple_op(AVMOpcode::Plus));
                ptr = ptr - 1;
            },
            I32Sub => {
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(simple_op(AVMOpcode::Minus));
                ptr = ptr - 1;
            },
            I32Const(x) => {
                res.push(push_value(Value::Int(Uint256::from_usize(*x as usize))));
                ptr = ptr+1;
            },
            I64Const(x) => {
                res.push(push_value(Value::Int(Uint256::from_usize(*x as usize))));
                ptr = ptr+1;
            },
            I32Eq => {
                res.push(simple_op(AVMOpcode::Equal));
                ptr = ptr - 1;
            },
            I32GtU => {
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(simple_op(AVMOpcode::GreaterThan));
                ptr = ptr - 1;
            },
            If(bt) => {
                ptr = ptr - 1;
                bptr = bptr + 1;
                let else_label = label;
                let end_label = label+1;
                let rets = block_len(&bt);
                eprintln!("Level if {}", ptr);
                stack.push(Control {level: ptr, rets: rets, target: end_label, else_label, is_ite: true, .. def});
                label = label+2;
                res.push(simple_op(AVMOpcode::IsZero));
                res.push(cjump(else_label));
            },
            Else => {
                let mut c : Control = stack.pop().unwrap();
                eprintln!("Level else {}", c.level);
                ptr = c.level;
                res.push(jump(c.target));
                res.push(mk_label(c.else_label));
                c.else_label = 0;
                stack.push(c);
            },
            End => {
                if stack.len() == 0 { break; }
                let c : Control = stack.pop().unwrap();
                bptr = bptr - 1;
                if c.is_ite {
                    if c.else_label != 0 {
                        res.push(mk_label(c.else_label));
                    }
                }
                if !c.is_loop {
                    res.push(mk_label(c.target));
                    ptr = c.level;
                }
                else {
                    ptr = c.level + c.rets;
                }
            },
            Loop(bt) => {
                let start_label = label;
                label = label + 1;
                bptr = bptr + 1;
                let rets = block_len(&bt);
                stack.push(Control {level: ptr, rets: rets, target: start_label, is_loop: true, .. def});
                res.push(mk_label(start_label));
            },
            Drop => {
                ptr = ptr - 1;
                res.push(simple_op(AVMOpcode::Pop));
            },
            Br(x) => {
                let c = &stack[stack.len() - (*x as usize) - 1];
                eprintln!("Debug br {:?} {}", c, c.level);
                adjust_stack(&mut res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                res.push(jump(c.target));
            },
            BrIf(x) => {
                let c = &stack[stack.len() - (*x as usize) - 1];
                let continue_label =label;
                let end_label = label+1;
                label = label+2;
                res.push(cjump(continue_label));
                res.push(jump(end_label));
                res.push(mk_label(continue_label));
                adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                res.push(jump(c.target));
                res.push(mk_label(end_label));
                ptr = ptr - 1;
            },
            // Just keep the expression stack
            Call(x) => {
                let ftype = find_func_type(m, *x);
                println!("calling {} with type {:?} return label {}", x, ftype, label+1);
                let return_label = label+1;
                label = label+1;
                // push new frame to aux stack
                res.push(push_frame(Value::new_tuple(vec![Value::new_buffer(vec![]), Value::Label(Label::Evm(return_label))])));
                // Push args to frame
                for i in 0..ftype.params().len() {
                    res.push(get_frame());
                    res.push(simple_op(AVMOpcode::Swap1));
                    res.push(set64_from_buffer(i));
                    res.push(set_frame());
                }
                res.push(call_jump(*x));
                res.push(mk_label(return_label));
                // Pop stack frame
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Pop));
                ptr = ptr - ftype.params().len() + ftype.results().len();
            },
            CallIndirect(x,_) => {
                let ftype = get_func_type(m, *x);
                println!("calling {} with type {:?} return label {}", x, ftype, label+1);
                let return_label = label+1;
                label = label+1;
                // push new frame to aux stack
                res.push(push_frame(Value::new_tuple(vec![Value::new_buffer(vec![]), Value::Label(Label::Evm(return_label))])));
                // Push args to frame
                for i in 0..ftype.params().len() {
                    res.push(get_frame());
                    res.push(simple_op(AVMOpcode::Swap1));
                    res.push(set64_from_buffer(i));
                    res.push(set_frame());
                }
                // Codepoints and hashes are in some kind of a table
                res.push(push_value(Value::Int(hash_ftype(&ftype))));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(call_jump(calli as u32));
                res.push(mk_label(return_label));
                // Pop stack frame
                res.push(simple_op(AVMOpcode::AuxPop));
                res.push(simple_op(AVMOpcode::Pop));
                ptr = ptr - ftype.params().len() + num_func_returns(ftype) - 1;
            },
            Return => {
                let c = &stack[0];
                println!("return {} level {} rets {}", ptr, c.level, c.rets);
                adjust_stack(&mut res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                res.push(jump(c.target));
            },
            Select => {
                let else_label = label;
                // let end_label = label+1;
                res.push(cjump(else_label));
                res.push(simple_op(AVMOpcode::Swap1));
                // res.push(jump(end_label));
                res.push(mk_label(else_label));
                res.push(simple_op(AVMOpcode::Pop));
                
                label = label+2;
                ptr = ptr-2;
            },

            BrTable(data) => {
                let tab = &data.table;
                let def = data.default;
                let len = tab.len();
                for (i,num) in tab.iter().enumerate() {
                    let c = &stack[stack.len() - (*num as usize) - 1];
                    res.push(simple_op(AVMOpcode::Dup0));
                    res.push(immed_op(AVMOpcode::Equal, Value::Int(Uint256::from_usize(i))));
                    res.push(simple_op(AVMOpcode::IsZero));
                    res.push(cjump(label+i));
                    res.push(simple_op(AVMOpcode::Pop));
                    adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                    res.push(jump(c.target));
                    res.push(mk_label(label+i));
                }
                let c = &stack[stack.len() - (def as usize) - 1];
                res.push(simple_op(AVMOpcode::Pop));
                adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                res.push(jump(c.target));
                
                ptr = ptr-1-c.rets;
                label = label + len + 2;
            },

            GetGlobal(x) => {
                ptr = ptr + 1;
                res.push(simple_op(AVMOpcode::Rget));
                res.push(get64_from_buffer((*x+1) as usize));
            },
            SetGlobal(x) => {
                ptr = ptr - 1;
                res.push(simple_op(AVMOpcode::Rget));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(set64_from_buffer((*x+1) as usize));
                res.push(simple_op(AVMOpcode::Rset));
            },
            
            I32Load(_, offset) => {
                /*
                res.push(simple_op(AVMOpcode::Rget));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(immed_op(AVMOpcode::Add, Value::Int(Uint256::from_usize(offset+memory_offset))));
                res.push(simple_op(AVMOpcode::GetBuffer64));
                change_endian_32(&res);
                */
                let offset = *offset as usize;
                res.push(immed_op(AVMOpcode::Noop, Value::Int(Uint256::from_usize(0))));
                load_byte(&mut res, offset+memory_offset);
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(AVMOpcode::ShiftLeft, Value::Int(Uint256::from_usize(8))));
                load_byte(&mut res, offset+memory_offset+1);
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(AVMOpcode::ShiftLeft, Value::Int(Uint256::from_usize(8))));
                load_byte(&mut res, offset+memory_offset+2);
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(immed_op(AVMOpcode::ShiftLeft, Value::Int(Uint256::from_usize(8))));
                load_byte(&mut res, offset+memory_offset+3);
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(simple_op(AVMOpcode::Pop));
            },

            I32Store(_, offset) => {
                let offset = *offset as usize;
                ptr = ptr - 2;
                store_byte(&mut res, offset+memory_offset+3);
                res.push(immed_op(AVMOpcode::ShiftRight, Value::Int(Uint256::from_usize(8))));
                store_byte(&mut res, offset+memory_offset+2);
                res.push(immed_op(AVMOpcode::ShiftRight, Value::Int(Uint256::from_usize(8))));
                store_byte(&mut res, offset+memory_offset+1);
                res.push(immed_op(AVMOpcode::ShiftRight, Value::Int(Uint256::from_usize(8))));
                store_byte(&mut res, offset+memory_offset+0);
                res.push(immed_op(AVMOpcode::ShiftRight, Value::Int(Uint256::from_usize(8))));
                res.push(simple_op(AVMOpcode::Pop));
                res.push(simple_op(AVMOpcode::Pop));
                /*
                res.push(simple_op(AVMOpcode::AuxPush));
                res.push(simple_op(AVMOpcode::Rget));
                res.push(simple_op(AVMOpcode::Swap1));
                res.push(immed_op(AVMOpcode::Add, Value::Int(Uint256::from_usize(offset+memory_offset))));
                res.push(simple_op(AVMOpcode::Dup0));
                res.push(simple_op(AVMOpcode::Swap2));
                res.push(simple_op(AVMOpcode::GetBuffer64));
                res.push(immed_op(AVMOpcode::BitwiseAnd, Value::Int(Uint256::from_usize(0xffffffffu64))));
                res.push(simple_op(AVMOpcode::AuxPop));
                change_endian_32(&res);
                res.push(simple_op(AVMOpcode::BitwiseOr));
                res.push(simple_op(AVMOpcode::Rget));
                res.push(simple_op(AVMOpcode::Swap2));
                res.push(simple_op(AVMOpcode::SetBuffer64));
                res.push(simple_op(AVMOpcode::Rset));
                */
            },

            /*
            
            I32Load8S(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem8, packing:Packing::SX, mtype:ValueType::I32});
            },
            I32Load8U(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem8, packing:Packing::ZX, mtype:ValueType::I32});
            },
            I32Load16S(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem16, packing:Packing::SX, mtype:ValueType::I32});
            },
            I32Load16U(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem16, packing:Packing::ZX, mtype:ValueType::I32});
            },
            
            I64Load(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem64, packing:Packing::ZX, mtype:ValueType::I64});
            },
            I64Load8S(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem8, packing:Packing::SX, mtype:ValueType::I64});
            },
            I64Load8U(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem8, packing:Packing::ZX, mtype:ValueType::I64});
            },
            I64Load16S(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem16, packing:Packing::SX, mtype:ValueType::I64});
            },
            I64Load16U(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem16, packing:Packing::ZX, mtype:ValueType::I64});
            },
            I64Load32S(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem32, packing:Packing::SX, mtype:ValueType::I64});
            },
            I64Load32U(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem32, packing:Packing::ZX, mtype:ValueType::I64});
            },
            
            I32Store8(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem8, mtype:ValueType::I32});
            },
            I32Store16(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem16, mtype:ValueType::I32});
            },
            
            I64Store(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem64, mtype:ValueType::I64});
            },
            I64Store8(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem8, mtype:ValueType::I64});
            },
            I64Store16(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem16, mtype:ValueType::I64});
            },
            I64Store32(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem32, mtype:ValueType::I64});
            },
            
            I32Eqz => res.push(UNOP(0x45)),
            I32Ne => { ptr = ptr - 1; res.push(BINOP(0x47)); },
            I32LtS => { ptr = ptr - 1; res.push(BINOP(0x48)); },
            I32LtU => { ptr = ptr - 1; res.push(BINOP(0x49)); },
            I32GtS => { ptr = ptr - 1; res.push(BINOP(0x4a)); },
            I32GtU => { ptr = ptr - 1; res.push(BINOP(0x4b)); },
            I32LeS => { ptr = ptr - 1; res.push(BINOP(0x4c)); },
            I32LeU => { ptr = ptr - 1; res.push(BINOP(0x4d)); },
            I32GeS => { ptr = ptr - 1; res.push(BINOP(0x4e)); },
            I32GeU => { ptr = ptr - 1; res.push(BINOP(0x4f)); },
            
            I64Eqz => res.push(UNOP(0x50)),
            I64Eq => { ptr = ptr - 1; res.push(BINOP(0x51)); },
            I64Ne => { ptr = ptr - 1; res.push(BINOP(0x52)); },
            I64LtS => { ptr = ptr - 1; res.push(BINOP(0x53)); },
            I64LtU => { ptr = ptr - 1; res.push(BINOP(0x54)); },
            I64GtS => { ptr = ptr - 1; res.push(BINOP(0x55)); },
            I64GtU => { ptr = ptr - 1; res.push(BINOP(0x56)); },
            I64LeS => { ptr = ptr - 1; res.push(BINOP(0x57)); },
            I64LeU => { ptr = ptr - 1; res.push(BINOP(0x58)); },
            I64GeS => { ptr = ptr - 1; res.push(BINOP(0x59)); },
            I64GeU => { ptr = ptr - 1; res.push(BINOP(0x5a)); },
            
            I32Clz => res.push(UNOP(0x67)),
            I32Ctz => res.push(UNOP(0x68)),
            I32Popcnt => res.push(UNOP(0x69)),
            I32Add => { ptr = ptr - 1; res.push(BINOP(0x6a)); },
            I32Sub => { ptr = ptr - 1; res.push(BINOP(0x6b)); },
            I32Mul => { ptr = ptr - 1; res.push(BINOP(0x6c)); },
            I32DivS => { ptr = ptr - 1; res.push(BINOP(0x6d)); },
            I32DivU => { ptr = ptr - 1; res.push(BINOP(0x6e)); },
            I32RemS => { ptr = ptr - 1; res.push(BINOP(0x6f)); },
            I32RemU => { ptr = ptr - 1; res.push(BINOP(0x70)); },
            I32And => { ptr = ptr - 1; res.push(BINOP(0x71)); },
            I32Or => { ptr = ptr - 1; res.push(BINOP(0x72)); },
            I32Xor => { ptr = ptr - 1; res.push(BINOP(0x73)); },
            I32Shl => { ptr = ptr - 1; res.push(BINOP(0x74)); },
            I32ShrS => { ptr = ptr - 1; res.push(BINOP(0x75)); },
            I32ShrU => { ptr = ptr - 1; res.push(BINOP(0x76)); },
            I32Rotl => { ptr = ptr - 1; res.push(BINOP(0x77)); },
            I32Rotr => { ptr = ptr - 1; res.push(BINOP(0x78)); },

            I64Clz => res.push(UNOP(0x79)),
            I64Ctz => res.push(UNOP(0x7a)),
            I64Popcnt => res.push(UNOP(0x7b)),
            I64Add => { ptr = ptr - 1; res.push(BINOP(0x7c)); },
            I64Sub => { ptr = ptr - 1; res.push(BINOP(0x7d)); },
            I64Mul => { ptr = ptr - 1; res.push(BINOP(0x7e)); },
            I64DivS => { ptr = ptr - 1; res.push(BINOP(0x7f)); },
            I64DivU => { ptr = ptr - 1; res.push(BINOP(0x80)); },
            I64RemS => { ptr = ptr - 1; res.push(BINOP(0x81)); },
            I64RemU => { ptr = ptr - 1; res.push(BINOP(0x82)); },
            I64And => { ptr = ptr - 1; res.push(BINOP(0x83)); },
            I64Or => { ptr = ptr - 1; res.push(BINOP(0x84)); },
            I64Xor => { ptr = ptr - 1; res.push(BINOP(0x85)); },
            I64Shl => { ptr = ptr - 1; res.push(BINOP(0x86)); },
            I64ShrS => { ptr = ptr - 1; res.push(BINOP(0x87)); },
            I64ShrU => { ptr = ptr - 1; res.push(BINOP(0x88)); },
            I64Rotl => { ptr = ptr - 1; res.push(BINOP(0x89)); },
            I64Rotr => { ptr = ptr - 1; res.push(BINOP(0x8a)); },
            
            I32WarpI64 => res.push(UNOP(0xa7)),
            I64ExtendSI32 => res.push(UNOP(0xac)),
            I64ExtendUI32 => res.push(UNOP(0xad)),

            CurrentMemory(_) => {
                ptr = ptr+1;
                res.push(CURMEM);
            },
            GrowMemory(_) => {
                ptr = ptr-1;
                res.push(GROW);
            },
            
            */
            _ => {},
        }
    }

    // Function return
    res.push(mk_label(end_label));
    res.push(get_return_pc());
    res.push(simple_op(AVMOpcode::Jump));

    return (res, label);

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

pub fn resolve_labels(arr: Vec<Instruction>) -> Vec<Instruction> {
    let mut labels = HashMap::new();
    for (idx, inst) in arr.iter().enumerate() {
        match inst.opcode {
            Opcode::Label(Label::Evm(num)) => {
                // println!("Found label {} -> {}", num, idx);
                labels.insert(Label::Evm(num), CodePt::Internal(idx));
            }
            Opcode::Label(Label::WasmFunc(num)) => {
                // println!("Found func label {} -> {}", num, idx);
                labels.insert(Label::WasmFunc(num), CodePt::Internal(idx));
            }
            _ => {}
        }
    }
    let mut res = vec![];
    for inst in arr.iter() {
        res.push(inst.clone().replace_labels(&labels).unwrap());
    }
    res
}

fn init_value(_m : &Module, expr : &InitExpr) -> usize {
    eprintln!("init {:?}", expr);
    match expr.code()[0] {
      I32Const(a) => a as usize,
      F32Const(a) => a as usize,
      I64Const(a) => a as usize,
      F64Const(a) => a as usize,
      _ => 0
    }
}

pub fn load(fname: String, param: usize) -> Vec<Instruction> {
    let module = parity_wasm::deserialize_file(fname).unwrap();
    assert!(module.code_section().is_some());

    let code_section = module.code_section().unwrap(); // Part of the module with functions code
    let f_count = code_section.bodies().len();

    println!("Function count in wasm file: {}", f_count);
    let mut init = vec![];

    // Save register
    init.push(simple_op(AVMOpcode::Rget));
    init.push(simple_op(AVMOpcode::AuxPush));

    // Construct initial memory with globals
    init.push(simple_op(AVMOpcode::NewBuffer));
    let mut globals = 1;
    if let Some(sec) = module.global_section() {
        for g in sec.entries().iter() {
            init.push(push_value(Value::Int(Uint256::from_usize(init_value(&module, g.init_expr())))));
            init.push(set64_from_buffer(globals));
            globals = globals + 1;
        }
    }
    init.push(simple_op(AVMOpcode::Rset));

    // Put initial frame to aux stack
    init.push(push_frame(Value::new_tuple(vec![Value::new_buffer(vec![]), Value::Label(Label::WasmFunc(f_count+1))])));

    // Add test argument to the frame
    init.push(get_frame());
    init.push(push_value(Value::Int(Uint256::from_usize(param))));
    init.push(set64_from_buffer(0));
    init.push(set_frame());

    let mut label = 1;
    let calli = f_count;
    let memory_offset = globals*8;

    for (idx,f) in code_section.bodies().iter().enumerate() {
        // function return will be in the stack
        init.push(mk_func_label(idx));
        let (mut res, n_label) = handle_function(&module, f, idx, label, calli, memory_offset);
        init.append(&mut res);
        label = n_label;
        // println!("labels {}", label);
    }

    // Indirect calls
    init.push(mk_func_label(f_count));
    if let Some(sec) = module.elements_section() {
        for seg in sec.entries().iter() {
            for (idx, f_idx) in seg.members().iter().enumerate() {
                let next_label = label;
                label = label + 1;
                // Function index in module
                let ftype = find_func_type(&module, *f_idx);
                init.push(simple_op(AVMOpcode::Dup0));
                init.push(push_value(Value::Int(Uint256::from_usize(idx))));
                init.push(simple_op(AVMOpcode::Equal));
                init.push(simple_op(AVMOpcode::IsZero));
                init.push(cjump(next_label));
                // We will call this function now or fail
                init.push(simple_op(AVMOpcode::Pop));
                init.push(push_value(Value::Int(hash_ftype(&ftype))));
                init.push(simple_op(AVMOpcode::Equal));
                init.push(call_cjump(*f_idx as u32));
                init.push(simple_op(AVMOpcode::Panic));
                init.push(mk_label(next_label));
            }
        }
    }

    // Cleaning up
    init.push(mk_func_label(f_count+1));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Pop));
    init.push(simple_op(AVMOpcode::AuxPop));
    init.push(simple_op(AVMOpcode::Rset));
    init.push(simple_op(AVMOpcode::Noop));

    clear_labels(resolve_labels(init))

}
