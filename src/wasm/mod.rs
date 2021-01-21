
use parity_wasm::elements::*;
use parity_wasm::elements::Instruction::*;
use crate::mavm::{AVMOpcode, Instruction, Label, LabelGenerator, Opcode, Value};
use crate::compile::{DebugInfo, MiniProperties};
use crate::uint256::Uint256;

#[derive(Debug, Clone)]
struct Control {
    target : u32,
    rets : u32,
    level : u32,
    else_label : u32,
    is_ite : bool,
    is_loop : bool,
}

fn block_len(bt : &BlockType) -> u32 {
    match *bt {
        BlockType::Value(_) => 1,
        BlockType::NoResult => 0,
    }
}

fn count_locals(func : &FuncBody) -> u32 {
    func.locals().iter().fold(0, |sum, x| sum + x.count())
}

fn get_func_type(m : &Module, sig : u32) -> &FunctionType {
    match m.type_section().unwrap().types()[sig as usize] {
        Type::Function(ref t) => t
    }
}

fn num_func_returns(ft : &FunctionType) -> u32 {
    ft.results().len() as u32
}

fn simple_op(op : AVMOpcode) -> Instruction {
    Instruction::from_opcode(Opcode::AVMOpcode(op), DebugInfo::from(None))
}

fn get_frame() -> Instruction {
    Instruction::from_opcode_imm(Opcode::AVMOpcode(AVMOpcode::Xget), Value::Int(Uint256::from_usize(0)), DebugInfo::from(None))
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

fn handle_function(m : &Module, func : &FuncBody, idx : usize) -> Vec<Instruction> {
    let sig = m.function_section().unwrap().entries()[idx].type_ref();
    let ftype = get_func_type(m, sig);
    
    // println!("{:?}", func.code().elements());
    
    // let num_imports = get_num_imports(m);

    let mut res : Vec<Instruction> = Vec::new();
    let mut stack : Vec<Control> = Vec::new();
    let mut label : u32 = 1;
    let mut ptr : u32 = count_locals(func) + (ftype.params().len() as u32);
    let mut bptr : u32 = 0;

    // Construct the function top level frame
    let end_label = label;
    label = label + 1;
    bptr = bptr + 1;
    let rets = num_func_returns(ftype);

    let def = Control {level: ptr+rets, rets: rets, target: end_label, else_label: 0, is_ite: false, is_loop: false};

    stack.push(def.clone());

    eprintln!("Got function with {:?} ops, {:?} locals, {} params, {} rets",
        func.code().elements().len(), count_locals(func), ftype.params().len(), rets);
    // Push default values
    // not necessary to push anything
    for _i in 0..(count_locals(func) as usize) {
        eprintln!("pushing default");
    }

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
                ptr = ptr - 1;
                res.push(simple_op(AVMOpcode::Plus));
            },
            /*
            Loop(bt) => {
                let start_label = label;
                label = label + 1;
                bptr = bptr + 1;
                let rets = block_len(&bt);
                stack.push(Control {level: ptr, rets: rets, target: start_label, luuppi: true, .. def});
                res.push(LABEL(start_label));
            },
            End => {
                if stack.len() == 0 { break; }
                let c : Control = stack.pop().unwrap();
                bptr = bptr - 1;
                if c.ite {
                    if c.else_label != 0 {
                        res.push(LABEL(c.else_label));
                    }
                    else {
                        res.push(NOP);
                    }
                }
                if !c.luuppi {
                    res.push(LABEL(c.target));
                    ptr = c.level;
                }
                else {
                    ptr = c.level + c.rets;
                }
            },
            If(bt) => {
                ptr = ptr - 1;
                bptr = bptr + 1;
                let else_label = label;
                let end_label = label+1;
                let rets = block_len(&bt);
                // eprintln!("Level if {}", ptr);
                stack.push(Control {level: ptr, rets: rets, target: end_label, else_label, ite: true, .. def});
                label = label+2;
                res.push(JUMPZ(else_label));
            },
            Else => {
                let mut c : Control = stack.pop().unwrap();
                // eprintln!("Level else {}", c.level);
                ptr = c.level;
                res.push(NOP);
                res.push(JUMP(c.target));
                res.push(LABEL(c.else_label));
                c.else_label = 0;
                stack.push(c);
            },
            Drop => {
                ptr = ptr - 1;
                res.push(DROP(1));
            },
            Br(x) => {
                let c = &stack[stack.len() - (x as usize) - 1];
                eprintln!("Debug br {:?} {}", c, c.level);
                adjust_stack(&mut res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                res.push(JUMP(c.target));
            },
            BrIf(x) => {
                let c = &stack[stack.len() - (x as usize) - 1];
                let continue_label =label;
                let end_label = label+1;
                label = label+2;
                res.push(JUMPI(continue_label));
                res.push(JUMP(end_label));
                res.push(LABEL(continue_label));
                adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                res.push(JUMP(c.target));
                res.push(LABEL(end_label));
                ptr = ptr - 1;
            },
            Return => {
                let c = &stack[0];
                adjust_stack(&mut res, ptr - c.level, c.rets);
                ptr = ptr - c.rets;
                res.push(JUMP(c.target));
            },
            BrTable(ref tab, def) => {
                let rets = &stack[stack.len() - (def as usize) - 1].rets;
                let len = tab.len() as u32;
                res.push(JUMPFORWARD(len));
                for i in 0..len {
                    res.push(JUMP (label+i as u32));
                }
                for (i,num) in tab.iter().enumerate() {
                    let c = &stack[stack.len() - (*num as usize) - 1];
                    res.push(LABEL(label+i as u32));
                    adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                    res.push(JUMP(c.target));
                }
                let c = &stack[stack.len() - (def as usize) - 1];
                res.push(LABEL(label+len as u32));
                adjust_stack(&mut res, ptr - c.level - 1, c.rets);
                res.push(JUMP(c.target));
                
                ptr = ptr-1-rets;
                label = label + len + 2;
            },
            
            Select => {
                let else_label = label;
                let end_label = label+1;
                res.push(JUMPI(else_label));
                res.push(SET(2));
                res.push(DROP(1));
                res.push(JUMP(end_label));
                res.push(LABEL(else_label));
                res.push(DROP(1));
                res.push(LABEL(end_label));
                
                label = label+2;
                ptr = ptr-2;
            },
            
            Call(x) => {
                let ftype = find_func_type(m, x);
                // println!("calling {} with type {:?}", x, ftype);
                res.push(CALL(x));
                ptr = ptr - (ftype.params().len() as u32) + num_func_returns(ftype);
            },
            CallIndirect(x,_) => {
                let ftype = get_func_type(m, x);
                res.push(CHECKCALLI(hash_ftype(&ftype)));
                res.push(CALLI);
                ptr = ptr - (ftype.params().len() as u32) + num_func_returns(ftype) - 1;
            },
            
            I32Const(x) => {
                res.push(PUSH(x as u64));
                ptr = ptr+1;
            },
            I64Const(x) => {
                res.push(PUSH(x as u64));
                ptr = ptr+1;
            },
            F32Const(x) => {
                res.push(PUSH(x as u64));
                ptr = ptr+1;
            },
            F64Const(x) => {
                res.push(PUSH(x as u64));
                ptr = ptr+1;
            },
            
            GetGlobal(x) => {
                ptr = ptr + 1;
                res.push(LOADGLOBAL(x));
            },
            SetGlobal(x) => {
                ptr = ptr - 1;
                res.push(STOREGLOBAL(x));
            },
            
            CurrentMemory(_) => {
                ptr = ptr+1;
                res.push(CURMEM);
            },
            GrowMemory(_) => {
                ptr = ptr-1;
                res.push(GROW);
            },
            
            I32Load(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem32, packing:Packing::ZX, mtype:ValueType::I32});
            },
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
            
            F32Load(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem32, packing:Packing::ZX, mtype:ValueType::F32});
            },
            F64Load(_, offset) => {
                res.push(LOAD {offset, memsize: Size::Mem64, packing:Packing::ZX, mtype:ValueType::F64});
            },
            
            I32Store(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem32, mtype:ValueType::I32});
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
            
            F32Store(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem32, mtype:ValueType::F32});
            },
            F64Store(_, offset) => {
                ptr = ptr - 2;
                res.push(STORE {offset, memsize: Size::Mem64, mtype:ValueType::F64});
            },
            
            I32Eqz => res.push(UNOP(0x45)),
            I32Eq => { ptr = ptr - 1; res.push(BINOP(0x46)); },
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
            
            F32Eq => { ptr = ptr - 1; res.push(BINOP(0x5b)); },
            F32Ne => { ptr = ptr - 1; res.push(BINOP(0x5c)); },
            F32Lt => { ptr = ptr - 1; res.push(BINOP(0x5d)); },
            F32Gt => { ptr = ptr - 1; res.push(BINOP(0x5e)); },
            F32Le => { ptr = ptr - 1; res.push(BINOP(0x5f)); },
            F32Ge => { ptr = ptr - 1; res.push(BINOP(0x60)); },
            
            F64Eq => { ptr = ptr - 1; res.push(BINOP(0x61)); },
            F64Ne => { ptr = ptr - 1; res.push(BINOP(0x62)); },
            F64Lt => { ptr = ptr - 1; res.push(BINOP(0x63)); },
            F64Gt => { ptr = ptr - 1; res.push(BINOP(0x64)); },
            F64Le => { ptr = ptr - 1; res.push(BINOP(0x65)); },
            F64Ge => { ptr = ptr - 1; res.push(BINOP(0x66)); },

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
            
            F32Abs => res.push(UNOP(0x8b)),
            F32Neg => res.push(UNOP(0x8c)),
            F32Ceil => res.push(UNOP(0x8d)),
            F32Floor => res.push(UNOP(0x8e)),
            F32Trunc => res.push(UNOP(0x8f)),
            F32Nearest => res.push(UNOP(0x90)),
            F32Sqrt => res.push(UNOP(0x91)),
            F32Add => { ptr = ptr - 1; res.push(BINOP(0x92)); },
            F32Sub => { ptr = ptr - 1; res.push(BINOP(0x93)); },
            F32Mul => { ptr = ptr - 1; res.push(BINOP(0x94)); },
            F32Div => { ptr = ptr - 1; res.push(BINOP(0x95)); },
            F32Min => { ptr = ptr - 1; res.push(BINOP(0x96)); },
            F32Max => { ptr = ptr - 1; res.push(BINOP(0x97)); },
            F32Copysign => { ptr = ptr - 1; res.push(BINOP(0x98)); },
            
            F64Abs => res.push(UNOP(0x99)),
            F64Neg => res.push(UNOP(0x9a)),
            F64Ceil => res.push(UNOP(0x9b)),
            F64Floor => res.push(UNOP(0x9c)),
            F64Trunc => res.push(UNOP(0x9d)),
            F64Nearest => res.push(UNOP(0x9e)),
            F64Sqrt => res.push(UNOP(0x9f)),
            F64Add => { ptr = ptr - 1; res.push(BINOP(0xa0)); },
            F64Sub => { ptr = ptr - 1; res.push(BINOP(0xa1)); },
            F64Mul => { ptr = ptr - 1; res.push(BINOP(0xa2)); },
            F64Div => { ptr = ptr - 1; res.push(BINOP(0xa3)); },
            F64Min => { ptr = ptr - 1; res.push(BINOP(0xa4)); },
            F64Max => { ptr = ptr - 1; res.push(BINOP(0xa5)); },
            F64Copysign => { ptr = ptr - 1; res.push(BINOP(0xa6)); },
            
            
            I32WarpI64 => res.push(UNOP(0xa7)),
            I32TruncSF32 => res.push(UNOP(0xa8)),
            I32TruncUF32 => res.push(UNOP(0xa9)),
            I32TruncSF64 => res.push(UNOP(0xaa)),
            I32TruncUF64 => res.push(UNOP(0xab)),
            I64ExtendSI32 => res.push(UNOP(0xac)),
            I64ExtendUI32 => res.push(UNOP(0xad)),
            I64TruncSF32 => res.push(UNOP(0xae)),
            I64TruncUF32 => res.push(UNOP(0xaf)),
            I64TruncSF64 => res.push(UNOP(0xb0)),
            I64TruncUF64 => res.push(UNOP(0xb1)),
            F32ConvertSI32 => res.push(UNOP(0xb2)),
            F32ConvertUI32 => res.push(UNOP(0xb3)),
            F32ConvertSI64 => res.push(UNOP(0xb4)),
            F32ConvertUI64 => res.push(UNOP(0xb5)),
            F32DemoteF64 => res.push(UNOP(0xb6)),
            F64ConvertSI32 => res.push(UNOP(0xb7)),
            F64ConvertUI32 => res.push(UNOP(0xb8)),
            F64ConvertSI64 => res.push(UNOP(0xb9)),
            F64ConvertUI64 => res.push(UNOP(0xba)),
            F64PromoteF32 => res.push(UNOP(0xbb)),

            I32ReinterpretF32 => res.push(UNOP(0xbc)),
            I64ReinterpretF64 => res.push(UNOP(0xbd)),
            F32ReinterpretI32 => res.push(UNOP(0xbe)),
            F64ReinterpretI64 => res.push(UNOP(0xbf)),
            */
            _ => {},
        }
    }

    return res;

}

pub fn load() -> Vec<Instruction> {
    let module = parity_wasm::deserialize_file("./wasm-tests/test.wasm").unwrap();
    assert!(module.code_section().is_some());

    let code_section = module.code_section().unwrap(); // Part of the module with functions code

    println!("Function count in wasm file: {}", code_section.bodies().len());

    // Put initial frame to aux stack
    let mut init = vec![];
    init.push(push_frame(Value::new_tuple(vec![Value::new_buffer(vec![])])));

    // Add test argument to the frame
    init.push(get_frame());
    init.push(push_value(Value::Int(Uint256::from_usize(1234))));
    init.push(set64_from_buffer(0));
    init.push(set_frame());

    for (idx,f) in code_section.bodies().iter().enumerate() {
        // function return will be in the stack
        let mut res = handle_function(&module, f, idx);
        init.append(&mut res);
    }

    init.push(simple_op(AVMOpcode::Noop));

    init

}
