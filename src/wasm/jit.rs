use crate::compile::DebugInfo;
use crate::mavm::{AVMOpcode, Buffer, Instruction, Opcode, Value};
use crate::uint256::Uint256;
use std::fmt;

fn int_from_usize(a: usize) -> Value {
    Value::Int(Uint256::from_usize(a))
}

pub fn get_answer(answer: wasmtime::TypedFunc<(), (i32)>) -> i32 {
    match answer.call(()) {
        Ok(result) => result as i32,
        Err(_) => {
            println!("Wasm error");
            0
        }
    }
}

pub struct JitWasm {
    instance: wasmtime::Instance,
    cell: std::rc::Rc<std::cell::RefCell<Buffer>>,
    len_cell: std::rc::Rc<std::cell::RefCell<i32>>,
    gas_cell: std::rc::Rc<std::cell::RefCell<i32>>,
    extra_cell: std::rc::Rc<std::cell::RefCell<Vec<u8>>>,
    immed_cell: std::rc::Rc<std::cell::RefCell<Value>>,
    insn_cell: std::rc::Rc<std::cell::RefCell<Vec<Instruction>>>,
    table_cell: std::rc::Rc<std::cell::RefCell<Vec<(usize, usize)>>>,
}

impl fmt::Debug for JitWasm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JitWasm")
    }
}

fn get_tuple(v: Option<Value>, idx: usize) -> Option<Value> {
    if let Some(Value::Tuple(vs)) = v {
        Some(vs[idx].clone())
    } else {
        None
    }
}

fn value_bytes(v: Option<Value>) -> Option<Vec<u8>> {
    if let Some(Value::Int(i)) = v {
        Some(i.to_bytes_be())
    } else {
        None
    }
}

fn buffer_bytes(v: Option<Value>, len: usize) -> Option<Vec<u8>> {
    if let Some(Value::Buffer(buf)) = v {
        let mut tmp = vec![];
        for i in 0..len {
            tmp.push(buf.read_byte(i as u128));
        }
        Some(tmp)
    } else if len == 0 {
        Some(vec![])
    } else {
        None
    }
}

fn get_tuple_bytes(v: Option<Value>, idx: usize) -> Option<Vec<u8>> {
    value_bytes(get_tuple(v, idx))
}

fn get_tuple2_bytes(v: Option<Value>, idx: usize, idx2: usize) -> Option<Vec<u8>> {
    get_tuple_bytes(get_tuple(v, idx), idx2)
}

fn get_tuple2_buffer(v: Option<Value>, idx: usize, idx2: usize, len: usize) -> Option<Vec<u8>> {
    buffer_bytes(get_tuple(get_tuple(v, idx), idx2), len)
}

fn error_to_wasm<E>(res: Result<(), E>) -> Result<(), wasmtime::Trap> {
    match res {
        Err(_) => Err(wasmtime::Trap::new("bad pointer")),
        _ => Ok(()),
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

        let buf = Buffer::from_bytes(vec![]);

        let memory_cell: Rc<RefCell<std::option::Option<Memory>>> = Rc::new(RefCell::new(None));
        let memory_cell2 = memory_cell.clone();
        let memory_cell3 = memory_cell.clone();
        let memory_cell4 = memory_cell.clone();
        let memory_cell5 = memory_cell.clone();
        let memory_cell6 = memory_cell.clone();
        let memory_cell7 = memory_cell.clone();

        let extra_cell: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(vec![]));
        let extra_cell1 = extra_cell.clone();

        let cell = Rc::new(RefCell::new(buf));
        let cell1 = cell.clone();
        let cell2 = cell.clone();
        let cell3 = cell.clone();
        let cell4 = cell.clone();

        let len_cell = Rc::new(RefCell::new(4));
        let len1 = len_cell.clone();
        let len2 = len_cell.clone();

        let gas_cell = Rc::new(RefCell::new(4));
        let gas1 = gas_cell.clone();

        let immed = Value::new_tuple(vec![
            int_from_usize(123),
            Value::new_buffer(vec![1u8; 32]),
            Value::new_tuple(vec![
                Value::new_buffer(vec![2u8; 32]),
                int_from_usize(234),
                int_from_usize(234),
            ]),
        ]);

        let immed_cell = Rc::new(RefCell::new(immed));
        let immed1 = immed_cell.clone();
        let immed2 = immed_cell.clone();
        let immed3 = immed_cell.clone();
        let immed4 = immed_cell.clone();
        let immed5 = immed_cell.clone();
        let immed6 = immed_cell.clone();
        let immed7 = immed_cell.clone();
        let immed8 = immed_cell.clone();

        let special_immed_func = Func::wrap(&store, move |_offset: i32| {
            immed1.replace_with(|_| {
                Value::new_tuple(vec![
                    Value::new_buffer(vec![]), // frame
                    int_from_usize(0),         // call table
                ])
            });
        });

        let uint_immed_func = Func::wrap(&store, move |ptr: i32| match &*memory_cell4.borrow() {
            None => panic!("Wasm error: no memory"),
            Some(memory) => {
                let mut tmp = vec![0; 32];
                let sz = memory.data_size();
                let ptr = ptr as usize;
                if sz > ptr + 32 {
                    memory.read(ptr, &mut tmp).expect("cannot read memory");
                } else if ptr > sz {
                } else {
                    let num = sz - ptr;
                    memory
                        .read(ptr, &mut tmp[0..num])
                        .expect("cannot read memory");
                }
                let v = Value::Int(Uint256::from_bytes(&tmp));
                immed3.replace_with(|_| v);
            }
        });

        let global_immed_func = Func::wrap(&store, move |_offset: i32| {
            immed2.replace_with(|_| {
                Value::new_tuple(vec![
                    Value::new_buffer(vec![]),  // memory
                    int_from_usize(0),          // call table
                    Value::new_buffer(vec![]),  // IO buffer
                    int_from_usize(0),          // IO len
                    int_from_usize(1000000000), // gas left
                    int_from_usize(0),          // Immed
                    int_from_usize(0),          // Code point
                    int_from_usize(0),          // Generated jump table
                ])
            });
        });

        let table_immed_func = Func::wrap(&store, move |_offset: i32| {
            immed5.replace_with(|_| {
                Value::new_tuple(vec![
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                    int_from_usize(0),
                ])
            });
        });

        let tuplebytes_func = Func::wrap(&store, move |ptr: i32, offset: i32| {
            let v = &*immed6.borrow();
            let tmp = get_tuple_bytes(Some(v.clone()), offset as usize);
            match (tmp, &*memory_cell5.borrow()) {
                (Some(tmp), Some(memory)) => {
                    error_to_wasm(memory.write(ptr as usize, &tmp))
                }
                _ => Err(Trap::new("tuplebytes")),
            }
        });

        let tuple2bytes_func = Func::wrap(&store, move |ptr: i32, offset: i32, offset2: i32| {
            let v = &*immed7.borrow();
            let tmp = get_tuple2_bytes(Some(v.clone()), offset as usize, offset2 as usize);
            match (tmp, &*memory_cell6.borrow()) {
                (Some(tmp), Some(memory)) => {
                    error_to_wasm(memory.write(ptr as usize, &tmp))
                }
                _ => Err(Trap::new("tuple2bytes")),
            }
        });

        let tuple2buffer_func = Func::wrap(
            &store,
            move |ptr: i32, offset: i32, offset2: i32, len: i32| {
                let v = &*immed8.borrow();
                let tmp =
                    get_tuple2_buffer(Some(v.clone()), offset as usize, offset2 as usize, len as usize);
                match (tmp, &*memory_cell7.borrow()) {
                    (Some(tmp), Some(memory)) => {
                        error_to_wasm(memory.write(ptr as usize, &tmp))
                    }
                    _ => Err(Trap::new("tuple2buffer")),
                }
            },
        );

        let insn_cell: Rc<RefCell<Vec<Instruction>>> = Rc::new(RefCell::new(vec![]));
        let insn1 = insn_cell.clone();
        let insn2 = insn_cell.clone();
        let insn3 = insn_cell.clone();

        let push_inst_func = Func::wrap(&store, move |opcode: i32| {
            let mut vec = insn1.borrow_mut();
            vec.push(Instruction::from_opcode(
                Opcode::AVMOpcode(AVMOpcode::from_number(opcode as usize).unwrap()),
                DebugInfo::from(None),
            ));
        });

        let push_immed_func = Func::wrap(&store, move |opcode: i32| {
            let mut vec = insn2.borrow_mut();
            let imm = immed4.borrow().clone();
            vec.push(Instruction::from_opcode_imm(
                Opcode::AVMOpcode(AVMOpcode::from_number(opcode as usize).unwrap()),
                imm,
                DebugInfo::from(None),
            ));
        });

        let table_cell: Rc<RefCell<Vec<(usize, usize)>>> = Rc::new(RefCell::new(vec![]));
        let table1 = table_cell.clone();

        let cptable_func = Func::wrap(&store, move |offset: i32| {
            let mut vec = table1.borrow_mut();
            let len = insn3.borrow().len();
            vec.push((offset as usize, len));
        });

        let read_func = Func::wrap(&store, move |offset: i32| {
            let ret = cell1.borrow().read_byte(offset as u128) as i32;
            ret
        });

        let write_func = Func::wrap(&store, move |offset: i32, v: i32| {
            cell2.replace_with(|buf| buf.set_byte(offset as u128, v as u8));
        });

        let extra_write_func = Func::wrap(&store, move |offset: i32, v: i32| {
            let mut vec = extra_cell1.borrow_mut();
            let offset = offset as usize;
            if vec.len() <= offset {
                vec.resize(offset + 1, 0)
            }
            vec[offset as usize] = v as u8;
        });

        let rvec_func = Func::wrap(&store, move |ptr: i32, offset: i32, len: i32| {
            let buf = cell3.borrow();
            match &*memory_cell2.borrow() {
                None => panic!("Wasm error: no memory"),
                Some(memory) => {
                    let mut tmp = vec![];
                    for i in offset..offset + len {
                        tmp.push(buf.read_byte(i as u128));
                    }
                    memory
                        .write(ptr as usize, &tmp)
                        .expect("cannot write memory");
                }
            }
        });

        let wvec_func =
            Func::wrap(
                &store,
                move |ptr: i32, offset: i32, len: i32| match &*memory_cell3.borrow() {
                    None => panic!("Wasm error: no memory"),
                    Some(memory) => {
                        let mut tmp = vec![0; len as usize];
                        memory
                            .read(ptr as usize, &mut tmp)
                            .expect("cannot read memory");

                        cell4.replace_with(|buf| {
                            let mut res = buf.clone();
                            for i in 0..len {
                                res = res.set_byte((offset + i) as u128, tmp[i as usize]);
                            }
                            res
                        });
                    }
                },
            );

        let len_func = Func::wrap(&store, move || len1.borrow().clone() as i32);

        let set_len_func = Func::wrap(&store, move |nlen: i32| {
            len2.replace_with(|_| nlen);
        });

        let callback_type = FuncType::new([ValType::I32].iter().cloned(), [].iter().cloned());
        let gas_func = Func::new(&store, callback_type, move |_, args, _results| {
            let gas_used = args[0].unwrap_i32();
            let gas = gas1.borrow().clone();
            if gas_used > gas {
                gas1.replace_with(|_gas| 0);
                Err(wasmtime::Trap::new("out of gas"))
            } else {
                gas1.replace_with(|gas| *gas - gas_used);
                Ok(())
            }
        });

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
                    } else if name.contains("usegas") {
                        imports.push(gas_func.clone().into())
                    } else if name.contains("rvec") {
                        imports.push(rvec_func.clone().into())
                    } else if name.contains("wvec") {
                        imports.push(wvec_func.clone().into())
                    } else if name.contains("wextra") {
                        imports.push(extra_write_func.clone().into())
                    } else if name.contains("uintimmed") {
                        imports.push(uint_immed_func.clone().into())
                    } else if name.contains("specialimmed") {
                        imports.push(special_immed_func.clone().into())
                    } else if name.contains("globalimmed") {
                        imports.push(global_immed_func.clone().into())
                    } else if name.contains("tableimmed") {
                        imports.push(table_immed_func.clone().into())
                    } else if name.contains("pushimmed") {
                        imports.push(push_immed_func.clone().into())
                    } else if name.contains("pushinst") {
                        imports.push(push_inst_func.clone().into())
                    } else if name.contains("cptable") {
                        imports.push(cptable_func.clone().into())
                    } else if name.contains("tuplebytes") {
                        imports.push(tuplebytes_func.clone().into())
                    } else if name.contains("tuple2bytes") {
                        imports.push(tuple2bytes_func.clone().into())
                    } else if name.contains("tuple2buffer") {
                        imports.push(tuple2buffer_func.clone().into())
                    } else {
                        imports.push(error_func.clone().into())
                    }
                }
                (ExternType::Func(_), None) => imports.push(error_func.clone().into()),
                _ => {}
            }
        }

        let instance = Instance::new(&store, &module, &imports).unwrap();

        let memory = instance.get_memory("memory");
        memory_cell.replace_with(|_prev| memory);

        return JitWasm {
            instance,
            cell,
            len_cell,
            gas_cell,
            extra_cell,
            immed_cell,
            insn_cell,
            table_cell,
        };
    }

    pub fn run_immed(
        &self,
        buf: Buffer,
        len: usize,
        v: Value,
    ) -> (
        Buffer,
        Vec<u8>,
        usize,
        u64,
        Vec<Instruction>,
        Vec<(usize, usize)>,
    ) {
        self.cell.replace_with(|_buf| buf);
        self.len_cell.replace_with(|_len| len as i32);
        self.immed_cell.replace_with(|_buf| v);
        // self.gas_cell.replace_with(|_gas| 1000000000);
        self.gas_cell.replace_with(|_gas| 1000000);

        let _res = match self.instance.get_typed_func::<(), (i32)>("test") {
            Ok(f) => get_answer(f) as i64,
            Err(_) => 0,
        };

        (
            self.cell.borrow().clone(),
            self.extra_cell.borrow().clone(),
            self.len_cell.borrow().clone() as usize,
            self.gas_cell.borrow().clone() as u64,
            self.insn_cell.borrow().clone(),
            self.table_cell.borrow().clone(),
        )
    }

    pub fn run(
        &self,
        buf: Buffer,
        len: usize,
    ) -> (
        Buffer,
        Vec<u8>,
        usize,
        u64,
        Vec<Instruction>,
        Vec<(usize, usize)>,
    ) {
        self.cell.replace_with(|_buf| buf);
        self.len_cell.replace_with(|_len| len as i32);
        self.gas_cell.replace_with(|_gas| 1000000000);

        let _res = match self.instance.get_typed_func::<(), (i32)>("test") {
            Ok(f) => get_answer(f) as i64,
            Err(_) => 0,
        };

        (
            self.cell.borrow().clone(),
            self.extra_cell.borrow().clone(),
            self.len_cell.borrow().clone() as usize,
            self.gas_cell.borrow().clone() as u64,
            self.insn_cell.borrow().clone(),
            self.table_cell.borrow().clone(),
        )
    }
}
