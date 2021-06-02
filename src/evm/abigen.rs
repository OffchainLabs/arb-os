/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved.
 */

use crate::evm::AbiForContract;
use ethabi::{Param, ParamType};
use ethers_core::utils::keccak256;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::{ErrorKind, Write};

pub fn write_mini_wrapper_to_file(
    contract_filename: &str,
    out_filename: &str,
) -> Result<(), io::Error> {
    generate_mini_wrapper(contract_filename, &mut File::create(out_filename)?)
}

pub fn generate_mini_wrapper(
    contract_filename: &str,
    out: &mut dyn io::Write,
) -> Result<(), io::Error> {
    let (contract, contract_name) = {
        let afc = AbiForContract::new_from_file(contract_filename)
            .map_err(|e| io::Error::new(ErrorKind::Other, e.to_string()))?;
        let cname = afc.name.clone();
        (afc.contract, cname[1..(cname.len() - 1)].to_string())
    };
    let functions = contract.functions;

    let mut header_out: Vec<u8> = Vec::new();
    let mut dispatch_func_out: Vec<u8> = Vec::new();
    let mut wrappers_out: Vec<u8> = Vec::new();

    // write start of header
    writeln!(
        header_out,
        "// This is machine-generated code. Don't modify it unless you know what you're doing."
    )?;
    writeln!(header_out, "")?;
    for import_name in &[
        "evmOp::evmOp_return",
        "evmOp::evmOp_revert_knownPc",
        "evmCallStack::evmCallStack_topFrame",
        "evmCallStack::evmCallStack_setTopFrameMemory",
        "std::bytearray::bytearray_new",
        "std::bytearray::bytearray_size",
        "std::bytearray::bytearray_get256",
        "std::bytearray::bytearray_set256",
        "std::bytearray::bytearray_copy",
    ] {
        writeln!(header_out, "use {};", import_name)?;
    }
    writeln!(header_out, "")?;

    // write start of dispatcher
    let dispatch_func_name = contract_name.clone() + "__dispatch";
    writeln!(
        dispatch_func_out,
        "public impure func {}() {{",
        dispatch_func_name
    )?;
    writeln!(
        dispatch_func_out,
        "    if let Some(topFrame) = evmCallStack_topFrame() {{"
    )?;
    writeln!(
        dispatch_func_out,
        "        let calldata = evmCallFrame_getCalldata(topFrame);"
    )?;
    writeln!(
        dispatch_func_out,
        "        if (bytearray_size(calldata) < 4) {{"
    )?;
    writeln!(
        dispatch_func_out,
        "            evmOp_revert_knownPc(0, 0, 0);"
    )?;
    writeln!(dispatch_func_out, "        }}")?;
    writeln!(
        dispatch_func_out,
        "        let funcCode = asm(224, bytearray_get256(calldata, 0)) uint {{ shr }};"
    )?;

    write_dispatch_if_statement(
        &mut header_out,
        &mut dispatch_func_out,
        &mut wrappers_out,
        contract_name,
        functions,
    )?;

    // write end of dispatcher
    writeln!(dispatch_func_out, "    }} else {{")?;
    writeln!(dispatch_func_out, "        evmOp_revert_knownPc(2, 0, 0);")?;
    writeln!(dispatch_func_out, "    }}")?;
    writeln!(dispatch_func_out, "}}")?;
    writeln!(dispatch_func_out, "")?;

    write!(
        out,
        "{}",
        std::str::from_utf8(&*header_out)
            .map_err(|_| io::Error::new(ErrorKind::Other, "UTF8 error"))?
    )?;
    write!(
        out,
        "{}",
        std::str::from_utf8(&*dispatch_func_out)
            .map_err(|_| io::Error::new(ErrorKind::Other, "UTF8 error"))?
    )?;
    write!(
        out,
        "{}",
        std::str::from_utf8(&*wrappers_out)
            .map_err(|_| io::Error::new(ErrorKind::Other, "UTF8 error"))?
    )?;

    Ok(())
}

fn write_dispatch_if_statement(
    header_out: &mut dyn io::Write,
    dispatch_func_out: &mut dyn io::Write,
    wrappers_out: &mut dyn io::Write,
    contract_name: String,
    functions: HashMap<String, Vec<ethabi::Function>>,
) -> Result<(), io::Error> {
    let mut first_one = true;
    for (_, func_list) in functions {
        assert_eq!(func_list.len(), 1);
        let func = &func_list[0];
        let wrapper_name = contract_name.clone() + "_" + &func.name + "_wrapper";

        write_dispatch_for_func(
            dispatch_func_out,
            func,
            wrapper_name.clone(),
            first_one,
        )?;
        first_one = false;

        write_header_for_func(header_out, func, contract_name.clone())?;

        write_wrapper_for_func(
            wrappers_out,
            func,
            contract_name.clone(),
            wrapper_name.clone(),
        )?;
    }
    writeln!(dispatch_func_out, "        }} else {{")?;
    writeln!(
        dispatch_func_out,
        "            evmOp_revert_knownPc(1, 0, 0);"
    )?;
    writeln!(dispatch_func_out, "        }}")?;

    writeln!(header_out, "")?;

    Ok(())
}

fn write_dispatch_for_func(
    out: &mut dyn io::Write,
    func: &ethabi::Function,
    wrapper_name: String,
    first_one: bool,
) -> Result<(), io::Error> {
    writeln!(
        out,
        "        {} (funcCode == 0x{}) {{",
        if first_one { "if" } else { "} elseif " },
        hex::encode(short_signature_for_function(func)),
    )?;
    writeln!(out, "            {}(topFrame, calldata);", wrapper_name)?;
    Ok(())
}

fn short_signature_for_function(func: &ethabi::Function) -> Vec<u8> {
    let long_sig = func.signature();
    let short_sig = long_sig.split(":").collect::<Vec<&str>>()[0];
    let long_result = keccak256(short_sig.as_bytes());
    long_result[0..4].to_vec()
}

fn write_header_for_func(
    out: &mut dyn io::Write,
    func: &ethabi::Function,
    contract_name: String,
) -> Result<(), io::Error> {
    writeln!(
        out,
        "use {}Impl::{}_{};",
        contract_name, contract_name, func.name
    )?;

    Ok(())
}

fn write_wrapper_for_func(
    out: &mut dyn io::Write,
    func: &ethabi::Function,
    contract_name: String,
    wrapper_name: String,
) -> Result<(), io::Error> {
    writeln!(
        out,
        "impure func {}(topFrame: EvmCallFrame, calldata: ByteArray) {{",
        wrapper_name
    )?;

    let params = &func.inputs;
    let rets = &func.outputs;
    if let Some(calldata_len) =
        get_calldata_length(params.into_iter().map(|p| p.kind.clone()).collect())
    {
        writeln!(
            out,
            "    if (bytearray_size(calldata) != {}) {{",
            4 + calldata_len
        )?;
        writeln!(out, "        evmOp_revert_knownPc(3, 0, 0);")?;
        writeln!(out, "    }}")?;
    }

    let mut offset = 4;
    for param in params {
        offset = offset + write_get_param_from_calldata(out, param, offset)?;
    }

    if rets.len() == 0 {
        write!(out, "    {}_{}(", contract_name, func.name)?;
        let mut first_one = true;
        for param in params {
            write!(out, "{}{}", if first_one { "" } else { ", " }, param.name)?;
            first_one = false;
        }
        writeln!(out, ");")?;
        writeln!(out, "    evmOp_return(0, 0);")?;
    } else {
        write!(out, "    let ret = {}_{}(", contract_name, func.name)?;
        let mut first_one = true;
        for param in params {
            write!(out, "{}{}", if first_one { "" } else { ", " }, param.name)?;
            first_one = false;
        }
        writeln!(out, ");")?;
        writeln!(out, "    let mem = bytearray_new(0);")?;
        writeln!(out, "    let __offset = 0;")?;
        let mut delayed_marshals = vec![];
        for (i, ret) in rets.iter().enumerate() {
            if let Some(delmar) = write_put_param_into_mem(out, ret, if rets.len() == 1 { None } else { Some(i)})? {
                delayed_marshals.push(delmar);
            }
        }
        for delmar in delayed_marshals {
            delmar.write_put_into_mem(out)?;
        }

        writeln!(out, "    if (evmCallStack_setTopFrameMemory(mem)) {{")?;
        writeln!(out, "        evmOp_return(0, __offset);")?;
        writeln!(out, "    }} else {{")?;
        writeln!(out, "        evmOp_revert(5, 0, 0);")?;
        writeln!(out, "    }}")?;
    }

    writeln!(out, "}}")?;
    writeln!(out, "")?;

    Ok(())
}

fn get_calldata_length(params: Vec<ethabi::ParamType>) -> Option<usize> {
    let mut size = 0;
    for param in params {
        size = size + get_calldata_length_one(param)?;
    }
    Some(size)
}

fn get_calldata_length_one(param: ethabi::ParamType) -> Option<usize> {
    match param {
        ParamType::Address => Some(32),
        ParamType::Bytes => None,
        ParamType::Int(_) => Some(32),
        ParamType::Uint(_) => Some(32),
        ParamType::Bool => Some(32),
        ParamType::String => None,
        ParamType::Array(_) => None,
        ParamType::FixedBytes(sz) => Some(sz),
        ParamType::FixedArray(tipe, sz) => Some(sz * get_calldata_length_one(*tipe)?),
        ParamType::Tuple(params) => get_calldata_length(params.into_iter().map(|x| *x).collect()),
    }
}

fn write_get_param_from_calldata(
    out: &mut dyn io::Write,
    param: &ethabi::Param,
    offset: usize,
) -> Result<usize, io::Error> {
    match param.kind {
        ParamType::Address => {
            writeln!(
                out,
                "    let {} = address(bytearray_get256(calldata, {}));",
                param.name, offset
            )?;
            Ok(32)
        }
        ParamType::Bytes | ParamType::String => {
            writeln!(
                out,
                "    let {}_offset = bytearray_get256(calldata, {});",
                param.name, offset
            )?;
            writeln!(
                out,
                "    let {}_nbytes = bytearray_get256(calldata, 4+{}_offset);",
                param.name, param.name
            )?;
            writeln!(
                out,
                "    let {} = bytearray_extract(calldata, 36+{}_offset, {}_nbytes);",
                param.name, param.name, param.name
            )?;
            Ok(32)
        }
        ParamType::FixedBytes(sz) => {
            if sz == 32 {
                writeln!(
                    out,
                    "    let {} = bytes32(bytearray_get256(calldata, offset));",
                    param.name
                )?;
                Ok(32)
            } else {
                Err(io::Error::new(
                    ErrorKind::Other,
                    "FixedBytes parameter must have size 32",
                ))
            }
        }
        ParamType::Uint(_) => {
            writeln!(
                out,
                "    let {} = bytearray_get256(calldata, {});",
                param.name, offset
            )?;
            Ok(32)
        }
        ParamType::Int(_) => {
            writeln!(
                out,
                "    let {} = int(bytearray_get256(calldata, {}));",
                param.name, offset
            )?;
            Ok(32)
        }
        ParamType::Bool => {
            writeln!(
                out,
                "    let {} = (bytearray_get256(calldata, {}) != 0);",
                param.name, offset
            )?;
            Ok(32)
        }
        ParamType::Array(_) => Err(io::Error::new(
            ErrorKind::Other,
            "ABI encoding of arrays not yet implemented",
        )),
        ParamType::FixedArray(_, _) => Err(io::Error::new(
            ErrorKind::Other,
            "ABI encoding of fixed-arrays not yet implemented",
        )),
        ParamType::Tuple(_) => Err(io::Error::new(
            ErrorKind::Other,
            "ABI encoding of tuples not yet implemented",
        )),
    }
}

enum DelayedMarshalSpec {
    Bytearray(String, String),   // param_selector, param_name
}

impl DelayedMarshalSpec {
    fn write_put_into_mem(&self, out: &mut dyn io::Write) -> Result<(), io::Error> {
        match self {
            DelayedMarshalSpec::Bytearray(param_selector, param_name) => {
                writeln!(out, "    let {}_size = bytearray_size({});", param_name, param_selector)?;
                writeln!(out, "    let {}_size_rounded = 32 * (({}_size + 31)/32);", param_name, param_name)?;
                writeln!(out, "    mem = bytearray_set256(mem, __offset_{}, __offset);", param_name)?;
                writeln!(out, "    mem = bytearray_set256(mem, __offset, {}_size);", param_selector)?;
                writeln!(out, "    mem = bytearray_copy({}, 0, mem, 32+__offset, {}_size_rounded);", param_selector, param_name)?;
                writeln!(out, "    __offset = __offset + 32 + {}_size_rounded;", param_selector)?;
                Ok(())
            }
        }
    }
}

fn write_put_param_into_mem(
    out: &mut dyn io::Write,
    param: &Param,
    arg_num: Option<usize>,
) -> Result<Option<DelayedMarshalSpec>, io::Error> {
    let (param_selector, param_name) = if let Some(an) = arg_num {
        (format!("ret.{}", an), format!("ret_{}", an))
    } else {
        ("ret".to_string(), "ret".to_string())
    };
    match param.kind {
        ParamType::Address | ParamType::Int(_) => {
            writeln!(out, "    mem = bytearray_set256(mem, __offset, uint({}));", param_selector)?;
            writeln!(out, "    __offset = __offset + 32;")?;
        }
        ParamType::Bytes | ParamType::String => {
            writeln!(out, "    let __offset_{} = __offset;", param_name)?;
            writeln!(out, "    __offset = 32 + __offset;")?;
            return Ok(Some(DelayedMarshalSpec::Bytearray(param_selector, param_name)));
        }
        ParamType::FixedBytes(sz) => {
            if sz == 32 {
                writeln!(out, "    mem = bytearray_set256(mem, __offset, uint({}));", param_selector)?;
                writeln!(out, "    __offset = __offset + 32;")?;
            } else {
                return Err(io::Error::new(
                    ErrorKind::Other,
                    "FixedBytes parameter must have size 32",
                ));
            }
        }
        ParamType::Uint(_) => {
            writeln!(out, "    mem = bytearray_set256(mem, __offset, {});", param_selector)?;
            writeln!(out, "    __offset = __offset + 32;")?;
        }
        ParamType::Bool => {
            writeln!(out, "    mem = bytearray_set256(mem, __offset, xif ({}) {{ 1 }} else {{ 0 }});", param_selector)?;
            writeln!(out, "    __offset = __offset + 32;")?;
        }
        ParamType::Array(_) => {
            return Err(io::Error::new(
                ErrorKind::Other,
                "ABI encoding of arrays not yet implemented",
            ));
        },
        ParamType::FixedArray(_, _) => {
            return Err(io::Error::new(
                ErrorKind::Other,
                "ABI encoding of fixed-arrays not yet implemented",
            ));
        }
        ParamType::Tuple(_) => {
            return Err(io::Error::new(
                ErrorKind::Other,
                "ABI encoding of tuples not yet implemented",
            ));
        }
    }
    Ok(None)
}
