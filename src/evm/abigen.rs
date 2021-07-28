/*
 * Copyright 2021, Offchain Labs, Inc. All rights reserved.
 */

use crate::evm::AbiForContract;
use ethabi::ParamType;
use ethers_core::utils::keccak256;
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io;
use std::io::{ErrorKind, Write};
use std::path::Path;

pub fn abigen_from_directory_structure(dirpath: &Path, out_dir: &Path) -> Result<(), io::Error> {
    for entry in fs::read_dir(dirpath)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            for subentry in fs::read_dir(path)? {
                let subentry = subentry?;
                let subpath = subentry.path();
                if !subpath
                    .clone()
                    .into_os_string()
                    .to_str()
                    .unwrap()
                    .ends_with(".dbg.json")
                {
                    write_mini_wrapper_to_file(
                        &subpath,
                        &out_dir.join(format!(
                            "{}.mini",
                            subpath.file_stem().unwrap().to_str().unwrap()
                        )),
                    )?;
                }
            }
        }
    }
    Ok(())
}

pub fn write_mini_wrapper_to_file(contract_path: &Path, out_path: &Path) -> Result<(), io::Error> {
    println!(
        "{} --> {}",
        contract_path.to_str().unwrap(),
        out_path.to_str().unwrap()
    );
    generate_mini_wrapper(contract_path, &mut File::create(out_path)?)
}

pub fn generate_mini_wrapper(
    contract_path: &Path,
    out: &mut dyn io::Write,
) -> Result<(), io::Error> {
    let (contract, contract_name) = {
        let afc = AbiForContract::new_from_file(contract_path.to_str().to_owned().unwrap())
            .map_err(|e| io::Error::new(ErrorKind::Other, e.to_string()))?;
        let cname = afc.name.clone();
        (afc.contract, cname[1..(cname.len() - 1)].to_string())
    };
    let functions: BTreeMap<_, _> = contract.functions.into_iter().collect();

    let mut header_out: Vec<u8> = Vec::new();
    let mut dispatch_func_out: Vec<u8> = Vec::new();
    let mut wrappers_out: Vec<u8> = Vec::new();

    // write start of header
    writeln!(
        header_out,
        "//\n// This is machine-generated code. Don't modify it unless you know what you're doing."
    )?;
    writeln!(
        header_out,
        "//\n// Copyright 2020, Offchain Labs, Inc. All rights reserved.\n//"
    )?;
    writeln!(header_out, "")?;

    for import_type in &["evmCallStack::EvmCallFrame", "std::bytearray::ByteArray"] {
        writeln!(header_out, "use {};", import_type)?;
    }
    writeln!(header_out, "")?;

    for import_func in &[
        "evmOps::evmOp_return",
        "evmOps::evmOp_revert_knownPc",
        "evmCallStack::evmCallStack_topFrame",
        "evmCallStack::evmCallFrame_getCalldata",
        "evmCallStack::evmCallStack_setTopFrameMemory",
        "std::bytearray::bytearray_new",
        "std::bytearray::bytearray_size",
        "std::bytearray::bytearray_get256",
        "std::bytearray::bytearray_set256",
        "std::bytearray::bytearray_copy",
        "std::bytearray::bytearray_extract",
    ] {
        writeln!(header_out, "use {};", import_func)?;
    }
    writeln!(header_out, "")?;

    macro_rules! wln {
        ($($line:tt)+) => {
            writeln!(dispatch_func_out, $($line)+)?
        };
    }
    macro_rules! w {
        ($($line:tt)+) => {
            write!(out, "{}", $($line)+)?
        };
    }

    // write start of dispatcher
    let dispatch_func_name = contract_name.clone() + "__dispatch";
    wln!("public impure func {}() {{", dispatch_func_name);
    wln!("    if let Some(topFrame) = evmCallStack_topFrame() {{");
    wln!("        let calldata = evmCallFrame_getCalldata(topFrame);");
    wln!("        if (bytearray_size(calldata) < 4) {{");
    wln!("            evmOp_revert_knownPc(0, 0, 0);");
    wln!("        }}");
    wln!("        let funcCode = asm(224, bytearray_get256(calldata, 0)) uint {{ shr }};");

    write_dispatch_if_statement(
        &mut header_out,
        &mut dispatch_func_out,
        &mut wrappers_out,
        contract_name,
        functions,
    )?;

    // write end of dispatcher
    wln!("    }} else {{");
    wln!("        evmOp_revert_knownPc(2, 0, 0);");
    wln!("    }}");
    wln!("}}");
    wln!("");

    w!(std::str::from_utf8(&*header_out)
        .map_err(|_| io::Error::new(ErrorKind::Other, "UTF8 error"))?);
    w!(std::str::from_utf8(&*dispatch_func_out)
        .map_err(|_| io::Error::new(ErrorKind::Other, "UTF8 error"))?);
    w!(std::str::from_utf8(&*wrappers_out)
        .map_err(|_| io::Error::new(ErrorKind::Other, "UTF8 error"))?);

    Ok(())
}

fn write_dispatch_if_statement(
    header_out: &mut dyn io::Write,
    dispatch_func_out: &mut dyn io::Write,
    wrappers_out: &mut dyn io::Write,
    contract_name: String,
    functions: BTreeMap<String, Vec<ethabi::Function>>,
) -> Result<(), io::Error> {
    let mut first_one = true;
    for (_, func_list) in functions {
        assert_eq!(func_list.len(), 1);
        let func = &func_list[0];
        let wrapper_name = contract_name.clone() + "_" + &func.name + "_wrapper";

        write_dispatch_for_func(dispatch_func_out, func, wrapper_name.clone(), first_one)?;
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
        write!(out, "    {}_{}(topFrame", contract_name, func.name)?;
        for param in params {
            write!(out, ", {}", param.name)?;
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
            let (param_selector, param_name) = if rets.len() == 0 {
                ("ret".to_string(), "ret".to_string())
            } else {
                (format!("ret.{}", i), format!("ret_{}", i))
            };
            if let Some(delmar) =
                write_put_param_into_mem(out, &ret.kind, param_selector, param_name)?
            {
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
    Bytearray(String, String), // param_selector, param_name
    Multi(Vec<DelayedMarshalSpec>),
}

impl DelayedMarshalSpec {
    fn write_put_into_mem(&self, out: &mut dyn io::Write) -> Result<(), io::Error> {
        match self {
            DelayedMarshalSpec::Bytearray(param_selector, param_name) => {
                writeln!(
                    out,
                    "    let {}_size = bytearray_size({});",
                    param_name, param_selector
                )?;
                writeln!(
                    out,
                    "    let {}_size_rounded = 32 * (({}_size + 31)/32);",
                    param_name, param_name
                )?;
                writeln!(
                    out,
                    "    mem = bytearray_set256(mem, __offset_{}, __offset);",
                    param_name
                )?;
                writeln!(
                    out,
                    "    mem = bytearray_set256(mem, __offset, {}_size);",
                    param_name
                )?;
                writeln!(
                    out,
                    "    mem = bytearray_copy({}, 0, mem, 32+__offset, {}_size_rounded);",
                    param_selector, param_name
                )?;
                writeln!(
                    out,
                    "    __offset = __offset + 32 + {}_size_rounded;",
                    param_name
                )?;
                Ok(())
            }
            DelayedMarshalSpec::Multi(v) => {
                for delmar in v {
                    delmar.write_put_into_mem(out)?;
                }
                Ok(())
            }
        }
    }
}

fn write_put_param_into_mem(
    out: &mut dyn io::Write,
    kind: &ParamType,
    param_selector: String,
    param_name: String,
) -> Result<Option<DelayedMarshalSpec>, io::Error> {
    match kind {
        ParamType::Address | ParamType::Int(_) => {
            writeln!(
                out,
                "    mem = bytearray_set256(mem, __offset, uint({}));",
                param_selector
            )?;
            writeln!(out, "    __offset = __offset + 32;")?;
        }
        ParamType::Bytes | ParamType::String => {
            writeln!(out, "    let __offset_{} = __offset;", param_name)?;
            writeln!(out, "    __offset = 32 + __offset;")?;
            return Ok(Some(DelayedMarshalSpec::Bytearray(
                param_selector,
                param_name,
            )));
        }
        ParamType::FixedBytes(sz) => {
            if *sz == 32 {
                writeln!(
                    out,
                    "    mem = bytearray_set256(mem, __offset, uint({}));",
                    param_selector
                )?;
                writeln!(out, "    __offset = __offset + 32;")?;
            } else {
                return Err(io::Error::new(
                    ErrorKind::Other,
                    "FixedBytes parameter must have size 32",
                ));
            }
        }
        ParamType::Uint(_) => {
            writeln!(
                out,
                "    mem = bytearray_set256(mem, __offset, {});",
                param_selector
            )?;
            writeln!(out, "    __offset = __offset + 32;")?;
        }
        ParamType::Bool => {
            writeln!(
                out,
                "    mem = bytearray_set256(mem, __offset, xif ({}) {{ 1 }} else {{ 0 }});",
                param_selector
            )?;
            writeln!(out, "    __offset = __offset + 32;")?;
        }
        ParamType::Array(_) => {
            return Err(io::Error::new(
                ErrorKind::Other,
                "ABI encoding of arrays not yet implemented",
            ));
        }
        ParamType::FixedArray(_, _) => {
            return Err(io::Error::new(
                ErrorKind::Other,
                "ABI encoding of fixed-arrays not yet implemented",
            ));
        }
        ParamType::Tuple(tup) => {
            let mut marshes = vec![];
            for (i, subkind) in tup.iter().enumerate() {
                if let Some(marsh) = write_put_param_into_mem(
                    out,
                    subkind,
                    format!("{}.{}", param_selector, i),
                    format!("{}_{}", param_name, i),
                )? {
                    marshes.push(marsh);
                }
            }
            return Ok(if marshes.len() == 0 {
                None
            } else {
                Some(DelayedMarshalSpec::Multi(marshes))
            });
        }
    }
    Ok(None)
}
