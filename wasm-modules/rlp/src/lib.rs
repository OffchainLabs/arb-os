mod utils;

use wasm_bindgen::prelude::*;
use tiny_keccak::{Keccak, Hasher};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn read_buffer(idx: i32) -> i32;
    fn setlen(idx: i32);
    fn getlen() -> i32;
    fn write_buffer(idx: i32, c: i32);
    fn usegas(gas: i32);
    fn rvec(ptr: *mut u8, idx: i32, len: i32);
    fn wvec(ptr: *mut u8, idx: i32, len: i32);
    fn tuplebytes(ptr: *mut u8, idx: i32);
    fn tuple2bytes(ptr: *mut u8, idx: i32, idx2: i32);
    fn tuple2buffer(ptr: *mut u8, idx: i32, idx2: i32, len: i32);
}

fn handle_uint(output: &mut Vec<u8>, num: &[u8]) {
    let mut first_nzero = 32;
    for i in 0..32 {
        if num[i] != 0 {
            first_nzero = i;
            break
        }
    }
    if first_nzero == 32 {
        // the int is 0
        output.push(0x80)
    } else if first_nzero == 31 && num[31] <= 0x7f {
        output.push(num[31])
    } else {
        // needed bytes
        output.push(((32 - first_nzero) + 0x80) as u8);
        for i in first_nzero..32 {
            output.push(num[i])
        }
    }
}

fn needed_bytes(num: usize) -> u8 {
    // needed bytes
    let mut acc = num;
    for i in 0..8 {
        if acc == 0 {
            return i
        }
        acc = acc/256
    }
    return 8    
}

fn handle_bytes(output: &mut Vec<u8>, num: &[u8]) {
    let len = num.len();
    if num.len() == 0 {
        output.push(0x80)
    } else if num.len() == 1 && num[0] <= 0x7f {
        output.push(num[0])
    } else if num.len() <= 55 {
        output.push((num.len() + 0x80) as u8);
        for b in num {
            output.push(*b)
        }
    } else {
        let needed = needed_bytes(len);
        output.push(needed + 0xb7);
        for i in 0..needed {
            output.push(((len >> (8*(needed-i-1))) & 0xff) as u8)
        }
        for b in num {
            output.push(*b)
        }
    }
}

fn handle_address(output: &mut Vec<u8>, num: &[u8]) {
    let mut all_zero = true;
    for i in 0..32 {
        if num[i] != 0 {
            all_zero = false;
            break
        }
    }
    if all_zero {
        output.push(0x80)
    } else {
        handle_bytes(output, &num[12..32])
    }
}

fn read_int(num: &[u8]) -> usize {
    let mut res : usize = 0;
    for i in 28..32 {
        res = res * 256;
        res += num[i] as usize;
    }
    res
}

fn handle_len(output: &mut Vec<u8>, len: usize) {
    if len <= 55 {
        output.push(len as u8 + 0xc0)
    } else {
        let needed = needed_bytes(len);
        output.push((needed + 0xf7) as u8);
        for i in 0..needed {
            output.push(((len >> (8*(needed-i-1))) & 0xff) as u8)
        }
    }
}

pub fn process() -> Vec<u8> {
    let mut output = vec![];
    output.reserve(1024);
    let seqnum = int_from_tuple(0);
    let gasprice = int_from_tuple(1);
    let gaslimit = int_from_tuple(2);
    let address = int_from_tuple(3);
    let value = int_from_tuple(4);
    let v = int_from_bigtuple(5, 0);
    let r = int_from_bigtuple(5, 1);
    let s = int_from_bigtuple(5, 2);
    let len = read_int(&int_from_bigtuple(6, 0));
    let offset = read_int(&int_from_bigtuple(6, 1));
    let data = buffer_from_bigtuple(6, 2, len+offset); // data
    let data = &data[offset..len+offset];

    handle_uint(&mut output, &seqnum); // seqnum
    handle_uint(&mut output, &gasprice); // gas price
    handle_uint(&mut output, &gaslimit); // gas limit
    handle_address(&mut output, &address); // address
    handle_uint(&mut output, &value); // value
    handle_bytes(&mut output, data); // data
    handle_uint(&mut output, &v); // v
    handle_uint(&mut output, &r); // r
    handle_uint(&mut output, &s); // s

    let mut res = vec![];
    res.reserve(1024);
    handle_len(&mut res, output.len());
    for c in output {
        res.push(c)
    }
    res
}

fn int_from_tuple(i: i32) -> Vec<u8> {
    let mut input = vec![0; 32];
    tuplebytes(input.as_mut_ptr(), i);
    input
}

fn int_from_bigtuple(i: i32, j: i32) -> Vec<u8> {
    let mut input = vec![0; 32];
    tuple2bytes(input.as_mut_ptr(), i, j);
    input
}

fn buffer_from_bigtuple(i: i32, j: i32, len: usize) -> Vec<u8> {
    let mut input = vec![0; len];
    tuple2buffer(input.as_mut_ptr(), i, j, len as i32);
    input
}

#[wasm_bindgen]
pub fn test() -> u32 {
    /*
    let input_len = getlen();
    let mut input = vec![0; input_len as usize];

    rvec(input.as_mut_ptr(), 0, input_len);

    usegas(input_len / 10 + 1);
    */


    /*
    let data = process();
    let mut output = vec![0u8; 32];
    let mut hasher = Keccak::v256();
    hasher.update(&data[..]);
    hasher.finalize(&mut output);
    */
    let mut output = process();

    wvec(output.as_mut_ptr(), 0, output.len() as i32);
    setlen(output.len() as i32);

    0

}
