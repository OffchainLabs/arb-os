mod utils;

use wasm_bindgen::prelude::*;

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
    fn wextra(idx: i32, c: i32);
    fn uintimmed(idx: *mut u8); // pointer to uint memory location
    fn specialimmed(idx: i32);
    fn globalimmed(idx: i32);
    fn pushimmed(idx: i32);
    fn pushinst(idx: i32);
    fn cptable(idx: i32);
    fn rvec(ptr: *mut u8, idx: i32, len: i32);
    fn wvec(ptr: *mut u8, idx: i32, len: i32);
    fn tuplebytes(ptr: *mut u8, idx: i32);
    fn tuple2bytes(ptr: *mut u8, idx: i32, idx2: i32);
    fn tuple2buffer(ptr: *mut u8, idx: i32, idx2: i32, len: i32);
    fn uintimmed_int(idx: i32); // pointer to uint memory location
    fn rvec_int(ptr: i32, idx: i32, len: i32);
    fn wvec_int(ptr: i32, idx: i32, len: i32);
    fn tuplebytes_int(ptr: i32, idx: i32);
    fn tuple2bytes_int(ptr: i32, idx: i32, idx2: i32);
    fn tuple2buffer_int(ptr: i32, idx: i32, idx2: i32, len: i32);
}

#[wasm_bindgen]
pub fn test() -> u32 {
    let mode = read_buffer(0);

    let out_bad : i32 = 1 << 31;
    let mut out = vec![0u8; 32];

    match mode {
        1 => {
            // writes zro to uint
            uintimmed_int(out_bad);
        }
        2 => {
            tuplebytes_int(out_bad, 0);
        }
        3 => {
            // read byte by byte
            let mut input = vec![];
            let input_len = getlen();
            for i in 0..input_len {
                input.push(read_buffer(i) as u8)
            }
            usegas(input_len / 10 + 1);
        
            let mut output : Vec<u8> = vec![];
        
            for b in input.iter().rev() {
                output.push(*b);
            }
        
            for i in 0..output.len() {
                write_buffer(i as i32, output[i as usize] as i32)
            };
            setlen(output.len() as i32);
        }
        4 => {
            // read all
            let input_len = getlen();
            let mut input = vec![0; input_len as usize];
            rvec(input.as_mut_ptr(), 0, input_len);
            usegas(input_len / 10 + 1);
        
            let mut output : Vec<u8> = vec![];
        
            for b in input.iter().rev() {
                output.push(*b);
            }
        
            wvec(output.as_mut_ptr(), 0, output.len() as i32);
            setlen(output.len() as i32);
        }
        5 => {
            tuple2bytes(out.as_mut_ptr(), 1, 1);
        }
        6 => {
            let mut output : Vec<u8> = vec![0;32];
            tuplebytes(output.as_mut_ptr(), 0);
            wvec(output.as_mut_ptr(), 0, output.len() as i32);
        }
        7 => {
            let mut input = vec![0; 32];
            rvec(input.as_mut_ptr(), 0, 30);
            wvec(input.as_mut_ptr(), 0, 32);
        }
        8 => {
            tuplebytes(out.as_mut_ptr(), 1);
        }
        9 => {
            tuple2buffer(out.as_mut_ptr(), 2, 0, 32);
            wvec(out.as_mut_ptr(), 0, 32);
        }
        _ => {
            panic!("foo");
        }
    }
    0
}
