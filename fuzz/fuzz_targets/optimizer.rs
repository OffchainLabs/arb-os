#![no_main]
#![allow(unused_parens)]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    mini::fuzz_optimizer(data, true);
});
