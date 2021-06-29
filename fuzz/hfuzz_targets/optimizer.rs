

fn main() {
    let mut args = std::env::args();
    args.next();
    let detail = args.next().is_some();
    loop {
        honggfuzz::fuzz!(|data: &[u8]| {
            mini::fuzz_optimizer(data, detail);
        });
    }
}
