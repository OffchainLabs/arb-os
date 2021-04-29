extern crate lalrpop;

fn main() {
    let mut config = lalrpop::Configuration::new();
    config.emit_rerun_directives(true);
    config.process_current_dir().unwrap();
}
