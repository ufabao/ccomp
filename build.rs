extern crate lalrpop;
fn main() {
    println!("cargo:rerun-if-changed=src/frontend/c_grammar.lalrpop");

    // Process the grammar
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process_file("src/frontend/c_grammar.lalrpop")
        .unwrap();
}
