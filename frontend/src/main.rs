use core::panic;
use std::fs;

use frontend::ast::{nameresolution::NameResolver, Accept};
use frontend::c_grammar;

fn driver(path: &str) {
  let input = match fs::read_to_string(path) {
    Ok(contents) => contents,
    Err(e) => panic!("Error reading file."),
  };

  let prog = match c_grammar::ProgramParser::new().parse(&input) {
    Ok(prog) => prog,
    Err(e) => {
      panic!("Error parsing input: {}", e);
    }
  };
  //dbg!(&prog);
  let name_resolved_ast = prog.accept(&mut NameResolver::new());
  dbg!(&name_resolved_ast);
  //let loop_labeled_ast = name_resolved_ast.accept(&mut LoopLabeler::new());
  //dbg!(&loop_labeled_ast);
  //Ok(loop_labeled_ast)
}

fn main() {
  let code = driver("blah.c");
}
