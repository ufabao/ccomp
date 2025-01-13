use lalrpop_util::lalrpop_mod;
pub mod ast;
lalrpop_mod!(pub c_grammar);
use core::panic;
use std::fs;

use ast::{
  looplabeling::LoopLabeler, nameresolution::NameResolver, typechecker::TypeChecker, Accept,
};

pub fn front_end_passes(path: &str) -> Result<ast::Program, String> {
  let input = match fs::read_to_string(path) {
    Ok(contents) => contents,
    Err(_) => panic!("Error reading file."),
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

  let type_checked = prog
    .accept(&mut TypeChecker::new())
    .map_err(|e| format!("{:?}", e));
  dbg!(&type_checked);

  let loop_labeled_ast = name_resolved_ast.accept(&mut LoopLabeler::new());
  dbg!(&loop_labeled_ast);

  Ok(loop_labeled_ast)
}

pub fn parse(input: &str) -> Result<ast::Program, String> {
  match c_grammar::ProgramParser::new().parse(input) {
    Ok(prog) => Ok(prog),
    Err(e) => Err(format!("Parse error: {}", e)),
  }
}

// Create a simple main.rs to test the parser
#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basic_parse() {
    let input = "int main() { return 42; }";
    let result = parse(input);
    assert!(result.is_ok());
  }
}
