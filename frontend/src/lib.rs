use lalrpop_util::lalrpop_mod;
pub mod ast;
lalrpop_mod!(pub c_grammar);
use std::{collections::HashMap, fs};

use ast::{
  looplabeling::LoopLabeler,
  nameresolution::NameResolver,
  typechecker::{TypeChecker, TypeInfo},
  Accept,
};

pub fn front_end_passes(path: &str) -> Result<(ast::Program, HashMap<String, TypeInfo>), String> {
  let input = fs::read_to_string(path).map_err(|e| format!("Error reading file: {}", e))?;

  let prog = c_grammar::ProgramParser::new()
    .parse(&input)
    .map_err(|e| format!("Parse error: {}", e))?;

  //dbg!(&prog);
  let name_resolved_ast = prog.accept(&mut NameResolver::new())?;
  //dbg!(&name_resolved_ast);
  let symbol_table = TypeChecker::type_check_program(&name_resolved_ast)?;
  //dbg!(&symbol_table);

  let loop_labeled_ast = name_resolved_ast.accept(&mut LoopLabeler::new())?;
  //dbg!(&loop_labeled_ast);

  Ok((loop_labeled_ast, symbol_table))
}
