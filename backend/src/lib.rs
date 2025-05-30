extern crate frontend;
pub mod asm;
pub mod tacky;
use asm::tac_to_asm;
use frontend::ast::{typechecker::TypeInfo, Program, TypedExpression};
use std::collections::HashMap;

use tacky::generate_tac;

pub fn generate_code(
  ast: &Program<TypedExpression>,
  symbol_table: HashMap<String, TypeInfo>,
) -> Result<String, String> {
  //let tac = TacBuilder::new().build_program(ast)?;
  //dbg!(&tac);
  //let static_symbols = convert_symbols_to_tacky(&symbol_table);
  //dbg!(&static_symbols);

  let (tac, symbol_table) = generate_tac(ast, symbol_table)?;
  //dbg!(&tac);
  let (asm, symbol_table) = tac_to_asm(&tac, symbol_table);
  //dbg!(&asm);
  let mut passes = asm::assemblyast::ASMPasses::new(&symbol_table);
  let replaced_asm = passes.replace_pseudo(&asm);
  //dbg!(&replaced_asm);
  let final_asm = passes.final_pass(&replaced_asm);
  //dbg!(&final_asm);
  Ok(asm::codegen::generate_assembly(&final_asm, symbol_table))
  //Ok("".to_string())
}
