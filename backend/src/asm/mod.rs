pub mod assemblyast;
pub mod codegen;
mod tacky_to_asm;

use self::tacky_to_asm::AsmBuilder;
use frontend::ast::typechecker::SymbolTable;

use self::tacky_to_asm::BackendSymbols;
use crate::tacky::{self, Tac};
use std::collections::HashMap;

pub fn tac_to_asm(
  tac: &Tac,
  symbol_table: SymbolTable,
) -> (assemblyast::Program, HashMap<String, BackendSymbols>) {
  let mut asm_builder = AsmBuilder::new(symbol_table);
  let prog = asm_builder.tacky_to_asm_ast(tac);
  let backend_symbols = asm_builder.convert_symbol_table();

  (prog, backend_symbols)
}
