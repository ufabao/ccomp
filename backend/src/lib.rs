extern crate frontend;
pub mod asm;
pub mod tacky;
use frontend::ast;
use tacky::TacBuilder;

pub fn generate_code(ast: &ast::Program) -> String {
  let tac = TacBuilder::new().build_program(ast);
  //dbg!(tac);
  let asm = asm::tacky_to_asm_ast(&tac);
  let mut passes = asm::assemblyast::ASMPasses::default();
  let replaced_asm = passes.replace_pseudo(&asm);
  let final_asm = passes.final_pass(&replaced_asm);
  asm::codegen::generate_assembly(&final_asm)
}
