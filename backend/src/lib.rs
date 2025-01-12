pub mod asm;
pub mod tacky;

pub fn generate_code(ast: &ccomp_ast::Program) -> String {
  let tac = ast.accept(&mut tacky::TacVisitor::new());
  let asm = asm::tacky_to_asm_ast(&tac);
  let mut passes = asm::assemblyast::ASMPasses::default();
  let replaced_asm = passes.replace_pseudo(&asm);
  let final_asm = passes.final_pass(&replaced_asm);
  asm::codegen::generate_assembly(&final_asm)
}

pub use asm::compile_and_run;
