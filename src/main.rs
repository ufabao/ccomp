use std::fs;

use ccomp::{
  backend::{
    asm::{assemblyast, codegen::generate_assembly, tacky_to_asm_ast},
    ast::ast::{Accept, NameResolver},
    tacky::ToTac,
  },
  compile_and_run,
  frontend::c_grammar,
};

pub fn driver(path: &str) -> Result<String, String> {
  let input = match fs::read_to_string(path) {
    Ok(contents) => contents,
    Err(e) => return Err(format!("Error reading file: {}", e)),
  };

  let prog = match c_grammar::ProgramParser::new().parse(&input) {
    Ok(prog) => prog,
    Err(e) => {
      return Err(format!("Error parsing input: {}", e));
    }
  };
  //dbg!(&prog);
  let name_resolved_asm = prog.accept(&mut NameResolver::new());
  //dbg!(&name_resolved_asm);
  let tac = name_resolved_asm.to_tac();
  //dbg!(&tac);
  let asm = tacky_to_asm_ast(&tac);
  //dbg!(&asm);
  // start making passes
  let mut passes = assemblyast::ASMPasses::default();
  let replaced_asm = passes.replace_pseudo(&asm);
  //dbg!(&replaced_asm);
  let final_asm = passes.final_pass(&replaced_asm);
  //dbg!(&final_asm);
  let code = generate_assembly(&final_asm);

  Ok(code)
}

fn main() {
  let code = driver("blah.c").unwrap_or_else(|e| {
    println!("{}", e);
    std::process::exit(1);
  });

  println!("Assembly code generated: \n{}", code);
  let return_value = compile_and_run(&code);
  println!("Value returned by function: \n{:?}", return_value);
}

#[cfg(test)]
mod tests {
  use super::*;
  use ccomp::compile_and_run;

  fn test_file_returns_value(path: &str, expected: i32) {
    let code = driver(path);
    let return_value = compile_and_run(&code.unwrap());
    match return_value {
      Ok(val) => {
        assert_eq!(val, expected);
      }
      Err(e) => {
        println!("{}", e);
        assert!(false);
      }
    }
  }

  #[test]
  fn test_complement_negate() {
    test_file_returns_value("test_programs/complement_negate.c", 1);
  }

  #[test]
  fn test_addition() {
    test_file_returns_value("test_programs/addition.c", 4);
  }

  #[test]
  fn test_subtraction() {
    test_file_returns_value("test_programs/subtraction.c", 0);
  }

  #[test]
  fn test_multiply() {
    test_file_returns_value("test_programs/multiply.c", 6);
  }

  #[test]
  fn test_divide() {
    test_file_returns_value("test_programs/divide.c", 2);
  }

  #[test]
  fn test_remainder() {
    test_file_returns_value("test_programs/remainder.c", 1);
  }

  #[test]
  fn test_greater_than() {
    test_file_returns_value("test_programs/greater_than.c", 1);
  }

  #[test]
  fn test_not_greater_than() {
    test_file_returns_value("test_programs/not_greater_than.c", 0);
  }

  #[test]
  fn test_greater_or_equal() {
    test_file_returns_value("test_programs/greater_or_equal.c", 1);
  }

  #[test]
  fn test_decs_and_defs() {
    test_file_returns_value("test_programs/decs_and_defs.c", 22);
  }

  #[test]
  fn test_if_else() {
    test_file_returns_value("test_programs/if_else.c", 0);
  }

  #[test]
  fn test_if() {
    test_file_returns_value("test_programs/conditional.c", 10);
  }
}
