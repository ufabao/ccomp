//use backend::asm::{assemblyast, codegen::generate_assembly, tacky_to_asm_ast};
use backend::generate_code;
use driver::compile_and_run;
use frontend::front_end_passes;

fn driver(path: &str) -> Result<String, String> {
  let (final_ast, symbol_table) = front_end_passes(path)?;
  //dbg!(&final_ast);

  let code = generate_code(&final_ast, symbol_table)?;
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
  use driver::compile_and_run;

  fn test_file_returns_value(path: &str, expected: i32) {
    let code = driver(path);
    let return_value = compile_and_run(&code.unwrap());
    match return_value {
      Ok((val, _)) => {
        assert_eq!(val, expected);
      }
      Err(e) => {
        println!("{}", e);
        assert!(false);
      }
    }
  }

  fn test_file_prints_value(path: &str, expected: &str) {
    let code = driver(path);
    let return_value = compile_and_run(&code.unwrap());
    match return_value {
      Ok((_, function_io)) => {
        assert_eq!(expected, function_io);
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

  #[test]
  fn test_nested_scopes() {
    test_file_returns_value("test_programs/nested_scope.c", 2);
  }

  #[test]
  fn test_break_statement() {
    test_file_returns_value("test_programs/break_statement.c", 7);
  }

  #[test]
  fn test_continue_statement() {
    test_file_returns_value("test_programs/nested_continue_statement.c", 10);
  }

  #[test]
  fn test_for_loops() {
    test_file_returns_value("test_programs/for_loops.c", 11);
  }

  #[test]
  fn test_function_call() {
    test_file_returns_value("test_programs/function_call.c", 6);
  }

  #[test]
  fn test_hello_world() {
    test_file_prints_value("test_programs/hello_world.c", "Hello, World!\n");
  }
}
