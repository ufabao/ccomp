use lalrpop_util::lalrpop_mod;
pub mod ast;
lalrpop_mod!(pub c_grammar);

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
