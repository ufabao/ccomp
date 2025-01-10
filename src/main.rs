use ccomp::backend::asm::assemblyast;
use ccomp::backend::asm::assemblyast::{pretty_print, tacky_to_asm_ast};
use ccomp::backend::asm::codegen::generate_assembly;
use ccomp::backend::tacky::tacky;
use ccomp::frontend::c_grammar;
use std::fs;

pub fn driver(path: &str) -> Result<String, String> {
    let input = match fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(e) => return Err("Error reading file".to_string()),
    };

    let prog = match c_grammar::ProgramParser::new().parse(&input) {
        Ok(prog) => prog,
        Err(e) => {
            return Err("Error parsing input".to_string());
        }
    };

    let tac = tacky::ast_to_tacky(&prog);
    let asm = tacky_to_asm_ast(&tac);
    let mut passes = assemblyast::TACPasses::default();
    let replaced_asm = passes.replace_pseudo(&asm);
    let final_asm = passes.final_pass(&replaced_asm);
    let code = generate_assembly(&final_asm);

    Ok(code)
}

fn main() {
    let code = driver("blah.c");
    println!("{}", code.unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    use ccomp::backend::ast::codegen::generate_assembly;
    use std::fs;
    use std::process::Command;
    use tempfile::tempdir;

    struct TestCase {
        input: &'static str,
        expected_return: i32,
    }

    fn compile_and_run(assembly: &str) -> Result<i32, Box<dyn std::error::Error>> {
        let temp_dir = tempdir()?;
        let asm_path = temp_dir.path().join("test.s");
        let exe_path = temp_dir.path().join("test");

        fs::write(&asm_path, assembly)?;

        let status = Command::new("gcc")
            .arg(asm_path.to_str().unwrap())
            .arg("-o")
            .arg(exe_path.to_str().unwrap())
            .status()?;

        if !status.success() {
            return Err("Compilation failed".into());
        }

        let output = Command::new(exe_path.to_str().unwrap()).status()?;

        Ok(output.code().unwrap_or(-1))
    }

    #[test]
    fn test_return_values() -> Result<(), Box<dyn std::error::Error>> {
        let test_cases = vec![
            TestCase {
                input: "int main() { return 2; }",
                expected_return: 2,
            },
            TestCase {
                input: "int main() { return 42; }",
                expected_return: 42,
            },
            TestCase {
                input: "int main() { return 0; }",
                expected_return: 0,
            },
        ];

        for test in test_cases {
            let ast = c_grammar::ProgramParser::new()
                .parse(test.input)
                .map_err(|e| format!("Parse error: {}", e))?;
            let assembly = generate_assembly(&ast)?;
            let return_value = compile_and_run(&assembly)?;

            assert_eq!(
                return_value, test.expected_return,
                "Program '{}' returned {}, expected {}",
                test.input, return_value, test.expected_return
            );
        }
        Ok(())
    }

    #[test]
    fn test_passes_and_negate() {
        let code = driver("test_programs/complement_negate.c");
        let return_value = compile_and_run(&code.unwrap());
        match return_value {
            Ok(val) => {
                assert_eq!(val, 1);
            }
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
    }
}
