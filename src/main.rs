pub mod ast;
pub mod codegen;
use codegen::generate_assembly;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub CGrammar);

fn main() {
    let expr = CGrammar::ProgramParser::new()
        .parse("int main(){ return 2; }")
        .unwrap();

    match generate_assembly(&expr) {
        Ok(code) => println!("{}", code),
        Err(e) => eprintln!("Error: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
            let ast = CGrammar::ProgramParser::new()
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
}
