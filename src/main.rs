use ccomp::backend::asm::assemblyast;
use ccomp::backend::asm::assemblyast::tacky_to_asm_ast;
use ccomp::backend::asm::codegen::generate_assembly;
use ccomp::backend::tacky::ToTac;
use ccomp::frontend::c_grammar;
use std::fs;

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

    let tac = prog.to_tac();
    let asm = tacky_to_asm_ast(&tac);
    let mut passes = assemblyast::TACPasses::default();
    let replaced_asm = passes.replace_pseudo(&asm);
    let final_asm = passes.final_pass(&replaced_asm);
    let code = generate_assembly(&final_asm);

    Ok(code)
}

fn main() {
    let code = driver("test_programs/remainder.c");
    println!("{}", code.unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::process::Command;
    use tempfile::tempdir;

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
}
