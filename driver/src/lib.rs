use std::fs;
use std::process::Command;
use tempfile::tempdir;

pub fn compile_and_run(assembly: &str) -> Result<(i32, String), Box<dyn std::error::Error>> {
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

  let output = Command::new(exe_path.to_str().unwrap()).output()?;

  let stdout = String::from_utf8(output.stdout)?;
  let exit_code = output.status.code().unwrap_or(-1);

  Ok((exit_code, stdout))
}
