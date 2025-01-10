use crate::ast::{self, Func, Prog, Statement};
use std::fmt::Write;

pub struct CodeGenerator {
    output: String,
    globals: Vec<String>,
}

pub fn generate_assembly(prog: &Prog) -> Result<String, std::fmt::Error> {
    let mut codegen = CodeGenerator::new();
    codegen.generate(prog);

    let mut global_text = String::new();
    writeln!(global_text, ".text")?;
    for global in codegen.globals {
        writeln!(global_text, "\t.globl {}", global)?;
    }

    codegen.output = global_text + &codegen.output;
    Ok(codegen.output)
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            output: String::new(),
            globals: Vec::new(),
        }
    }

    pub fn generate(&mut self, prog: &Prog) -> Result<String, std::fmt::Error> {
        for func in prog {
            self.generate_func(func);
        }
        Ok(self.output.clone())
    }

    fn generate_func(&mut self, func: &Func) -> Result<(), std::fmt::Error> {
        self.globals.push(func.name.clone());
        writeln!(self.output, "{}:", func.name)?;

        writeln!(self.output, "\tpushq %rbp")?;
        writeln!(self.output, "\tmovq %rsp, %rbp")?;

        for stmt in &func.body {
            self.generate_statement(stmt)?;
        }

        if func.body.is_empty() {
            writeln!(self.output, "\tmovl $0, %eax")?;
        }

        writeln!(self.output, "\tpopq %rbp")?;
        writeln!(self.output, "\tret")?;
        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Statement) -> Result<(), std::fmt::Error> {
        match stmt {
            Statement::Return(value) => {
                writeln!(self.output, "\tmovl ${}, %eax", value)?;
            }
        }
        Ok(())
    }
}
