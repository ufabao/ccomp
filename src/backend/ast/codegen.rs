use crate::backend::ast::ast::{Expression, Function, Program, Statement, UnaryOp};
use std::fmt::Write;

pub struct CodeGenerator {
    output: String,
    globals: Vec<String>,
}

pub fn generate_assembly(prog: &Program) -> Result<String, std::fmt::Error> {
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

    pub fn generate(&mut self, prog: &Program) -> Result<String, std::fmt::Error> {
        match prog {
            Program::Func(func) => self.generate_func(func)?,
        }
        Ok(self.output.clone())
    }

    fn generate_func(&mut self, func: &Function) -> Result<(), std::fmt::Error> {
        self.globals.push(func.name.clone());
        writeln!(self.output, "{}:", func.name)?;

        writeln!(self.output, "\tpushq %rbp")?;
        writeln!(self.output, "\tmovq %rsp, %rbp")?;

        match &func.body {
            Statement::Return(expr) => self.generate_expression(expr)?,
        }

        writeln!(self.output, "\tpopq %rbp")?;
        writeln!(self.output, "\tret")?;
        Ok(())
    }

    fn generate_expression(&mut self, expr: &Expression) -> Result<(), std::fmt::Error> {
        match expr {
            Expression::Int(value) => {
                writeln!(self.output, "\tmovl ${}, %eax", value)?;
            }
            Expression::Unary(op, expr) => {
                self.generate_expression(expr)?;
                match op {
                    UnaryOp::Neg => {
                        writeln!(self.output, "\tnegl %eax")?;
                    }
                    UnaryOp::PrefixDec => {
                        writeln!(self.output, "\tdecl %eax")?;
                    }
                    UnaryOp::Complement => {
                        writeln!(self.output, "\tnotl %eax")?;
                    }
                }
            }
        }
        Ok(())
    }
}
