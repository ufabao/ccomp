pub mod tacky;
use std::{collections::HashMap, mem};
use tacky::{BinaryOp, Function, Instruction, Program, UnaryOp, Val};

use super::ast::ast;

#[derive(Debug, Default)]
struct TacBuilder {
    instructions: Vec<Instruction>,
    temp_counter: usize,
}

impl TacBuilder {
    fn new() -> Self {
        TacBuilder {
            instructions: Vec::new(),
            temp_counter: 0,
        }
    }

    fn new_temp(&mut self) -> Val {
        let temp = Val::Var(format!("t{}", self.temp_counter));
        self.temp_counter += 1;
        temp
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn build(self) -> Vec<Instruction> {
        self.instructions
    }
}

#[derive(Debug, Default)]
struct AstToTacTransformer {
    builder: TacBuilder,
}

impl AstToTacTransformer {
    fn new() -> Self {
        Self {
            builder: TacBuilder::new(),
        }
    }

    fn transform_program(mut self, program: &ast::Program) -> Program {
        match program {
            ast::Program::Func(func) => {
                let instructions = self.transform_function(func);
                Program::Func(Function {
                    name: func.name.clone(),
                    instructions,
                })
            }
        }
    }

    fn transform_function(&mut self, func: &ast::Function) -> Vec<Instruction> {
        self.transform_statement(&func.body);
        mem::take(&mut self.builder).build()
    }

    fn transform_statement(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Return(expression) => {
                let val = self.transform_expression(expression);
                self.builder.add_instruction(Instruction::Return(val));
            }
        }
    }

    fn transform_expression(&mut self, expr: &ast::Expression) -> Val {
        match expr {
            ast::Expression::Int(n) => Val::Int(*n),
            ast::Expression::Unary(op, expression) => {
                let val = self.transform_expression(&expression);
                let temp = self.builder.new_temp();
                self.builder.add_instruction(Instruction::Unary(
                    self.transform_unary_op(*op),
                    val,
                    temp.clone(),
                ));
                temp
            }
            ast::Expression::Binary(binary_op, expression1, expression2) => {
                let val1 = self.transform_expression(&expression1);
                let val2 = self.transform_expression(&expression2);
                let result_var = self.builder.new_temp();
                self.builder.add_instruction(Instruction::Binary(
                    self.transform_binary_op(*binary_op),
                    val1,
                    val2,
                    result_var.clone(),
                ));
                result_var
            }
        }
    }

    fn transform_unary_op(&self, op: ast::UnaryOp) -> UnaryOp {
        match op {
            ast::UnaryOp::Neg => UnaryOp::Neg,
            ast::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
            ast::UnaryOp::Complement => UnaryOp::Complement,
        }
    }

    fn transform_binary_op(&self, op: ast::BinaryOp) -> BinaryOp {
        match op {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::Mod => BinaryOp::Mod,
        }
    }
}

pub trait ToTac {
    type Output;
    fn to_tac(self) -> Self::Output;
}

impl ToTac for &ast::Program {
    type Output = Program;

    fn to_tac(self) -> Self::Output {
        let transformer = AstToTacTransformer::new();
        transformer.transform_program(self)
    }
}
