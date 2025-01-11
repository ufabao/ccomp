pub mod tacky;
use std::mem;
use tacky::{BinaryOp, Function, Instruction, Program, UnaryOp, Val};

use super::ast::ast;

#[derive(Debug, Default)]
struct TacBuilder {
  instructions: Vec<Instruction>,
  temp_counter: usize,
  label_counter: usize,
}

impl TacBuilder {
  fn new() -> Self {
    TacBuilder {
      instructions: Vec::new(),
      temp_counter: 0,
      label_counter: 0,
    }
  }

  fn new_temp(&mut self) -> Val {
    let temp = Val::Var(format!("t{}", self.temp_counter));
    self.temp_counter += 1;
    temp
  }

  fn new_label(&mut self, prefix: &str) -> String {
    let label = format!("{}{}", prefix, self.label_counter);
    self.label_counter += 1;
    label
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
      ast::Expression::Unary(op, expression) => self.transform_unary_op(*op, expression),
      ast::Expression::Binary(binary_op, expression1, expression2) => {
        self.transform_binary_op(*binary_op, expression1, expression2)
      }
    }
  }

  fn transform_binary_op(
    &mut self,
    binary_op: ast::BinaryOp,      // Changed: take by value since it's Copy
    expression1: &ast::Expression, // Keep reference since Expression is large
    expression2: &ast::Expression,
  ) -> Val {
    match binary_op {
      // And short circuits, so this is more complicated
      ast::BinaryOp::And => {
        let val1 = self.transform_expression(expression1);
        let false_label = self.builder.new_label("and_false");
        self
          .builder
          .add_instruction(Instruction::JumpIfZero(val1, false_label.clone()));
        let val2 = self.transform_expression(expression2);
        self
          .builder
          .add_instruction(Instruction::JumpIfZero(val2, false_label.clone()));
        let result = self.builder.new_temp();
        // and statemnent is true
        self
          .builder
          .add_instruction(Instruction::Copy(Val::Int(1), result.clone()));
        let end_label = self.builder.new_label("end_");
        self
          .builder
          .add_instruction(Instruction::Jump(end_label.clone()));
        self
          .builder
          .add_instruction(Instruction::Label(false_label.clone()));
        // and statement is false
        self
          .builder
          .add_instruction(Instruction::Copy(Val::Int(0), result.clone()));
        self
          .builder
          .add_instruction(Instruction::Label(end_label.clone()));
        result
      }
      // Or also short circuits, so this is complicated
      ast::BinaryOp::Or => {
        let val1 = self.transform_expression(expression1);
        let true_label = self.builder.new_label("or_true");
        self
          .builder
          .add_instruction(Instruction::JumpIfNotZero(val1, true_label.clone()));
        let val2 = self.transform_expression(expression2);
        self
          .builder
          .add_instruction(Instruction::JumpIfNotZero(val2, true_label.clone()));
        let result = self.builder.new_temp();
        self
          .builder
          .add_instruction(Instruction::Copy(Val::Int(0), result.clone()));
        let end_label = self.builder.new_label("end_");
        self
          .builder
          .add_instruction(Instruction::Jump(end_label.clone()));
        self
          .builder
          .add_instruction(Instruction::Label(true_label.clone()));
        self
          .builder
          .add_instruction(Instruction::Copy(Val::Int(1), result.clone()));
        self
          .builder
          .add_instruction(Instruction::Label(end_label.clone()));
        result
      }
      _ => {
        let val1 = self.transform_expression(expression1);
        let val2 = self.transform_expression(expression2);
        let temp = self.builder.new_temp();
        self.builder.add_instruction(Instruction::Binary(
          self.binary_name_match(binary_op), // Remove dereferencing
          val1,
          val2,
          temp.clone(),
        ));
        temp
      }
    }
  }

  fn transform_unary_op(
    &mut self,
    op: ast::UnaryOp, // Changed: take by value
    expression: &ast::Expression,
  ) -> Val {
    match op {
      _ => {
        let val = self.transform_expression(&expression);
        let temp = self.builder.new_temp();
        self.builder.add_instruction(Instruction::Unary(
          self.unary_name_match(op),
          val,
          temp.clone(),
        ));
        temp
      }
    }
  }

  fn unary_name_match(&self, op: ast::UnaryOp) -> UnaryOp {
    match op {
      ast::UnaryOp::Neg => UnaryOp::Neg,
      ast::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
      ast::UnaryOp::Complement => UnaryOp::Complement,
      ast::UnaryOp::Not => UnaryOp::Not,
    }
  }

  fn binary_name_match(&self, op: ast::BinaryOp) -> BinaryOp {
    match op {
      ast::BinaryOp::Add => BinaryOp::Add,
      ast::BinaryOp::Sub => BinaryOp::Sub,
      ast::BinaryOp::Mul => BinaryOp::Mul,
      ast::BinaryOp::Div => BinaryOp::Div,
      ast::BinaryOp::Mod => BinaryOp::Mod,
      ast::BinaryOp::And => BinaryOp::And,
      ast::BinaryOp::Or => BinaryOp::Or,
      ast::BinaryOp::Equal => BinaryOp::Equal,
      ast::BinaryOp::NotEqual => BinaryOp::NotEqual,
      ast::BinaryOp::LessThan => BinaryOp::LessThan,
      ast::BinaryOp::LessOrEqual => BinaryOp::LessOrEqual,
      ast::BinaryOp::GreaterThan => BinaryOp::GreaterThan,
      ast::BinaryOp::GreaterOrEqual => BinaryOp::GreaterOrEqual,
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
