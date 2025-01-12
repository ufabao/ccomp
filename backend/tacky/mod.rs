pub mod tacky;
use std::collections::HashMap;

use super::ast::{self, Visitor};
use tacky::{BinaryOp, Function, Instruction, Program, UnaryOp, Val};

#[derive(Debug)]
pub struct TacVisitor {
  instructions: Vec<Instruction>,
  loop_map: HashMap<String, (String, String)>,
  temp_counter: usize,
  label_counter: usize,
}

impl TacVisitor {
  pub fn new() -> Self {
    TacVisitor {
      instructions: Vec::new(),
      loop_map: HashMap::new(),
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

  fn new_loop(&mut self, label: String) -> (String, String) {
    let continue_label = self.new_label("continue_");
    let break_label = self.new_label("break_");
    self
      .loop_map
      .insert(label, (continue_label.clone(), break_label.clone()));
    (continue_label, break_label)
  }

  fn get_loop(&self, label: &str) -> Option<&(String, String)> {
    self.loop_map.get(label)
  }

  fn add_instruction(&mut self, instruction: Instruction) {
    self.instructions.push(instruction);
  }

  fn build(self) -> Vec<Instruction> {
    self.instructions
  }
}

impl Visitor for TacVisitor {
  type Program = Program;
  type FunctionDecl = Vec<Instruction>;
  type VariableDecl = ();
  type BlockItem = ();
  type Declaration = ();
  type Statement = ();
  type ForInit = ();
  type Expression = Val;

  fn visit_program(&mut self, program: &ast::Program) -> Self::Program {
    match program {
      ast::Program::Program(functions) => {
        let instructions = self.visit_function(func);
        Program::Func(Function {
          name: func.name.clone(),
          instructions,
        })
      }
    }
  }

  fn visit_function(&mut self, func: &ast::Function) -> Self::Function {
    for item in &func.body.items {
      self.visit_block_item(item);
    }
    std::mem::take(&mut self.instructions)
  }

  fn visit_block_item(&mut self, block_item: &ast::BlockItem) -> Self::BlockItem {
    match block_item {
      ast::BlockItem::Statement(stmt) => self.visit_statement(stmt),
      ast::BlockItem::Declaration(decl) => self.visit_declaration(decl),
    }
  }

  fn visit_declaration(&mut self, decl: &ast::Declaration) -> Self::Declaration {
    if let Some(exp) = &decl.exp {
      let val = self.visit_expression(exp);
      self.add_instruction(Instruction::Copy(val, Val::Var(decl.name.clone())));
    }
  }

  fn visit_statement(&mut self, stmt: &ast::Statement) -> Self::Statement {
    match stmt {
      ast::Statement::Return(expression) => {
        let val = self.visit_expression(expression);
        self.add_instruction(Instruction::Return(val));
      }
      ast::Statement::Expression(expression) => {
        self.visit_expression(expression);
      }
      ast::Statement::If(expression, then_stmt, else_stmt) => {
        let cond = self.visit_expression(expression);
        let temp = self.new_temp();
        self.add_instruction(Instruction::Copy(cond, temp.clone()));

        let false_label = self.new_label("if_false");
        self.add_instruction(Instruction::JumpIfZero(temp, false_label.clone()));

        self.visit_statement(then_stmt);

        if let Some(else_statement) = else_stmt {
          let end_label = self.new_label("end_");
          self.add_instruction(Instruction::Jump(end_label.clone()));
          self.add_instruction(Instruction::Label(false_label));
          self.visit_statement(else_statement);
          self.add_instruction(Instruction::Label(end_label));
        } else {
          self.add_instruction(Instruction::Label(false_label));
        }
      }
      ast::Statement::For(init, exp1, exp2, stmt, name) => {
        self.visit_for_init(init);
        let (continue_label, break_label) = self.new_loop(name.clone());
        self.add_instruction(Instruction::Label(continue_label.clone()));
        if let Some(exp1) = exp1 {
          let cond = self.visit_expression(exp1);
          let temp = self.new_temp();
          self.add_instruction(Instruction::Copy(cond, temp.clone()));
          self.add_instruction(Instruction::JumpIfZero(temp, break_label.clone()));
        }
        self.visit_statement(stmt);
        exp2.as_ref().map(|e| self.visit_expression(e));
        self.add_instruction(Instruction::Jump(continue_label));
        self.add_instruction(Instruction::Label(break_label));
      }
      ast::Statement::While(exp, stmt, name) => {
        let (continue_label, break_label) = self.new_loop(name.clone());
        self.add_instruction(Instruction::Label(continue_label.clone()));
        let cond = self.visit_expression(exp);
        let temp = self.new_temp();
        self.add_instruction(Instruction::Copy(cond, temp.clone()));
        self.add_instruction(Instruction::JumpIfZero(temp, break_label.clone()));
        self.visit_statement(stmt);
        self.add_instruction(Instruction::Jump(continue_label));
        self.add_instruction(Instruction::Label(break_label));
      }
      ast::Statement::Break(name) => {
        let (_, break_label) = self.get_loop(name).unwrap_or_else(|| {
          panic!("Break statement outside of loop");
        });
        self.add_instruction(Instruction::Jump(break_label.to_string()));
      }
      ast::Statement::Continue(name) => {
        let (continue_label, _) = self.get_loop(name).unwrap_or_else(|| {
          panic!("Continue statement outside of loop");
        });
        self.add_instruction(Instruction::Jump(continue_label.to_string()));
      }
      ast::Statement::Compound(block) => {
        for item in &block.items {
          self.visit_block_item(item);
        }
      }
      ast::Statement::Null => {}
    }
  }

  fn visit_for_init(&mut self, init: &ast::ForInit) -> Self::ForInit {
    match init {
      ast::ForInit::Declaration(declaration) => {
        self.visit_declaration(declaration);
      }
      ast::ForInit::Expression(expression) => {
        if let Some(expression) = expression {
          self.visit_expression(expression);
        }
      }
    }
  }

  fn visit_expression(&mut self, expr: &ast::Expression) -> Self::Expression {
    match expr {
      ast::Expression::Int(n) => Val::Int(*n),
      ast::Expression::Var(name) => Val::Var(name.clone()),
      ast::Expression::Unary(op, expression) => {
        let val = self.visit_expression(expression);
        let temp = self.new_temp();
        self.add_instruction(Instruction::Unary(
          self.unary_name_match(*op),
          val,
          temp.clone(),
        ));
        temp
      }
      ast::Expression::Binary(op, lhs, rhs) => match op {
        ast::BinaryOp::And => self.handle_and_expression(lhs, rhs),
        ast::BinaryOp::Or => self.handle_or_expression(lhs, rhs),
        _ => self.handle_regular_binary_expression(*op, lhs, rhs),
      },
      ast::Expression::Assignment(lhs, rhs) => {
        let var_name = match &**lhs {
          ast::Expression::Var(name) => name,
          _ => unreachable!(),
        };
        let result = self.visit_expression(rhs);
        self.add_instruction(Instruction::Copy(
          result.clone(),
          Val::Var(var_name.clone()),
        ));
        result
      }
      ast::Expression::Conditional(_, _, _) => todo!(),
    }
  }
}

// Helper methods for TacVisitor
impl TacVisitor {
  fn handle_and_expression(&mut self, lhs: &ast::Expression, rhs: &ast::Expression) -> Val {
    let val1 = self.visit_expression(lhs);
    let false_label = self.new_label("and_false");
    self.add_instruction(Instruction::JumpIfZero(val1, false_label.clone()));

    let val2 = self.visit_expression(rhs);
    self.add_instruction(Instruction::JumpIfZero(val2, false_label.clone()));

    let result = self.new_temp();
    self.add_instruction(Instruction::Copy(Val::Int(1), result.clone()));

    let end_label = self.new_label("end_");
    self.add_instruction(Instruction::Jump(end_label.clone()));

    self.add_instruction(Instruction::Label(false_label));
    self.add_instruction(Instruction::Copy(Val::Int(0), result.clone()));
    self.add_instruction(Instruction::Label(end_label));

    result
  }

  fn handle_or_expression(&mut self, lhs: &ast::Expression, rhs: &ast::Expression) -> Val {
    let val1 = self.visit_expression(lhs);
    let true_label = self.new_label("or_true");
    self.add_instruction(Instruction::JumpIfNotZero(val1, true_label.clone()));

    let val2 = self.visit_expression(rhs);
    self.add_instruction(Instruction::JumpIfNotZero(val2, true_label.clone()));

    let result = self.new_temp();
    self.add_instruction(Instruction::Copy(Val::Int(0), result.clone()));

    let end_label = self.new_label("end_");
    self.add_instruction(Instruction::Jump(end_label.clone()));

    self.add_instruction(Instruction::Label(true_label));
    self.add_instruction(Instruction::Copy(Val::Int(1), result.clone()));
    self.add_instruction(Instruction::Label(end_label));

    result
  }

  fn handle_regular_binary_expression(
    &mut self,
    op: ast::BinaryOp,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
  ) -> Val {
    let val1 = self.visit_expression(lhs);
    let val2 = self.visit_expression(rhs);
    let temp = self.new_temp();
    self.add_instruction(Instruction::Binary(
      self.binary_name_match(op),
      val1,
      val2,
      temp.clone(),
    ));
    temp
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

// // Implementation of ToTac trait using the visitor pattern
// impl ToTac for &ast::Program {
//   type Output = tacky::Program;

//   fn to_tac(self) -> Self::Output {
//     let mut visitor = TacVisitor::new();
//     self.accept(&mut visitor)
//   }
// }
