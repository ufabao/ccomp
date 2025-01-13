use frontend::ast::{
  self, BlockItem, Declaration, Expression, ForInit, FunctionDecl, Program, Statement, VariableDecl,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TacBuilder {
  instructions: Vec<Instruction>,
  loop_map: HashMap<String, (String, String)>,
  temp_counter: usize,
  label_counter: usize,
}

impl TacBuilder {
  pub fn new() -> Self {
    TacBuilder {
      instructions: Vec::new(),
      loop_map: HashMap::new(),
      temp_counter: 0,
      label_counter: 0,
    }
  }

  pub fn build_program(mut self, program: &Program) -> Result<Tac, String> {
    let functions = match program {
      Program::Program(functions) => functions
        .iter()
        .map(|f| self.build_function(f))
        .collect::<Result<Vec<_>, String>>()?
        .into_iter()
        .filter_map(|f| f)
        .collect(),
    };

    Ok(Tac { functions })
  }

  fn build_function(&mut self, func: &FunctionDecl) -> Result<Option<FunctionDef>, String> {
    if let Some(body) = &func.body {
      self.instructions.clear();
      for item in &body.items {
        self.build_block_item(item)?;
      }
      Ok(Some(FunctionDef {
        name: func.name.clone(),
        params: func.params.clone(),
        body: std::mem::take(&mut self.instructions),
      }))
    } else {
      Ok(None)
    }
  }

  fn build_block_item(&mut self, item: &BlockItem) -> Result<(), String> {
    match item {
      BlockItem::Statement(stmt) => self.build_statement(stmt)?,
      BlockItem::Declaration(decl) => self.build_declaration(decl)?,
    }
    Ok(())
  }

  fn build_declaration(&mut self, decl: &Declaration) -> Result<(), String> {
    match decl {
      Declaration::VarDeclaration(var) => {
        self.build_var_declaration(var)?;
      }
      Declaration::FuncDeclaration(funcdecl) => {
        return Err(format!(
          "Function {} declarared within a function is not support",
          funcdecl.name
        ));
      }
    }
    Ok(())
  }

  fn build_statement(&mut self, stmt: &Statement) -> Result<(), String> {
    match stmt {
      Statement::Return(expr) => {
        let val = self.build_expression(expr)?;
        self.add_instruction(Instruction::Return(val));
      }
      Statement::Expression(expr) => {
        self.build_expression(expr)?;
      }
      Statement::If(cond, then_stmt, else_stmt) => {
        self.build_if_statement(cond, then_stmt, else_stmt)?;
      }
      Statement::While(cond, body, label) => {
        self.build_while_statement(cond, body, label)?;
      }
      Statement::For(init, cond, update, body, label) => {
        self.build_for_statement(init, cond, update, body, label)?;
      }
      Statement::Break(label) => self.build_break(label)?,
      Statement::Continue(label) => self.build_continue(label)?,
      Statement::Compound(block) => {
        for item in &block.items {
          self.build_block_item(item)?;
        }
      }
      Statement::Null => {}
    }
    Ok(())
  }

  fn build_expression(&mut self, expr: &Expression) -> Result<Val, String> {
    match expr {
      Expression::Int(n) => Ok(Val::Int(*n)),
      Expression::Var(name) => Ok(Val::Var(name.clone())),
      Expression::Binary(op, lhs, rhs) => Ok(self.build_binary_expression(*op, lhs, rhs)?),
      Expression::Unary(op, expr) => Ok(self.build_unary_expression(*op, expr)?),
      Expression::Assignment(lhs, rhs) => Ok(self.build_assignment(lhs, rhs)?),
      Expression::Conditional(_, _, _) => todo!(),
      Expression::FunctionCall(name, args) => {
        let mut new_args = Vec::new();
        for arg in args {
          let temp = self.build_expression(arg)?;
          let arg = self.new_temp();
          self.add_instruction(Instruction::Copy(temp, arg.clone()));
          new_args.push(arg);
        }
        let result = self.new_temp();
        self.add_instruction(Instruction::FunctionCall(
          name.clone(),
          new_args,
          result.clone(),
        ));
        Ok(result)
      }
    }
  }

  fn build_binary_expression(
    &mut self,
    op: ast::BinaryOp,
    lhs: &Expression,
    rhs: &Expression,
  ) -> Result<Val, String> {
    match op {
      ast::BinaryOp::And => Ok(self.build_and_expression(lhs, rhs)?),
      ast::BinaryOp::Or => Ok(self.build_or_expression(lhs, rhs)?),
      _ => {
        let lhs_val = self.build_expression(lhs)?;
        let rhs_val = self.build_expression(rhs)?;
        let result = self.new_temp();
        self.add_instruction(Instruction::Binary(
          self.binary_name_match(op),
          lhs_val,
          rhs_val,
          result.clone(),
        ));
        Ok(result)
      }
    }
  }

  fn build_if_statement(
    &mut self,
    cond: &Expression,
    then_stmt: &Statement,
    else_stmt: &Option<Box<Statement>>,
  ) -> Result<(), String> {
    let cond_val = self.build_expression(cond)?;
    let false_label = self.new_label("if_false");
    self.add_instruction(Instruction::JumpIfZero(cond_val, false_label.clone()));

    self.build_statement(then_stmt)?;

    if let Some(else_stmt) = else_stmt {
      let end_label = self.new_label("end");
      self.add_instruction(Instruction::Jump(end_label.clone()));
      self.add_instruction(Instruction::Label(false_label));
      self.build_statement(else_stmt)?;
      self.add_instruction(Instruction::Label(end_label));
    } else {
      self.add_instruction(Instruction::Label(false_label));
    }
    Ok(())
  }

  // Helper methods
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

  fn build_and_expression(&mut self, lhs: &Expression, rhs: &Expression) -> Result<Val, String> {
    let val1 = self.build_expression(lhs)?;
    let false_label = self.new_label("and_false");
    self.add_instruction(Instruction::JumpIfZero(val1, false_label.clone()));

    let val2 = self.build_expression(rhs)?;
    self.add_instruction(Instruction::JumpIfZero(val2, false_label.clone()));

    let result = self.new_temp();
    self.add_instruction(Instruction::Copy(Val::Int(1), result.clone()));

    let end_label = self.new_label("end");
    self.add_instruction(Instruction::Jump(end_label.clone()));

    self.add_instruction(Instruction::Label(false_label));
    self.add_instruction(Instruction::Copy(Val::Int(0), result.clone()));
    self.add_instruction(Instruction::Label(end_label));

    Ok(result)
  }

  fn build_or_expression(&mut self, lhs: &Expression, rhs: &Expression) -> Result<Val, String> {
    let val1 = self.build_expression(lhs)?;
    let true_label = self.new_label("or_true");
    self.add_instruction(Instruction::JumpIfNotZero(val1, true_label.clone()));

    let val2 = self.build_expression(rhs)?;
    self.add_instruction(Instruction::JumpIfNotZero(val2, true_label.clone()));

    let result = self.new_temp();
    self.add_instruction(Instruction::Copy(Val::Int(0), result.clone()));

    let end_label = self.new_label("end");
    self.add_instruction(Instruction::Jump(end_label.clone()));

    self.add_instruction(Instruction::Label(true_label));
    self.add_instruction(Instruction::Copy(Val::Int(1), result.clone()));
    self.add_instruction(Instruction::Label(end_label));

    Ok(result)
  }

  fn build_unary_expression(&mut self, op: ast::UnaryOp, expr: &Expression) -> Result<Val, String> {
    let val = self.build_expression(expr)?;
    let result = self.new_temp();
    self.add_instruction(Instruction::Unary(
      self.unary_name_match(op),
      val,
      result.clone(),
    ));
    Ok(result)
  }

  fn build_assignment(&mut self, lhs: &Expression, rhs: &Expression) -> Result<Val, String> {
    let var_name = match lhs {
      Expression::Var(name) => name.clone(),
      _ => return Err("Left-hand side of assignment must be a variable".to_string()),
    };
    let result = self.build_expression(rhs)?;
    self.add_instruction(Instruction::Copy(result.clone(), Val::Var(var_name)));
    Ok(result)
  }

  fn build_while_statement(
    &mut self,
    cond: &Expression,
    body: &Statement,
    label: &str,
  ) -> Result<(), String> {
    let (continue_label, break_label) = self.new_loop(label.to_string());

    self.add_instruction(Instruction::Label(continue_label.clone()));
    let cond_val = self.build_expression(cond)?;
    self.add_instruction(Instruction::JumpIfZero(cond_val, break_label.clone()));

    self.build_statement(body)?;
    self.add_instruction(Instruction::Jump(continue_label));
    self.add_instruction(Instruction::Label(break_label));
    Ok(())
  }

  fn build_for_statement(
    &mut self,
    init: &ForInit,
    cond: &Option<Expression>,
    update: &Option<Expression>,
    body: &Statement,
    label: &str,
  ) -> Result<(), String> {
    // Build initialization
    match init {
      ForInit::Declaration(decl) => self.build_var_declaration(decl)?,
      ForInit::Expression(expr) => {
        if let Some(expr) = expr {
          self.build_expression(expr)?;
        }
      }
    }

    let (continue_label, break_label) = self.new_loop(label.to_string());
    self.add_instruction(Instruction::Label(continue_label.clone()));

    // Build condition check
    if let Some(cond) = cond {
      let cond_val = self.build_expression(cond)?;
      self.add_instruction(Instruction::JumpIfZero(cond_val, break_label.clone()));
    }

    // Build body
    self.build_statement(body)?;

    // Build update expression
    if let Some(update) = update {
      self.build_expression(update)?;
    }

    self.add_instruction(Instruction::Jump(continue_label));
    self.add_instruction(Instruction::Label(break_label));
    Ok(())
  }

  fn build_break(&mut self, label: &str) -> Result<(), String> {
    if let Some((_, break_label)) = self.loop_map.get(label) {
      self.add_instruction(Instruction::Jump(break_label.clone()));
      Ok(())
    } else {
      return Err("Break statement outside of loop".to_string());
    }
  }

  fn build_continue(&mut self, label: &str) -> Result<(), String> {
    if let Some((continue_label, _)) = self.loop_map.get(label) {
      self.add_instruction(Instruction::Jump(continue_label.clone()));
      Ok(())
    } else {
      return Err("Continue statement outside of loop".to_string());
    }
  }

  fn new_loop(&mut self, label: String) -> (String, String) {
    let continue_label = self.new_label("continue");
    let break_label = self.new_label("break");
    self
      .loop_map
      .insert(label, (continue_label.clone(), break_label.clone()));
    (continue_label, break_label)
  }

  fn unary_name_match(&self, op: ast::UnaryOp) -> UnaryOp {
    match op {
      ast::UnaryOp::Neg => UnaryOp::Neg,
      ast::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
      ast::UnaryOp::Complement => UnaryOp::Complement,
      ast::UnaryOp::Not => UnaryOp::Not,
    }
  }
  fn build_var_declaration(&mut self, var: &VariableDecl) -> Result<(), String> {
    if let Some(init) = &var.value {
      let val = self.build_expression(init)?;
      self.add_instruction(Instruction::Copy(val, Val::Var(var.name.clone())));
    }
    Ok(())
  }
}

// Keep the existing type definitions
#[derive(Debug)]
pub struct Tac {
  pub functions: Vec<FunctionDef>,
}

#[derive(Debug)]
pub struct FunctionDef {
  pub name: String,
  pub params: Vec<String>,
  pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
  Return(Val),
  Unary(UnaryOp, Val, Val),
  Binary(BinaryOp, Val, Val, Val),
  Copy(Val, Val),
  Jump(String),
  JumpIfZero(Val, String),
  JumpIfNotZero(Val, String),
  Label(String),
  FunctionCall(String, Vec<Val>, Val),
}

#[derive(Debug, Clone)]
pub enum Val {
  Int(i32),
  Var(String),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Equal,
  NotEqual,
  LessThan,
  LessOrEqual,
  GreaterThan,
  GreaterOrEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  PrefixDec,
  Complement,
  Not,
}
