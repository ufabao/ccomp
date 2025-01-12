use super::*;
use std::collections::HashMap;

pub trait Visitor {
  type Program;
  type Function;
  type BlockItem;
  type Declaration;
  type Statement;
  type Expression;

  fn visit_program(&mut self, program: &Program) -> Self::Program;
  fn visit_function(&mut self, function: &Function) -> Self::Function;
  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem;
  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration;
  fn visit_statement(&mut self, statement: &Statement) -> Self::Statement;
  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression;
}

pub trait Accept {
  fn accept<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Program;
}

impl Accept for Program {
  fn accept<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Program {
    visitor.visit_program(self)
  }
}

#[derive(Default)]
pub struct NameResolver {
  scopes: HashMap<String, String>,
  errors: Vec<String>,
  var_count: usize,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      scopes: HashMap::new(),
      errors: Vec::new(),
      var_count: 0,
    }
  }

  fn declare_variable(&mut self, name: &str) {
    if self.scopes.contains_key(name) {
      self
        .errors
        .push(format!("Variable '{}' declared twice", name));
    } else {
      self.scopes.insert(
        name.to_string(),
        "user_var".to_string() + &self.var_count.to_string(),
      );
      self.var_count += 1;
    }
  }

  fn resolve_variable(&mut self, name: &str) -> Option<String> {
    match self.scopes.get(name) {
      Some(resolved_name) => Some(resolved_name.clone()),
      None => {
        self
          .errors
          .push(format!("Variable '{}' used before declared", name));
        None
      }
    }
  }
}

impl Visitor for NameResolver {
  type Program = Program;
  type Function = Function;
  type BlockItem = BlockItem;
  type Declaration = Declaration;
  type Statement = Statement;
  type Expression = Expression;

  fn visit_program(&mut self, program: &Program) -> Self::Program {
    match program {
      Program::Func(function) => Program::Func(self.visit_function(function)),
    }
  }

  fn visit_function(&mut self, function: &Function) -> Self::Function {
    Function {
      name: function.name.clone(),
      body: function
        .body
        .iter()
        .map(|bi| self.visit_block_item(bi))
        .collect(),
    }
  }

  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem {
    match block_item {
      BlockItem::Statement(stmt) => BlockItem::Statement(self.visit_statement(stmt)),
      BlockItem::Declaration(decl) => BlockItem::Declaration(self.visit_declaration(decl)),
    }
  }

  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration {
    self.declare_variable(&declaration.name);
    Declaration {
      name: self.resolve_variable(&declaration.name).unwrap().clone(),
      exp: {
        if let Some(exp) = &declaration.exp {
          Some(self.visit_expression(exp))
        } else {
          None
        }
      },
    }
  }

  fn visit_statement(&mut self, stmt: &Statement) -> Self::Statement {
    match stmt {
      Statement::Expression(expression) => Statement::Expression(self.visit_expression(expression)),
      Statement::Return(expression) => Statement::Return(self.visit_expression(expression)),
      Statement::Null => Statement::Null,
    }
  }

  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression {
    match expression {
      Expression::Int(i) => Expression::Int(*i),
      Expression::Var(name) => {
        let resolved_name = self.resolve_variable(name);
        if let Some(resolved_name) = resolved_name {
          Expression::Var(resolved_name)
        } else {
          panic!("Variable '{}' used before declared", name);
        }
      }
      Expression::Unary(op, exp) => Expression::Unary(*op, Box::new(self.visit_expression(exp))),
      Expression::Binary(op, exp1, exp2) => Expression::Binary(
        *op,
        Box::new(self.visit_expression(exp1)),
        Box::new(self.visit_expression(exp2)),
      ),
      Expression::Assignment(exp1, exp2) => {
        if let Expression::Var(_) = **exp1 {
          Expression::Assignment(
            Box::new(self.visit_expression(exp1)),
            Box::new(self.visit_expression(exp2)),
          )
        } else {
          panic!("Left hand side of assignment must be a variable");
        }
      }
    }
  }
}
