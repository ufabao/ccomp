use super::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct NameResolver {
  scopes: Vec<HashMap<String, String>>,
  errors: Vec<String>,
  var_count: usize,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      scopes: vec![HashMap::new()],
      errors: Vec::new(),
      var_count: 0,
    }
  }

  fn begin_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn end_scope(&mut self) {
    self.scopes.pop();
    if self.scopes.len() == 0 {
      panic!("Popped too many scopes");
    }
  }

  fn declare_variable(&mut self, name: &str) {
    if self.scopes.last().unwrap().contains_key(name) {
      self
        .errors
        .push(format!("Variable '{}' declared twice", name));
    } else {
      self.scopes.last_mut().unwrap().insert(
        name.to_string(),
        "user_var".to_string() + &self.var_count.to_string(),
      );
      self.var_count += 1;
    }
  }

  fn resolve_variable(&mut self, name: &str) -> Option<String> {
    for scope in self.scopes.iter().rev() {
      if let Some(resolved_name) = scope.get(name) {
        return Some(resolved_name.clone());
      }
    }
    None
  }
}

impl Visitor for NameResolver {
  type Program = Program;
  type Function = Function;
  type BlockItem = BlockItem;
  type Declaration = Declaration;
  type Statement = Statement;
  type ForInit = ForInit;
  type Expression = Expression;

  fn visit_program(&mut self, program: &Program) -> Self::Program {
    match program {
      Program::Func(function) => Program::Func(self.visit_function(function)),
    }
  }

  fn visit_function(&mut self, function: &Function) -> Self::Function {
    self.begin_scope();
    let func = Function {
      name: function.name.clone(),
      body: function
        .body
        .items
        .iter()
        .map(|bi| self.visit_block_item(bi))
        .collect(),
    };
    self.end_scope();

    func
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
      Statement::If(exp, stmt1, stmt2) => Statement::If(
        self.visit_expression(exp),
        Box::new(self.visit_statement(stmt1)),
        match stmt2 {
          Some(stmt) => Some(Box::new(self.visit_statement(stmt))),
          None => None,
        },
      ),
      Statement::Compound(block) => {
        self.begin_scope();
        let stmt = Statement::Compound(Block {
          items: block
            .items
            .iter()
            .map(|bi| self.visit_block_item(bi))
            .collect(),
        });
        self.end_scope();
        stmt
      }
      Statement::While(exp, statement, label) => Statement::While(
        self.visit_expression(exp),
        Box::new(self.visit_statement(statement)),
        label.clone(),
      ),
      Statement::For(init, exp2, exp3, statement, label) => {
        self.begin_scope();
        let stmt = Statement::For(
          self.visit_for_init(init),
          match exp2 {
            Some(exp) => Some(self.visit_expression(exp)),
            None => None,
          },
          match exp3 {
            Some(exp) => Some(self.visit_expression(exp)),
            None => None,
          },
          Box::new(self.visit_statement(statement)),
          label.clone(),
        );
        self.end_scope();
        stmt
      }
      Statement::Continue(label) => Statement::Continue(label.clone()),
      Statement::Break(label) => Statement::Break(label.clone()),
      Statement::Null => Statement::Null,
    }
  }

  fn visit_for_init(&mut self, init: &ForInit) -> Self::ForInit {
    match init {
      ForInit::Declaration(decl) => ForInit::Declaration(self.visit_declaration(decl)),
      ForInit::Expression(exp) => {
        if let Some(exp) = exp {
          ForInit::Expression(Some(self.visit_expression(exp)))
        } else {
          ForInit::Expression(None)
        }
      }
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
      Expression::Conditional(exp1, exp2, exp3) => Expression::Conditional(
        Box::new(self.visit_expression(exp1)),
        Box::new(self.visit_expression(exp2)),
        Box::new(self.visit_expression(exp3)),
      ),
    }
  }
}
