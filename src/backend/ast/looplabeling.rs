use super::*;
use core::panic;

pub struct LoopLabeler {
  loop_count: usize,
  loop_stack: Vec<String>,
}

impl LoopLabeler {
  pub fn new() -> Self {
    Self {
      loop_count: 0,
      loop_stack: Vec::new(),
    }
  }

  fn push_loop(&mut self, label: String) {
    self.loop_stack.push(label);
  }

  fn pop_loop(&mut self) -> Option<String> {
    self.loop_stack.pop()
  }

  fn get_current_loop(&self) -> Option<&String> {
    self.loop_stack.last()
  }

  fn label_loop(&mut self, label: &str) -> String {
    let new_label = format!("loop_{}_{}", label, self.loop_count);
    self.loop_count += 1;
    new_label
  }
}

impl Visitor for LoopLabeler {
  type Program = Program;
  type Function = Function;
  type BlockItem = BlockItem;
  type Declaration = Declaration;
  type Statement = Statement;
  type ForInit = ForInit;
  type Expression = Expression;

  fn visit_program(&mut self, program: &Program) -> Self::Program {
    match program {
      Program::Func(func) => Program::Func(self.visit_function(func)),
    }
  }

  fn visit_function(&mut self, function: &Function) -> Self::Function {
    let body = function
      .body
      .items
      .iter()
      .map(|item| self.visit_block_item(item))
      .collect();
    Function {
      name: function.name.clone(),
      body: Block { items: body },
    }
  }

  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem {
    match block_item {
      BlockItem::Statement(statement) => BlockItem::Statement(self.visit_statement(statement)),
      BlockItem::Declaration(declaration) => {
        BlockItem::Declaration(self.visit_declaration(declaration))
      }
    }
  }

  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration {
    Declaration {
      name: declaration.name.clone(),
      exp: declaration
        .exp
        .as_ref()
        .map(|exp| self.visit_expression(exp)),
    }
  }

  fn visit_statement(&mut self, statement: &Statement) -> Self::Statement {
    match statement {
      Statement::While(condition, body, _) => {
        let label = self.label_loop("while");
        self.push_loop(label.clone());
        let new_body = self.visit_statement(body);
        self.pop_loop();
        Statement::While(
          self.visit_expression(condition),
          Box::new(new_body),
          label.clone(),
        )
      }
      Statement::For(init, exp1, exp2, stmt, _) => {
        let label = self.label_loop("for");
        self.push_loop(label.clone());
        let new_init = self.visit_for_init(init);
        let new_exp1 = exp1.as_ref().map(|exp| self.visit_expression(exp));
        let new_exp2 = exp2.as_ref().map(|exp| self.visit_expression(exp));
        let new_stmt = Box::new(self.visit_statement(stmt));
        self.pop_loop();
        Statement::For(new_init, new_exp1, new_exp2, new_stmt, label.clone())
      }
      Statement::Break(_) => {
        let label = self.get_current_loop().unwrap_or_else(|| {
          panic!("Break statement outside of loop");
        });
        Statement::Break(label.clone())
      }
      Statement::Continue(_) => {
        let label = self.get_current_loop().unwrap_or_else(|| {
          panic!("Continue statement outside of loop");
        });
        Statement::Continue(label.clone())
      }
      Statement::Compound(block) => {
        let new_block = block
          .items
          .iter()
          .map(|item| self.visit_block_item(item))
          .collect();
        Statement::Compound(new_block)
      }
      Statement::Return(exp) => Statement::Return(self.visit_expression(exp)),
      Statement::Expression(exp) => Statement::Expression(self.visit_expression(exp)),
      Statement::If(condition, body, else_body) => {
        let new_body = Box::new(self.visit_statement(body));
        let new_else_body = else_body
          .as_ref()
          .map(|body| Box::new(self.visit_statement(body)));
        Statement::If(self.visit_expression(condition), new_body, new_else_body)
      }
      Statement::Null => Statement::Null,
    }
  }

  fn visit_for_init(&mut self, init: &ForInit) -> Self::ForInit {
    match init {
      ForInit::Declaration(declaration) => {
        ForInit::Declaration(self.visit_declaration(declaration))
      }
      ForInit::Expression(expression) => {
        if let Some(expression) = expression {
          ForInit::Expression(Some(self.visit_expression(expression)))
        } else {
          ForInit::Expression(None)
        }
      }
    }
  }

  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression {
    match expression {
      Expression::Binary(op, exp1, exp2) => {
        let new_exp1 = self.visit_expression(exp1);
        let new_exp2 = self.visit_expression(exp2);
        Expression::Binary(op.clone(), Box::new(new_exp1), Box::new(new_exp2))
      }
      Expression::Unary(op, exp) => {
        let new_exp = self.visit_expression(exp);
        Expression::Unary(op.clone(), Box::new(new_exp))
      }
      Expression::Int(lit) => Expression::Int(*lit),
      Expression::Var(var) => Expression::Var(var.clone()),
      Expression::Assignment(var, exp) => {
        let new_exp = self.visit_expression(exp);
        Expression::Assignment(var.clone(), Box::new(new_exp))
      }
      Expression::Conditional(_, _, _) => todo!(),
    }
  }
}
