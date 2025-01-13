use super::*;

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

  fn get_current_loop(&self) -> Option<String> {
    self.loop_stack.last().map(|s| s.clone())
  }

  fn label_loop(&mut self, label: &str) -> String {
    let new_label = format!("loop_{}_{}", label, self.loop_count);
    self.loop_count += 1;
    new_label
  }
}

impl Visitor for LoopLabeler {
  type Program = Result<Program, String>;
  type FunctionDecl = Result<FunctionDecl, String>;
  type VariableDecl = Result<VariableDecl, String>;
  type BlockItem = Result<BlockItem, String>;
  type Declaration = Result<Declaration, String>;
  type Statement = Result<Statement, String>;
  type ForInit = Result<ForInit, String>;
  type Expression = Result<Expression, String>;

  fn visit_program(&mut self, program: &Program) -> Self::Program {
    match program {
      Program::Program(functions) => {
        let new_functions = functions
          .iter()
          .map(|function| self.visit_function_decl(function))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Program::Program(new_functions))
      }
    }
  }

  fn visit_function_decl(&mut self, function: &FunctionDecl) -> Self::FunctionDecl {
    let new_body = match function.body.as_ref() {
      Some(block) => Some(Block {
        items: block
          .items
          .iter()
          .map(|item| self.visit_block_item(item))
          .collect::<Result<Vec<_>, _>>()?,
      }),
      None => None,
    };
    Ok(FunctionDecl {
      name: function.name.clone(),
      params: function.params.clone(),
      body: new_body,
    })
  }

  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem {
    match block_item {
      BlockItem::Statement(statement) => Ok(BlockItem::Statement(self.visit_statement(statement)?)),
      BlockItem::Declaration(declaration) => {
        Ok(BlockItem::Declaration(self.visit_declaration(declaration)?))
      }
    }
  }

  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration {
    match declaration {
      Declaration::FuncDeclaration(function) => Ok(Declaration::FuncDeclaration(
        self.visit_function_decl(function)?,
      )),
      Declaration::VarDeclaration(variable) => Ok(Declaration::VarDeclaration(
        self.visit_variable_decl(variable)?,
      )),
    }
  }

  fn visit_statement(&mut self, statement: &Statement) -> Self::Statement {
    match statement {
      Statement::While(condition, body, _) => {
        let label = self.label_loop("while");
        self.push_loop(label.clone());
        let new_body = self.visit_statement(body)?;
        self.pop_loop();
        Ok(Statement::While(
          self.visit_expression(condition)?,
          Box::new(new_body),
          label.clone(),
        ))
      }
      Statement::For(init, exp1, exp2, stmt, _) => {
        let label = self.label_loop("for");
        self.push_loop(label.clone());
        let new_init = self.visit_for_init(init)?;
        let new_exp1 = match exp1 {
          Some(exp1) => Some(self.visit_expression(exp1)?),
          None => None,
        };
        let new_exp2 = match exp2 {
          Some(exp2) => Some(self.visit_expression(exp2)?),
          None => None,
        };
        let new_stmt = Box::new(self.visit_statement(stmt)?);
        self.pop_loop();
        Ok(Statement::For(
          new_init,
          new_exp1,
          new_exp2,
          new_stmt,
          label.clone(),
        ))
      }
      Statement::Break(b) => {
        let label = self
          .get_current_loop()
          .ok_or_else(|| format!("Break statement {} outside of loop", b))?;
        Ok(Statement::Break(label.clone()))
      }
      Statement::Continue(c) => {
        let label = self
          .get_current_loop()
          .ok_or_else(|| format!("Continue statement {} outside of loop", c))?;
        Ok(Statement::Continue(label.clone()))
      }
      Statement::Compound(block) => {
        let new_block = block
          .items
          .iter()
          .map(|item| self.visit_block_item(item))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Statement::Compound(Block { items: new_block }))
      }
      Statement::Return(exp) => Ok(Statement::Return(self.visit_expression(exp)?)),
      Statement::Expression(exp) => Ok(Statement::Expression(self.visit_expression(exp)?)),
      Statement::If(condition, body, else_body) => {
        let new_body = Box::new(self.visit_statement(body)?);
        let new_else_body = match else_body.as_ref() {
          Some(body) => Some(Box::new(self.visit_statement(body)?)),
          None => None,
        };
        Ok(Statement::If(
          self.visit_expression(condition)?,
          new_body,
          new_else_body,
        ))
      }
      Statement::Null => Ok(Statement::Null),
    }
  }

  fn visit_for_init(&mut self, init: &ForInit) -> Self::ForInit {
    match init {
      ForInit::Declaration(declaration) => {
        Ok(ForInit::Declaration(self.visit_variable_decl(declaration)?))
      }
      ForInit::Expression(expression) => {
        if let Some(expression) = expression {
          Ok(ForInit::Expression(Some(
            self.visit_expression(expression)?,
          )))
        } else {
          Ok(ForInit::Expression(None))
        }
      }
    }
  }

  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression {
    match expression {
      Expression::Binary(op, exp1, exp2) => {
        let new_exp1 = self.visit_expression(exp1)?;
        let new_exp2 = self.visit_expression(exp2)?;
        Ok(Expression::Binary(
          op.clone(),
          Box::new(new_exp1),
          Box::new(new_exp2),
        ))
      }
      Expression::Unary(op, exp) => {
        let new_exp = self.visit_expression(exp)?;
        Ok(Expression::Unary(op.clone(), Box::new(new_exp)))
      }
      Expression::Int(lit) => Ok(Expression::Int(*lit)),
      Expression::Var(var) => Ok(Expression::Var(var.clone())),
      Expression::Assignment(var, exp) => {
        let new_exp = self.visit_expression(exp)?;
        Ok(Expression::Assignment(var.clone(), Box::new(new_exp)))
      }
      Expression::Conditional(_, _, _) => todo!(),
      Expression::FunctionCall(name, vec) => {
        let new_vec = vec
          .iter()
          .map(|exp| self.visit_expression(exp))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Expression::FunctionCall(name.clone(), new_vec))
      }
    }
  }

  fn visit_variable_decl(&mut self, variable: &VariableDecl) -> Self::VariableDecl {
    let new_value = match variable.value.as_ref() {
      Some(value) => Some(self.visit_expression(&value)?),
      None => None,
    };
    Ok(VariableDecl {
      name: variable.name.clone(),
      value: new_value,
    })
  }
}
