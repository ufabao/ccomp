use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Linkage {
  External,
  Internal,
}

#[derive(Debug, Clone)]
struct IdentifierInfo {
  name: String,
  linkage: Linkage,
}

#[derive(Default)]
pub struct NameResolver {
  identifier_maps: Vec<HashMap<String, IdentifierInfo>>,
  errors: Vec<String>,
  var_count: usize,
}

impl NameResolver {
  pub fn new() -> Self {
    Self {
      identifier_maps: vec![HashMap::new()],
      errors: Vec::new(),
      var_count: 0,
    }
  }

  fn begin_scope(&mut self) {
    self.identifier_maps.push(HashMap::new());
  }

  fn end_scope(&mut self) -> Result<(), String> {
    self.identifier_maps.pop();
    if self.identifier_maps.len() == 0 {
      return Err(format!("Popped too many scopes"));
    }
    Ok(())
  }

  fn declare_variable(&mut self, name: &str, linkage: Linkage) {
    if self.identifier_maps.last().unwrap().contains_key(name) {
      self
        .errors
        .push(format!("Variable '{}' declared twice", name));
    } else {
      let new_name = match linkage {
        Linkage::External => name.to_string(),
        Linkage::Internal => "user_var".to_string() + &self.var_count.to_string(),
      };
      self.identifier_maps.last_mut().unwrap().insert(
        name.to_string(),
        IdentifierInfo {
          name: new_name,
          linkage: linkage,
        },
      );
      self.var_count += 1;
    }
  }

  fn resolve_variable(&mut self, name: &str) -> Option<IdentifierInfo> {
    for scope in self.identifier_maps.iter().rev() {
      if let Some(resolved_name) = scope.get(name) {
        return Some(resolved_name.clone());
      }
    }
    None
  }
}

impl Visitor for NameResolver {
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
        let new_funcs = functions
          .iter()
          .map(|f| self.visit_function_decl(f))
          .collect::<Result<Vec<_>, _>>()?;

        Ok(Program::Program(new_funcs))
      }
    }
  }

  fn visit_function_decl(&mut self, function: &FunctionDecl) -> Self::FunctionDecl {
    self.declare_variable(&function.name, Linkage::External);
    let new_name = self.resolve_variable(&function.name).unwrap().name;
    self.begin_scope();
    for param in &function.params {
      self.declare_variable(param, Linkage::Internal);
    }
    let new_params: Vec<String> = function
      .params
      .iter()
      .map(|param| self.resolve_variable(param).unwrap().name.clone())
      .collect();
    let new_body = match &function.body {
      Some(block) => {
        self.begin_scope();
        let new_block = Some(Block {
          items: block
            .items
            .iter()
            .map(|bi| self.visit_block_item(bi))
            .collect::<Result<Vec<_>, _>>()?,
        });
        self.end_scope()?;
        new_block
      }
      None => None,
    };
    self.end_scope()?;

    Ok(FunctionDecl {
      name: new_name,
      params: new_params,
      body: new_body,
    })
  }

  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem {
    match block_item {
      BlockItem::Statement(stmt) => Ok(BlockItem::Statement(self.visit_statement(stmt)?)),
      BlockItem::Declaration(decl) => Ok(BlockItem::Declaration(self.visit_declaration(decl)?)),
    }
  }

  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration {
    match declaration {
      Declaration::FuncDeclaration(func) => Ok(Declaration::FuncDeclaration(
        self.visit_function_decl(func)?,
      )),
      Declaration::VarDeclaration(var) => {
        self.declare_variable(&var.name, Linkage::Internal);
        Ok(Declaration::VarDeclaration(VariableDecl {
          name: self.resolve_variable(&var.name).unwrap().name,
          value: match &var.value {
            Some(exp) => Some(self.visit_expression(exp)?),
            None => None,
          },
        }))
      }
    }
  }

  fn visit_statement(&mut self, stmt: &Statement) -> Self::Statement {
    match stmt {
      Statement::Expression(expression) => {
        Ok(Statement::Expression(self.visit_expression(expression)?))
      }
      Statement::Return(expression) => Ok(Statement::Return(self.visit_expression(expression)?)),
      Statement::If(exp, stmt1, stmt2) => Ok(Statement::If(
        self.visit_expression(exp)?,
        Box::new(self.visit_statement(stmt1)?),
        match stmt2 {
          Some(stmt) => Some(Box::new(self.visit_statement(stmt)?)),
          None => None,
        },
      )),
      Statement::Compound(block) => {
        self.begin_scope();
        let stmt = Statement::Compound(Block {
          items: block
            .items
            .iter()
            .map(|bi| self.visit_block_item(bi))
            .collect::<Result<Vec<_>, _>>()?,
        });
        self.end_scope()?;
        Ok(stmt)
      }
      Statement::While(exp, statement, label) => Ok(Statement::While(
        self.visit_expression(exp)?,
        Box::new(self.visit_statement(statement)?),
        label.clone(),
      )),
      Statement::For(init, exp2, exp3, statement, label) => {
        self.begin_scope();
        let stmt = Statement::For(
          self.visit_for_init(init)?,
          match exp2 {
            Some(exp) => Some(self.visit_expression(exp)?),
            None => None,
          },
          match exp3 {
            Some(exp) => Some(self.visit_expression(exp)?),
            None => None,
          },
          Box::new(self.visit_statement(statement)?),
          label.clone(),
        );
        self.end_scope()?;
        Ok(stmt)
      }
      Statement::Continue(label) => Ok(Statement::Continue(label.clone())),
      Statement::Break(label) => Ok(Statement::Break(label.clone())),
      Statement::Null => Ok(Statement::Null),
    }
  }

  fn visit_for_init(&mut self, init: &ForInit) -> Self::ForInit {
    match init {
      ForInit::Expression(exp) => {
        if let Some(exp) = exp {
          Ok(ForInit::Expression(Some(self.visit_expression(exp)?)))
        } else {
          Ok(ForInit::Expression(None))
        }
      }
      ForInit::Declaration(decl) => Ok(ForInit::Declaration(self.visit_variable_decl(decl)?)),
    }
  }

  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression {
    match expression {
      Expression::Int(i) => Ok(Expression::Int(*i)),
      Expression::Var(name) => {
        let resolved_name = self.resolve_variable(name);
        if let Some(resolved_name) = resolved_name {
          Ok(Expression::Var(resolved_name.name))
        } else {
          return Err(format!("Variable '{}' used before declared", name));
        }
      }
      Expression::Unary(op, exp) => Ok(Expression::Unary(
        *op,
        Box::new(self.visit_expression(exp)?),
      )),
      Expression::Binary(op, exp1, exp2) => Ok(Expression::Binary(
        *op,
        Box::new(self.visit_expression(exp1)?),
        Box::new(self.visit_expression(exp2)?),
      )),
      Expression::Assignment(exp1, exp2) => {
        if let Expression::Var(_) = **exp1 {
          Ok(Expression::Assignment(
            Box::new(self.visit_expression(exp1)?),
            Box::new(self.visit_expression(exp2)?),
          ))
        } else {
          return Err(format!("Left hand side of assignment must be a variable"));
        }
      }
      Expression::Conditional(exp1, exp2, exp3) => Ok(Expression::Conditional(
        Box::new(self.visit_expression(exp1)?),
        Box::new(self.visit_expression(exp2)?),
        Box::new(self.visit_expression(exp3)?),
      )),
      Expression::FunctionCall(name, vec) => {
        let resolved_name = self.resolve_variable(name);
        if let Some(resolved_name) = resolved_name {
          Ok(Expression::FunctionCall(
            resolved_name.name,
            vec
              .iter()
              .map(|exp| self.visit_expression(exp))
              .collect::<Result<Vec<_>, _>>()?,
          ))
        } else {
          return Err(format!("Function '{}' used before declared", name));
        }
      }
    }
  }

  fn visit_variable_decl(&mut self, variable: &VariableDecl) -> Self::VariableDecl {
    self.declare_variable(&variable.name, Linkage::Internal);
    Ok(VariableDecl {
      name: self.resolve_variable(&variable.name).unwrap().name,
      value: match &variable.value {
        Some(exp) => Some(self.visit_expression(exp)?),
        None => None,
      },
    })
  }
}
