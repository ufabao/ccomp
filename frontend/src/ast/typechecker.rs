use super::*;
use crate::ast::Accept;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum TypeError {
  FunctionRedefinition {
    name: String,
    reason: String,
  },
  SymbolRedefinition {
    name: String,
    existing_type: String,
    attempted_type: String,
  },
  FunctionCallError {
    name: String,
    expected_params: i32,
    actual_params: i32,
  },
  UndefinedFunction(String),
  UndefinedVariable(String),
  WrongTypeForVariable(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeInfo {
  Int,
  FunType(i32, bool), // param count, already_defined_flag
}

pub struct TypeChecker {
  symbol_table: HashMap<String, TypeInfo>,
}

impl TypeChecker {
  pub fn type_check_program(program: &Program) -> Result<HashMap<String, TypeInfo>, String> {
    let mut type_checker = TypeChecker::new();
    let _ = program
      .accept(&mut type_checker)
      .map_err(|e| format!("{:?}", e));

    Ok(type_checker.symbol_table)
  }

  pub fn new() -> Self {
    TypeChecker {
      symbol_table: HashMap::new(),
    }
  }

  fn add_symbol(&mut self, name: &str, info: TypeInfo) {
    self.symbol_table.insert(name.to_string(), info);
  }

  fn get_symbol(&self, name: &str) -> Option<&TypeInfo> {
    self.symbol_table.get(name)
  }
}

impl Visitor for TypeChecker {
  type Program = Result<(), TypeError>;
  type FunctionDecl = Result<(), TypeError>;
  type VariableDecl = Result<(), TypeError>;
  type BlockItem = Result<(), TypeError>;
  type Declaration = Result<(), TypeError>;
  type Statement = Result<(), TypeError>;
  type ForInit = Result<(), TypeError>;
  type Expression = Result<(), TypeError>;

  fn visit_program(&mut self, program: &Program) -> Self::Program {
    match program {
      Program::Program(decls) => {
        for decl in decls {
          self.visit_function_decl(decl)?;
        }
        Ok(())
      }
    }
  }

  fn visit_function_decl(&mut self, function: &FunctionDecl) -> Self::FunctionDecl {
    let param_count = function.params.len() as i32;
    let has_body = function.body.is_some();

    if let Some(old_decl) = self.symbol_table.get(&function.name) {
      match old_decl {
        TypeInfo::FunType(num_params, defined) => {
          if *num_params != param_count {
            return Err(TypeError::FunctionRedefinition {
              name: function.name.clone(),
              reason: format!(
                "different parameter count: expected {}, got {}",
                num_params, param_count
              ),
            });
          }
          if *defined && has_body {
            return Err(TypeError::FunctionRedefinition {
              name: function.name.clone(),
              reason: "function already has a body".to_string(),
            });
          }
        }
        TypeInfo::Int => {
          return Err(TypeError::SymbolRedefinition {
            name: function.name.clone(),
            existing_type: "variable".to_string(),
            attempted_type: "function".to_string(),
          });
        }
      }
    }

    self.add_symbol(
      &function.name,
      TypeInfo::FunType(
        param_count,
        has_body
          || self
            .get_symbol(&function.name)
            .map(|info| matches!(info, TypeInfo::FunType(_, true)))
            .unwrap_or(false),
      ),
    );

    if let Some(body) = &function.body {
      for item in &body.items {
        self.visit_block_item(item)?;
      }
    }
    Ok(())
  }

  fn visit_variable_decl(&mut self, variable: &VariableDecl) -> Self::VariableDecl {
    self.add_symbol(&variable.name, TypeInfo::Int);
    if let Some(value) = &variable.value {
      self.visit_expression(value)?;
    }
    Ok(())
  }

  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem {
    match block_item {
      BlockItem::Declaration(decl) => self.visit_declaration(decl)?,
      BlockItem::Statement(stmt) => self.visit_statement(stmt)?,
    }
    Ok(())
  }

  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration {
    match declaration {
      Declaration::FuncDeclaration(func) => self.visit_function_decl(func)?,
      Declaration::VarDeclaration(var) => self.visit_variable_decl(var)?,
    }
    Ok(())
  }

  fn visit_statement(&mut self, statement: &Statement) -> Self::Statement {
    match statement {
      Statement::Expression(expression) => {
        self.visit_expression(expression)?;
      }
      Statement::Return(expression) => {
        self.visit_expression(expression)?;
      }
      Statement::If(expression, statement, statement1) => {
        self.visit_expression(expression)?;
        self.visit_statement(statement)?;
        if let Some(stmt) = statement1 {
          self.visit_statement(stmt)?;
        }
      }
      Statement::Compound(block) => {
        for item in &block.items {
          self.visit_block_item(item)?;
        }
      }
      Statement::While(expression, statement, _) => {
        self.visit_expression(expression)?;
        self.visit_statement(statement)?;
      }
      Statement::For(for_init, expression1, expression2, statement, _) => {
        self.visit_for_init(for_init)?;
        if let Some(expression1) = expression1 {
          self.visit_expression(expression1)?;
        }
        if let Some(expression2) = expression2 {
          self.visit_expression(expression2)?;
        }
        self.visit_statement(statement)?;
      }
      Statement::Null => {}
      Statement::Break(_) => {}
      Statement::Continue(_) => {}
    }
    Ok(())
  }

  fn visit_for_init(&mut self, statement: &ForInit) -> Self::ForInit {
    match statement {
      ForInit::Declaration(variable_decl) => {
        self.visit_variable_decl(variable_decl)?;
      }
      ForInit::Expression(expression) => {
        if let Some(expression) = expression {
          self.visit_expression(expression)?;
        }
      }
    }
    Ok(())
  }

  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression {
    match expression {
      Expression::Binary(_, lhs, rhs) => {
        self.visit_expression(lhs)?;
        self.visit_expression(rhs)?;
      }
      Expression::Unary(_, expr) => self.visit_expression(expr)?,
      Expression::Int(_) => {}
      Expression::Var(name) => {
        if let Some(info) = self.get_symbol(name) {
          match info {
            TypeInfo::Int => {}
            _ => {
              return Err(TypeError::WrongTypeForVariable(format!(
                "Wrong type for variable {}",
                name
              )));
            }
          }
        }
      }
      Expression::FunctionCall(name, args) => match self.get_symbol(name) {
        Some(TypeInfo::FunType(param_count, _)) => {
          if *param_count != args.len() as i32 {
            return Err(TypeError::FunctionCallError {
              name: name.clone(),
              expected_params: *param_count,
              actual_params: args.len() as i32,
            });
          }
          for arg in args {
            self.visit_expression(arg)?;
          }
        }
        Some(TypeInfo::Int) => {
          return Err(TypeError::SymbolRedefinition {
            name: name.clone(),
            existing_type: "variable".to_string(),
            attempted_type: "function".to_string(),
          });
        }
        None => {
          return Err(TypeError::UndefinedFunction(name.clone()));
        }
      },
      Expression::Assignment(lhs, rhs) => {
        self.visit_expression(lhs)?;
        self.visit_expression(rhs)?;
      }
      Expression::Conditional(exp1, exp2, exp3) => {
        self.visit_expression(exp1)?;
        self.visit_expression(exp2)?;
        self.visit_expression(exp3)?;
      }
    }
    Ok(())
  }
}

impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeError::FunctionRedefinition { name, reason } => {
        write!(f, "Function '{}' redefinition error: {}", name, reason)
      }
      TypeError::SymbolRedefinition {
        name,
        existing_type,
        attempted_type,
      } => {
        write!(
          f,
          "Symbol '{}' already defined as {} but attempted to define as {}",
          name, existing_type, attempted_type
        )
      }
      TypeError::FunctionCallError {
        name,
        expected_params,
        actual_params,
      } => {
        write!(
          f,
          "Function '{}' called with wrong number of arguments. Expected: {}, got: {}",
          name, expected_params, actual_params
        )
      }
      TypeError::UndefinedFunction(name) => {
        write!(f, "Call to undefined function '{}'", name)
      }
      TypeError::UndefinedVariable(name) => {
        write!(f, "Use of undefined variable '{}'", name)
      }
      TypeError::WrongTypeForVariable(name) => {
        write!(f, "Symbol '{}' is not a variable", name)
      }
    }
  }
}
