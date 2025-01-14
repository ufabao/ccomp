use super::*;
use crate::ast::Accept;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Defined {
  Yes,
  No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Global {
  Yes,
  No,
}

#[derive(Debug, PartialEq, Eq)]
pub enum InitialValue {
  Tentative,
  Initial(i32),
  NoInitializer,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Types {
  Int,
  FunType(i32),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IdentifierAttributes {
  FunAttr(i32, Defined, Global),
  StaticAttr(InitialValue, Global),
  LocalAttr,
}

#[derive(Debug)]
pub struct TypeInfo {
  pub type_name: Types,
  pub attrs: IdentifierAttributes,
}

pub struct TypeChecker {
  symbol_table: HashMap<String, TypeInfo>,
}

impl TypeChecker {
  pub fn type_check_program(program: &Program) -> Result<HashMap<String, TypeInfo>, String> {
    let mut type_checker = TypeChecker::new();
    type_checker.visit_program(program)?;
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

  fn typecheck_file_scope_var(&mut self, variable: &VariableDecl) -> Result<(), String> {
    // First check - extern variables cannot have initializers
    if variable.storage == Some(StorageClass::Extern) && variable.value.is_some() {
      return Err(format!(
        "Extern variable {} cannot have an initializer",
        variable.name.clone()
      ));
    }

    let mut initial_val = match variable.value {
      Some(Expression::Int(val)) => InitialValue::Initial(val),
      None => match variable.storage {
        Some(StorageClass::Static) => InitialValue::Tentative,
        _ => InitialValue::NoInitializer,
      },
      _ => {
        return Err(format!(
          "variable {} has non const initializer.",
          variable.name.clone()
        ))
      }
    };

    let mut global = match variable.storage {
      Some(StorageClass::Static) => Global::No,
      _ => Global::Yes,
    };

    if let Some(old_decl) = self.symbol_table.get(&variable.name) {
      if old_decl.type_name != Types::Int {
        return Err(format!(
          "variable {} already defined as function",
          variable.name.clone()
        ));
      }
      if variable.storage == Some(StorageClass::Extern) {
        global = match &old_decl.attrs {
          IdentifierAttributes::FunAttr(_, _, _) => Global::No,
          IdentifierAttributes::StaticAttr(_, global) => *global,
          IdentifierAttributes::LocalAttr => Global::No,
        };
      } else if let IdentifierAttributes::FunAttr(_, _, _) = old_decl.attrs {
        return Err(format!(
          "Conflicting variable linkage for {}",
          variable.name.clone()
        ));
      } else if let IdentifierAttributes::LocalAttr = old_decl.attrs {
        return Err(format!(
          "Conflicting variable linkage for {}",
          variable.name.clone()
        ));
      }
      if let IdentifierAttributes::StaticAttr(InitialValue::Initial(n), _) = old_decl.attrs {
        if initial_val != InitialValue::Initial(n) {
          return Err(format!(
            "Conflicting file scope variable definitions for {}",
            variable.name.clone()
          ));
        } else {
          initial_val = InitialValue::Initial(n);
        }
      }
    }
    let attrs = IdentifierAttributes::StaticAttr(initial_val, global);
    self.add_symbol(
      &variable.name,
      TypeInfo {
        type_name: Types::Int,
        attrs,
      },
    );
    Ok(())
  }
}

impl Visitor for TypeChecker {
  type Program = Result<(), String>;
  type FunctionDecl = Result<(), String>;
  type VariableDecl = Result<(), String>;
  type BlockItem = Result<(), String>;
  type Declaration = Result<(), String>;
  type Statement = Result<(), String>;
  type ForInit = Result<(), String>;
  type Expression = Result<(), String>;

  fn visit_program(&mut self, program: &Program) -> Self::Program {
    match program {
      Program::Program(decls) => {
        for decl in decls {
          match decl {
            Declaration::FuncDeclaration(func) => self.visit_function_decl(func)?,
            Declaration::VarDeclaration(var) => self.typecheck_file_scope_var(var)?,
          }
        }
        Ok(())
      }
    }
  }

  fn visit_function_decl(&mut self, function: &FunctionDecl) -> Self::FunctionDecl {
    let param_count = function.params.len() as i32;
    let has_body = function.body.is_some();
    let mut already_defined = Defined::No;

    if let Some(old_decl) = self.symbol_table.get(&function.name) {
      match old_decl.attrs {
        IdentifierAttributes::FunAttr(num_params, defined, linkage) => {
          already_defined = match defined {
            Defined::Yes => Defined::Yes,
            Defined::No => Defined::No,
          };
          if num_params != param_count {
            return Err(format!(
              "Type mismatch for {}, different parameter count: expected {}, got {}",
              function.name.clone(),
              num_params,
              param_count
            ));
          };

          if let (Defined::Yes, true) = (defined, has_body) {
            return Err(format!(
              "function {} already has a body",
              function.name.clone()
            ));
          }
          if let (Global::Yes, Some(StorageClass::Static)) = (linkage, function.storage.clone()) {
            return Err(format!("Conflicting linkage for {}", function.name.clone()));
          }
        }
        _ => {
          return Err(format!(
            "Symbol {} already defined as variable",
            function.name.clone()
          ));
        }
      }
    }
    let defined = match function.body {
      Some(_) => Defined::Yes,
      None => already_defined,
    };
    let storage = match function.storage {
      Some(StorageClass::Static) => Global::Yes,
      _ => Global::No,
    };
    self.add_symbol(
      &function.name,
      TypeInfo {
        type_name: Types::FunType(function.params.len() as i32),
        attrs: IdentifierAttributes::FunAttr(param_count, defined, storage),
      },
    );

    if let Some(body) = &function.body {
      for item in &body.items {
        self.visit_block_item(item)?;
      }
    }
    Ok(())
  }

  fn visit_variable_decl(&mut self, variable: &VariableDecl) -> Self::VariableDecl {
    if variable.storage == Some(StorageClass::Extern) {
      // First check - extern variables cannot have initializers
      if variable.value.is_some() {
        return Err(format!(
          "Extern variable {} cannot have an initializer",
          variable.name.clone()
        ));
      }

      // Check existing declaration if any
      if let Some(old_decl) = self.symbol_table.get(&variable.name) {
        match &old_decl.attrs {
          IdentifierAttributes::FunAttr(_, _, _) => {
            return Err(format!(
              "Extern variable {} already defined as function",
              variable.name.clone()
            ));
          }
          IdentifierAttributes::StaticAttr(_, global) => {
            if *global == Global::No {
              return Err(format!(
                "Extern declaration of {} conflicts with static definition",
                variable.name.clone()
              ));
            }
          }
          IdentifierAttributes::LocalAttr => {
            return Err(format!(
              "Extern declaration of {} conflicts with local definition",
              variable.name.clone()
            ));
          }
        }
      } else {
        // New extern declaration - always has external linkage (Global::Yes)
        self.add_symbol(
          &variable.name,
          TypeInfo {
            type_name: Types::Int,
            attrs: IdentifierAttributes::StaticAttr(InitialValue::NoInitializer, Global::Yes),
          },
        );
      }
    } else if variable.storage == Some(StorageClass::Static) {
      if let Some(Expression::Int(n)) = variable.value {
        self.add_symbol(
          &variable.name,
          TypeInfo {
            type_name: Types::Int,
            attrs: IdentifierAttributes::StaticAttr(InitialValue::Initial(n), Global::No),
          },
        );
      } else if variable.value.is_none() {
        self.add_symbol(
          &variable.name,
          TypeInfo {
            type_name: Types::Int,
            attrs: IdentifierAttributes::StaticAttr(InitialValue::Initial(0), Global::No),
          },
        );
      } else {
        return Err(format!(
          "Static variable {} has non const initializer",
          variable.name.clone()
        ));
      }
    } else {
      self.add_symbol(
        &variable.name,
        TypeInfo {
          type_name: Types::Int,
          attrs: IdentifierAttributes::LocalAttr,
        },
      );
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
          match info.type_name {
            Types::Int => {}
            _ => {
              return Err(format!("Wrong type for variable {}", name));
            }
          }
        }
      }
      Expression::FunctionCall(name, args) => match self.get_symbol(name) {
        Some(info) => match info.type_name {
          Types::FunType(param_count) => {
            if param_count != args.len() as i32 {
              return Err(format!(
                "Function '{}' called with wrong number of arguments. Expected: {}, got: {}",
                name,
                param_count,
                args.len()
              ));
            }
            for arg in args {
              self.visit_expression(arg)?;
            }
          }
          Types::Int => {
            return Err(format!("Symbol '{}' is not a function", name));
          }
        },
        None => {
          return Err(format!("Function {} called before defined", name.clone()));
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
