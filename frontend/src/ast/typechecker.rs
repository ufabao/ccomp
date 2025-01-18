use super::*;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaticInit {
  Int(i32),
  Long(i64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InitialValue {
  Tentative,
  Initial(StaticInit),
  NoInitializer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdentifierAttributes {
  FunAttr(Defined, Global),
  StaticAttr(InitialValue, Global),
  LocalAttr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInfo {
  pub type_name: Type,
  pub attrs: IdentifierAttributes,
}

pub type SymbolTable = HashMap<String, TypeInfo>;

pub struct TypeChecker {
  symbol_table: SymbolTable,
  current_function: Option<String>,
}

impl TypeChecker {
  pub fn type_check_program(
    program: &Program<Expression>,
  ) -> Result<(Program<TypedExpression>, SymbolTable), String> {
    let mut type_checker = TypeChecker::new();

    Ok((
      type_checker.visit_program(program)?,
      type_checker.symbol_table,
    ))
  }

  pub fn new() -> Self {
    TypeChecker {
      symbol_table: HashMap::new(),
      current_function: None,
    }
  }

  fn add_symbol(&mut self, name: &str, info: TypeInfo) {
    self.symbol_table.insert(name.to_string(), info);
  }

  fn get_symbol(&self, name: &str) -> Option<&TypeInfo> {
    self.symbol_table.get(name)
  }

  fn get_expression_type(&self, exp: &TypedExpression) -> Type {
    match exp {
      TypedExpression::Const(_, t) => t.clone(),
      TypedExpression::Var(_, t) => t.clone(),
      TypedExpression::Cast(t, _) => t.clone(),
      TypedExpression::Unary(_, _, t) => t.clone(),
      TypedExpression::Binary(_, _, _, t) => t.clone(),
      TypedExpression::Assignment(_, _, t) => t.clone(),
      TypedExpression::FunctionCall(_, _, t) => t.clone(),
    }
  }

  fn typecheck_file_scope_var(
    &mut self,
    variable: &VariableDecl<Expression>,
  ) -> Result<VariableDecl<TypedExpression>, String> {
    // First check - extern variables cannot have initializers
    if variable.storage == Some(StorageClass::Extern) && variable.value.is_some() {
      return Err(format!(
        "Extern variable {} cannot have an initializer",
        variable.name.clone()
      ));
    }

    let mut initial_val = match variable.value {
      Some(Expression::Const(val)) => match val {
        Const::Int(n) => InitialValue::Initial(StaticInit::Int(n)),
        Const::Long(n) => InitialValue::Initial(StaticInit::Long(n)),
      },
      None => match variable.storage {
        Some(StorageClass::Static) => InitialValue::Tentative,
        _ => InitialValue::NoInitializer,
      },
      Some(_) => InitialValue::NoInitializer,
    };

    let mut global = match variable.storage {
      Some(StorageClass::Static) => Global::No,
      _ => Global::Yes,
    };

    if let Some(old_decl) = self.symbol_table.get(&variable.name) {
      if let Type::FunType(_, _) = old_decl.type_name {
        return Err(format!(
          "variable {} already defined as function",
          variable.name.clone()
        ));
      }
      if variable.storage == Some(StorageClass::Extern) {
        global = match &old_decl.attrs {
          IdentifierAttributes::FunAttr(_, _) => Global::No,
          IdentifierAttributes::StaticAttr(_, global) => *global,
          IdentifierAttributes::LocalAttr => Global::No,
        };
      } else if let IdentifierAttributes::FunAttr(_, _) = old_decl.attrs {
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
    let attrs = IdentifierAttributes::StaticAttr(initial_val.clone(), global);
    self.add_symbol(
      &variable.name,
      TypeInfo {
        type_name: variable.typ.clone(),
        attrs,
      },
    );

    Ok(VariableDecl {
      name: variable.name.clone(),
      value: match initial_val {
        InitialValue::Initial(StaticInit::Int(n)) => {
          Some(TypedExpression::Const(Const::Int(n), Type::Int))
        }
        InitialValue::Initial(StaticInit::Long(n)) => {
          Some(TypedExpression::Const(Const::Long(n), Type::Long))
        }
        _ => None,
      },
      typ: variable.typ.clone(),
      storage: variable.storage,
    })
  }

  fn handle_extern_var(
    &mut self,
    var: &VariableDecl<Expression>,
  ) -> Result<VariableDecl<TypedExpression>, String> {
    if var.value.is_some() {
      return Err(format!(
        "Extern variable {} cannot have an initializer",
        var.name
      ));
    }

    self.check_symbol_conflicts(&var.name, false)?;

    // Add to symbol table with external linkage
    self.add_symbol(
      &var.name,
      TypeInfo {
        type_name: var.typ.clone(),
        attrs: IdentifierAttributes::StaticAttr(InitialValue::NoInitializer, Global::Yes),
      },
    );

    Ok(VariableDecl {
      name: var.name.clone(),
      value: None,
      typ: var.typ.clone(),
      storage: var.storage,
    })
  }

  fn handle_static_var(
    &mut self,
    var: &VariableDecl<Expression>,
  ) -> Result<VariableDecl<TypedExpression>, String> {
    let initial_value = self.get_initial_value(&var.value)?;

    self.add_symbol(
      &var.name,
      TypeInfo {
        type_name: var.typ.clone(),
        attrs: IdentifierAttributes::StaticAttr(initial_value, Global::No),
      },
    );

    Ok(VariableDecl {
      name: var.name.clone(),
      value: match &var.value {
        Some(v) => Some(self.visit_expression(v)?),
        None => None,
      },
      typ: var.typ.clone(),
      storage: var.storage,
    })
  }

  fn get_initial_value(&self, value: &Option<Expression>) -> Result<InitialValue, String> {
    match value {
      Some(Expression::Const(c)) => Ok(InitialValue::Initial(match c {
        Const::Int(n) => StaticInit::Int(*n),
        Const::Long(n) => StaticInit::Long(*n),
      })),
      None => Ok(InitialValue::NoInitializer),
      _ => Err("Non-constant initializer".to_string()),
    }
  }

  fn check_symbol_conflicts(&self, name: &str, is_function: bool) -> Result<(), String> {
    if let Some(old_decl) = self.get_symbol(name) {
      let is_old_function = matches!(old_decl.type_name, Type::FunType(_, _));
      if is_function != is_old_function {
        Err(format!(
          "Symbol '{}' already defined as {}",
          name,
          if is_old_function {
            "function"
          } else {
            "variable"
          }
        ))
      } else {
        Ok(())
      }
    } else {
      Ok(())
    }
  }

  fn get_common_type(type1: Type, type2: Type) -> Type {
    if type1 == type2 {
      type1
    } else {
      Type::Long
    }
  }

  fn convert_to(&self, exp: TypedExpression, t: &Type) -> TypedExpression {
    if self.get_expression_type(&exp) == *t {
      return exp;
    }
    return TypedExpression::Cast(t.clone(), Box::new(exp));
  }
}

impl Visitor<Expression> for TypeChecker {
  type Program = Result<Program<TypedExpression>, String>;
  type FunctionDecl = Result<FunctionDecl<TypedExpression>, String>;
  type VariableDecl = Result<VariableDecl<TypedExpression>, String>;
  type BlockItem = Result<BlockItem<TypedExpression>, String>;
  type Declaration = Result<Declaration<TypedExpression>, String>;
  type Statement = Result<Statement<TypedExpression>, String>;
  type ForInit = Result<ForInit<TypedExpression>, String>;
  type Expression = Result<TypedExpression, String>;

  fn visit_program(&mut self, program: &Program<Expression>) -> Self::Program {
    let new_decls = program
      .declarations
      .iter()
      .map(|decl| match decl {
        Declaration::FuncDeclaration(func) => Ok::<_, String>(Declaration::FuncDeclaration(
          self.visit_function_decl(func)?,
        )),
        Declaration::VarDeclaration(var) => Ok::<_, String>(Declaration::VarDeclaration(
          self.typecheck_file_scope_var(var)?,
        )),
      })
      .collect::<Result<_, _>>()?;
    Ok(Program {
      declarations: new_decls,
    })
  }

  fn visit_function_decl(&mut self, function: &FunctionDecl<Expression>) -> Self::FunctionDecl {
    self.current_function = Some(function.name.clone());
    let param_count = function.params.len() as i32;
    let has_body = function.body.is_some();
    let mut already_defined = Defined::No;

    if let Some(old_decl) = self.symbol_table.get(&function.name) {
      match old_decl {
        TypeInfo {
          type_name: Type::FunType(params, ret),
          attrs: IdentifierAttributes::FunAttr(defined, linkage),
        } => {
          already_defined = match defined {
            Defined::Yes => Defined::Yes,
            Defined::No => Defined::No,
          };
          if params.len() != param_count as usize {
            return Err(format!(
              "Type mismatch for {}, different parameter count: expected {}, got {}",
              function.name.clone(),
              params.len(),
              param_count
            ));
          };

          if let Type::FunType(new_params, new_ret) = &function.typ {
            if new_params != params {
              for (i, (expected, given)) in params.iter().zip(new_params.iter()).enumerate() {
                if expected != given {
                  return Err(format!(
                    "Type mismatch for parameter {} of function {}: expected {:?}, got {:?}",
                    i,
                    function.name.clone(),
                    expected,
                    given
                  ));
                }
              }
            }
            if new_ret != ret {
              return Err(format!("Redefinition of function {} with different return type, previous definition has return type {:?}, new definition has return type {:?}",
              function.name.clone(),
              ret,
              new_ret));
            }
          } else {
            unreachable!()
          }

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
    } else {
      if let Type::FunType(param_types, _) = &function.typ {
        for (param, param_type) in function.params.iter().zip(param_types.iter()) {
          self.add_symbol(
            param,
            TypeInfo {
              type_name: param_type.clone(),
              attrs: IdentifierAttributes::LocalAttr,
            },
          );
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
        type_name: function.typ.clone(),
        attrs: IdentifierAttributes::FunAttr(defined, storage),
      },
    );

    let typed_body = if let Some(body) = &function.body {
      Some(Block {
        items: body
          .items
          .iter()
          .map(|item| self.visit_block_item(item))
          .collect::<Result<Vec<_>, _>>()?,
      })
    } else {
      None
    };
    self.current_function = None;

    Ok(FunctionDecl {
      name: function.name.clone(),
      params: function.params.clone(),
      body: typed_body,
      typ: function.typ.clone(),
      storage: function.storage.clone(),
    })
  }

  fn visit_variable_decl(&mut self, variable: &VariableDecl<Expression>) -> Self::VariableDecl {
    match variable.storage {
      Some(StorageClass::Extern) => self.handle_extern_var(variable),
      Some(StorageClass::Static) => self.handle_static_var(variable),
      None => {
        let mut typed_value = if let Some(value) = &variable.value {
          Some(self.visit_expression(value)?)
        } else {
          None
        };

        if let Some(value) = &typed_value {
          let value_type = self.get_expression_type(value);
          if value_type != variable.typ {
            typed_value = Some(TypedExpression::Cast(
              variable.typ.clone(),
              Box::new(value.clone()),
            ));
          }
        }

        self.add_symbol(
          &variable.name,
          TypeInfo {
            type_name: variable.typ.clone(),
            attrs: IdentifierAttributes::LocalAttr,
          },
        );
        Ok(VariableDecl {
          name: variable.name.clone(),
          value: typed_value,
          typ: variable.typ.clone(),
          storage: variable.storage,
        })
      }
    }
  }

  fn visit_block_item(&mut self, block_item: &BlockItem<Expression>) -> Self::BlockItem {
    match block_item {
      BlockItem::Declaration(decl) => Ok(BlockItem::Declaration(self.visit_declaration(decl)?)),
      BlockItem::Statement(stmt) => Ok(BlockItem::Statement(self.visit_statement(stmt)?)),
    }
  }

  fn visit_declaration(&mut self, declaration: &Declaration<Expression>) -> Self::Declaration {
    match declaration {
      Declaration::FuncDeclaration(func) => Ok(Declaration::FuncDeclaration(
        self.visit_function_decl(func)?,
      )),
      Declaration::VarDeclaration(var) => {
        Ok(Declaration::VarDeclaration(self.visit_variable_decl(var)?))
      }
    }
  }

  fn visit_statement(&mut self, statement: &Statement<Expression>) -> Self::Statement {
    match statement {
      Statement::Expression(expression) => {
        Ok(Statement::Expression(self.visit_expression(expression)?))
      }
      Statement::Return(expression) => {
        let ret_type = if let Some(func) = &self.current_function {
          if let Some(type_info) = self.get_symbol(&func) {
            match type_info {
              TypeInfo {
                type_name: Type::FunType(_, ret_type),
                ..
              } => ret_type.clone(),
              _ => {
                return Err("Return statement outside of function call".to_string());
              }
            }
          } else {
            return Err("Return statement outside of function call".to_string());
          }
        } else {
          return Err("Return statement outside of function call".to_string());
        };
        let expression = self.visit_expression(expression)?;
        Ok(Statement::Return(self.convert_to(expression, &ret_type)))
      }
      Statement::If(expression, statement, statement1) => {
        let exp = self.visit_expression(expression)?;
        let stmt1 = Box::new(self.visit_statement(statement)?);
        let stmt2 = if let Some(stmt) = statement1 {
          Some(Box::new(self.visit_statement(stmt)?))
        } else {
          None
        };
        Ok(Statement::If(exp, stmt1, stmt2))
      }
      Statement::Compound(block) => {
        let items = block
          .items
          .iter()
          .map(|item| self.visit_block_item(item))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Statement::Compound(Block { items }))
      }
      Statement::While(expression, statement, label) => Ok(Statement::While(
        self.visit_expression(expression)?,
        Box::new(self.visit_statement(statement)?),
        label.clone(),
      )),
      Statement::For(for_init, expression1, expression2, statement, label) => {
        let init = self.visit_for_init(for_init)?;
        let exp1 = if let Some(expression) = expression1 {
          Some(self.visit_expression(expression)?)
        } else {
          None
        };
        let exp2 = if let Some(expression) = expression2 {
          Some(self.visit_expression(expression)?)
        } else {
          None
        };
        let stmt = Box::new(self.visit_statement(statement)?);
        Ok(Statement::For(init, exp1, exp2, stmt, label.clone()))
      }
      Statement::Null => Ok(Statement::Null),
      Statement::Break(label) => Ok(Statement::Break(label.clone())),
      Statement::Continue(label) => Ok(Statement::Continue(label.clone())),
    }
  }

  fn visit_for_init(&mut self, statement: &ForInit<Expression>) -> Self::ForInit {
    match statement {
      ForInit::Declaration(variable_decl) => Ok(ForInit::Declaration(
        self.visit_variable_decl(variable_decl)?,
      )),
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
      Expression::Binary(op, lhs, rhs) => {
        let lhs = self.visit_expression(lhs)?;
        let rhs = self.visit_expression(rhs)?;
        if *op == BinaryOp::And || *op == BinaryOp::Or {
          return Ok(TypedExpression::Binary(
            *op,
            Box::new(lhs),
            Box::new(rhs),
            Type::Int,
          ));
        }
        let t1 = self.get_expression_type(&lhs);
        let t2 = self.get_expression_type(&rhs);
        let common_type = TypeChecker::get_common_type(t1, t2);
        let lhs = self.convert_to(lhs, &common_type);
        let rhs = self.convert_to(rhs, &common_type);
        match op {
          BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Ok(
            TypedExpression::Binary(*op, Box::new(lhs), Box::new(rhs), common_type),
          ),
          BinaryOp::Equal
          | BinaryOp::NotEqual
          | BinaryOp::LessThan
          | BinaryOp::GreaterThan
          | BinaryOp::LessOrEqual
          | BinaryOp::GreaterOrEqual => Ok(TypedExpression::Binary(
            *op,
            Box::new(lhs),
            Box::new(rhs),
            Type::Int,
          )),
          _ => Err("Invalid binary operator".to_string()),
        }
      }
      Expression::Unary(op, expr) => {
        let expr = self.visit_expression(expr)?;
        let t = self.get_expression_type(&expr);
        match op {
          UnaryOp::Not => Ok(TypedExpression::Unary(*op, Box::new(expr), Type::Int)),
          _ => Ok(TypedExpression::Unary(*op, Box::new(expr), t)),
        }
      }
      Expression::Const(c) => {
        let (const_val, const_type) = match c {
          Const::Int(n) => (Const::Int(*n), Type::Int),
          Const::Long(n) => (Const::Long(*n), Type::Long),
        };
        Ok(TypedExpression::Const(const_val, const_type))
      }
      Expression::Var(name) => {
        if let Some(info) = self.get_symbol(name) {
          Ok(TypedExpression::Var(name.clone(), info.type_name.clone()))
        } else {
          Err(format!("Undefined variable '{}'", name))
        }
      }
      Expression::Cast(typ, expr) => {
        let typed_expr = self.visit_expression(expr)?;
        Ok(TypedExpression::Cast(typ.clone(), Box::new(typed_expr)))
      }
      Expression::FunctionCall(name, args) => {
        let func_type = if let Some(type_info) = self.get_symbol(name) {
          type_info.type_name.clone()
        } else {
          return Err(format!("Undefined function '{}'", name));
        };
        match func_type {
          Type::FunType(param_types, ret_type) => {
            if args.len() != param_types.len() {
              return Err(format!(
                "Function {} expects {} arguments, got {}",
                name,
                param_types.len(),
                args.len()
              ));
            }
            let mut converted_args = Vec::new();

            for (arg, param_type) in args.iter().zip(param_types.iter()) {
              let arg = self.visit_expression(arg)?;
              let arg = self.convert_to(arg, &param_type);
              converted_args.push(arg);
            }
            Ok(TypedExpression::FunctionCall(
              name.clone(),
              converted_args,
              *ret_type.clone(),
            ))
          }
          _ => Err(format!("{} is not a function", name)),
        }
      }
      Expression::Assignment(lhs, rhs) => {
        let lhs = self.visit_expression(lhs)?;
        let rhs = self.visit_expression(rhs)?;
        let left_type = self.get_expression_type(&lhs);
        let rhs = self.convert_to(rhs, &left_type);
        Ok(TypedExpression::Assignment(
          Box::new(lhs),
          Box::new(rhs),
          left_type,
        ))
      }
    }
  }
}
