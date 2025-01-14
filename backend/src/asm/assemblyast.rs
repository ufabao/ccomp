use frontend::ast::typechecker::{IdentifierAttributes, TypeInfo};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
  pub top_level: Vec<TopLevel>,
}

#[derive(Debug)]
pub struct ASMPasses<'a> {
  identifiers: HashMap<String, i32>,
  symbol_table: &'a HashMap<String, TypeInfo>,
  function_variable_count: HashMap<String, i32>,
  current_function: String,
}

impl<'a> ASMPasses<'a> {
  pub fn new(symbol_table: &'a HashMap<String, TypeInfo>) -> Self {
    ASMPasses {
      identifiers: HashMap::new(),
      symbol_table: symbol_table,
      function_variable_count: HashMap::new(),
      current_function: String::new(),
    }
  }

  pub fn final_pass(&self, prog: &Program) -> Program {
    let mut processed_top_levels = Vec::new();

    for top_level in &prog.top_level {
      match top_level {
        TopLevel::Function(function) => {
          let processed_function = self.process_function(&function);
          processed_top_levels.push(TopLevel::Function(processed_function));
        }
        TopLevel::StaticVariable(static_var) => {
          // Static variables don't need processing in the final pass
          processed_top_levels.push(TopLevel::StaticVariable(static_var.to_owned()));
        }
      }
    }

    Program {
      top_level: processed_top_levels,
    }
  }

  fn process_function(&self, function: &Function) -> Function {
    let mut replaced_instructions: Vec<Instruction> = Vec::new();
    let stack_size = 4 * self.get_function_variable_count(&function.name);
    let stack_size = ((stack_size + 15) / 16) * 16;
    replaced_instructions.push(Instruction::AllocateStack(stack_size));

    for instruction in &function.instructions {
      match instruction {
        Instruction::Mov(src, dst) => match (src, dst) {
          (Operand::Stack(s), Operand::Stack(d)) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Stack(*s),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              Operand::Reg(Register::R10),
              Operand::Stack(*d),
            ));
          }
          (Operand::Data(d), Operand::Stack(s)) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Data(d.clone()),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              Operand::Reg(Register::R10),
              Operand::Stack(*s),
            ));
          }
          (Operand::Stack(s), Operand::Data(d)) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Stack(*s),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              Operand::Reg(Register::R10),
              Operand::Data(d.clone()),
            ));
          }
          (Operand::Data(s), Operand::Data(d)) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Data(s.clone()),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              Operand::Reg(Register::R10),
              Operand::Data(d.clone()),
            ));
          }
          _ => {
            replaced_instructions.push(instruction.clone());
          }
        },
        Instruction::Binary(binary_op, operand1, operand2) => {
          match (binary_op, operand1, operand2) {
            (BinaryOp::Add | BinaryOp::Sub, Operand::Stack(n1), Operand::Stack(n2)) => {
              replaced_instructions.push(Instruction::Mov(
                Operand::Stack(*n1),
                Operand::Reg(Register::R10),
              ));
              replaced_instructions.push(Instruction::Binary(
                *binary_op,
                Operand::Reg(Register::R10),
                Operand::Stack(*n2),
              ));
            }
            (BinaryOp::Mul, _, Operand::Stack(n)) => {
              replaced_instructions.push(Instruction::Mov(
                Operand::Stack(*n),
                Operand::Reg(Register::R11),
              ));
              replaced_instructions.push(Instruction::Binary(
                *binary_op,
                operand1.clone(),
                Operand::Reg(Register::R11),
              ));
              replaced_instructions.push(Instruction::Mov(
                Operand::Reg(Register::R11),
                Operand::Stack(*n),
              ));
            }
            _ => {
              replaced_instructions.push(instruction.clone());
            }
          }
        }
        Instruction::IDiv(operand) => match operand {
          Operand::Imm(n) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Imm(*n),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::IDiv(Operand::Reg(Register::R10)));
          }
          _ => {
            replaced_instructions.push(Instruction::IDiv(operand.clone()));
          }
        },
        Instruction::Cmp(src, dst) => match (src, dst) {
          (Operand::Stack(s), Operand::Stack(d)) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Stack(*s),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Cmp(
              Operand::Reg(Register::R10),
              Operand::Stack(*d),
            ));
          }
          (_, Operand::Imm(n)) => {
            replaced_instructions.push(Instruction::Mov(
              Operand::Imm(*n),
              Operand::Reg(Register::R11),
            ));
            replaced_instructions.push(Instruction::Cmp(src.clone(), Operand::Reg(Register::R11)));
          }
          _ => {
            replaced_instructions.push(instruction.clone());
          }
        },
        _ => {
          replaced_instructions.push(instruction.clone());
        }
      }
    }

    Function {
      name: function.name.clone(),
      global: function.global,
      instructions: replaced_instructions,
    }
  }

  pub fn replace_pseudo(&mut self, prog: &Program) -> Program {
    let mut processed_top_levels = Vec::new();

    for top_level in &prog.top_level {
      match top_level {
        TopLevel::Function(function) => {
          let processed_function = self.process_pseudo_function(function);
          processed_top_levels.push(TopLevel::Function(processed_function));
        }
        TopLevel::StaticVariable(static_var) => {
          // Static variables don't need pseudo replacement
          processed_top_levels.push(TopLevel::StaticVariable(static_var.clone()));
        }
      }
    }

    Program {
      top_level: processed_top_levels,
    }
  }

  fn process_pseudo_function(&mut self, function: &Function) -> Function {
    self.enter_function(&function.name);
    let mut replaced_instructions: Vec<Instruction> = Vec::new();

    for instruction in &function.instructions {
      match instruction {
        Instruction::Mov(src, dst) => {
          replaced_instructions.push(Instruction::Mov(
            self.replace_operand(src),
            self.replace_operand(dst),
          ));
        }
        Instruction::Unary(op, val) => {
          replaced_instructions.push(Instruction::Unary(op.clone(), self.replace_operand(val)));
        }
        Instruction::Binary(binary_op, operand1, operand2) => {
          replaced_instructions.push(Instruction::Binary(
            *binary_op,
            self.replace_operand(operand1),
            self.replace_operand(operand2),
          ));
        }
        Instruction::IDiv(operand) => {
          replaced_instructions.push(Instruction::IDiv(self.replace_operand(operand)));
        }
        Instruction::Cmp(operand, operand1) => {
          replaced_instructions.push(Instruction::Cmp(
            self.replace_operand(operand),
            self.replace_operand(operand1),
          ));
        }
        Instruction::SetCC(conditional_code, operand) => {
          replaced_instructions.push(Instruction::SetCC(
            *conditional_code,
            self.replace_operand(operand),
          ));
        }
        Instruction::Push(operand) => {
          replaced_instructions.push(Instruction::Push(self.replace_operand(operand)));
        }
        _ => {
          replaced_instructions.push(instruction.clone());
        }
      }
    }

    Function {
      name: function.name.clone(),
      global: function.global,
      instructions: replaced_instructions,
    }
  }

  fn replace_operand(&mut self, operand: &Operand) -> Operand {
    match operand {
      Operand::Imm(i) => Operand::Imm(*i),
      Operand::Reg(r) => Operand::Reg(*r),
      Operand::Pseudo(pseudo) => {
        if self.identifiers.contains_key(pseudo) {
          Operand::Stack(*self.identifiers.get(pseudo).unwrap())
        } else if let Some(symbol) = self.symbol_table.get(pseudo) {
          match symbol.attrs {
            IdentifierAttributes::StaticAttr(_, _) => Operand::Data(pseudo.clone()),
            _ => {
              self.add_variable_to_function();
              let stack_offset = -4 * self.get_current_function_variable_count();
              self.identifiers.insert(pseudo.clone(), stack_offset);
              Operand::Stack(stack_offset)
            }
          }
        } else {
          self.add_variable_to_function();
          let stack_offset = -4 * self.get_current_function_variable_count();
          self.identifiers.insert(pseudo.clone(), stack_offset);
          Operand::Stack(stack_offset)
        }
      }
      Operand::Stack(s) => Operand::Stack(*s),
      Operand::Data(_) => unreachable!(), // I think???
    }
  }

  fn enter_function(&mut self, function_name: &str) {
    self.current_function = function_name.to_string();
    self
      .function_variable_count
      .insert(self.current_function.clone(), 0);
  }

  fn get_function_variable_count(&self, function_name: &str) -> i32 {
    *self.function_variable_count.get(function_name).unwrap()
  }

  fn get_current_function_variable_count(&self) -> i32 {
    *self
      .function_variable_count
      .get(&self.current_function)
      .unwrap()
  }

  fn add_variable_to_function(&mut self) {
    let count = self
      .function_variable_count
      .get_mut(&self.current_function)
      .unwrap();
    *count += 1;
  }
}

#[derive(Debug)]
pub enum TopLevel {
  Function(Function),
  StaticVariable(StaticVariable),
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  pub global: bool,
  pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct StaticVariable {
  pub name: String,
  pub global: bool,
  pub value: i32,
}

#[derive(Debug, Clone)]
pub enum Instruction {
  Mov(Operand, Operand),
  Unary(UnaryOp, Operand),
  Binary(BinaryOp, Operand, Operand),
  Cmp(Operand, Operand),
  IDiv(Operand),
  Cdq,
  Jmp(String),
  JmpCC(ConditionalCode, String),
  SetCC(ConditionalCode, Operand),
  Label(String),
  AllocateStack(i32),
  DeAllocateStack(i32),
  Push(Operand),
  Call(String),
  Return,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  And,
  Or,
  GreaterThan,
  GreaterOrEqual,
  LessThan,
  LessOrEqual,
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  PrefixDec,
  Complement,
  Not,
}

#[derive(Debug, Clone)]
pub enum Operand {
  Imm(i32),
  Reg(Register),
  Pseudo(String),
  Stack(i32),
  Data(String),
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
  Ax,
  Cx,
  Dx,
  Di,
  Si,
  R8,
  R9,
  R10,
  R11,
}

#[derive(Debug, Clone, Copy)]
pub enum ConditionalCode {
  E,
  NE,
  G,
  GE,
  L,
  LE,
}
