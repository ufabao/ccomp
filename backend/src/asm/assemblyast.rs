use frontend::ast::typechecker::{IdentifierAttributes, StaticInit};
use std::collections::HashMap;

use super::tacky_to_asm::BackendSymbols;

#[derive(Debug)]
pub struct ASMPasses<'a> {
  identifiers: HashMap<String, i32>,
  symbol_table: &'a HashMap<String, BackendSymbols>,
  function_stack_offset: HashMap<String, i32>,
  current_function: String,
}

impl<'a> ASMPasses<'a> {
  pub fn new(symbol_table: &'a HashMap<String, BackendSymbols>) -> Self {
    ASMPasses {
      identifiers: HashMap::new(),
      symbol_table: symbol_table,
      function_stack_offset: HashMap::new(),
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
    let stack_size = self.get_function_stack_size(&function.name);
    let stack_size = ((stack_size + 15) / 16) * 16;
    replaced_instructions.push(Instruction::Binary(
      BinaryOp::Sub,
      AssemblyType::QuadWord,
      Operand::Imm(stack_size as i64),
      Operand::Reg(Register::Sp),
    ));

    for instruction in &function.instructions {
      match instruction {
        Instruction::Mov(typ, src, dst) => match (src, dst) {
          (Operand::Stack(s), Operand::Stack(d)) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Stack(*s),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Reg(Register::R10),
              Operand::Stack(*d),
            ));
          }
          (Operand::Data(d), Operand::Stack(s)) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Data(d.clone()),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Reg(Register::R10),
              Operand::Stack(*s),
            ));
          }
          (Operand::Stack(s), Operand::Data(d)) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Stack(*s),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Reg(Register::R10),
              Operand::Data(d.clone()),
            ));
          }
          (Operand::Data(s), Operand::Data(d)) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Data(s.clone()),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Reg(Register::R10),
              Operand::Data(d.clone()),
            ));
          }
          _ => {
            replaced_instructions.push(instruction.clone());
          }
        },
        Instruction::MovSx(src, dst) => match (src, dst) {
          (Operand::Imm(n), Operand::Stack(s)) => {
            replaced_instructions.push(Instruction::Mov(
              AssemblyType::LongWord,
              Operand::Imm(*n),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::MovSx(
              Operand::Reg(Register::R10),
              Operand::Reg(Register::R11),
            ));
            replaced_instructions.push(Instruction::Mov(
              AssemblyType::QuadWord,
              Operand::Reg(Register::R11),
              Operand::Stack(*s),
            ));
          }
          (Operand::Imm(n), _) => {
            replaced_instructions.push(Instruction::Mov(
              AssemblyType::LongWord,
              Operand::Imm(*n),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Mov(
              AssemblyType::QuadWord,
              Operand::Reg(Register::R10),
              dst.clone(),
            ));
          }
          (_, Operand::Stack(s)) => {
            replaced_instructions
              .push(Instruction::MovSx(src.clone(), Operand::Reg(Register::R11)));
            replaced_instructions.push(Instruction::Mov(
              AssemblyType::QuadWord,
              Operand::Reg(Register::R11),
              Operand::Stack(*s),
            ));
          }
          _ => replaced_instructions.push(instruction.clone()),
        },
        Instruction::Binary(binary_op, typ, operand1, operand2) => {
          match (binary_op, typ, operand1, operand2) {
            (
              BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul,
              AssemblyType::QuadWord,
              Operand::Imm(_),
              _,
            ) => {
              replaced_instructions.push(Instruction::Mov(
                AssemblyType::QuadWord,
                operand1.clone(),
                Operand::Reg(Register::R10),
              ));
              replaced_instructions.push(Instruction::Binary(
                *binary_op,
                *typ,
                Operand::Reg(Register::R10),
                operand2.clone(),
              ));
            }
            (BinaryOp::Add | BinaryOp::Sub, _, Operand::Stack(n1), Operand::Stack(n2)) => {
              replaced_instructions.push(Instruction::Mov(
                *typ,
                Operand::Stack(*n1),
                Operand::Reg(Register::R10),
              ));
              replaced_instructions.push(Instruction::Binary(
                *binary_op,
                *typ,
                Operand::Reg(Register::R10),
                Operand::Stack(*n2),
              ));
            }
            (BinaryOp::Mul, _, _, Operand::Stack(n)) => {
              replaced_instructions.push(Instruction::Mov(
                *typ,
                Operand::Stack(*n),
                Operand::Reg(Register::R11),
              ));
              replaced_instructions.push(Instruction::Binary(
                *binary_op,
                *typ,
                operand1.clone(),
                Operand::Reg(Register::R11),
              ));
              replaced_instructions.push(Instruction::Mov(
                *typ,
                Operand::Reg(Register::R11),
                Operand::Stack(*n),
              ));
            }
            _ => {
              replaced_instructions.push(instruction.clone());
            }
          }
        }
        Instruction::IDiv(typ, operand) => match operand {
          Operand::Imm(n) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Imm(*n),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::IDiv(*typ, Operand::Reg(Register::R10)));
          }
          _ => {
            replaced_instructions.push(Instruction::IDiv(*typ, operand.clone()));
          }
        },
        Instruction::Cmp(typ, src, dst) => match (src, dst) {
          (Operand::Stack(s), Operand::Stack(d)) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Stack(*s),
              Operand::Reg(Register::R10),
            ));
            replaced_instructions.push(Instruction::Cmp(
              *typ,
              Operand::Reg(Register::R10),
              Operand::Stack(*d),
            ));
          }
          (_, Operand::Imm(n)) => {
            replaced_instructions.push(Instruction::Mov(
              *typ,
              Operand::Imm(*n),
              Operand::Reg(Register::R11),
            ));
            replaced_instructions.push(Instruction::Cmp(
              *typ,
              src.clone(),
              Operand::Reg(Register::R11),
            ));
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
        Instruction::Mov(typ, src, dst) => {
          replaced_instructions.push(Instruction::Mov(
            *typ,
            self.replace_operand(src),
            self.replace_operand(dst),
          ));
        }
        Instruction::MovSx(src, dst) => {
          replaced_instructions.push(Instruction::MovSx(
            self.replace_operand(src),
            self.replace_operand(dst),
          ));
        }
        Instruction::Unary(op, typ, val) => {
          replaced_instructions.push(Instruction::Unary(
            op.clone(),
            *typ,
            self.replace_operand(val),
          ));
        }
        Instruction::Binary(binary_op, typ, operand1, operand2) => {
          replaced_instructions.push(Instruction::Binary(
            *binary_op,
            *typ,
            self.replace_operand(operand1),
            self.replace_operand(operand2),
          ));
        }
        Instruction::IDiv(typ, operand) => {
          replaced_instructions.push(Instruction::IDiv(*typ, self.replace_operand(operand)));
        }
        Instruction::Cmp(typ, operand, operand1) => {
          replaced_instructions.push(Instruction::Cmp(
            *typ,
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
          match symbol {
            BackendSymbols::ObjEntry(_, true) => Operand::Data(pseudo.clone()),
            BackendSymbols::ObjEntry(typ, false) => match typ {
              AssemblyType::LongWord => {
                let stack_offset = self.add_variable_to_function(AssemblyType::LongWord);
                self.identifiers.insert(pseudo.clone(), stack_offset);
                Operand::Stack(stack_offset)
              }
              AssemblyType::QuadWord => {
                let stack_offset = self.add_variable_to_function(AssemblyType::QuadWord);
                self.identifiers.insert(pseudo.clone(), stack_offset);
                Operand::Stack(stack_offset)
              }
            },
            _ => unreachable!(),
            //IdentifierAttributes::StaticAttr(_, _) => Operand::Data(pseudo.clone()),
            // _ => {
            //   self.add_variable_to_function();
            //   let stack_offset = -4 * self.get_current_function_variable_count();
            //   self.identifiers.insert(pseudo.clone(), stack_offset);
            //   Operand::Stack(stack_offset)
            // }
          }
        } else {
          panic!("What does this code do?");
          self.add_variable_to_function(AssemblyType::LongWord);
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
      .function_stack_offset
      .insert(self.current_function.clone(), 0);
  }

  fn get_function_stack_size(&self, function_name: &str) -> i32 {
    *self.function_stack_offset.get(function_name).unwrap() * -1
  }

  fn get_current_function_variable_count(&self) -> i32 {
    *self
      .function_stack_offset
      .get(&self.current_function)
      .unwrap()
  }

  fn add_variable_to_function(&mut self, typ: AssemblyType) -> i32 {
    let offset = self
      .function_stack_offset
      .get_mut(&self.current_function)
      .unwrap();
    match typ {
      AssemblyType::LongWord => *offset -= 4,
      AssemblyType::QuadWord => {
        if *offset % 8 != 0 {
          *offset -= 12;
        } else {
          *offset -= 8;
        }
      }
    }
    *offset
  }
}

#[derive(Debug)]
pub struct Program {
  pub top_level: Vec<TopLevel>,
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
  pub alignment: i32,
  pub init: StaticInit,
}

#[derive(Debug, Clone)]
pub enum Instruction {
  Mov(AssemblyType, Operand, Operand),
  MovSx(Operand, Operand),
  Unary(UnaryOp, AssemblyType, Operand),
  Binary(BinaryOp, AssemblyType, Operand, Operand),
  Cmp(AssemblyType, Operand, Operand),
  IDiv(AssemblyType, Operand),
  Cdq(AssemblyType),
  Jmp(String),
  JmpCC(ConditionalCode, String),
  SetCC(ConditionalCode, Operand),
  Label(String),
  Push(Operand),
  Call(String),
  Return,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssemblyType {
  LongWord,
  QuadWord,
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
  Imm(i64),
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
  Sp,
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
