use std::cmp::min;
use std::collections::HashMap;

use frontend::ast::typechecker::{IdentifierAttributes, SymbolTable};
use frontend::ast::Type;

use crate::tacky::Tac;

use super::assemblyast::{
  AssemblyType, BinaryOp, ConditionalCode, Function, Instruction, Operand, Program, Register,
  StaticVariable, TopLevel, UnaryOp,
};
use super::tacky;

// This is a builder pattern that will convert our TAC into an AST for the assembly
pub struct AsmBuilder {
  symbol_table: SymbolTable,
}

impl AsmBuilder {
  pub(crate) fn tacky_to_asm_ast(&mut self, tacky: &Tac) -> Program {
    let top_level = tacky
      .top_level
      .iter()
      .map(|toplevel| match toplevel {
        tacky::TopLevel::Function(function) => TopLevel::Function(self.convert_function(function)),
        tacky::TopLevel::StaticVariable(static_var) => {
          TopLevel::StaticVariable(self.convert_static_variable(&static_var))
        }
      })
      .collect();

    Program { top_level }
  }

  pub(crate) fn convert_symbol_table(&mut self) -> HashMap<String, BackendSymbols> {
    let backend_table = self
      .symbol_table
      .iter()
      .map(|(key, val)| match (&val.type_name, &val.attrs) {
        (Type::FunType(_, _), IdentifierAttributes::FunAttr(defined, _)) => match defined {
          frontend::ast::typechecker::Defined::Yes => (key.clone(), BackendSymbols::FunEntry(true)),
          frontend::ast::typechecker::Defined::No => (key.clone(), BackendSymbols::FunEntry(false)),
        },
        (typ, IdentifierAttributes::StaticAttr(_, _)) => match typ {
          Type::Int => (
            key.clone(),
            BackendSymbols::ObjEntry(AssemblyType::LongWord, true),
          ),
          Type::Long => (
            key.clone(),
            BackendSymbols::ObjEntry(AssemblyType::QuadWord, true),
          ),
          Type::FunType(_, _) => unreachable!(),
        },
        (typ, IdentifierAttributes::LocalAttr) => match typ {
          Type::Int => (
            key.clone(),
            BackendSymbols::ObjEntry(AssemblyType::LongWord, false),
          ),
          Type::Long => (
            key.clone(),
            BackendSymbols::ObjEntry(AssemblyType::QuadWord, false),
          ),
          Type::FunType(_, _) => unreachable!(),
        },
        _ => unreachable!(),
      })
      .collect();

    backend_table
  }

  pub fn new(symbol_table: SymbolTable) -> Self {
    AsmBuilder { symbol_table }
  }

  fn convert_static_variable(&self, static_var: &tacky::StaticVariable) -> StaticVariable {
    let align = match static_var.typ {
      frontend::ast::Type::Int => 4,
      frontend::ast::Type::Long => 8,
      frontend::ast::Type::FunType(_, _) => unreachable!(),
    };
    StaticVariable {
      name: static_var.name.clone(),
      global: static_var.global,
      alignment: align,
      init: static_var.init,
    }
  }

  fn convert_function(&mut self, function: &tacky::Function) -> Function {
    Function {
      name: function.name.clone(),
      global: function.global,
      instructions: {
        let mut instructions: Vec<Instruction> = Vec::new();
        let arg_registers = [
          Register::Di,
          Register::Si,
          Register::Dx,
          Register::Cx,
          Register::R8,
          Register::R9,
        ];

        let arg_typs: &Vec<Type> = match &self.symbol_table.get(&function.name).unwrap().type_name {
          frontend::ast::Type::FunType(vec, _) => vec,
          _ => {
            unreachable!()
          }
        };

        let split_index = min(function.params.len(), 6);
        let (register_args, stack_args) = function.params.split_at(split_index);
        for (i, param) in register_args.iter().enumerate() {
          let typ = convert_type(&arg_typs[i]);
          instructions.push(Instruction::Mov(
            typ,
            Operand::Reg(arg_registers[i]),
            Operand::Pseudo(param.clone()),
          ));
        }
        for (i, param) in stack_args.iter().enumerate() {
          let typ = convert_type(&arg_typs[i]);
          instructions.push(Instruction::Mov(
            typ,
            Operand::Stack(((i + 1) * 8) as i32),
            Operand::Pseudo(param.clone()),
          ));
          instructions.push(Instruction::Push(Operand::Reg(Register::Ax)));
        }
        for instruction in &function.body {
          match instruction {
            tacky::Instruction::Return(val) => {
              instructions.push(Instruction::Mov(
                self.get_asm_type_of(val),
                convert_val(val),
                Operand::Reg(Register::Ax),
              ));
              instructions.push(Instruction::Return);
            }
            tacky::Instruction::Unary(op, src, dst) => {
              self.convert_unary_op(*op, src, dst, &mut instructions);
            }
            tacky::Instruction::Binary(op, val1, val2, dst) => {
              self.convert_binary_op(*op, val1, val2, dst, &mut instructions);
            }
            tacky::Instruction::JumpIfZero(val, label) => {
              let cond_type = self.get_asm_type_of(val);
              instructions.push(Instruction::Cmp(
                cond_type,
                Operand::Imm(0),
                convert_val(val),
              ));
              instructions.push(Instruction::JmpCC(ConditionalCode::E, label.clone()));
            }
            tacky::Instruction::JumpIfNotZero(val, label) => {
              let cond_type = self.get_asm_type_of(val);
              instructions.push(Instruction::Cmp(
                cond_type,
                Operand::Imm(0),
                convert_val(val),
              ));
              instructions.push(Instruction::JmpCC(ConditionalCode::NE, label.clone()));
            }
            tacky::Instruction::Jump(label) => {
              instructions.push(Instruction::Jmp(label.clone()));
            }
            tacky::Instruction::Label(label) => {
              instructions.push(Instruction::Label(label.clone()));
            }
            tacky::Instruction::Copy(src, dst) => {
              let src_type = self.get_asm_type_of(src);
              instructions.push(Instruction::Mov(
                src_type,
                convert_val(src),
                convert_val(dst),
              ));
            }
            tacky::Instruction::SignExtend(src, dst) => {
              instructions.push(Instruction::MovSx(convert_val(src), convert_val(dst)));
            }
            tacky::Instruction::Truncate(src, dst) => {
              instructions.push(Instruction::Mov(
                AssemblyType::LongWord,
                convert_val(src),
                convert_val(dst),
              ));
            }
            tacky::Instruction::FunctionCall(name, args, dst) => {
              let split_index = min(args.len(), 6);
              let (register_args, stack_args) = args.split_at(split_index);

              let stack_padding = if stack_args.len() % 2 == 0 { 8 } else { 0 };

              if stack_padding != 0 {
                instructions.push(Instruction::Binary(
                  BinaryOp::Sub,
                  AssemblyType::QuadWord,
                  Operand::Imm(stack_padding),
                  Operand::Reg(Register::Sp),
                ));
              }

              let mut reg_index = 0;
              for tacky_arg in register_args {
                let r = arg_registers[reg_index];
                let assembly_arg = convert_val(tacky_arg);
                let typ = self.get_asm_type_of(tacky_arg);
                instructions.push(Instruction::Mov(typ, assembly_arg, Operand::Reg(r)));
                reg_index += 1;
              }

              for tacky_arg in stack_args.iter().rev() {
                let assembly_arg = convert_val(tacky_arg);
                let typ = self.get_asm_type_of(tacky_arg);
                match assembly_arg {
                  Operand::Reg(_) | Operand::Imm(_) => {
                    instructions.push(Instruction::Push(assembly_arg));
                  }
                  _ => {
                    if typ == AssemblyType::QuadWord {
                      instructions.push(Instruction::Push(assembly_arg));
                    } else {
                      instructions.push(Instruction::Mov(
                        AssemblyType::LongWord,
                        assembly_arg,
                        Operand::Reg(Register::Ax),
                      ));
                      instructions.push(Instruction::Push(Operand::Reg(Register::Ax)));
                    }
                  }
                }
              }

              instructions.push(Instruction::Call(name.clone()));

              let bytes_to_remove: i64 = 8 * (stack_args.len() as i64) + stack_padding;
              if bytes_to_remove != 0 {
                instructions.push(Instruction::Binary(
                  BinaryOp::Add,
                  AssemblyType::QuadWord,
                  Operand::Imm(bytes_to_remove),
                  Operand::Reg(Register::Sp),
                ));
              }

              let assembly_dst = convert_val(dst);
              let dst_type = self.get_asm_type_of(dst);
              instructions.push(Instruction::Mov(
                dst_type,
                Operand::Reg(Register::Ax),
                assembly_dst,
              ));
            }
          }
        }
        instructions
      },
    }
  }

  fn get_asm_type_of(&self, val: &tacky::Val) -> AssemblyType {
    match val {
      tacky::Val::Const(c) => match c {
        frontend::ast::Const::Int(_) => AssemblyType::LongWord,
        frontend::ast::Const::Long(_) => AssemblyType::QuadWord,
      },
      tacky::Val::Var(name) => match self.symbol_table.get(name).unwrap().type_name {
        Type::Int => AssemblyType::LongWord,
        Type::Long => AssemblyType::QuadWord,
        Type::FunType(_, _) => unreachable!(),
      },
    }
  }

  fn convert_binary_op(
    &self,
    op: tacky::BinaryOp,
    val1: &tacky::Val,
    val2: &tacky::Val,
    dst: &tacky::Val,
    instructions: &mut Vec<Instruction>,
  ) {
    let src1_typ = self.get_asm_type_of(val1);
    let dst_typ = self.get_asm_type_of(dst);
    match op {
      tacky::BinaryOp::Div => {
        instructions.push(Instruction::Mov(
          src1_typ,
          convert_val(val1),
          Operand::Reg(Register::Ax),
        ));
        instructions.push(Instruction::Cdq(src1_typ));
        instructions.push(Instruction::IDiv(src1_typ, convert_val(val2)));
        instructions.push(Instruction::Mov(
          src1_typ,
          Operand::Reg(Register::Ax),
          convert_val(dst),
        ));
      }
      tacky::BinaryOp::Mod => {
        instructions.push(Instruction::Mov(
          src1_typ,
          convert_val(val1),
          Operand::Reg(Register::Ax),
        ));
        instructions.push(Instruction::Cdq(src1_typ));
        instructions.push(Instruction::IDiv(src1_typ, convert_val(val2)));
        instructions.push(Instruction::Mov(
          src1_typ,
          Operand::Reg(Register::Dx),
          convert_val(dst),
        ));
      }
      tacky::BinaryOp::GreaterThan
      | tacky::BinaryOp::GreaterOrEqual
      | tacky::BinaryOp::LessOrEqual
      | tacky::BinaryOp::LessThan
      | tacky::BinaryOp::NotEqual
      | tacky::BinaryOp::Equal => {
        instructions.push(Instruction::Cmp(
          src1_typ,
          convert_val(val2),
          convert_val(val1),
        ));
        instructions.push(Instruction::Mov(dst_typ, Operand::Imm(0), convert_val(dst)));
        instructions.push(Instruction::SetCC(
          conditional_name_lookup(op),
          convert_val(dst),
        ));
      }
      _ => {
        instructions.push(Instruction::Mov(
          src1_typ,
          convert_val(&val1),
          convert_val(&dst),
        ));
        instructions.push(Instruction::Binary(
          binary_op_name_lookup(op),
          src1_typ,
          convert_val(&val2),
          convert_val(&dst),
        ));
      }
    }
  }

  fn convert_unary_op(
    &self,
    op: tacky::UnaryOp,
    src: &tacky::Val,
    dst: &tacky::Val,
    instructions: &mut Vec<Instruction>,
  ) {
    let src_typ = self.get_asm_type_of(src);
    let dst_typ = self.get_asm_type_of(dst);
    match op {
      tacky::UnaryOp::Not => {
        instructions.push(Instruction::Cmp(src_typ, Operand::Imm(0), convert_val(src)));
        instructions.push(Instruction::Mov(dst_typ, Operand::Imm(0), convert_val(dst)));
        instructions.push(Instruction::SetCC(ConditionalCode::E, convert_val(dst)));
      }
      _ => {
        instructions.push(Instruction::Mov(
          src_typ,
          convert_val(src),
          convert_val(dst),
        ));
        instructions.push(Instruction::Unary(
          unary_op_name_lookup(op),
          src_typ,
          convert_val(dst),
        ));
      }
    }
  }
}

pub(crate) fn convert_val(val: &tacky::Val) -> Operand {
  match val {
    tacky::Val::Const(c) => match c {
      frontend::ast::Const::Int(n) => Operand::Imm(*n as i64),
      frontend::ast::Const::Long(n) => Operand::Imm(*n),
    },
    tacky::Val::Var(var) => Operand::Pseudo(var.clone()),
  }
}

pub(crate) fn unary_op_name_lookup(op: tacky::UnaryOp) -> UnaryOp {
  match op {
    tacky::UnaryOp::Neg => UnaryOp::Neg,
    tacky::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
    tacky::UnaryOp::Complement => UnaryOp::Complement,
    _ => todo!(),
  }
}

pub(crate) fn binary_op_name_lookup(op: tacky::BinaryOp) -> BinaryOp {
  match op {
    tacky::BinaryOp::Add => BinaryOp::Add,
    tacky::BinaryOp::Sub => BinaryOp::Sub,
    tacky::BinaryOp::Mul => BinaryOp::Mul,
    _ => unreachable!(),
  }
}

pub(crate) fn conditional_name_lookup(op: tacky::BinaryOp) -> ConditionalCode {
  match op {
    tacky::BinaryOp::GreaterThan => ConditionalCode::G,
    tacky::BinaryOp::GreaterOrEqual => ConditionalCode::GE,
    tacky::BinaryOp::LessThan => ConditionalCode::L,
    tacky::BinaryOp::LessOrEqual => ConditionalCode::LE,
    tacky::BinaryOp::Equal => ConditionalCode::E,
    tacky::BinaryOp::NotEqual => ConditionalCode::NE,
    _ => unreachable!(),
  }
}

fn convert_type(typ: &Type) -> AssemblyType {
  match typ {
    Type::Int => AssemblyType::LongWord,
    Type::Long => AssemblyType::QuadWord,
    Type::FunType(_, _) => unreachable!(),
  }
}

#[derive(Debug, Clone, Copy)]
pub enum BackendSymbols {
  ObjEntry(AssemblyType, bool), // type, is_static
  FunEntry(bool),               // is defined
}
