pub mod assemblyast;
pub mod codegen;

use crate::backend::tacky::tacky;
use assemblyast::{
  BinaryOp, ConditionalCode, Function, Instruction, Operand, Program, Register, UnaryOp,
};

pub fn tacky_to_asm_ast(tacky: &tacky::Program) -> Program {
  match tacky {
    tacky::Program::Func(func) => Program::Func(Function {
      name: func.name.clone(),
      instructions: {
        let mut instructions: Vec<Instruction> = Vec::new();
        for instruction in &func.instructions {
          match instruction {
            tacky::Instruction::Return(val) => {
              instructions.push(Instruction::Mov(
                convert_val(val),
                Operand::Reg(Register::Rax),
              ));
            }
            tacky::Instruction::Unary(op, src, dst) => {
              convert_unary_op(*op, src, dst, &mut instructions);
            }
            tacky::Instruction::Binary(op, val1, val2, dst) => {
              convert_binary_op(*op, val1, val2, dst, &mut instructions);
            }
            tacky::Instruction::JumpIfZero(val, label) => {
              instructions.push(Instruction::Cmp(Operand::Imm(0), convert_val(val)));
              instructions.push(Instruction::JmpCC(ConditionalCode::E, label.clone()));
            }
            tacky::Instruction::JumpIfNotZero(val, label) => {
              instructions.push(Instruction::Cmp(Operand::Imm(0), convert_val(val)));
              instructions.push(Instruction::JmpCC(ConditionalCode::NE, label.clone()));
            }
            tacky::Instruction::Jump(label) => {
              instructions.push(Instruction::Jmp(label.clone()));
            }
            tacky::Instruction::Label(label) => {
              instructions.push(Instruction::Label(label.clone()));
            }
            tacky::Instruction::Copy(src, dst) => {
              instructions.push(Instruction::Mov(convert_val(src), convert_val(dst)));
            }
          }
        }
        instructions
      },
    }),
  }
}

fn convert_binary_op(
  op: tacky::BinaryOp,
  val1: &tacky::Val,
  val2: &tacky::Val,
  dst: &tacky::Val,
  instructions: &mut Vec<Instruction>,
) {
  match op {
    tacky::BinaryOp::Div => {
      instructions.push(Instruction::Mov(
        convert_val(val1),
        Operand::Reg(Register::Rax),
      ));
      instructions.push(Instruction::Cdq);
      instructions.push(Instruction::IDiv(convert_val(val2)));
      instructions.push(Instruction::Mov(
        Operand::Reg(Register::Rax),
        convert_val(dst),
      ));
    }
    tacky::BinaryOp::Mod => {
      instructions.push(Instruction::Mov(
        convert_val(val1),
        Operand::Reg(Register::Rax),
      ));
      instructions.push(Instruction::Cdq);
      instructions.push(Instruction::IDiv(convert_val(val2)));
      instructions.push(Instruction::Mov(
        Operand::Reg(Register::Rdx),
        convert_val(dst),
      ));
    }
    tacky::BinaryOp::GreaterThan
    | tacky::BinaryOp::GreaterOrEqual
    | tacky::BinaryOp::LessOrEqual
    | tacky::BinaryOp::LessThan => {
      instructions.push(Instruction::Cmp(convert_val(val2), convert_val(val1)));
      instructions.push(Instruction::Mov(Operand::Imm(0), convert_val(dst)));
      instructions.push(Instruction::SetCC(
        conditional_name_lookup(op),
        convert_val(dst),
      ));
    }
    _ => {
      instructions.push(Instruction::Mov(convert_val(&val1), convert_val(&dst)));
      instructions.push(Instruction::Binary(
        binary_op_name_lookup(op),
        convert_val(&val2),
        convert_val(&dst),
      ));
    }
  }
}

fn convert_unary_op(
  op: tacky::UnaryOp,
  src: &tacky::Val,
  dst: &tacky::Val,
  instructions: &mut Vec<Instruction>,
) {
  match op {
    tacky::UnaryOp::Not => {
      instructions.push(Instruction::Cmp(Operand::Imm(0), convert_val(src)));
      instructions.push(Instruction::Mov(Operand::Imm(0), convert_val(dst)));
      instructions.push(Instruction::SetCC(ConditionalCode::E, convert_val(dst)));
    }
    _ => {
      instructions.push(Instruction::Mov(convert_val(src), convert_val(dst)));
      instructions.push(Instruction::Unary(
        unary_op_name_lookup(op),
        convert_val(dst),
      ));
    }
  }
}

fn convert_val(val: &tacky::Val) -> Operand {
  match val {
    tacky::Val::Int(i) => Operand::Imm(*i),
    tacky::Val::Var(var) => Operand::Pseudo(var.clone()),
  }
}

fn unary_op_name_lookup(op: tacky::UnaryOp) -> UnaryOp {
  match op {
    tacky::UnaryOp::Neg => UnaryOp::Neg,
    tacky::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
    tacky::UnaryOp::Complement => UnaryOp::Complement,
    _ => todo!(),
  }
}

fn binary_op_name_lookup(op: tacky::BinaryOp) -> BinaryOp {
  match op {
    tacky::BinaryOp::Add => BinaryOp::Add,
    tacky::BinaryOp::Sub => BinaryOp::Sub,
    tacky::BinaryOp::Mul => BinaryOp::Mul,
    _ => unreachable!(),
  }
}

fn conditional_name_lookup(op: tacky::BinaryOp) -> ConditionalCode {
  match op {
    tacky::BinaryOp::GreaterThan => ConditionalCode::G,
    tacky::BinaryOp::GreaterOrEqual => ConditionalCode::GE,
    tacky::BinaryOp::LessThan => ConditionalCode::L,
    tacky::BinaryOp::LessOrEqual => ConditionalCode::LE,
    _ => unreachable!(),
  }
}
