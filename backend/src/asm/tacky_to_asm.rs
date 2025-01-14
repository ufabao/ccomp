use super::assemblyast::{BinaryOp, ConditionalCode, Instruction, Operand, Register, UnaryOp};
use super::tacky;

pub(crate) fn convert_binary_op(
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
        Operand::Reg(Register::Ax),
      ));
      instructions.push(Instruction::Cdq);
      instructions.push(Instruction::IDiv(convert_val(val2)));
      instructions.push(Instruction::Mov(
        Operand::Reg(Register::Ax),
        convert_val(dst),
      ));
    }
    tacky::BinaryOp::Mod => {
      instructions.push(Instruction::Mov(
        convert_val(val1),
        Operand::Reg(Register::Ax),
      ));
      instructions.push(Instruction::Cdq);
      instructions.push(Instruction::IDiv(convert_val(val2)));
      instructions.push(Instruction::Mov(
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

pub(crate) fn convert_unary_op(
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

pub(crate) fn convert_val(val: &tacky::Val) -> Operand {
  match val {
    tacky::Val::Int(i) => Operand::Imm(*i),
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
