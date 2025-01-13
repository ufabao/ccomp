pub mod assemblyast;
pub mod codegen;
mod tacky_to_asm;

use self::assemblyast::{ConditionalCode, FunctionDef, Instruction, Operand, Program, Register};
use self::tacky_to_asm::{convert_binary_op, convert_unary_op, convert_val};
use crate::tacky;
use crate::tacky::Tac;

pub fn tacky_to_asm_ast(tacky: &Tac) -> Program {
  let functions = tacky
    .functions
    .iter()
    .map(|function| FunctionDef {
      name: function.name.clone(),
      instructions: {
        let mut instructions: Vec<Instruction> = Vec::new();
        for instruction in &function.body {
          match instruction {
            tacky::Instruction::Return(val) => {
              instructions.push(Instruction::Mov(
                convert_val(val),
                Operand::Reg(Register::Ax),
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
            tacky::Instruction::FunctionCall(_, _, _) => todo!(),
          }
        }
        instructions
      },
    })
    .collect();
  Program { functions }
}
