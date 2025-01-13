pub mod assemblyast;
pub mod codegen;
mod tacky_to_asm;

use std::cmp::min;

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
        let arg_registers = [
          Register::Di,
          Register::Si,
          Register::Dx,
          Register::Cx,
          Register::R8,
          Register::R9,
        ];

        let split_index = min(function.params.len(), 6);
        let (register_args, stack_args) = function.params.split_at(split_index);
        for (i, param) in register_args.iter().enumerate() {
          instructions.push(Instruction::Mov(
            Operand::Reg(arg_registers[i]),
            Operand::Pseudo(param.clone()),
          ));
        }
        for (i, param) in stack_args.iter().enumerate() {
          instructions.push(Instruction::Mov(
            Operand::Stack(((i + 1) * 8) as i32),
            Operand::Pseudo(param.clone()),
          ));
          instructions.push(Instruction::Push(Operand::Reg(Register::Ax)));
        }
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
            tacky::Instruction::FunctionCall(name, args, dst) => {
              let split_index = min(args.len(), 6);
              let (register_args, stack_args) = args.split_at(split_index);

              let stack_padding = if stack_args.len() % 2 == 0 { 8 } else { 0 };

              if stack_padding != 0 {
                instructions.push(Instruction::AllocateStack(stack_padding));
              }

              let mut reg_index = 0;
              for tacky_arg in register_args {
                let r = arg_registers[reg_index];
                let assembly_arg = convert_val(tacky_arg);
                instructions.push(Instruction::Mov(assembly_arg, Operand::Reg(r)));
                reg_index += 1;
              }

              for tacky_arg in stack_args.iter().rev() {
                let assembly_arg = convert_val(tacky_arg);
                match assembly_arg {
                  Operand::Reg(_) | Operand::Imm(_) => {
                    instructions.push(Instruction::Push(assembly_arg));
                  }
                  _ => {
                    instructions.push(Instruction::Mov(assembly_arg, Operand::Reg(Register::Ax)));
                    instructions.push(Instruction::Push(Operand::Reg(Register::Ax)));
                  }
                }
              }

              instructions.push(Instruction::Call(name.clone()));

              let bytes_to_remove: i32 = 8 * (stack_args.len() as i32) + stack_padding;
              if bytes_to_remove != 0 {
                instructions.push(Instruction::DeAllocateStack(bytes_to_remove));
              }

              let assembly_dst = convert_val(dst);
              instructions.push(Instruction::Mov(Operand::Reg(Register::Ax), assembly_dst));
            }
          }
        }
        instructions
      },
    })
    .collect();
  Program { functions }
}
