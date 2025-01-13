use super::assemblyast::ConditionalCode;
use crate::asm::assemblyast::{
  BinaryOp, FunctionDef, Instruction, Operand, Program, Register, UnaryOp,
};

pub fn generate_assembly(prog: &Program) -> String {
  let mut assembly = String::new();

  for func in &prog.functions {
    assembly.push_str(&format!(".globl {}\n", func.name));
    assembly.push_str(&format!("{}:\n", func.name));
    assembly.push_str("pushq %rbp\n");
    assembly.push_str("movq %rsp, %rbp\n");

    for instruction in &func.instructions {
      match instruction {
        Instruction::Mov(src, dest) => {
          assembly.push_str(&format!(
            "{} {}, {}\n",
            "movl",
            operand_to_string(&src, 4),
            operand_to_string(&dest, 4)
          ));
        }
        Instruction::Unary(op, val) => {
          assembly.push_str(&format!(
            "{} {}\n",
            unary_op_to_string(&op),
            operand_to_string(&val, 4)
          ));
        }
        Instruction::AllocateStack(n) => {
          assembly.push_str(&format!("subq ${}, %rsp\n", n));
        }
        Instruction::Return => {
          // Add function epilogue before each return
          assembly.push_str("movq %rbp, %rsp\n");
          assembly.push_str("popq %rbp\n");
          assembly.push_str("ret\n");
        }
        Instruction::Binary(binary_op, operand, operand1) => {
          assembly.push_str(&format!(
            "{} {}, {}\n",
            match binary_op {
              BinaryOp::Add => "addl",
              BinaryOp::Sub => "subl",
              BinaryOp::Mul => "imull",
              _ => todo!(),
            },
            operand_to_string(&operand, 4),
            operand_to_string(&operand1, 4)
          ));
        }
        Instruction::IDiv(operand) => {
          assembly.push_str(&format!("idivl {}\n", operand_to_string(&operand, 4)));
        }
        Instruction::Cdq => {
          assembly.push_str("cdq\n");
        }
        Instruction::Cmp(operand1, operand2) => {
          assembly.push_str(&format!(
            "cmpl {}, {}\n",
            operand_to_string(&operand1, 4),
            operand_to_string(&operand2, 4)
          ));
        }
        Instruction::Jmp(label) => {
          assembly.push_str(&format!("jmp .L{}\n", label));
        }
        Instruction::JmpCC(cc, label) => {
          assembly.push_str(&format!("j{} .L{}\n", cond_to_string(*cc), label));
        }
        Instruction::SetCC(cc, operand) => {
          assembly.push_str(&format!(
            "set{} {}\n",
            cond_to_string(*cc),
            operand_to_string(&operand, 1)
          ));
        }
        Instruction::Label(label) => {
          assembly.push_str(&format!(".L{}:\n", label));
        }
      }
    }

    // Only add function epilogue if the last instruction wasn't a Return
    if let Some(last_instruction) = func.instructions.last() {
      if !matches!(last_instruction, Instruction::Return) {
        assembly.push_str("movq %rbp, %rsp\n");
        assembly.push_str("popq %rbp\n");
        assembly.push_str("ret\n");
      }
    }
  }

  assembly
}

fn operand_to_string(operand: &Operand, size: usize) -> String {
  match operand {
    Operand::Reg(reg) => match (reg, size) {
      (Register::Ax, 1) => "%al".to_string(),
      (Register::Dx, 1) => "%dl".to_string(),
      (Register::R10, 1) => "%r10b".to_string(),
      (Register::R11, 1) => "%r11b".to_string(),
      (Register::Ax, _) => "%eax".to_string(),
      (Register::R10, _) => "%r10d".to_string(),
      (Register::Dx, _) => "%edx".to_string(),
      (Register::R11, _) => "%r11d".to_string(),
    },
    Operand::Stack(offset) => format!("{}(%rbp)", offset),
    Operand::Imm(imm) => "$".to_string() + &imm.to_string(),
    Operand::Pseudo(_) => unreachable!(),
  }
}

fn unary_op_to_string(op: &UnaryOp) -> String {
  match op {
    UnaryOp::Neg => "negl",
    UnaryOp::PrefixDec => "decl",
    UnaryOp::Complement => "notl",
    _ => todo!(),
  }
  .to_string()
}

fn cond_to_string(cc: ConditionalCode) -> String {
  match cc {
    ConditionalCode::E => "e",
    ConditionalCode::NE => "ne",
    ConditionalCode::G => "g",
    ConditionalCode::GE => "ge",
    ConditionalCode::L => "l",
    ConditionalCode::LE => "le",
  }
  .to_string()
}
