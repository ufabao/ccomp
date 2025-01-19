use std::collections::HashMap;

use frontend::ast::typechecker::{Defined, IdentifierAttributes};

use super::{
  assemblyast::{AssemblyType, ConditionalCode, Function, StaticVariable, TopLevel},
  tacky_to_asm::BackendSymbols,
};
use crate::asm::assemblyast::{BinaryOp, Instruction, Operand, Program, Register, UnaryOp};

pub fn generate_assembly(prog: &Program, symbol_table: HashMap<String, BackendSymbols>) -> String {
  let mut assembly = String::new();

  for top_level in &prog.top_level {
    match top_level {
      TopLevel::Function(func) => {
        generate_function(func, &symbol_table, &mut assembly);
      }
      TopLevel::StaticVariable(var) => {
        generate_static_variable(var, &mut assembly);
      }
    }
  }
  assembly.push_str(&format!(r#".section .note.GNU-stack,"",@progbits"#));
  assembly.push('\n');
  assembly
}

fn generate_function(
  func: &Function,
  symbol_table: &HashMap<String, BackendSymbols>,
  assembly: &mut String,
) {
  if func.global || func.name == "main" {
    assembly.push_str(&format!(".globl {}\n", func.name));
  }
  assembly.push_str(&format!("{}:\n", func.name));
  assembly.push_str(".text\n");
  assembly.push_str("pushq %rbp\n");
  assembly.push_str("movq %rsp, %rbp\n");

  for instruction in &func.instructions {
    match instruction {
      Instruction::Mov(typ, src, dest) => {
        let size = get_type_size(typ);
        assembly.push_str(&format!(
          "mov{} {}, {}\n",
          get_type_prefix(typ),
          operand_to_string(&src, size),
          operand_to_string(&dest, size)
        ));
      }
      Instruction::MovSx(src, dst) => {
        assembly.push_str(&format!(
          "movslq {}, {}",
          operand_to_string(src, 4),
          operand_to_string(dst, 8)
        ));
      }
      Instruction::Unary(op, typ, val) => {
        assembly.push_str(&format!(
          "{}{} {}\n",
          unary_op_to_string(&op),
          get_type_prefix(typ),
          operand_to_string(&val, 4)
        ));
      }
      Instruction::Return => {
        // Add function epilogue before each return
        assembly.push_str("movq %rbp, %rsp\n");
        assembly.push_str("popq %rbp\n");
        assembly.push_str("ret\n");
      }
      Instruction::Binary(binary_op, typ, operand, operand1) => {
        let size = get_type_size(typ);
        assembly.push_str(&format!(
          "{}{} {}, {}\n",
          match binary_op {
            BinaryOp::Add => "add",
            BinaryOp::Sub => "sub",
            BinaryOp::Mul => "imul",
            _ => todo!(),
          },
          get_type_prefix(typ),
          operand_to_string(&operand, size),
          operand_to_string(&operand1, size)
        ));
      }
      Instruction::IDiv(typ, operand) => {
        assembly.push_str(&format!(
          "idiv{} {}\n",
          get_type_prefix(typ),
          operand_to_string(&operand, 4)
        ));
      }
      Instruction::Cdq(typ) => match typ {
        AssemblyType::LongWord => assembly.push_str("cdq\n"),
        AssemblyType::QuadWord => assembly.push_str("cdo\n"),
      },
      Instruction::Cmp(typ, operand1, operand2) => {
        let size = get_type_size(typ);
        assembly.push_str(&format!(
          "cmp{} {}, {}\n",
          get_type_prefix(typ),
          operand_to_string(&operand1, size),
          operand_to_string(&operand2, size)
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
      Instruction::Call(name) => {
        let plt_name = match symbol_table.get(name).unwrap() {
          BackendSymbols::FunEntry(true) => name.clone(),
          BackendSymbols::FunEntry(false) => format!("{}@PLT", name),
          _ => unreachable!(),
        };
        assembly.push_str(&format!("call {}\n", plt_name));
      }
      Instruction::Push(operand) => {
        assembly.push_str(&format!("pushq {}\n", operand_to_string(&operand, 8)));
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

fn generate_static_variable(var: &StaticVariable, assembly: &mut String) {
  {
    if var.global {
      assembly.push_str(&format!(".globl {}\n", var.name));
    }
    match var.init {
      frontend::ast::typechecker::StaticInit::Int(0) => {
        assembly.push_str(".bss\n");
        assembly.push_str(".align 4\n");
        assembly.push_str(&format!("{}:\n", var.name));
        assembly.push_str(".zero 4\n");
      }
      frontend::ast::typechecker::StaticInit::Int(n) => {
        assembly.push_str(".data\n");
        assembly.push_str(".align 4\n");
        assembly.push_str(&format!("{}:\n", var.name));
        assembly.push_str(&format!(".long {}\n", n));
      }
      frontend::ast::typechecker::StaticInit::Long(0) => {
        assembly.push_str(".bss\n");
        assembly.push_str(".align 8\n");
        assembly.push_str(&format!("{}:\n", var.name));
        assembly.push_str(".zero 8\n");
      }
      frontend::ast::typechecker::StaticInit::Long(n) => {
        assembly.push_str(".data\n");
        assembly.push_str(".align 8\n");
        assembly.push_str(&format!("{}:\n", var.name));
        assembly.push_str(&format!(".quad {}\n", n));
      }
    }
  }
}

fn operand_to_string(operand: &Operand, size: i32) -> String {
  match operand {
    Operand::Reg(reg) => match (reg, size) {
      (Register::Ax, 1) => "%al".to_string(),
      (Register::Ax, 4) => "%eax".to_string(),
      (Register::Ax, 8) => "%rax".to_string(),

      (Register::Cx, 1) => "%cl".to_string(),
      (Register::Cx, 4) => "%ecx".to_string(),
      (Register::Cx, 8) => "%rcx".to_string(),

      (Register::Dx, 1) => "%dl".to_string(),
      (Register::Dx, 4) => "%edx".to_string(),
      (Register::Dx, 8) => "%rdx".to_string(),

      (Register::Di, 1) => "%dil".to_string(),
      (Register::Di, 4) => "%edi".to_string(),
      (Register::Di, 8) => "%rdi".to_string(),

      (Register::Si, 1) => "%sil".to_string(),
      (Register::Si, 4) => "%esi".to_string(),
      (Register::Si, 8) => "%rsi".to_string(),

      (Register::R8, 1) => "%r8b".to_string(),
      (Register::R8, 4) => "%r8d".to_string(),
      (Register::R8, 8) => "%r8".to_string(),

      (Register::R9, 1) => "%r9b".to_string(),
      (Register::R9, 4) => "%r9d".to_string(),
      (Register::R9, 8) => "%r9".to_string(),

      (Register::R10, 1) => "%r10b".to_string(),
      (Register::R10, 4) => "%r10d".to_string(),
      (Register::R10, 8) => "%r10".to_string(),

      (Register::R11, 1) => "%r11b".to_string(),
      (Register::R11, 4) => "%r11d".to_string(),
      (Register::R11, 8) => "%r11".to_string(),

      (Register::Sp, _) => "%rsp".to_string(),

      _ => todo!(),
    },
    Operand::Stack(offset) => format!("{}(%rbp)", offset),
    Operand::Imm(imm) => format!("${}", imm),
    Operand::Data(data) => format!("{}(%rip)", data),
    Operand::Pseudo(_) => unreachable!(),
  }
}

fn unary_op_to_string(op: &UnaryOp) -> String {
  match op {
    UnaryOp::Neg => "neg",
    UnaryOp::PrefixDec => "decl",
    UnaryOp::Complement => "not",
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

fn get_type_prefix(typ: &AssemblyType) -> String {
  match typ {
    AssemblyType::LongWord => "l".to_string(),
    AssemblyType::QuadWord => "q".to_string(),
  }
}

fn get_type_size(typ: &AssemblyType) -> i32 {
  match typ {
    AssemblyType::LongWord => 4,
    AssemblyType::QuadWord => 8,
  }
}
