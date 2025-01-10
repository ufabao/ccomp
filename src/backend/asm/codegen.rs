use crate::backend::asm::assemblyast::{Instruction, Operand, Program, Register, UnaryOp};

pub fn generate_assembly(prog: &Program) -> String {
    let mut assembly = String::new();

    match prog {
        Program::Func(func) => {
            assembly.push_str(&format!(".globl {}\n", func.name));
            assembly.push_str(&format!("{}:\n", func.name));
            assembly.push_str(&format!("pushq %rbp\n"));
            assembly.push_str(&format!("movq %rsp, %rbp\n"));

            for instruction in &func.instructions {
                match instruction {
                    Instruction::Mov(src, dest) => {
                        assembly.push_str(&format!(
                            "{} {}, {}\n",
                            "movl",
                            operand_to_string(&src),
                            operand_to_string(&dest)
                        ));
                    }
                    Instruction::Unary(op, val) => {
                        assembly.push_str(&format!(
                            "{} {}\n",
                            unary_op_to_string(&op),
                            operand_to_string(&val)
                        ));
                    }
                    Instruction::AllocateStack(n) => {
                        assembly.push_str(&format!("subq ${}, %rsp\n", n));
                    }
                    Instruction::Return => {
                        assembly.push_str("ret\n");
                    }
                }
            }
        }
    }
    assembly.push_str(&format!("movq %rbp, %rsp\n"));
    assembly.push_str("popq %rbp\n");
    assembly.push_str("ret\n");

    assembly
}

fn operand_to_string(operand: &Operand) -> String {
    match operand {
        Operand::Reg(reg) => match reg {
            Register::Rax => "%eax".to_string(),
            Register::R10d => "%r10d".to_string(),
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
    }
    .to_string()
}
