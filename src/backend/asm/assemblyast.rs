use std::collections::HashMap;
use std::fmt;

use crate::backend::tacky::tacky;

#[derive(Debug, Default)]
pub struct TACPasses {
    identifiers: HashMap<String, i32>,
    variable_count: i32,
}

impl TACPasses {
    pub fn final_pass(&self, prog: &Program) -> Program {
        let function = match prog {
            Program::Func(func) => func,
        };

        let mut replaced_instructions: Vec<Instruction> = Vec::new();
        replaced_instructions.push(Instruction::AllocateStack(4 * self.variable_count));

        for instruction in &function.instructions {
            match instruction {
                Instruction::Mov(src, dst) => match (src, dst) {
                    (Operand::Stack(s), Operand::Stack(d)) => {
                        replaced_instructions.push(Instruction::Mov(
                            Operand::Stack(*s),
                            Operand::Reg(Register::R10d),
                        ));
                        replaced_instructions.push(Instruction::Mov(
                            Operand::Reg(Register::R10d),
                            Operand::Stack(*d),
                        ));
                    }
                    _ => {
                        replaced_instructions.push(instruction.clone());
                    }
                },
                Instruction::Unary(op, val) => {
                    replaced_instructions.push(Instruction::Unary(op.clone(), val.clone()));
                }
                Instruction::AllocateStack(n) => {
                    replaced_instructions.push(Instruction::AllocateStack(*n));
                }
                Instruction::Return => {
                    replaced_instructions.push(Instruction::Return);
                }
                Instruction::Binary(binary_op, operand1, operand2) => {
                    match (binary_op, operand1, operand2) {
                        (BinaryOp::Add | BinaryOp::Sub, Operand::Stack(n1), Operand::Stack(n2)) => {
                            replaced_instructions.push(Instruction::Mov(
                                Operand::Stack(*n1),
                                Operand::Reg(Register::R10d),
                            ));
                            replaced_instructions.push(Instruction::Binary(
                                *binary_op,
                                Operand::Reg(Register::R10d),
                                Operand::Stack(*n2),
                            ));
                        }
                        (BinaryOp::Mul, _, Operand::Stack(n)) => {
                            replaced_instructions.push(Instruction::Mov(
                                Operand::Stack(*n),
                                Operand::Reg(Register::R11d),
                            ));
                            replaced_instructions.push(Instruction::Binary(
                                *binary_op,
                                operand1.clone(),
                                Operand::Reg(Register::R11d),
                            ));
                            replaced_instructions.push(Instruction::Mov(
                                Operand::Reg(Register::R11d),
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
                            Operand::Reg(Register::R10d),
                        ));
                        replaced_instructions.push(Instruction::IDiv(Operand::Reg(Register::R10d)));
                    }
                    _ => {
                        replaced_instructions.push(Instruction::IDiv(operand.clone()));
                    }
                },
                Instruction::Cdq => {
                    replaced_instructions.push(Instruction::Cdq);
                }
            }
        }

        Program::Func(Function {
            name: function.name.clone(),
            instructions: replaced_instructions,
        })
    }

    pub fn replace_pseudo(&mut self, prog: &Program) -> Program {
        let function = match prog {
            Program::Func(func) => func,
        };

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
                    replaced_instructions
                        .push(Instruction::Unary(op.clone(), self.replace_operand(val)));
                }
                Instruction::AllocateStack(n) => {
                    replaced_instructions.push(instruction.clone());
                }
                Instruction::Return => {
                    replaced_instructions.push(Instruction::Return);
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
                Instruction::Cdq => {
                    replaced_instructions.push(Instruction::Cdq);
                }
            }
        }
        Program::Func(Function {
            name: function.name.clone(),
            instructions: replaced_instructions,
        })
    }

    fn replace_operand(&mut self, operand: &Operand) -> Operand {
        match operand {
            Operand::Imm(i) => Operand::Imm(*i),
            Operand::Reg(r) => Operand::Reg(*r),
            Operand::Pseudo(pseudo) => {
                if self.identifiers.contains_key(pseudo) {
                    Operand::Stack(*self.identifiers.get(pseudo).unwrap())
                } else {
                    self.variable_count += 1;
                    let stack_offset = -4 * self.variable_count;
                    self.identifiers.insert(pseudo.clone(), stack_offset);
                    Operand::Stack(stack_offset)
                }
            }
            Operand::Stack(s) => Operand::Stack(*s),
        }
    }
}

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
                            instructions.push(Instruction::Mov(convert_val(src), convert_val(dst)));
                            instructions
                                .push(Instruction::Unary(convert_unary_op(op), convert_val(dst)));
                        }
                        tacky::Instruction::Binary(op, val1, val2, dst) => match op {
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
                            _ => {
                                instructions
                                    .push(Instruction::Mov(convert_val(&val1), convert_val(&dst)));
                                instructions.push(Instruction::Binary(
                                    convert_binary_op(&op),
                                    convert_val(&val2),
                                    convert_val(&dst),
                                ));
                            }
                        },
                    }
                }
                instructions
            },
        }),
    }
}

fn convert_val(val: &tacky::Val) -> Operand {
    match val {
        tacky::Val::Int(i) => Operand::Imm(*i),
        tacky::Val::Var(var) => Operand::Pseudo(var.clone()),
    }
}

fn convert_unary_op(op: &tacky::UnaryOp) -> UnaryOp {
    match op {
        tacky::UnaryOp::Neg => UnaryOp::Neg,
        tacky::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
        tacky::UnaryOp::Complement => UnaryOp::Complement,
    }
}

fn convert_binary_op(op: &tacky::BinaryOp) -> BinaryOp {
    match op {
        tacky::BinaryOp::Add => BinaryOp::Add,
        tacky::BinaryOp::Sub => BinaryOp::Sub,
        tacky::BinaryOp::Mul => BinaryOp::Mul,
        _ => unreachable!(),
    }
}

#[derive(Debug)]
pub enum Program {
    Func(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOp, Operand),
    Binary(BinaryOp, Operand, Operand),
    IDiv(Operand),
    Cdq,
    AllocateStack(i32),
    Return,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    PrefixDec,
    Complement,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String),
    Stack(i32),
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    Rax,
    Rdx,
    R10d,
    R11d,
}

// impl fmt::Display for Program {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Program::Func(function) => write!(f, "{}", function),
//         }
//     }
// }

// impl fmt::Display for Function {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         writeln!(f, "Function {{")?;
//         writeln!(f, "    name: \"{}\",", self.name)?;
//         writeln!(f, "    instructions: [")?;
//         for instruction in &self.instructions {
//             writeln!(f, "        {},", instruction)?;
//         }
//         write!(f, "    ],")?;
//         writeln!(f, "}}")
//     }
// }

// impl fmt::Display for Instruction {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             // For Mov, dst and src are swapped in display to match AT&T syntax
//             Instruction::Mov(dst, src) => write!(f, "Mov({}, {})", dst, src),
//             Instruction::Unary(op, operand) => write!(f, "Unary({}, {})", op, operand),
//             Instruction::AllocateStack(size) => write!(f, "AllocateStack({})", size),
//             Instruction::Return => write!(f, "Return"),
//             Instruction::Binary(binary_op, operand, operand1) => todo!(),
//             Instruction::IDiv(operand) => todo!(),
//             Instruction::Cdq => todo!(),
//         }
//     }
// }

// impl fmt::Display for UnaryOp {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             UnaryOp::Neg => write!(f, "Neg"),
//             UnaryOp::PrefixDec => write!(f, "PrefixDec"),
//             UnaryOp::Complement => write!(f, "Complement"),
//         }
//     }
// }

// impl fmt::Display for Operand {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Operand::Imm(value) => write!(f, "Imm({})", value),
//             Operand::Reg(reg) => write!(f, "Reg({})", reg),
//             Operand::Pseudo(name) => write!(f, "Pseudo(\"{}\")", name),
//             Operand::Stack(offset) => write!(f, "Stack({})", offset),
//         }
//     }
// }

// impl fmt::Display for Register {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Register::Rax => write!(f, "rax"),
//             Register::R10d => write!(f, "r10d"),
//         }
//     }
// }

// // Helper function to pretty print a program
// pub fn pretty_print(program: &Program) -> String {
//     program.to_string()
// }
