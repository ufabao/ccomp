use crate::backend::ast::ast;

pub fn ast_to_tacky(ast: &ast::Program) -> Program {
    let mut instructions: Vec<Instruction> = Vec::new();
    match ast {
        ast::Program::Func(func) => Program::Func(Function {
            name: func.name.clone(),
            instructions: {
                statement_to_tacky(&func.body, &mut instructions);
                instructions
            },
        }),
    }
}

fn statement_to_tacky(stmt: &ast::Statement, instructions: &mut Vec<Instruction>) {
    match stmt {
        ast::Statement::Return(expr) => {
            let result = expression_to_tacky(expr, instructions);
            instructions.push(Instruction::Return(result));
        }
    }
}

fn expression_to_tacky(expr: &ast::Expression, instructions: &mut Vec<Instruction>) -> Val {
    match expr {
        ast::Expression::Int(i) => Val::Int(*i),
        ast::Expression::Unary(op, expr) => {
            let val = expression_to_tacky(expr, instructions);
            let var = Val::Var(format!("t{}", instructions.len()));
            instructions.push(Instruction::Unary(unary_op_to_tacky(op), val, var.clone()));
            var
        }
    }
}

fn unary_op_to_tacky(op: &ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Neg => UnaryOp::Neg,
        ast::UnaryOp::PrefixDec => UnaryOp::PrefixDec,
        ast::UnaryOp::Complement => UnaryOp::Complement,
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

#[derive(Debug)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOp, Val, Val),
}

#[derive(Debug, Clone)]
pub enum Val {
    Int(i32),
    Var(String),
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    PrefixDec,
    Complement,
}
