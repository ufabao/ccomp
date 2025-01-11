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
  Unary(UnaryOp, Val, Val),        // UnaryOp(op, src, dst)
  Binary(BinaryOp, Val, Val, Val), // BinaryOp(op, src1, src2, dst)
  Copy(Val, Val),                  // Copy(src, dst)
  Jump(String),
  JumpIfZero(Val, String),
  JumpIfNotZero(Val, String),
  Label(String),
}

#[derive(Debug, Clone)]
pub enum Val {
  Int(i32),
  Var(String),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Equal,
  NotEqual,
  LessThan,
  LessOrEqual,
  GreaterThan,
  GreaterOrEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  PrefixDec,
  Complement,
  Not,
}
