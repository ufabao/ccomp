pub mod ast;

#[derive(Debug)]
pub enum Program {
  Func(Function),
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  pub body: Block,
}

#[derive(Debug)]
pub struct Block {
  pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
  Statement(Statement),
  Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct Declaration {
  pub name: String,
  pub exp: Option<Expression>,
}

#[derive(Debug)]
pub enum Statement {
  Expression(Expression),
  Return(Expression),
  If(Expression, Box<Statement>, Option<Box<Statement>>),
  Compound(Block),
  Null,
}

#[derive(Debug, Clone)]
pub enum Expression {
  Int(i32),
  Var(String),
  Unary(UnaryOp, Box<Expression>),
  Binary(BinaryOp, Box<Expression>, Box<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
  Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  PrefixDec,
  Complement,
  Not,
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

impl FromIterator<BlockItem> for Block {
  fn from_iter<I: IntoIterator<Item = BlockItem>>(iter: I) -> Self {
    Block {
      items: iter.into_iter().collect(),
    }
  }
}
