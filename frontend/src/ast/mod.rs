pub mod looplabeling;
pub mod nameresolution;
pub mod typechecker;

pub trait Visitor {
  type Program;
  type FunctionDecl;
  type VariableDecl;
  type BlockItem;
  type Declaration;
  type Statement;
  type ForInit;
  type Expression;

  fn visit_program(&mut self, program: &Program) -> Self::Program;
  fn visit_function_decl(&mut self, function: &FunctionDecl) -> Self::FunctionDecl;
  fn visit_variable_decl(&mut self, variable: &VariableDecl) -> Self::VariableDecl;
  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::BlockItem;
  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Declaration;
  fn visit_statement(&mut self, statement: &Statement) -> Self::Statement;
  fn visit_for_init(&mut self, statement: &ForInit) -> Self::ForInit;
  fn visit_expression(&mut self, expression: &Expression) -> Self::Expression;
}

pub trait Accept {
  fn accept<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Program;
}

impl Accept for Program {
  fn accept<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Program {
    visitor.visit_program(self)
  }
}

#[derive(Debug)]
pub enum Program {
  Program(Vec<FunctionDecl>),
}

#[derive(Debug)]
pub enum Declaration {
  FuncDeclaration(FunctionDecl),
  VarDeclaration(VariableDecl),
}

#[derive(Debug)]
pub struct FunctionDecl {
  pub name: String,
  pub params: Vec<String>,
  pub body: Option<Block>,
}

#[derive(Debug)]
pub struct VariableDecl {
  pub name: String,
  pub value: Option<Expression>,
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

#[derive(Debug)]
pub enum Statement {
  Expression(Expression),
  Return(Expression),
  If(Expression, Box<Statement>, Option<Box<Statement>>),
  Compound(Block),
  Null,
  Break(String),
  Continue(String),
  While(Expression, Box<Statement>, String),
  For(
    ForInit,
    Option<Expression>,
    Option<Expression>,
    Box<Statement>,
    String,
  ),
}

#[derive(Debug)]
pub enum ForInit {
  Declaration(VariableDecl),
  Expression(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
  Int(i32),
  Var(String),
  Unary(UnaryOp, Box<Expression>),
  Binary(BinaryOp, Box<Expression>, Box<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
  Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
  FunctionCall(String, Vec<Expression>),
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
