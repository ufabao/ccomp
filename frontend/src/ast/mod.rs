pub mod looplabeling;
pub mod nameresolution;
pub mod typechecker;

pub trait Visitor {
  type Output;
  fn visit_program(&mut self, program: &Program) -> Self::Output;
  fn visit_function_decl(&mut self, function: &FunctionDecl) -> Self::Output;
  fn visit_variable_decl(&mut self, variable: &VariableDecl) -> Self::Output;
  fn visit_block_item(&mut self, block_item: &BlockItem) -> Self::Output;
  fn visit_declaration(&mut self, declaration: &Declaration) -> Self::Output;
  fn visit_statement(&mut self, statement: &Statement) -> Self::Output;
  fn visit_for_init(&mut self, for_init: &ForInit) -> Self::Output;
  fn visit_expression(&mut self, expression: &Expression) -> Self::Output;
  fn visit_expression_base(&mut self, expression: &Expression) -> Self::Output;
}

#[derive(Debug, Clone)]
pub struct Program {
  pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
  FuncDeclaration(FunctionDecl),
  VarDeclaration(VariableDecl),
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
  pub name: String,
  pub params: Vec<String>,
  pub body: Option<Block>,
  pub typ: Type,
  pub storage: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub struct VariableDecl {
  pub name: String,
  pub value: Option<Expression>,
  pub typ: Type,
  pub storage: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub enum Expression {
  Const(Const),
  Var(String),
  Cast(Type, Box<Expression>),
  Unary(UnaryOp, Box<Expression>),
  Binary(BinaryOp, Box<Expression>, Box<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
  Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
  FunctionCall(String, Vec<Expression>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
  Int,
  Long,
  FunType(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum StorageClass {
  Static,
  Extern,
}

#[derive(Debug, Clone)]
pub struct Block {
  pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
  Statement(Statement),
  Declaration(Declaration),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ForInit {
  Declaration(VariableDecl),
  Expression(Option<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum Const {
  Int(i32),
  Long(i64),
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
