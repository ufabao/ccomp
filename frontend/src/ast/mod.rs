pub mod looplabeling;
pub mod nameresolution;
pub mod typechecker;

pub trait Visitor<T: ExpressionType> {
  type Program;
  type FunctionDecl;
  type VariableDecl;
  type BlockItem;
  type Declaration;
  type Statement;
  type ForInit;
  type Expression;

  fn visit_program(&mut self, program: &Program<T>) -> Self::Program;
  fn visit_function_decl(&mut self, function: &FunctionDecl<T>) -> Self::FunctionDecl;
  fn visit_variable_decl(&mut self, variable: &VariableDecl<T>) -> Self::VariableDecl;
  fn visit_block_item(&mut self, block_item: &BlockItem<T>) -> Self::BlockItem;
  fn visit_declaration(&mut self, declaration: &Declaration<T>) -> Self::Declaration;
  fn visit_statement(&mut self, statement: &Statement<T>) -> Self::Statement;
  fn visit_for_init(&mut self, statement: &ForInit<T>) -> Self::ForInit;
  fn visit_expression(&mut self, expression: &T) -> Self::Expression;
}

pub trait Accept<T: ExpressionType> {
  fn accept<V: Visitor<T>>(&self, visitor: &mut V) -> <V as Visitor<T>>::Program;
}

impl<T: ExpressionType> Accept<T> for Program<T> {
  fn accept<V: Visitor<T>>(&self, visitor: &mut V) -> <V as Visitor<T>>::Program {
    visitor.visit_program(self)
  }
}

#[derive(Debug, Clone)]
pub enum Expression {
  Const(Const),
  Var(String),
  Cast(Type, Box<Expression>),
  Unary(UnaryOp, Box<Expression>),
  Binary(BinaryOp, Box<Expression>, Box<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
  FunctionCall(String, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
  Const(Const, Type),
  Var(String, Type),
  Cast(Type, Box<TypedExpression>),
  Unary(UnaryOp, Box<TypedExpression>, Type),
  Binary(BinaryOp, Box<TypedExpression>, Box<TypedExpression>, Type),
  Assignment(Box<TypedExpression>, Box<TypedExpression>, Type),
  FunctionCall(String, Vec<TypedExpression>, Type),
}

pub trait ExpressionType {}

impl ExpressionType for TypedExpression {}
impl ExpressionType for Expression {}

#[derive(Debug, Clone)]
pub struct Program<T: ExpressionType> {
  pub declarations: Vec<Declaration<T>>,
}

#[derive(Debug, Clone)]
pub enum Declaration<T: ExpressionType> {
  FuncDeclaration(FunctionDecl<T>),
  VarDeclaration(VariableDecl<T>),
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<T: ExpressionType> {
  pub name: String,
  pub params: Vec<String>,
  pub body: Option<Block<T>>,
  pub typ: Type,
  pub storage: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub struct VariableDecl<T: ExpressionType> {
  pub name: String,
  pub value: Option<T>,
  pub typ: Type,
  pub storage: Option<StorageClass>,
}

#[derive(Debug, Clone)]
pub struct Block<T: ExpressionType> {
  pub items: Vec<BlockItem<T>>,
}

#[derive(Debug, Clone)]
pub enum BlockItem<T: ExpressionType> {
  Statement(Statement<T>),
  Declaration(Declaration<T>),
}

#[derive(Debug, Clone)]
pub enum Statement<T: ExpressionType> {
  Expression(T),
  Return(T),
  If(T, Box<Statement<T>>, Option<Box<Statement<T>>>),
  Compound(Block<T>),
  Null,
  Break(String),
  Continue(String),
  While(T, Box<Statement<T>>, String),
  For(ForInit<T>, Option<T>, Option<T>, Box<Statement<T>>, String),
}

#[derive(Debug, Clone)]
pub enum ForInit<T: ExpressionType> {
  Declaration(VariableDecl<T>),
  Expression(Option<T>),
}

#[derive(Debug, Clone, Copy)]
pub enum Const {
  Int(i32),
  Long(i64),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
  Neg,
  PrefixDec,
  Complement,
  Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
