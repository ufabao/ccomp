#[derive(Debug, Clone)]
pub enum Typename {
    Void,
    Int,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(i32),
}

pub type Block = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub rettype: Typename,
    pub params: Vec<(Typename, String)>,
    pub body: Block,
}

pub type Prog = Vec<Box<Func>>;
