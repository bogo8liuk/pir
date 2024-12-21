#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Char(u32),
    Str(String),
    Int32(i32),
    Float32(f32),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntExpr {
    Add(Box<IntExpr>, Box<IntExpr>),
    Sub(Box<IntExpr>, Box<IntExpr>),
    Mul(Box<IntExpr>, Box<IntExpr>),
    Div(Box<IntExpr>, Box<IntExpr>),
    Mod(Box<IntExpr>, Box<IntExpr>),
    Neg(Box<IntExpr>),
    Lit(i32),
}

#[derive(Debug, PartialOrd)]
pub enum Expression {
    Val(Value),
    IntExpr(IntExpr),
}

impl From<IntExpr> for Expression {
    fn from(value: IntExpr) -> Self {
        Expression::IntExpr(value)
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Val(v1), Expression::Val(v2)) => v1 == v2,
            (Expression::IntExpr(e1), Expression::IntExpr(e2)) => e1 == e2,
            _ => false,
        }
    }
}

impl Eq for Expression {}

#[derive(Debug)]
pub enum Process {
    Eval(Expression),
    Loop(Box<Process>),
    NewChan(String, Box<Process>),
}

// This is not equivalence notion for processes
impl PartialEq for Process {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Process::Eval(e1), Process::Eval(e2)) => e1 == e2,
            (Process::Loop(p1), Process::Loop(p2)) => p1 == p2,
            (Process::NewChan(id1, p1), Process::NewChan(id2, p2)) => id1 == id2 && p1 == p2,
            _ => false,
        }
    }
}

impl From<Expression> for Process {
    fn from(value: Expression) -> Self {
        Process::Eval(value)
    }
}

impl From<IntExpr> for Process {
    fn from(value: IntExpr) -> Self {
        Process::Eval(Expression::IntExpr(value))
    }
}
