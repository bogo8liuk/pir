#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Value {
    Char(u32),
    Str(String),
    Int32(i32),
    Float32(f32),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum IntExpr {
    Add(Box<IntExpr>, Box<IntExpr>),
    Sub(Box<IntExpr>, Box<IntExpr>),
    Mul(Box<IntExpr>, Box<IntExpr>),
    Div(Box<IntExpr>, Box<IntExpr>),
    Mod(Box<IntExpr>, Box<IntExpr>),
    Neg(Box<IntExpr>),
    Lit(i32),
}

#[derive(Debug, PartialOrd, Clone)]
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

#[derive(Debug, Clone)]
pub enum Process {
    Expr(Expression),
    Loop(Box<Process>),
    ChanDeclaration(String, Box<Process>),
    Par(Box<Process>, Box<Process>),
    Send(Expression, String, Box<Process>),
    Receive(String, String, Box<Process>),
}

// This is not equivalence notion for processes
impl PartialEq for Process {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Process::Expr(e1), Process::Expr(e2)) => e1 == e2,
            (Process::Loop(p1), Process::Loop(p2)) => p1 == p2,
            (Process::ChanDeclaration(id1, p1), Process::ChanDeclaration(id2, p2)) => {
                id1 == id2 && p1 == p2
            }
            (Process::Par(pa1, pa2), Process::Par(pb1, pb2)) => pa1 == pb1 && pa2 == pb2,
            (Process::Send(expr1, chan_id1, p1), Process::Send(expr2, chan_id2, p2)) => {
                expr1 == expr2 && chan_id1 == chan_id2 && p1 == p2
            }
            (Process::Receive(id1, chan_id1, p1), Process::Receive(id2, chan_id2, p2)) => {
                id1 == id2 && chan_id1 == chan_id2 && p1 == p2
            }
            _ => false,
        }
    }
}

impl From<Expression> for Process {
    fn from(value: Expression) -> Self {
        Process::Expr(value)
    }
}

impl From<IntExpr> for Process {
    fn from(value: IntExpr) -> Self {
        Process::Expr(Expression::IntExpr(value))
    }
}
