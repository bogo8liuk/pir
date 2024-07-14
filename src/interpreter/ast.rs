#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Byte(u8),
    Str(String),
    Int32(i32),
    Float32(f32),
}

#[derive(Debug, PartialOrd)]
pub enum Expression {
    Val(Value),
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Val(v1), Expression::Val(v2)) => v1 == v2,
        }
    }
}

impl Eq for Expression {}

#[derive(Debug, PartialOrd)]
pub enum Process {
    Eval(Expression),
}

impl PartialEq for Process {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Process::Eval(e1), Process::Eval(e2)) => e1 == e2,
        }
    }
}
