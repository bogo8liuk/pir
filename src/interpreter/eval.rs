use crate::interpreter::ast;

type ToEval = String;

pub fn eval_and_flush(process: ast::Process) {
    let to_eval = eval(process);
    let to_eval_str = to_eval.as_str();
    println!("{}", to_eval_str)
}

pub fn eval(process: ast::Process) -> ToEval {
    match process {
        ast::Process::Eval(expr) => eval_expression(expr),
    }
}

fn eval_expression(expr: ast::Expression) -> ToEval {
    match expr {
        ast::Expression::Val(val) => eval_value(val),
    }
}

fn eval_value(val: ast::Value) -> ToEval {
    match val {
        ast::Value::Str(str) => str,
        ast::Value::Byte(_) => todo!(),
        ast::Value::Int32(_) => todo!(),
        ast::Value::Float32(_) => todo!(),
    }
}
