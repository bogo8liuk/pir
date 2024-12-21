use crate::interpreter::ast;

type ToFlush = String;

pub fn eval_and_flush(process: ast::Process) {
    let to_flush = eval_process(process);
    let to_flush_str = to_flush.as_str();
    println!("{}", to_flush_str)
}

pub fn eval_process(process: ast::Process) -> ToFlush {
    match process {
        ast::Process::Eval(expr) => eval_expr(expr),
        ast::Process::Loop(_) => todo!(),
        ast::Process::NewChan(_, _) => todo!(),
    }
}

fn eval_expr(expr: ast::Expression) -> ToFlush {
    match expr {
        ast::Expression::Val(val) => eval_value(val),
        ast::Expression::IntExpr(_) => todo!(),
    }
}

fn eval_value(val: ast::Value) -> ToFlush {
    match val {
        ast::Value::Str(str) => {
            let wrapper = "\"";
            let pieces = [wrapper, str.as_str(), wrapper];
            pieces.concat()
        }
        ast::Value::Char(_) => todo!(),
        ast::Value::Int32(num) => num.to_string(),
        ast::Value::Float32(fl) => fl.to_string(),
    }
}
