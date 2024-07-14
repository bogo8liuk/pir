use crate::interpreter::ast;

type ToEval = String;

pub fn eval_and_flush(process: ast::Process) {
    let to_eval = prepare_evaluation_of_process(process);
    let to_eval_str = to_eval.as_str();
    println!("{}", to_eval_str)
}

pub fn prepare_evaluation_of_process(process: ast::Process) -> ToEval {
    match process {
        ast::Process::Eval(expr) => prepare_evaluation_of_expr(expr),
    }
}

fn prepare_evaluation_of_expr(expr: ast::Expression) -> ToEval {
    match expr {
        ast::Expression::Val(val) => prepare_evaluation_of_value(val),
    }
}

fn prepare_evaluation_of_value(val: ast::Value) -> ToEval {
    match val {
        ast::Value::Str(str) => str,
        ast::Value::Byte(_) => todo!(),
        ast::Value::Int32(num) => num.to_string(),
        ast::Value::Float32(_) => todo!(),
    }
}
