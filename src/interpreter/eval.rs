use crate::interpreter::{ast::*, context::NamesStack};

type ToFlush = String;

pub fn eval_and_flush(process: Process) {
    let mut names_stack = NamesStack::new();
    let to_flush = eval_process(&process, &mut names_stack);
    let to_flush_str = to_flush.as_str();
    println!("{}", to_flush_str)
}

pub fn eval_process(process: &Process, names_stack: &mut NamesStack) -> ToFlush {
    match process {
        Process::Eval(expr) => eval_expr(expr),
        Process::Loop(proc) => {
            loop {
                eval_process(proc, names_stack);
            }
            //"".to_owned()
        }
        Process::ChanDeclaration(name_id, proc) => {
            names_stack.push_channel(name_id.to_owned());
            eval_process(proc, names_stack)
        }
        Process::Or(_, _) => {
            todo!("TODO")
        }
    }
}

fn eval_expr(expr: &Expression) -> ToFlush {
    match expr {
        Expression::Val(val) => eval_value(val),
        Expression::IntExpr(expr) => eval_int_expr(expr).to_string(),
    }
}

fn eval_value(val: &Value) -> ToFlush {
    match val {
        Value::Str(str) => {
            let wrapper = "\"";
            let pieces = [wrapper, str.as_str(), wrapper];
            pieces.concat()
        }
        Value::Char(_) => todo!(),
        Value::Int32(num) => num.to_string(),
        Value::Float32(fl) => fl.to_string(),
    }
}

fn eval_int_expr(expr: &IntExpr) -> i32 {
    match expr {
        IntExpr::Add(e1, e2) => eval_int_expr(e1) + eval_int_expr(e2),
        IntExpr::Sub(e1, e2) => eval_int_expr(e1) - eval_int_expr(e2),
        IntExpr::Mul(e1, e2) => eval_int_expr(e1) * eval_int_expr(e2),
        IntExpr::Div(e1, e2) => eval_int_expr(e1) / eval_int_expr(e2),
        IntExpr::Mod(e1, e2) => eval_int_expr(e1) % eval_int_expr(e2),
        IntExpr::Neg(e) => -eval_int_expr(e),
        IntExpr::Lit(i) => *i,
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::{
        context::NamesStack,
        eval::{eval_process, Expression, IntExpr, Process},
    };

    #[test]
    fn should_add_name() {
        let mut names_stack = NamesStack::new();
        let process = Process::ChanDeclaration(
            "a_name".to_string(),
            Box::new(Process::Eval(Expression::IntExpr(IntExpr::Lit(7)))),
        );

        eval_process(&process, &mut names_stack);

        assert!(
            names_stack.lookup("a_name".to_owned()).is_some(),
            "Expected 'a_name' to be in the names stack"
        );
    }
}
