use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};

use crate::interpreter::{ast::*, context::NamesStack};

type ToFlush = String;

pub async fn eval_and_flush(process: Process) {
    //let names_stack = NamesStack::new();
    let names_stack_handle = StackHandle::new();
    let to_flush = eval_process(Box::new(process), names_stack_handle).await;
    let to_flush_str = to_flush.as_str();
    println!("{}", to_flush_str)
}

/* The runtime context is handled with the actor pattern. No shared data, just
a task whose single goal is to handle requests to access the context (like the
stack). Requests (and responses) are implemented through message passing. This
could be sub-optimal (???), but it avoids the headaches that could cause things
like Arc + RwLock _et similia_, sources of ownership and lifetime issues,
especially with async recursion. */
async fn eval_process(process: Box<Process>, names_stack_handle: StackHandle) -> ToFlush {
    match *process {
        Process::Eval(expr) => eval_expr(&expr),
        Process::Loop(proc) => {
            loop {
                let handle_clone = names_stack_handle.clone();
                // Ast cloning is probably actually sub-optimal
                Box::pin(eval_process(proc.clone(), handle_clone)).await;
            }
            //"".to_owned()
        }
        Process::ChanDeclaration(name_id, proc) => {
            names_stack_handle.push_channel(name_id.to_owned()).await;
            Box::pin(eval_process(proc, names_stack_handle)).await
        }
        Process::Or(proc1, proc2) => {
            // This is necessary due to rustc typecheck. The clean way would be
            // pass the future returned by `eval_process` directly to spawn
            // function
            fn eval_wrap(
                process: Box<Process>,
                names_stack_handle: StackHandle,
            ) -> JoinHandle<ToFlush> {
                tokio::spawn(eval_process(process, names_stack_handle))
            }
            let handle_clone = names_stack_handle.clone();
            let join1 = eval_wrap(proc1, names_stack_handle);
            let join2 = eval_wrap(proc2, handle_clone);

            join1.await;
            join2.await;
            "".to_owned()
        }
    }
}

struct StackActor {
    receiver: mpsc::Receiver<StackMessage>,
    names_stack: NamesStack,
}

#[derive(Clone)]
struct StackHandle {
    sender: mpsc::Sender<StackMessage>,
}

struct StackMessage {
    payload: StackMessagePayload,
    respond_to: oneshot::Sender<()>,
}

enum StackMessagePayload {
    PushChannel(String),
}

impl StackActor {
    fn handle_message(&mut self, msg: StackMessage) {
        match msg.payload {
            StackMessagePayload::PushChannel(id) => {
                self.names_stack.push_channel(id);
                let _ = msg.respond_to.send(());
            }
        }
    }
}

async fn run_stack_actor(mut actor: StackActor) {
    while let Some(msg) = actor.receiver.recv().await {
        actor.handle_message(msg)
    }
}

impl StackHandle {
    fn new() -> Self {
        let (sender, receiver) = tokio::sync::mpsc::channel(500);
        let actor = StackActor {
            receiver,
            names_stack: NamesStack::new(),
        };

        tokio::spawn(run_stack_actor(actor));

        Self { sender }
    }

    async fn push_channel(&self, id: String) {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::PushChannel(id),
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
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

    //#[test]
    //fn should_add_name() {
    //    let mut names_stack = NamesStack::new();
    //    let process = Process::ChanDeclaration(
    //        "a_name".to_string(),
    //        Box::new(Process::Eval(Expression::IntExpr(IntExpr::Lit(7)))),
    //    );

    //    eval_process(&process, &mut names_stack);

    //    assert!(
    //        names_stack.lookup("a_name".to_owned()).is_some(),
    //        "Expected 'a_name' to be in the names stack"
    //    );
    //}
}
