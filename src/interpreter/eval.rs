use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};

use crate::interpreter::{ast::*, context::NamesStack};

use super::context;

type ToFlush = String;

#[derive(Debug)]
struct ProcError;

impl Display for ProcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*** Process error ***")
    }
}

pub async fn eval_and_flush(process: Process) {
    //let names_stack = NamesStack::new();
    let names_stack_handle = StackHandle::new();
    let to_flush_res = eval_process(Box::new(process), names_stack_handle).await;

    match to_flush_res {
        (Ok(to_flush), _) => println!("{}", to_flush.as_str()),
        (Err(err), names_stack_handle) => println!("{}", err), // TODO: print stack trace
    }
}

/* The runtime context is handled with the actor pattern. No shared data, just
a task whose single goal is to handle requests to access the context (like the
stack). Requests (and responses) are implemented through message passing. This
could be sub-optimal (???), but it avoids the headaches that could cause things
like Arc + RwLock _et similia_, sources of ownership and lifetime issues,
especially with async recursion. */
async fn eval_process(
    process: Box<Process>,
    names_stack_handle: StackHandle,
) -> (Result<ToFlush, ProcError>, StackHandle) {
    match *process {
        Process::Expr(expr) => (eval_expr(&expr), names_stack_handle),
        Process::Loop(proc) => {
            loop {
                let handle_clone = names_stack_handle.clone();
                // Ast cloning is probably actually sub-optimal
                let res = Box::pin(eval_process(proc.clone(), handle_clone)).await;

                match res {
                    (Ok(_), _) => (),
                    // Break on errors
                    (Err(_), _) => return res,
                }
            }
            //"".to_owned()
        }
        Process::ChanDeclaration(name_id, proc) => {
            names_stack_handle.push_channel(name_id.to_owned()).await;
            Box::pin(eval_process(proc, names_stack_handle)).await
        }
        Process::Par(proc1, proc2) => {
            // This is necessary due to rustc typecheck. The clean way would be
            // pass the future returned by `eval_process` directly to spawn
            // function
            fn eval_wrap(
                process: Box<Process>,
                names_stack_handle: StackHandle,
            ) -> JoinHandle<(Result<ToFlush, ProcError>, StackHandle)> {
                tokio::spawn(eval_process(process, names_stack_handle))
            }
            let handle_clone = names_stack_handle.clone();
            let join1 = eval_wrap(proc1, names_stack_handle);
            let join2 = eval_wrap(proc2, handle_clone);

            let res1 = join1.await.unwrap();
            let res2 = join2.await.unwrap();

            match (res1, res2) {
                ((Ok(v1), names_stack_handle), (Ok(v2), _)) => (
                    Ok(["<", v1.as_str(), ",", v2.as_str(), ">"].concat()),
                    // Completely arbitrary: returning the first stack handle
                    names_stack_handle,
                ),
                ((Err(e1), names_stack_handle), _) => (Err(e1), names_stack_handle),
                ((_, names_stack_handle), (Err(e2), _)) => (Err(e2), names_stack_handle),
            }
        }
        Process::Send(_, _, _) => todo!(),
        Process::Receive(_, _, _) => todo!(),
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
    respond_to: oneshot::Sender<StackMessageResponse>,
}

enum StackMessagePayload {
    PushChannel(String),
    Lookup(String),
}

enum StackMessageResponse {
    Nothing,
    StackValue(context::Value),
}

impl StackActor {
    fn handle_message(&mut self, msg: StackMessage) {
        match msg.payload {
            StackMessagePayload::PushChannel(id) => {
                self.names_stack.push_channel(id);
                let _ = msg.respond_to.send(StackMessageResponse::Nothing);
            }
            StackMessagePayload::Lookup(id) => {
                match self.names_stack.lookup(id) {
                    Some(val) => msg.respond_to.send(StackMessageResponse::StackValue(val)),
                    None => msg.respond_to.send(StackMessageResponse::Nothing),
                };
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

    async fn push_channel(&self, id: String) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::PushChannel(id),
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn lookup(&self, id: String) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::Lookup(id),
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }
}

fn eval_expr(expr: &Expression) -> Result<ToFlush, ProcError> {
    match expr {
        Expression::Val(val) => Ok(eval_value(val)),
        Expression::IntExpr(expr) => eval_int_expr(expr).map(|n| n.to_string()).into(),
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

struct ExprResult<T>(Result<T, ProcError>);

impl<T> ExprResult<T> {
    fn map<B>(self, f: impl FnOnce(T) -> B) -> ExprResult<B> {
        match self {
            ExprResult(Ok(x)) => ExprResult(Ok(f(x))),
            ExprResult(Err(err)) => ExprResult(Err(err)),
        }
    }

    fn to_result(self) -> Result<T, ProcError> {
        let ExprResult(res) = self;
        res
    }
}

impl<T> From<T> for ExprResult<T> {
    fn from(value: T) -> Self {
        ExprResult(Ok(value))
    }
}

impl<T> From<Result<T, ProcError>> for ExprResult<T> {
    fn from(value: Result<T, ProcError>) -> Self {
        ExprResult(value)
    }
}

impl<T> Into<Result<T, ProcError>> for ExprResult<T> {
    fn into(self) -> Result<T, ProcError> {
        self.to_result()
    }
}

impl<T: Add> Add for ExprResult<T> {
    type Output = ExprResult<<T as Add>::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ExprResult(Ok(n1)), ExprResult(Ok(n2))) => ExprResult(Ok(n1 + n2)),
            (ExprResult(Err(e1)), _) => ExprResult(Err(e1)),
            (_, ExprResult(Err(e2))) => ExprResult(Err(e2)),
        }
    }
}

impl<T: Sub> Sub for ExprResult<T> {
    type Output = ExprResult<<T as Sub>::Output>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ExprResult(Ok(n1)), ExprResult(Ok(n2))) => ExprResult(Ok(n1 - n2)),
            (ExprResult(Err(e1)), _) => ExprResult(Err(e1)),
            (_, ExprResult(Err(e2))) => ExprResult(Err(e2)),
        }
    }
}

impl<T: Mul> Mul for ExprResult<T> {
    type Output = ExprResult<<T as Mul>::Output>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ExprResult(Ok(n1)), ExprResult(Ok(n2))) => ExprResult(Ok(n1 * n2)),
            (ExprResult(Err(e1)), _) => ExprResult(Err(e1)),
            (_, ExprResult(Err(e2))) => ExprResult(Err(e2)),
        }
    }
}

impl<T: Div> Div for ExprResult<T> {
    type Output = ExprResult<<T as Div>::Output>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // TODO: check for division by 0 ???
            (ExprResult(Ok(n1)), ExprResult(Ok(n2))) => ExprResult(Ok(n1 / n2)),
            (ExprResult(Err(e1)), _) => ExprResult(Err(e1)),
            (_, ExprResult(Err(e2))) => ExprResult(Err(e2)),
        }
    }
}

impl<T: Rem> Rem for ExprResult<T> {
    type Output = ExprResult<<T as Rem>::Output>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ExprResult(Ok(n1)), ExprResult(Ok(n2))) => ExprResult(Ok(n1 % n2)),
            (ExprResult(Err(e1)), _) => ExprResult(Err(e1)),
            (_, ExprResult(Err(e2))) => ExprResult(Err(e2)),
        }
    }
}

impl<T: Neg> Neg for ExprResult<T> {
    type Output = ExprResult<<T as Neg>::Output>;

    fn neg(self) -> Self::Output {
        match self {
            ExprResult(Ok(n)) => ExprResult(Ok(-n)),
            ExprResult(Err(e)) => ExprResult(Err(e)),
        }
    }
}

fn eval_int_expr(expr: &IntExpr) -> ExprResult<i32> {
    match expr {
        IntExpr::Add(e1, e2) => eval_int_expr(e1) + eval_int_expr(e2),
        IntExpr::Sub(e1, e2) => eval_int_expr(e1) - eval_int_expr(e2),
        IntExpr::Mul(e1, e2) => eval_int_expr(e1) * eval_int_expr(e2),
        IntExpr::Div(e1, e2) => eval_int_expr(e1) / eval_int_expr(e2),
        IntExpr::Mod(e1, e2) => eval_int_expr(e1) % eval_int_expr(e2),
        IntExpr::Neg(e) => -eval_int_expr(e),
        IntExpr::Lit(i) => (*i).into(),
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::eval::{
        eval_process, Expression, IntExpr, Process, StackHandle, StackMessageResponse,
    };

    #[tokio::test]
    async fn should_add_name() {
        let names_stack_handle = StackHandle::new();
        let process = Process::ChanDeclaration(
            "a_name".to_string(),
            Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(7)))),
        );

        let (_, names_stack_handle) = eval_process(Box::new(process), names_stack_handle).await;

        let has_stack_value = match names_stack_handle.lookup("a_name".to_owned()).await {
            StackMessageResponse::StackValue(_) => true,
            _ => false,
        };
        assert!(
            has_stack_value,
            "Expected 'a_name' to be in the names stack"
        );
    }
}
