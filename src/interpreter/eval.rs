use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    time::Duration,
};

use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};

use crate::interpreter::{ast::*, context::NamesStack, context::Value as ContextValue};

use super::{
    actor::{ActorId, ExtendedOption},
    context::{self, ChannelData, NameId},
};

type ToFlush = String;
type EvalResult = (Result<ToFlush, ProcError>, StackHandle);

#[derive(Debug)]
enum ProcError {
    Generic,
    ExpectingOneAid(ExtendedOption<ActorId>),
    ExpectingTwoAid(ExtendedOption<ActorId>),
    NameIdNotFound(NameId),
}

impl Display for ProcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcError::Generic => writeln!(f, "*** Process error ***"),
            ProcError::ExpectingOneAid(e) => writeln!(
                f,
                "*** Unreachable code ***\nExpecting one aid, found {:#?}",
                e
            ),
            ProcError::ExpectingTwoAid(e) => writeln!(
                f,
                "*** Unreachable code ***\nExpecting two aid, found {:#?}",
                e
            ),
            ProcError::NameIdNotFound(name_id) => writeln!(f, "Name id <{}> not found", name_id),
        }
    }
}

pub async fn eval_and_flush(process: Process) {
    let (names_stack_handle, _) = StackHandle::new();
    let to_flush_res = eval_process(Box::new(process), names_stack_handle.clone(), None).await;

    match to_flush_res {
        (Ok(to_flush), names_stack_handle) => {
            println!("{}", to_flush.as_str());

            match names_stack_handle.get_stack_trace().await {
                StackMessageResponse::StackTrace(stack_trace) => println!("{}", stack_trace),
                _ => (),
            }
        }
        (Err(err), names_stack_handle) => {
            println!("{}", err);

            match names_stack_handle.get_stack_trace().await {
                StackMessageResponse::StackTrace(stack_trace) => println!("{}", stack_trace),
                _ => (),
            }
        }
    };

    // idle
    loop {
        tokio::time::sleep(Duration::from_secs(3)).await;
        match names_stack_handle.get_stack_trace().await {
            StackMessageResponse::StackTrace(stack_trace) => println!("{}", stack_trace),
            _ => (),
        }
    }
}

fn make_value_flushable(v: ContextValue) -> ToFlush {
    match v {
        ContextValue::I32(n) => n.to_string(),
        ContextValue::Channel(_) => "@".to_owned(),
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
    parent_id: Option<ActorId>,
) -> EvalResult {
    match *process {
        Process::Expr(expr) => (
            eval_expr(&expr).map(make_value_flushable),
            names_stack_handle,
        ),
        Process::Loop(proc) => {
            loop {
                let handle_clone = names_stack_handle.clone();
                // Ast cloning is probably actually sub-optimal
                let res =
                    Box::pin(eval_process(proc.clone(), handle_clone, parent_id.clone())).await;

                match res {
                    (Ok(_), _) => (),
                    // Break on errors
                    (Err(_), _) => return res,
                }
            }
            //"".to_owned()
        }
        Process::ChanDeclaration(name_id, proc) => {
            let actor_id_res = make_actor_id(
                &parent_id,
                &Process::ChanDeclaration(name_id.clone(), proc.clone()),
            );
            match actor_id_res {
                ExtendedOption::One(aid) => {
                    names_stack_handle.push_channel(aid.clone(), name_id).await;
                    Box::pin(eval_process(proc, names_stack_handle, Some(aid))).await
                }
                e => (Err(ProcError::ExpectingOneAid(e)), names_stack_handle),
            }
        }
        Process::Par(proc1, proc2) => {
            // This is necessary due to rustc typecheck. The clean way would be
            // pass the future returned by `eval_process` directly to spawn
            // function
            fn spawn_eval(
                process: Box<Process>,
                names_stack_handle: StackHandle,
                parent_id: Option<ActorId>,
            ) -> JoinHandle<EvalResult> {
                tokio::spawn(eval_process(process, names_stack_handle, parent_id))
            }
            match make_actor_id(&parent_id, &Process::Par(proc1.clone(), proc2.clone())) {
                ExtendedOption::Two(aid1, aid2) => {
                    names_stack_handle
                        .push_two_aid(aid1.clone(), aid2.clone())
                        .await;

                    let _join1 = spawn_eval(proc1, names_stack_handle.clone(), Some(aid1));
                    let _join2 = spawn_eval(proc2, names_stack_handle.clone(), Some(aid2));

                    (Ok("".to_owned()), names_stack_handle)
                }
                e => (Err(ProcError::ExpectingTwoAid(e)), names_stack_handle),
            }
        }
        Process::Send(expr, name_id, continuation) => {
            let parent_id_clone = parent_id.clone();
            // If there is no parent id, then no declaration for anything
            // has been done, so the name_id cannot be found
            if parent_id_clone.is_none() {
                return (Err(ProcError::NameIdNotFound(name_id)), names_stack_handle);
            }

            let aid = parent_id_clone.unwrap();

            let expr_res = eval_expr(&expr);
            match expr_res {
                Ok(val) => {
                    let send_trial = names_stack_handle
                        .lookup_channel(aid, name_id.clone())
                        .await
                        .and_with_channel(
                            |sender| {
                                let _ = sender.send(ContextValue::I32(42) /*val*/);
                            },
                            ProcError::NameIdNotFound(name_id),
                        );
                    match send_trial {
                        Ok(_) => {
                            Box::pin(eval_process(continuation, names_stack_handle, parent_id))
                                .await
                        }
                        Err(proc_err) => (Err(proc_err), names_stack_handle),
                    }
                }
                Err(_) => (expr_res.map(make_value_flushable), names_stack_handle),
            }
        }
        Process::Receive(var_id, name_id, continuation) => {
            let parent_id_clone = parent_id.clone();
            // If there is no parent id, then no declaration for anything has
            // been done, so the name_id cannot be found
            if parent_id_clone.is_none() {
                return (Err(ProcError::NameIdNotFound(name_id)), names_stack_handle);
            }
            let actor_id_res = make_actor_id(
                &parent_id,
                &Process::Receive(var_id.clone(), name_id.clone(), continuation.clone()),
            );

            match actor_id_res {
                ExtendedOption::One(aid) => {
                    let lookup_res = names_stack_handle
                        .lookup_channel(aid.clone(), name_id.clone())
                        .await
                        .and_with_channel(
                            |sender| sender.subscribe(),
                            ProcError::NameIdNotFound(name_id.clone()),
                        );

                    match lookup_res {
                        Err(err) => (Err(err), names_stack_handle),
                        Ok(mut receiver) => {
                            let val = receiver.recv().await.unwrap(); //TODO: eval error
                            names_stack_handle
                                .push_value(aid.clone(), var_id, val)
                                .await;
                            Box::pin(eval_process(
                                continuation,
                                names_stack_handle,
                                Some(aid.clone()),
                            ))
                            .await
                        }
                    }
                }
                e => (Err(ProcError::ExpectingOneAid(e)), names_stack_handle),
            }
        }
    }
}

fn make_actor_id(
    eventual_parent_id: &Option<ActorId>,
    process: &Process,
) -> ExtendedOption<ActorId> {
    match eventual_parent_id {
        Some(parent_id) => ActorId::from_parent(parent_id, process),
        None => ActorId::root(process),
    }
}

struct StackActor {
    receiver: mpsc::Receiver<StackMessage>,
    names_stack: NamesStack<ActorId>,
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
    PushAid {
        aid: ActorId,
    },
    PushTwoAid {
        aid1: ActorId,
        aid2: ActorId,
    },
    PushChannel {
        aid: ActorId,
        name_id: NameId,
    },
    PushValue {
        aid: ActorId,
        name_id: NameId,
        value: ContextValue,
    },
    Lookup {
        aid: ActorId,
        name_id: NameId,
    },
    LookupChannel {
        aid: ActorId,
        name_id: NameId,
    },
    GetStackTrace,
    /*
    SpawnTask {
        future: Pin<Box<dyn Future<Output = EvalResult> + Send>>,
    },
    SpawnMultipleTasks {
        futures:
            Vec<Pin<Box<dyn Future<Output = EvalResult> + Send>>>,
    },
    */
}

enum StackMessageResponse {
    Nothing,
    StackValue(ContextValue),
    // This is just a shortcut for StackValue with unwrapped channel
    StackChannel(context::ChannelData),
    StackTrace(String),
}

impl StackMessageResponse {
    fn and_with_channel<F, E, U>(self, continuation: F, default: E) -> Result<U, E>
    where
        F: FnOnce(ChannelData) -> U,
    {
        match self {
            StackMessageResponse::StackChannel(sender) => Ok(continuation(sender)),
            _ => Err(default),
        }
    }
}

impl StackActor {
    fn handle_message(&mut self, msg: StackMessage) {
        match msg.payload {
            StackMessagePayload::PushChannel { aid, name_id } => {
                self.names_stack.push_channel(aid, name_id);
                let _ = msg.respond_to.send(StackMessageResponse::Nothing);
            }
            StackMessagePayload::PushValue {
                aid,
                name_id,
                value,
            } => {
                self.names_stack.push(aid, name_id, value);
                let _ = msg.respond_to.send(StackMessageResponse::Nothing);
            }
            StackMessagePayload::Lookup { aid, name_id } => {
                let _ = match self.names_stack.lookup(aid, name_id) {
                    Some(val) => msg.respond_to.send(StackMessageResponse::StackValue(val)),
                    None => msg.respond_to.send(StackMessageResponse::Nothing),
                };
            }
            StackMessagePayload::PushAid { aid } => {
                self.names_stack.push_pid(aid);
                let _ = msg.respond_to.send(StackMessageResponse::Nothing);
            }
            StackMessagePayload::PushTwoAid { aid1, aid2 } => {
                self.names_stack.push_pid(aid1);
                self.names_stack.push_pid(aid2);
                let _ = msg.respond_to.send(StackMessageResponse::Nothing);
            }
            StackMessagePayload::LookupChannel { aid, name_id } => {
                let _ = match self.names_stack.lookup_channel(aid, name_id) {
                    Some(ContextValue::Channel(data)) => msg
                        .respond_to
                        .send(StackMessageResponse::StackChannel(data)),
                    Some(_) => msg.respond_to.send(StackMessageResponse::Nothing),
                    None => msg.respond_to.send(StackMessageResponse::Nothing),
                };
            }
            StackMessagePayload::GetStackTrace => {
                let stack_trace = self.names_stack.draw();
                let _ = msg
                    .respond_to
                    .send(StackMessageResponse::StackTrace(stack_trace));
            } /*
              StackMessagePayload::SpawnTask { future } => {
                  self.jobs_handler.spawn(future);
                  let _ = msg.respond_to.send(StackMessageResponse::Nothing);
              }
              StackMessagePayload::SpawnMultipleTasks { futures } => {
                  futures.into_iter().for_each(|future| {
                      self.jobs_handler.spawn(future);
                  });
                  let _ = msg.respond_to.send(StackMessageResponse::Nothing);
              }
              */
        }
    }
}

async fn run_stack_actor(mut actor: StackActor) {
    while let Some(msg) = actor.receiver.recv().await {
        actor.handle_message(msg)
    }
}

impl StackHandle {
    fn new() -> (Self, JoinHandle<()>) {
        let (sender, receiver) = tokio::sync::mpsc::channel(500); //TODO: remove hardcoding
        let actor = StackActor {
            receiver,
            names_stack: NamesStack::new(),
            //jobs_handler: JoinSet::new(),
        };

        let join = tokio::spawn(run_stack_actor(actor));

        (Self { sender }, join)
    }

    async fn get_stack_trace(&self) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::GetStackTrace,
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn push_aid(&self, aid: ActorId) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::PushAid { aid },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn push_two_aid(&self, aid1: ActorId, aid2: ActorId) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::PushTwoAid { aid1, aid2 },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn push_channel(&self, aid: ActorId, name_id: NameId) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::PushChannel { aid, name_id },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn push_value(
        &self,
        aid: ActorId,
        name_id: NameId,
        value: ContextValue,
    ) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::PushValue {
                aid,
                name_id,
                value,
            },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn lookup(&self, aid: ActorId, name_id: String) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::Lookup { aid, name_id },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn lookup_channel(&self, aid: ActorId, name_id: String) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::LookupChannel { aid, name_id },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    /*
    async fn add_join_handle(&self, handle: JoinHandle<EvalResult>) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::AddJoinHandle { handle },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }

    async fn add_multiple_join_handles(
        &self,
        handles: Vec<JoinHandle<EvalResult>>,
    ) -> StackMessageResponse {
        let (send, recv) = oneshot::channel();
        let msg = StackMessage {
            payload: StackMessagePayload::AddMultipleJoinHandles { handles },
            respond_to: send,
        };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Stack actor task has been killed")
    }
    */
}

fn eval_expr(expr: &Expression) -> Result<ContextValue, ProcError> {
    match expr {
        Expression::Val(val) => Ok(ContextValue::I32(42)),
        Expression::IntExpr(expr) => eval_int_expr(expr)
            .map(|n| ContextValue::I32(n))
            .to_result(),
    }
}

fn eval_single_value(val: &Value) -> ContextValue {
    match val {
        Value::Str(str) => {
            let wrapper = "\"";
            let pieces = [wrapper, str.as_str(), wrapper];
            pieces.concat();
            ContextValue::I32(42)
        }
        Value::Char(char) => {
            char.to_string();
            ContextValue::I32(42)
        }
        Value::Int32(num) => ContextValue::I32(*num),
        Value::Float32(fl) => {
            fl.to_string();
            ContextValue::I32(42)
        }
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
    use tokio::sync::{
        broadcast,
        mpsc::{self},
    };

    use crate::interpreter::{
        actor::ActorId,
        eval::{eval_process, Expression, IntExpr, Process, StackHandle, StackMessageResponse},
    };

    #[tokio::test]
    async fn test_tokio_message_passing() {
        let (sender, receiver): (mpsc::Sender<String>, mpsc::Receiver<String>) = mpsc::channel(4);
        let (sender1, receiver1): (broadcast::Sender<String>, broadcast::Receiver<String>) =
            broadcast::channel(4);

        //sender.subscribe();  //This cannot create new receiver
        sender1.subscribe(); //This create a new receiver
    }

    #[tokio::test]
    async fn should_add_name() {
        let (names_stack_handle, _) = StackHandle::new();
        let process = Process::ChanDeclaration(
            "a_name".to_string(),
            Box::new(Process::Expr(Expression::IntExpr(IntExpr::Lit(7)))),
        );

        let (_, names_stack_handle) =
            eval_process(Box::new(process.clone()), names_stack_handle, None).await;

        let aid = ActorId::root(&process).unwrap_one();
        let has_stack_value = match names_stack_handle.lookup(aid, "a_name".to_owned()).await {
            StackMessageResponse::StackValue(_) => true,
            _ => false,
        };
        assert!(
            has_stack_value,
            "Expected 'a_name' to be in the names stack"
        );
    }
}
