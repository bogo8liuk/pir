use std::env;

pub mod interpreter;

pub async fn interpreter_exe() {
    let args = env::args().collect::<Vec<String>>();
    let path = &args[1];
    interpreter::execute(path).await
}
