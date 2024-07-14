use std::env;

mod interpreter;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let path = &args[1];
    interpreter::execute(path)
}
