use pir::interpreter::execute;

fn main() {
    let path = String::from("examples/loop/loop.pir");
    execute(&path)
}
