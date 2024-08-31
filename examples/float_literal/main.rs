use pir::interpreter::execute;

fn main() {
    let path = String::from("examples/float_literal/float_literal.pir");
    execute(&path)
}
