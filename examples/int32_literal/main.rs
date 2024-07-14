use pir::interpreter::execute;

fn main() {
    let path = String::from("examples/int32_literal/int32_literal.pir");
    execute(&path)
}
