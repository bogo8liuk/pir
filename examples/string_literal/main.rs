use pir::interpreter::execute;

fn main() {
    let path = String::from("examples/string_literal/string_literal.pir");
    execute(&path)
}
