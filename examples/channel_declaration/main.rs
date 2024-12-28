use pir::interpreter::execute;

fn main() {
    let path = String::from("examples/channel_declaration/channel_declaration.pir");
    execute(&path)
}
