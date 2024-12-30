use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/string_literal/string_literal.pir");
    execute(&path).await
}
