use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/float_literal/float_literal.pir");
    execute(&path).await
}
