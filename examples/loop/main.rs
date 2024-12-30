use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/loop/loop.pir");
    execute(&path).await
}
