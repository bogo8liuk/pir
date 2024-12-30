use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/par/par.pir");
    execute(&path).await
}
