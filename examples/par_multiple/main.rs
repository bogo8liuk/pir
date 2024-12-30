use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/par_multiple/par_multiple.pir");
    execute(&path).await
}
