use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/channel_declaration/channel_declaration.pir");
    execute(&path).await
}
