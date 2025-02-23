use pir::interpreter::execute;

#[tokio::main]
async fn main() {
    let path = String::from("examples/send_receive/send_receive.pir");
    execute(&path).await
}
