mod ast;
mod context;
mod eval;
mod parser;

use std::{
    fs::{self},
    io::Error,
};

use self::{eval::eval_and_flush, parser::parse};

pub async fn execute(path: &String) {
    match read_src(path) {
        Err(err) => println!("Unable to open the file: {}", err),
        Ok(src) => {
            let src_raw = src.as_str();
            match parse(src_raw) {
                Err(err) => println!("{}", err),
                Ok(proc) => eval_and_flush(proc).await,
            }
        }
    }
}

fn read_src(path: &String) -> Result<String, Error> {
    fs::read_to_string(path)
}
