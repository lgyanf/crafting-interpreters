#![allow(dead_code)]
#![allow(unused_must_use)]

use std::env;
use std::error::Error;
use std::process;

mod ast;
mod error;
mod interpreter;
mod scanner;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args();
    match args.len() {
        1 => interpreter::run_prompt(),
        2 => interpreter::run_file(&args.last().unwrap()),
        _ => {
            println!("Usage: lox [script]");
            process::exit(1);
        }
    }
}
