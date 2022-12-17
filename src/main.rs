use std::env;
use std::error::Error;
use std::process;

mod interpreter;
mod scanner;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    match args.len() {
        0 => interpreter::run_prompt(),
        1 => interpreter::run_file(args.next().unwrap()),
        _ => {
            println!("Usage: lox [script]");
            process::exit(1);
        }
    }
}
