use std::error::Error;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::scanner;

#[derive(Debug)]
struct Interpreter {
    had_error: bool,
}

impl Interpreter {}

fn run(program: &str) -> Result<(), Box<dyn Error>> {
    let tokens = scanner::scan(program);
    match tokens {
        Ok(tokens) => println!("{:?}", tokens),
        Err(e) => error(e.line, &format!("{:?}", e.kind)),
    };

    Ok(())
}

fn error(line: u32, message: &str) {
    report(line, "", message);
}

fn report(line: u32, where_: &str, message: &str) -> Result<(), io::Error> {
    let mut stderr = io::stderr();
    writeln!(&mut stderr, "[line {}] Error{}: {}", line, where_, message)
}

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let program = fs::read_to_string(path)?;
    run(&program)
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        let r = handle.read_line(&mut buffer)?;
        println!("buffer: {}", buffer);
        if r == 0 {
            return Ok(());
        }
        run(&buffer);
    }
}
