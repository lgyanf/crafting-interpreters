use std::error::Error;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

#[derive(Debug)]
struct Interpreter {
    had_error: bool,
}

impl Interpreter {}

fn run(program: &String) -> Result<(), Box<dyn Error>> {
    Ok(())
}

fn error(line: u32, message: &String) {
    report(line, &"".to_owned(), message);
}

fn report(line: u32, where_: &String, message: &String) -> Result<(), io::Error> {
    let mut stderr = io::stderr();
    writeln!(&mut stderr, "[line {}] Error{}: {}", line, where_, message)
}

pub fn run_file(path: String) -> Result<(), Box<dyn Error>> {
    let program = fs::read_to_string(path)?;
    run(&program)
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut buffer = String::new();
    loop {
        print!("> ");
        let r = handle.read_line(&mut buffer)?;
        if r == 0 {
            return Ok(());
        }
        // TODO: use result
        run(&buffer);
    }
}
