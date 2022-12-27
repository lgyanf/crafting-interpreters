use std::error::Error;
use std::fmt::Display;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::ast;
use crate::ast::visit::Visitor;
use crate::scanner;
use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug)]
enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Number(n) => write!(f, "Number({})", n),
            Value::String(s) => write!(f, "String({})", s),
        }
    }
}

struct InterpreterState {}

#[derive(Debug)]
struct Interpreter {
    had_error: bool,
}

struct RuntimeError {
    line: u32,
    message: String,
}

impl Interpreter {
    fn unary_op(&mut self, op: &Token, value: &ast::Expr) -> Result<Value, RuntimeError> {
        match op.type_ {
            TokenType::Bang => {
                let expr = self.visit_expr(&value)?;
                Ok(Value::Boolean(!Self::is_truthy(&expr)))
            }
            TokenType::Minus => {
                let expr_value = self.visit_expr(&value)?;
                match expr_value {
                    Value::Number(n) => Ok(Value::Number(-1.0 * n)),
                    _ => Err(RuntimeError {
                        line: op.line,
                        message: format!("Expected a number, got {}", expr_value),
                    }),
                }
            }
            _ => unreachable!("Unsupported unary operation: {}", op.type_),
        }
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl ast::visit::Visitor for Interpreter {
    type Result = Result<Value, RuntimeError>;

    fn visit_expr(&mut self, expr: &crate::ast::Expr) -> Self::Result {
        match expr {
            ast::Expr::Literal(token) => match &token.type_ {
                TokenType::String(s) => Ok(Value::String(s.clone())),
                TokenType::Number(n) => Ok(Value::Number(*n)),
                TokenType::True => Ok(Value::Boolean(true)),
                TokenType::False => Ok(Value::Boolean(false)),
                _ => unreachable!("Unsupported literal type: {}", token.type_),
            },
            ast::Expr::Unary(op, token) => self.unary_op(op, token.as_ref()),
            ast::Expr::Binary(_, _, _) => todo!(),
            ast::Expr::Grouping(_) => todo!(),
        }
    }
}

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
