use std::error::Error;
use std::fmt::Display;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::ast;
use crate::ast::{Expr, Visitor};
use crate::error::LoxError;
use crate::error::LoxErrorKind;
use crate::scanner;
use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug, PartialEq)]
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

impl Interpreter {
    fn new() -> Self {
        Interpreter { had_error: false }
    }

    fn unary_op(&mut self, op: &Token, value: &Expr) -> Result<Value, LoxError> {
        match op.type_ {
            TokenType::Bang => {
                let expr = self.visit_expr(value)?;
                Ok(Value::Boolean(!Self::is_truthy(&expr)))
            }
            TokenType::Minus => {
                let expr_value = self.visit_expr(value)?;
                match expr_value {
                    Value::Number(n) => Ok(Value::Number(-1.0 * n)),
                    _ => Err(LoxError {
                        kind: LoxErrorKind::Runtime,
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

    fn binary_op(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, LoxError> {
        let left_value = self.visit_expr(left)?;
        let right_value = self.visit_expr(right)?;
        match (&left_value, &op.type_, &right_value) {
            (Value::Number(l), _, Value::Number(r)) => {
                let result = Self::binary_arithmentic_op(*l, op, *r)?;
                Ok(Value::Number(result))
            }
            (Value::String(l), TokenType::Plus, Value::String(r)) => {
                let mut result = l.clone();
                result.push_str(r);
                Ok(Value::String(result))
            }
            _ => Err(LoxError {
                kind: LoxErrorKind::Runtime,
                line: op.line,
                message: format!(
                    "Unsupported operand types for binary operator {} ({}, {})",
                    op.type_, left_value, right_value
                ),
            }),
        }
    }

    fn binary_arithmentic_op(left: f64, op: &Token, right: f64) -> Result<f64, LoxError> {
        match op.type_ {
            TokenType::Minus => Ok(left - right),
            TokenType::Plus => Ok(left + right),
            TokenType::Star => Ok(left * right),
            TokenType::Slash => Ok(left / right),
            _ => Err(LoxError {
                kind: LoxErrorKind::Runtime,
                line: op.line,
                message: format!("Unknown binary operator: {}", op.type_),
            }),
        }
    }
}

impl Visitor for Interpreter {
    type Result = Result<Value, LoxError>;

    fn visit_expr(&mut self, expr: &crate::ast::Expr) -> Self::Result {
        match expr {
            Expr::Literal(token) => match &token.type_ {
                TokenType::String(s) => Ok(Value::String(s.clone())),
                TokenType::Number(n) => Ok(Value::Number(*n)),
                TokenType::True => Ok(Value::Boolean(true)),
                TokenType::False => Ok(Value::Boolean(false)),
                _ => unreachable!("Unsupported literal type: {}", token.type_),
            },
            Expr::Unary(op, token) => self.unary_op(op, token.as_ref()),
            Expr::Binary(l, op, r) => self.binary_op(l, op, r),
            Expr::Grouping(expr) => self.visit_expr(expr),
        }
    }
}

fn run_internal(program: &str) -> Result<Value, LoxError> {
    let tokens = scanner::scan(program)?;
    let expr = ast::parse(tokens)?;
    let mut interpreter = Interpreter::new();
    let result = interpreter.visit_expr(&expr)?;
    Ok(result)
}

fn run(program: &str) -> Result<(), LoxError> {
    let program_result = run_internal(program)?;
    println!("{}", program_result);
    Ok(())
}

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let program = fs::read_to_string(path)?;
    run(&program)?;
    Ok(())
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

#[cfg(test)]
mod run_tests {
    use super::*;

    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                assert_eq!(expected, run_internal(input));
            }
        )*
        }
    }

    parametrized_tests!(
        empty_string: (
            "",
            Err(LoxError {
                kind: LoxErrorKind::Syntax,
                line: 1,
                message: "Expected expression.".to_owned(),
            })
        ),
        int_sum: (
            "1+2",
            Ok(Value::Number(3.0))
        ),
        arithmetic_operator_priority: (
            "2 + 2 * 2",
            Ok(Value::Number(6.0)),
        ),
        grouping_priority: (
            "2 * (2 + 2)",
            Ok(Value::Number(8.0)),
        ),
        number_plus_string_produces_error: (
            "\"abc\" + 123",
            Err(LoxError {
                kind: LoxErrorKind::Runtime,
                line: 1,
                message: "Unsupported operand types for binary operator + (String(abc), Number(123))".to_owned(),
            }),
        ),
        string_sum: (
            "\"abc\" + \"def\"",
            Ok(Value::String("abcdef".to_owned())),
        ),
    );
}
