use std::error::Error;
use std::fmt::Display;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::ast;
use crate::ast::visitor::StatementVisitor;
use crate::ast::{Expr, Visitor};
use crate::error::LoxError;
use crate::error::LoxErrorKind;
use crate::scanner;
use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Value {
    fn to_debug_string(&self) -> String {
        match self {
            Value::Nil => "Nil".to_owned(),
            Value::Boolean(b) => format!("Boolean({})", b),
            Value::Number(n) => format!("Number({})", n),
            Value::String(s) => format!("String({})", s),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

struct Environment {
    map: std::collections::HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Self {
            map: std::collections::HashMap::new()
        }
    }

    fn get(&self, name: &str) -> Option<&Value> {
        self.map.get(name)
    }

    fn set(&mut self, name: String, value: Value) {
        self.map.insert(name, value);
    }
}

struct Interpreter <'a> {
    environment: Environment,
    stdout: &'a mut dyn std::io::Write,
}

impl<'a> Interpreter <'a> {
    fn new(stdout: &'a mut dyn std::io::Write) -> Self {
        Interpreter {
            environment: Environment::new(),
            stdout,
        }
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
                    op.type_, left_value.to_debug_string(), right_value.to_debug_string()
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

    pub fn interpret_expression(&mut self, expr: &crate::ast::Expr) -> Result<Value, LoxError> {
        self.visit_expr(expr)
    }

    pub fn execute_statement(&mut self, statement: &crate::ast::Statement) -> Result<(), LoxError> {
        self.visit_statement(statement)
    }
}

impl Visitor for Interpreter<'_> {
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
            Expr::Variable { name, line, } => match self.environment.get(name) {
                None => Err(LoxError {
                    kind: LoxErrorKind::Runtime,
                    message: format!("Name \"{}\" is not defined", name),
                    line: *line,
                }),
                // TODO: can we avoid cloning here?
                Some(value) => Ok((*value).clone()),
            },
        }
    }
}

impl StatementVisitor for Interpreter<'_> {
    type Result = Result<(), LoxError>;

    fn visit_statement(&mut self, statement: &ast::Statement) -> Self::Result {
        match statement {
            ast::Statement::Expression { expr } => {
                self.visit_expr(expr)?;
            }
            ast::Statement::Print { expr } => {
                let value = self.visit_expr(expr)?;
                println!("{}", value);
                writeln!(self.stdout, "{}", value);
            }
            ast::Statement::Var {
                name,
                initializer,
            } => {
                let value = match initializer {
                    None => Value::Nil,
                    Some(expr) => self.interpret_expression(expr)?
                };
                self.environment.set(name.clone(), value);
            },
        };
        Ok(())
    }
}

fn run_with_interpreter(program: &str, interpreter: &mut Interpreter) -> Result<(), Vec<LoxError>> {
    let scanner_result = scanner::scan(program);
    let tokens = match scanner_result {
        Ok(tokens) => tokens,
        Err(error) => return Err(vec![error]),
    };
    let statements = ast::parse(tokens)?;
    for statement in statements {
        if let Err(error) = interpreter.execute_statement(&statement) {
            return Err(vec![error]);
        };
    }
    Ok(())
}

fn run(program: &str) -> Result<(), Vec<LoxError>> {
    let mut stdout = std::io::stdout();
    let mut interpreter = Interpreter::new(&mut stdout);
    run_with_interpreter(program, &mut interpreter)
}

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let program = fs::read_to_string(path)?;
    match run(&program) {
        Ok(()) => Ok(()),
        // TODO: return more than 1 error
        Err(errors) => Err(Box::new(errors[0].clone())),
    }
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        let r = handle.read_line(&mut buffer)?;
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
                let mut stdout = Vec::new();
                let mut interpreter = Interpreter::new(&mut stdout);
                let (input, expected, expected_stdout) = $value;
                assert_eq!(expected, run_with_interpreter(input, &mut interpreter));
                assert_eq!(expected_stdout, stdout);
            }
        )*
        }
    }

    // TODO: use .visit_expr in these tests to test values
    parametrized_tests!(
        empty_string: (
            "",
            Ok(()),
            b"".to_vec(),
        ),
        int_sum: (
            "1+2;",
            Ok(()),
            b"".to_vec(),
        ),
        arithmetic_operator_priority: (
            "2 + 2 * 2;",
            Ok(()),
            b"".to_vec(),
        ),
        grouping_priority: (
            "2 * (2 + 2);",
            Ok(()),
            b"".to_vec(),
        ),
        number_plus_string_produces_error: (
            "\"abc\" + 123;",
            Err(vec![
                LoxError {
                    kind: LoxErrorKind::Runtime,
                    line: 1,
                    message: "Unsupported operand types for binary operator + (String(abc), Number(123))".to_owned(),
                },
            ]),
            b"".to_vec(),
        ),
        string_sum: (
            "print \"abc\" + \"def\";",
            Ok(()),
            b"abcdef\n".to_vec(),
        ),
        var_declaration: (
            "var a;
            print a;",
            Ok(()),
            b"Nil\n".to_vec(),
        ),
        var_declaration_with_initializer: (
            "var a = 1;
            print a;",
            Ok(()),
            b"1\n".to_vec(),
        ),
        var_declaration_with_initializer_expression: (
            "var a = 2 + 2 * 2;
            print a;",
            Ok(()),
            b"6\n".to_vec(),
        ),
    );
}
