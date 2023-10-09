mod environment;
mod value;

use std::error::Error;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::ast;
use crate::ast::expr::BinaryOp;
use crate::ast::expr::ExprType;
use crate::ast::expr::UnaryOp;
use crate::ast::visitor::StatementVisitor;
use crate::ast::{Expr, Visitor};
use crate::error::LoxError;
use crate::error::LoxErrorKind;
use crate::position::PositionRange;
use crate::scanner;
use super::interpreter::environment::Environment;
use super::interpreter::value::Value;




struct Interpreter<'a> {
    environment: Environment,
    stdout: &'a mut dyn std::io::Write,
}

impl<'a> Interpreter<'a> {
    fn new(stdout: &'a mut dyn std::io::Write) -> Self {
        Interpreter {
            environment: Environment::new(),
            stdout,
        }
    }

    fn unary_op(
        &mut self,
        op: &UnaryOp,
        value: &Expr,
        position: &PositionRange,
    ) -> Result<Value, LoxError> {
        match op {
            UnaryOp::Bang => {
                let expr = self.visit_expr(value)?;
                Ok(Value::Boolean(!Self::is_truthy(&expr)))
            }
            UnaryOp::Minus => {
                let expr_value = self.visit_expr(value)?;
                match expr_value {
                    Value::Number(n) => Ok(Value::Number(-1.0 * n)),
                    _ => Err(LoxError {
                        kind: LoxErrorKind::Runtime,
                        position: position.clone(),
                        message: format!("Expected a number, got {}", expr_value),
                    }),
                }
            }
        }
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            Value::Number(f) => *f > 0.0,
            _ => true,
        }
    }

    fn binary_op(
        &mut self,
        left: &Expr,
        op: &BinaryOp,
        right: &Expr,
        position: &PositionRange,
    ) -> Result<Value, LoxError> {
        // TOOD: right value should be calculated lazily
        let left_value = self.visit_expr(left)?;
        let right_value = self.visit_expr(right)?;
        match (&left_value, &op, &right_value) {
            (_, BinaryOp::And, _) => Ok(if !Self::is_truthy(&left_value) { left_value } else { right_value }),
            (_, BinaryOp::Or, _) => Ok(if Self::is_truthy(&left_value) { left_value } else { right_value }),
            (Value::Number(l), _, Value::Number(r)) => {
                let result = Self::binary_arithmentic_op(*l, op, *r, position);
                Ok(result)
            }
            (Value::String(l), BinaryOp::Plus, Value::String(r)) => {
                let mut result = l.clone();
                result.push_str(r);
                Ok(Value::String(result))
            }
            (_, BinaryOp::EqualEqual, _) => Ok(Value::Boolean(left_value == right_value)),
            (_, BinaryOp::BangEqual, _) => Ok(Value::Boolean(left_value != right_value)),
            _ => Err(LoxError {
                kind: LoxErrorKind::Runtime,
                position: position.clone(),
                message: format!(
                    "Unsupported operand types for binary operator {} ({}, {})",
                    op,
                    left_value.to_debug_string(),
                    right_value.to_debug_string()
                ),
            }),
        }
    }

    fn binary_arithmentic_op(
        left: f64,
        op: &BinaryOp,
        right: f64,
        _position: &PositionRange,
    ) -> Value {
        match op {
            BinaryOp::Minus => Value::Number(left - right),
            BinaryOp::Plus => Value::Number(left + right),
            BinaryOp::Star => Value::Number(left * right),
            BinaryOp::Slash => Value::Number(left / right),
            BinaryOp::Greater => Value::Boolean(left > right),
            BinaryOp::GreaterEqual => Value::Boolean(left >= right),
            BinaryOp::Less => Value::Boolean(left < right),
            BinaryOp::LessEqual => Value::Boolean(left <= right),
            BinaryOp::EqualEqual => Value::Boolean(left == right),
            BinaryOp::BangEqual => Value::Boolean(left != right),
            BinaryOp::And => unreachable!(),
            BinaryOp::Or => unreachable!(),
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
        match &expr.expr_type {
            ExprType::StringLiteral(s) => Ok(Value::String(s.clone())),
            ExprType::NumberLiteral(n) => Ok(Value::Number(*n)),
            ExprType::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
            ExprType::Nil => Ok(Value::Nil),
            ExprType::Unary(op, expr) => self.unary_op(op, expr, &expr.position),
            ExprType::Binary(left, op, right) => self.binary_op(left, op, right, &expr.position),
            ExprType::Grouping(inner_expr) => self.visit_expr(inner_expr),
            ExprType::Variable { name } => match self.environment.get(name) {
                None => Err(LoxError {
                    kind: LoxErrorKind::Runtime,
                    message: format!("Name \"{}\" is not defined", name),
                    position: expr.position.clone(),
                }),
                // TODO: can we avoid cloning here?
                Some(value) => Ok((*value).clone()),
            },
            ExprType::Assignment { name, value: value_expression } => {
                let new_value = self.visit_expr(&value_expression)?;
                match self.environment.lookup_and_set(name.clone(), new_value) {
                    Ok(_) => Ok(Value::Nil),
                    Err(_) => Err(LoxError {
                        kind: LoxErrorKind::Runtime,
                        message: format!("Undefined variable '{}'", name),
                        position: expr.position.clone(),
                    })
                }
            }
        }
    }
}

impl StatementVisitor for Interpreter<'_> {
    type Result = Result<(), LoxError>;

    fn visit_statement(&mut self, statement: &ast::Statement) -> Self::Result {
        match statement {
            ast::Statement::Expression { expr, position: _ } => {
                self.visit_expr(expr)?;
            }
            ast::Statement::Print { expr, position: _ } => {
                let value = self.visit_expr(expr)?;
                writeln!(self.stdout, "{}", value);
            }
            ast::Statement::Var {
                name,
                initializer,
                position: _,
            } => {
                let value = match initializer {
                    None => Value::Nil,
                    Some(expr) => self.interpret_expression(expr)?,
                };
                self.environment.set_in_current_scope(name.clone(), value);
            }
            ast::Statement::Block { statements, position: _ } => {
                self.environment.new_scope();
                for statement in statements {
                    self.execute_statement(statement)?;
                }
                self.environment.destroy_scope();
            },
            ast::Statement::IfElse { condition, then_branch, else_branch, position: _ } => {
                if Self::is_truthy(&self.visit_expr(condition)?) {
                    self.execute_statement(&then_branch)?;
                } else if else_branch.is_some() {
                    self.execute_statement(else_branch.as_ref().unwrap())?;
                }
            },
            ast::Statement::While { condition, body, position: _ } => {
                while Self::is_truthy(&self.visit_expr(condition)?) {
                    self.visit_statement(body)?;
                }
            },
            ast::Statement::For { initializer, condition, increment, body, position: _ } => {
                if let Some(initializer) = initializer {
                    self.visit_statement(&initializer);
                }
                while condition.is_none() || Self::is_truthy(&self.visit_expr(&condition.as_ref().unwrap())?) {
                    self.visit_statement(&body)?;
                    if let Some(increment) = increment {
                        self.visit_statement(&increment)?;
                    }
                }
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
    let statements = ast::parse(&tokens)?;
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
    let mut stdout = std::io::stdout();
    let mut interpreter = Interpreter::new(&mut stdout);
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        let r = handle.read_line(&mut buffer)?;
        if r == 0 {
            return Ok(());
        }
        let result = run_with_interpreter(&buffer, &mut interpreter);
        match result {
            Err(errors) => {
                for error in errors {
                    println!("Error at ({}, {}): {}", error.position.start.line, error.position.start.column, error.message);
                }
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod run_tests {
    use std::str;

    use crate::position::Position;

    use super::*;

    use pretty_assertions::assert_eq;


    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let mut stdout = Vec::new();
                let mut interpreter = Interpreter::new(&mut stdout);
                let (input, expected, expected_stdout) = $value;
                assert_eq!(expected, run_with_interpreter(input, &mut interpreter));
                let stdout_string = str::from_utf8(&stdout).unwrap();
                assert_eq!(expected_stdout, stdout_string);
            }
        )*
        }
    }

    // TODO: use .visit_expr in these tests to test values
    parametrized_tests!(
        empty_string: (
            "",
            Ok(()),
            "",
        ),
        int_sum: (
            "1+2;",
            Ok(()),
            "",
        ),
        arithmetic_operator_priority: (
            "2 + 2 * 2;",
            Ok(()),
            "",
        ),
        grouping_priority: (
            "2 * (2 + 2);",
            Ok(()),
            "",
        ),
        number_plus_string_produces_error: (
            "\"abc\" + 123;",
            Err(vec![
                LoxError {
                    kind: LoxErrorKind::Runtime,
                    position: PositionRange { start: Position {
                        line: 1,
                        column: 1,
                    },
                    end: Position {
                        line: 1,
                        column: 11,
                    }
                },
                    message: "Unsupported operand types for binary operator + (String(abc), Number(123))".to_owned(),
                },
            ]),
            "",
        ),
        string_sum: (
            "print \"abc\" + \"def\";",
            Ok(()),
            "abcdef\n",
        ),
        var_declaration: (
            "var a;
            print a;",
            Ok(()),
            "Nil\n",
        ),
        var_declaration_with_initializer: (
            "var a = 1;
            print a;",
            Ok(()),
            "1\n",
        ),
        var_declaration_with_initializer_expression: (
            "var a = 2 + 2 * 2;
            print a;",
            Ok(()),
            "6\n",
        ),
        assignment: (
            "var a = 1;
            a = 2;
            print a;",
            Ok(()),
            "2\n",
        ),
        nested_scopes: (
            "var a = \"global a\";
             var b = \"global b\";
             var c = \"global c\";
             {
                var a = \"outer a\";
                var b = \"outer b\";
                {
                    var a = \"inner a\";
                    print a;
                    print b;
                    print c;
                }
                print a;
                print b;
                print c;
             }
             print a;
             print b;
             print c;",
             Ok(()),
             "inner a
outer b
global c
outer a
outer b
global c
global a
global b
global c
",
        ),
        var_shadow: (
            "var a = 1;
             {
                var a = a + 2;
                print a;
             }
             print a;
             ",
             Ok(()),
             "3
1
"
        ),
        if_statement: (
            "var a = 1;
            if (a > 0) {
                print \"true\";
            }",
            Ok(()),
            "true\n",
        ),
        if_else_statement: (
            "var a = 1;
            if (a < 0) {
                print \"true\";
            } else {
                print \"false\";
            }",
            Ok(()),
            "false\n",
        ),
        or_statement: (
            "var a = true;
             var b = false;
             print a or b;",
            Ok(()),
            "true\n",
        ),
        or_statement_false: (
            "var a = false;
             var b = false;
             print a or b;",
            Ok(()),
            "false\n",
        ),
        and_statement: (
            "var a = true;
             var b = true;
             print a and b;",
            Ok(()),
            "true\n",
        ),
        and_statement_false: (
            "var a = true;
             var b = false;
             print a and b;",
            Ok(()),
            "false\n",
        ),
        print_hi_or_2: (
            "print \"hi\" or 2;",
            Ok(()),
            "hi\n",
        ),
        print_nil_or_yes: (
            "print nil or \"yes\";",
            Ok(()),
            "yes\n",
        ),
        while_statement: (
            "
            var i = 0;
            while (i < 3) {
                print \"hello\";
                i = i + 1;
            }
            ",
            Ok(()),
            "hello
hello
hello\n",
        ),
        while_statement_not_executed_if_condition_is_false: (
            "
            var i = 10;
            while (i < 3) {
                print \"hello\";
                i = i + 1;
            }
            ",
            Ok(()),
            "",
        ),
        for_loop: (
            "
            var a = 0;
            var temp;
            for (var b = 1; a < 3; b = temp + b) {
             print a;
             temp = a;
             a = b;
            }
            ",
            Ok(()),
            "0
1
1
2\n",
        ),
        for_loop_no_initializer: (
            "
            var a = 0;
            for (; a < 3; a = a + 1) {
             print a;
            }
            ",
            Ok(()),
            "0
1
2\n",
        ),
        for_loop_no_increment: (
            "
            for (var a = 0; a < 3;) {
             print a;
             a = a + 1;
            }
            ",
            Ok(()),
            "0
1
2\n",
        ),
    );
}
