use std::iter::Peekable;

use crate::token::{Token, TokenType};

#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(TokenType),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
}

mod visit {
    use super::*;

    pub trait Visitor {
        type Result;

        fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
    }
}

struct AstPrinter {}

impl visit::Visitor for AstPrinter {
    type Result = String;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match expr {
            Expr::Literal(token) => token.to_string(),
            Expr::Unary(op, e) => {
                format!("({} {})", op.type_.to_string(), self.visit_expr(&e))
            }
            Expr::Binary(left, op, right) => format!(
                "({} {} {})",
                op.type_.to_string(),
                self.visit_expr(&left),
                self.visit_expr(&right)
            ),
            Expr::Grouping(e) => format!("(group {})", self.visit_expr(&e)),
        }
    }
}

pub struct SyntaxError {
    line: u32,
    message: String,
}

struct Parser<'a> {
    // token_iterator: Peekable<Box<dyn Iterator<Item = Token>>>,
    token_iterator: Peekable<std::slice::Iter<'a, Token>>,
}

impl Parser<'_> {
    fn expression(&mut self) -> Result<Expr, SyntaxError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, SyntaxError> {
        let mut left = self.comparison()?;
        while let Some(operator) =
            self.consume_if_matches(&[TokenType::BangEqual, TokenType::EqualEqual])
        {
            let right = self.comparison()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, SyntaxError> {
        let mut left = self.term()?;
        while let Some(operator) = self.consume_if_matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = self.term()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, SyntaxError> {
        let mut left = self.factor()?;
        while let Some(operator) = self.consume_if_matches(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.factor()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, SyntaxError> {
        let mut left = self.unary()?;
        while let Some(operator) = self.consume_if_matches(&[TokenType::Star, TokenType::Slash]) {
            let right = self.unary()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, SyntaxError> {
        if let Some(operator) = self.consume_if_matches(&[TokenType::Bang, TokenType::Minus]) {
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        return Ok(self.primary()?);
    }

    fn primary(&mut self) -> Result<Expr, SyntaxError> {
        let peek = self.token_iterator.peek();
        match peek {
            None => unreachable!(),
            Some(t) => match t.type_ {
                TokenType::False
                | TokenType::True
                | TokenType::Nil
                | TokenType::Number(_)
                | TokenType::String(_) => Ok(Expr::Literal(t.type_.clone())),
                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    // TODO: check for RightParen
                    Ok(Expr::Grouping(Box::new(expr)))
                }
                _ => Err(SyntaxError {
                    line: t.line,
                    message: "Expected expression.".to_owned(),
                }),
            },
        }
    }

    fn consume_if_matches(&mut self, token_types: &[TokenType]) -> Option<Token> {
        match self.token_iterator.peek() {
            None => None,
            Some(t)
                if (*token_types)
                    .iter()
                    .any(|expected_type| *expected_type == t.type_) =>
            {
                Some((*self.token_iterator.next().unwrap()).clone())
            }
            _ => None,
        }
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.token_iterator.next() {
            match token.type_ {
                TokenType::Semicolon => return,
                _ => {}
            }
            match self.token_iterator.peek() {
                None => return,
                Some(next_token) => match next_token.type_ {
                    TokenType::Class
                    | TokenType::For
                    | TokenType::Fun
                    | TokenType::If
                    | TokenType::Print
                    | TokenType::Return
                    | TokenType::Var
                    | TokenType::While => return,
                    _ => {}
                },
            }
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Expr, SyntaxError> {
    let mut parser = Parser {
        token_iterator: tokens.iter().peekable(),
    };
    parser.expression()
}

#[cfg(test)]

mod ast_printer_tests {
    use super::*;
    use visit::*;

    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let mut printer = AstPrinter{};
                let printed_ast = printer.visit_expr(&input);
                assert_eq!(printed_ast, expected);
            }
        )*
        }
    }

    parametrized_tests!(
        example1: (
            Expr::Binary(
                Box::new(
                    Expr::Unary(
                        Token { type_: TokenType::Minus, line: 1 },
                        Box::new(Expr::Literal(TokenType::Number(123.0)))
                    )
                ),
                Token { type_: TokenType::Star, line: 1},
                Box::new(
                    Expr::Grouping(
                        Box::new(Expr::Literal(TokenType::Number(45.67))),
                    ),
                ),
            ),
            "(* (- 123) (group 45.67))"
        ),
    );
}
