use std::any::Any;
use std::iter::Peekable;

use crate::ast::expr::Expr;
use crate::error::{LoxError, LoxErrorKind};
use crate::token::{Token, TokenType};

use super::Statement;

struct Parser<'a> {
    token_iterator: Peekable<std::slice::Iter<'a, Token>>,
}

impl Parser<'_> {
    fn parse(&mut self) -> Result<Vec<Statement>, Vec<LoxError>> {
        let mut statements: Vec<Statement> = Vec::new();
        let mut errors: Vec<LoxError> = Vec::new();
        while let Some(token) = self.token_iterator.next() {

        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement, LoxError> {
        let peek = self.token_iterator.peek();
        match peek {
            Some(t) if t.type_ == TokenType::Var => {
                self.var_declaration()
            }
            _ => self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, LoxError> {
        // consume var keyword
        let last_token = self.token_iterator.next().unwrap();
        let name = match self.token_iterator.peek() {
            Some(token) => {
                match &token.type_ {
                    TokenType::Identifier(name) => name,
                    _ => return Err(LoxError {
                        kind: LoxErrorKind::Syntax,
                        message: "Expected variable name".to_owned(),
                        line: token.line,
                    })
                }
            }
            _ => return Err(LoxError {
                kind: LoxErrorKind::Syntax,
                message: "Expected variable name".to_owned(),
                // TODO: handle position better
                line: last_token.line,
            })
        };
        let initializer = match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Equal => {
                self.token_iterator.next().unwrap();
                Some(self.expression()?)
            },
            _ => None,
        };
        self.consume_semicolon("variable declaration")?;
        Ok(Statement::Var { name: name.clone(), initializer })
    }

    fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Print => self.print_statement(),
            _ => self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Statement, LoxError> {
        // consume 'print' keyword
        let last_token = self.token_iterator.next().unwrap();
        let expr = self.expression()?;
        self.consume_semicolon("after value")?;
        Ok(Statement::Print { expr, })
    }

    fn expression_statement(&mut self) -> Result<Statement, LoxError> {
        let expr = self.expression()?;
        self.consume_semicolon("after expression")?;
        Ok(Statement::Expression { expr, })
    }

    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut left = self.comparison()?;
        while let Some(operator) =
            self.consume_if_matches(&[TokenType::BangEqual, TokenType::EqualEqual])
        {
            let right = self.comparison()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, LoxError> {
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

    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut left = self.factor()?;
        while let Some(operator) = self.consume_if_matches(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.factor()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut left = self.unary()?;
        while let Some(operator) = self.consume_if_matches(&[TokenType::Star, TokenType::Slash]) {
            let right = self.unary()?;
            left = Expr::Binary(Box::new(left), operator, Box::new(right));
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, LoxError> {
        if let Some(operator) = self.consume_if_matches(&[TokenType::Bang, TokenType::Minus]) {
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, LoxError> {
        match self.token_iterator.peek() {
            None => unreachable!(),
            Some(t) => match &t.type_ {
                TokenType::False
                | TokenType::True
                | TokenType::Nil
                | TokenType::Number(_)
                | TokenType::String(_) => Ok(Expr::Literal(
                    (*self.token_iterator.next().unwrap()).clone(),
                )),
                TokenType::Identifier(name) => Ok(Expr::Variable { name: name.clone() }),
                TokenType::LeftParen => {
                    let line = t.line;
                    self.token_iterator.next().unwrap();
                    let expr = self.expression()?;
                    match self.token_iterator.peek() {
                        Some(tt) if tt.type_ == TokenType::RightParen => {
                            self.token_iterator.next();
                            Ok(Expr::Grouping(Box::new(expr)))
                        }
                        _ => Err(LoxError {
                            kind: LoxErrorKind::Syntax,
                            line,
                            message: "Expected ')' after expression".to_owned(),
                        }),
                    }
                }
                _ => Err(LoxError {
                    kind: LoxErrorKind::Syntax,
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

    fn consume_semicolon(&mut self, error_message_suffix: &str) -> Result<(), LoxError> {
        match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Semicolon => {
                self.token_iterator.next();
                Ok(())
            }
            _ => Err(LoxError {
                kind: LoxErrorKind::Syntax,
                // TODO: fix line position
                line: 0,
                message: format!("Expected ';' after {}", error_message_suffix)
            })
        }
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.token_iterator.next() {
            if token.type_ == TokenType::Semicolon {
                return;
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

pub fn parse(tokens: Vec<Token>) -> Result<Expr, LoxError> {
    let mut parser = Parser {
        token_iterator: tokens.iter().peekable(),
    };
    parser.expression()
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    fn eof(line: u32) -> Token {
        Token {
            type_: TokenType::Eof,
            line,
        }
    }

    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let expr = parse(input);
                assert_eq!(expr, expected);
            }
        )*
        }
    }

    parametrized_tests!(
        empty_string: (
            vec![eof(1)],
            Err(LoxError {
                kind: LoxErrorKind::Syntax,
                line: 1,
                message: "Expected expression.".to_owned()
            })
        ),
        number_literal: (
            vec![
                Token::new_number(123.1, 1),
                eof(2),
            ],
            Ok(Expr::Literal(
                Token::new_number(123.1, 1),
            ))
        ),
        number_sum: (
            vec![
                Token::new_number(123.1, 1),
                Token::new(TokenType::Plus, 1),
                Token::new_number(456.2, 1),
                eof(2),
            ],
            Ok(Expr::Binary(
                Box::new(Expr::Literal(Token::new_number(123.1, 1))),
                Token::new(TokenType::Plus, 1),
                Box::new(Expr::Literal(Token::new_number(456.2, 1))),
            )),
        ),
        binary_priorities: (
            vec![
                Token::new_number(1.0, 1),
                Token::new(TokenType::Plus, 1),
                Token::new_number(2.0, 1),
                Token::new(TokenType::Star, 1),
                Token::new_number(3.0, 1),
                eof(2),
            ],
            Ok(Expr::Binary(
                Box::new(Expr::Literal(Token::new_number(1.0, 1))),
                Token::new(TokenType::Plus, 1),
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Token::new_number(2.0, 1))),
                    Token::new(TokenType::Star, 1),
                    Box::new(Expr::Literal(Token::new_number(3.0, 1))),
                )),
            )),

        ),
        error_sum_without_second_operand: (
            vec![
                Token::new_number(123.1, 1),
                Token::new(TokenType::Plus, 1),
                eof(2),
            ],
            Err(LoxError {
                kind: LoxErrorKind::Syntax,
                line: 2,
                message: "Expected expression.".to_owned()
            })
        ),
        grouping_with_binary_inside: (
            vec![
                Token::new(TokenType::LeftParen, 1),
                Token::new_number(123.1, 1),
                Token::new(TokenType::Plus, 1),
                Token::new_number(456.2, 1),
                Token::new(TokenType::RightParen, 1),
                eof(2),
            ],
            Ok(Expr::Grouping(
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Token::new_number(123.1, 1))),
                    Token::new(TokenType::Plus, 1),
                    Box::new(Expr::Literal(Token::new_number(456.2, 1))),
                )),
            )),
        ),
        grouping_with_unary_minus: (
            vec![
                Token::new(TokenType::LeftParen, 1),
                Token::new(TokenType::Minus, 1),
                Token::new_number(456.2, 1),
                Token::new(TokenType::RightParen, 1),
                eof(2),
            ],
            Ok(Expr::Grouping(
                Box::new(Expr::Unary(
                    Token::new(TokenType::Minus, 1),
                    Box::new(Expr::Literal(Token::new_number(456.2, 1))),
                )),
            )),
        ),
        error_grouping_no_right_paren: (
            vec![
                Token::new(TokenType::LeftParen, 1),
                Token::new_number(123.1, 1),
                Token::new(TokenType::Plus, 1),
                Token::new_number(456.2, 1),
                eof(2),
            ],
            Err(
                LoxError {
                    kind: LoxErrorKind::Syntax,
                    message: "Expected ')' after expression".to_owned(),
                    line: 1,
                },
            ),
        ),
        error_unary_no_operand: (
            vec![
                Token::new(TokenType::Minus, 1),
                eof(2),
            ],
            Err(
                LoxError {
                    kind: LoxErrorKind::Syntax,
                    message: "Expected expression.".to_owned(),
                    line: 2,
                },
            )
        ),
    );
}
