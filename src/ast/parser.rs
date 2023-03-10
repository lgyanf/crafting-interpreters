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
        while let Some(_token) = self.token_iterator.peek() {
            let statement = self.declaration();
            match statement {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    errors.push(error);
                    self.synchronize();
                }
            };
        }
        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn declaration(&mut self) -> Result<Statement, LoxError> {
        let peek = self.token_iterator.peek();
        match peek {
            Some(t) if t.type_ == TokenType::Var => self.var_declaration(),
            _ => self.statement(),
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, LoxError> {
        // consume var keyword
        let last_token = self.token_iterator.next().unwrap();
        let name = match self.token_iterator.peek() {
            Some(token) => match &token.type_ {
                TokenType::Identifier(name) => {
                    self.token_iterator.next();
                    name
                }
                _ => {
                    return Err(LoxError {
                        kind: LoxErrorKind::Syntax,
                        message: "Expected variable name".to_owned(),
                        line: token.line,
                    })
                }
            },
            _ => {
                return Err(LoxError {
                    kind: LoxErrorKind::Syntax,
                    message: "Expected variable name".to_owned(),
                    // TODO: handle position better
                    line: last_token.line,
                });
            }
        };
        let initializer = match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Equal => {
                self.token_iterator.next().unwrap();
                Some(self.expression()?)
            }
            _ => None,
        };
        self.consume_semicolon("variable declaration")?;
        Ok(Statement::Var {
            name: name.clone(),
            initializer,
        })
    }

    fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Print => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Statement, LoxError> {
        // consume 'print' keyword
        self.token_iterator.next();
        let expr = self.expression()?;
        self.consume_semicolon("value")?;
        Ok(Statement::Print { expr })
    }

    fn expression_statement(&mut self) -> Result<Statement, LoxError> {
        let expr = self.expression()?;
        self.consume_semicolon("expression")?;
        Ok(Statement::Expression { expr })
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
        let result = match self.token_iterator.peek() {
            None => Err(LoxError {
                kind: LoxErrorKind::Syntax,
                // TODO: fix position
                line: 0,
                message: "Expected expression.".to_owned(),
            }),
            Some(t) => match &t.type_ {
                TokenType::False
                | TokenType::True
                | TokenType::Nil
                | TokenType::Number(_)
                | TokenType::String(_) => Ok(Expr::Literal(
                    (*self.token_iterator.next().unwrap()).clone(),
                )),
                TokenType::Identifier(name) => {
                    let line = t.line;
                    self.token_iterator.next();
                    Ok(Expr::Variable {
                        name: name.clone(),
                        line,
                    })
                },
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
        };
        result
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
        let next_token_type = self.token_iterator.peek()
            .map(|t| t.type_.to_string())
            .unwrap_or_else( || "<EOF>".to_owned());
        match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Semicolon => {
                self.token_iterator.next();
                Ok(())
            }
            _ => Err(LoxError {
                kind: LoxErrorKind::Syntax,
                // TODO: fix line position
                line: 0,
                message: format!("Expected ';' after {}, got {}",
                    error_message_suffix, next_token_type),
            }),
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

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, Vec<LoxError>> {
    let mut parser = Parser {
        token_iterator: tokens.iter().peekable(),
    };
    parser.parse()
}

#[cfg(test)]
mod parser_tests {
    use super::*;

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
        empty_string: (vec![], Ok(vec![]),),
        number_literal:
            (
                vec![
                    Token::new_number(123.1, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr::number_literal(123.1, 1),
                    }],
                )
            ),
        number_sum:
            (
                vec![
                    Token::new_number(123.1, 1),
                    Token::new(TokenType::Plus, 1),
                    Token::new_number(456.2, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr::Binary(
                            Expr::number_literal(123.1, 1).boxed(),
                            Token::new(TokenType::Plus, 1),
                            Expr::number_literal(456.2, 1).boxed(),
                        ),
                    }],
                ),
            ),
        binary_priorities:
            (
                vec![
                    Token::new_number(1.0, 1),
                    Token::new(TokenType::Plus, 1),
                    Token::new_number(2.0, 1),
                    Token::new(TokenType::Star, 1),
                    Token::new_number(3.0, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr::Binary(
                            Expr::number_literal(1.0, 1).boxed(),
                            Token::new(TokenType::Plus, 1),
                            Expr::Binary(
                                Expr::number_literal(2.0, 1).boxed(),
                                Token::new(TokenType::Star, 1),
                                Expr::number_literal(3.0, 1).boxed(),
                            )
                            .boxed(),
                        ),
                    }],
                ),
            ),
        error_sum_without_second_operand:
            (
                vec![
                    Token::new_number(123.1, 1),
                    Token::new(TokenType::Plus, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Err(
                    vec![LoxError {
                        kind: LoxErrorKind::Syntax,
                        line: 1,
                        message: "Expected expression.".to_owned()
                    },],
                ),
            ),
        grouping_with_binary_inside:
            (
                vec![
                    Token::new(TokenType::LeftParen, 1),
                    Token::new_number(123.1, 1),
                    Token::new(TokenType::Plus, 1),
                    Token::new_number(456.2, 1),
                    Token::new(TokenType::RightParen, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr::Grouping(
                            Expr::Binary(
                                Expr::number_literal(123.1, 1).boxed(),
                                Token::new(TokenType::Plus, 1),
                                Expr::number_literal(456.2, 1).boxed(),
                            )
                            .boxed(),
                        ),
                    },],
                ),
            ),
        grouping_with_unary_minus:
            (
                vec![
                    Token::new(TokenType::LeftParen, 1),
                    Token::new(TokenType::Minus, 1),
                    Token::new_number(456.2, 1),
                    Token::new(TokenType::RightParen, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr::Grouping(
                            Expr::Unary(
                                Token::new(TokenType::Minus, 1),
                                Expr::number_literal(456.2, 1).boxed(),
                            )
                            .boxed(),
                        ),
                    },],
                ),
            ),
        error_grouping_no_right_paren:
            (
                vec![
                    Token::new(TokenType::LeftParen, 1),
                    Token::new_number(123.1, 1),
                    Token::new(TokenType::Plus, 1),
                    Token::new_number(456.2, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Err(
                    vec![LoxError {
                        kind: LoxErrorKind::Syntax,
                        message: "Expected ')' after expression".to_owned(),
                        line: 1,
                    },],
                ),
            ),
        error_unary_no_operand:
            (
                vec![
                    Token::new(TokenType::Minus, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Err(
                    vec![LoxError {
                        kind: LoxErrorKind::Syntax,
                        message: "Expected expression.".to_owned(),
                        line: 1,
                    },],
                ),
            ),
        var_declaration:
            (
                vec![
                    Token::new(TokenType::Var, 1),
                    Token::new(TokenType::Identifier("test".to_owned()), 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Var {
                        name: "test".to_owned(),
                        initializer: None,
                    },],
                ),
            ),
        var_declaration_with_initializer:
            (
                vec![
                    Token::new(TokenType::Var, 1),
                    Token::new(TokenType::Identifier("test".to_owned()), 1),
                    Token::new(TokenType::Equal, 1),
                    Token::new_number(123.1, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Var {
                        name: "test".to_owned(),
                        initializer: Some(Expr::number_literal(123.1, 1)),
                    },],
                ),
            ),
        var_declaration_with_initializer_expression:
            (
                vec![
                    Token::new(TokenType::Var, 1),
                    Token::new(TokenType::Identifier("test".to_owned()), 1),
                    Token::new(TokenType::Equal, 1),
                    Token::new_number(1.0, 1),
                    Token::new(TokenType::Plus, 1),
                    Token::new_number(2.0, 1),
                    Token::new(TokenType::Star, 1),
                    Token::new_number(3.0, 1),
                    Token::new(TokenType::Semicolon, 1),
                ],
                Ok(
                    vec![Statement::Var {
                        name: "test".to_owned(),
                        initializer: Some(Expr::Binary(
                            Expr::number_literal(1.0, 1).boxed(),
                            Token::new(TokenType::Plus, 1),
                            Expr::Binary(
                                Expr::number_literal(2.0, 1).boxed(),
                                Token::new(TokenType::Star, 1),
                                Expr::number_literal(3.0, 1).boxed(),
                            )
                            .boxed(),
                        )),
                    },],
                ),
            ),
        print_statement_with_string_sum: (
            vec![
                Token::new(TokenType::Print, 1),
                Token::new_string("abc", 1),
                Token::new(TokenType::Plus, 1),
                Token::new_string("def", 1),
                Token::new(TokenType::Semicolon, 1),
            ],
            Ok(
                vec![Statement::Print {
                    expr: Expr::Binary(
                        Expr::string_literal("abc", 1).boxed(),
                        Token::new(TokenType::Plus, 1),
                        Expr::string_literal("def", 1).boxed(),
                    ),
                }],
            )
        ),
    );
}
