use crate::ast::expr::Expr;
use crate::error::{LoxError, LoxErrorKind};
use crate::position::{ Position, PositionRange };
use crate::token::{Token, TokenType};

use super::Statement;
use super::expr::{ExprType, UnaryOp, BinaryOp};
use super::token_iterator::TokenIterator;

struct Parser<'a> {
    token_iterator: TokenIterator<'a>,
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
        let var_keyword = self.token_iterator.next().unwrap();
        let peek = self.token_iterator.peek_clone();
        let name = match peek {
            Some(token) => {
                let token_type = token.type_.clone();
                match token_type {
                    TokenType::Identifier(name) => {
                        self.token_iterator.next();
                        name
                    }
                    _ => {
                        return Err(LoxError {
                            kind: LoxErrorKind::Syntax,
                            message: "Expected variable name".to_owned(),
                            position: PositionRange::from_bounds(&var_keyword.position, &token.position),
                        });
                    }
                }
            },
            _ => {
                return Err(LoxError {
                    kind: LoxErrorKind::Syntax,
                    message: "Expected variable name".to_owned(),
                    position: var_keyword.position.clone(),
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
        self.consume_semicolon(&var_keyword.position, "variable declaration")?;
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
        let expression_start = self.token_iterator.next().unwrap();
        let expr = self.expression()?;
        self.consume_semicolon(&expression_start.position, "value")?;
        Ok(Statement::Print { expr })
    }

    fn expression_statement(&mut self) -> Result<Statement, LoxError> {
        let expr = self.expression()?;
        self.consume_semicolon(&expr.position, "expression")?;
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
            left = Expr {
                expr_type: ExprType::Binary(
                    left.boxed(),
                    BinaryOp::from(operator),
                    right.boxed()
                ),
                position: PositionRange::from_bounds(&left.position, &right.position),
            };
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
            left = Expr {
                expr_type: ExprType::Binary(
                    left.boxed(),
                    BinaryOp::from(operator),
                    right.boxed()
                ),
                position: PositionRange::from_bounds(&left.position, &right.position),
            };
        }
        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut left = self.factor()?;
        while let Some(operator) = self.consume_if_matches(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.factor()?;
            left = Expr {
                expr_type: ExprType::Binary(
                    left.boxed(),
                    BinaryOp::from(operator),
                    right.boxed()
                ),
                position: PositionRange::from_bounds(&left.position, &right.position),
            };
        }
        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut left = self.unary()?;
        while let Some(operator) = self.consume_if_matches(&[TokenType::Star, TokenType::Slash]) {
            let right = self.unary()?;
            left = Expr {
                expr_type: ExprType::Binary(
                    left.boxed(),
                    BinaryOp::from(operator),
                    right.boxed()
                ),
                position: PositionRange::from_bounds(&left.position, &right.position),
            };
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, LoxError> {
        if let Some(operator) = self.consume_if_matches(&[TokenType::Bang, TokenType::Minus]) {
            let right = self.unary()?;
            let operator_position = operator.position.clone();
            return Ok(Expr {
                expr_type: ExprType::Unary(UnaryOp::from(operator), right.boxed()),
                position: PositionRange::from_bounds(&operator_position, &right.position),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, LoxError> {
        let result = match self.token_iterator.peek_clone() {
            None => Err(LoxError {
                kind: LoxErrorKind::Syntax,
                position: self.token_iterator
                    .last_position()
                    .unwrap_or_else(|| PositionRange {
                        start: Position { line: 0, column: 0 },
                        end: Position { line: 0, column: 0 },
                    }),
                message: "Expected expression.".to_owned(),
            }),
            Some(t) => match &t.type_ {
                TokenType::Nil => Ok(Expr::nil(t.position.clone())),
                TokenType::False => Ok(Expr::boolean_literal(false, t.position.clone())),
                TokenType::True => Ok(Expr::boolean_literal(true, t.position.clone())),
                TokenType::Number(n) => Ok(Expr::number_literal(*n, t.position.clone())),
                TokenType::String(s) => Ok(Expr::string_literal(s, t.position.clone())),
                TokenType::Identifier(name) => {
                    let position = t.position.clone();
                    self.token_iterator.next();
                    Ok(Expr {
                        expr_type: ExprType::Variable { name: name.clone(), },
                        position,
                    })
                },
                TokenType::LeftParen => {
                    let left_paren = self.token_iterator.next().unwrap();
                    let expr = self.expression()?;
                    match self.token_iterator.peek() {
                        Some(tt) if tt.type_ == TokenType::RightParen => {
                            let right_paren = self.token_iterator.next().unwrap();
                            Ok(Expr {
                                expr_type: ExprType::Grouping(expr.boxed()),
                                position: PositionRange::from_bounds(&left_paren.position, &right_paren.position)
                            })
                        }
                        _ => Err(LoxError {
                            kind: LoxErrorKind::Syntax,
                            position: PositionRange::from_bounds(&left_paren.position, &self.token_iterator.last_position().unwrap()),
                            message: "Expected ')' after expression".to_owned(),
                        }),
                    }
                }
                _ => Err(LoxError {
                    kind: LoxErrorKind::Syntax,
                    position: self.token_iterator
                        .last_position()
                        .unwrap_or_else(|| PositionRange {
                            start: Position { line: 0, column: 0 },
                            end: Position { line: 0, column: 0 },
                        },
                    ),
                    message: format!("Expected expression, got {}.", t.type_),
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

    fn consume_semicolon(&mut self, expression_start: &PositionRange, error_message_suffix: &str) -> Result<(), LoxError> {
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
                position: PositionRange::from_bounds(expression_start, &self.token_iterator.last_position().unwrap()),
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

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Statement>, Vec<LoxError>> {
    let mut parser = Parser {
        token_iterator: TokenIterator::new(tokens),
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

    // parametrized_tests!(
    //     empty_string: (vec![], Ok(vec![]),),
    //     number_literal:
    //         (
    //             vec![
    //                 Token::new_number(123.1, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Expression {
    //                     expr: Expr::number_literal(123.1, 1),
    //                 }],
    //             )
    //         ),
    //     number_sum:
    //         (
    //             vec![
    //                 Token::new_number(123.1, 1),
    //                 Token::new(TokenType::Plus, 1),
    //                 Token::new_number(456.2, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Expression {
    //                     expr: Expr::Binary(
    //                         Expr::number_literal(123.1, 1).boxed(),
    //                         Token::new(TokenType::Plus, 1),
    //                         Expr::number_literal(456.2, 1).boxed(),
    //                     ),
    //                 }],
    //             ),
    //         ),
    //     binary_priorities:
    //         (
    //             vec![
    //                 Token::new_number(1.0, 1),
    //                 Token::new(TokenType::Plus, 1),
    //                 Token::new_number(2.0, 1),
    //                 Token::new(TokenType::Star, 1),
    //                 Token::new_number(3.0, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Expression {
    //                     expr: Expr::Binary(
    //                         Expr::number_literal(1.0, 1).boxed(),
    //                         Token::new(TokenType::Plus, 1),
    //                         Expr::Binary(
    //                             Expr::number_literal(2.0, 1).boxed(),
    //                             Token::new(TokenType::Star, 1),
    //                             Expr::number_literal(3.0, 1).boxed(),
    //                         )
    //                         .boxed(),
    //                     ),
    //                 }],
    //             ),
    //         ),
    //     error_sum_without_second_operand:
    //         (
    //             vec![
    //                 Token::new_number(123.1, 1),
    //                 Token::new(TokenType::Plus, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Err(
    //                 vec![LoxError {
    //                     kind: LoxErrorKind::Syntax,
    //                     line: 1,
    //                     message: "Expected expression.".to_owned()
    //                 },],
    //             ),
    //         ),
    //     grouping_with_binary_inside:
    //         (
    //             vec![
    //                 Token::new(TokenType::LeftParen, 1),
    //                 Token::new_number(123.1, 1),
    //                 Token::new(TokenType::Plus, 1),
    //                 Token::new_number(456.2, 1),
    //                 Token::new(TokenType::RightParen, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Expression {
    //                     expr: Expr::Grouping(
    //                         Expr::Binary(
    //                             Expr::number_literal(123.1, 1).boxed(),
    //                             Token::new(TokenType::Plus, 1),
    //                             Expr::number_literal(456.2, 1).boxed(),
    //                         )
    //                         .boxed(),
    //                     ),
    //                 },],
    //             ),
    //         ),
    //     grouping_with_unary_minus:
    //         (
    //             vec![
    //                 Token::new(TokenType::LeftParen, 1),
    //                 Token::new(TokenType::Minus, 1),
    //                 Token::new_number(456.2, 1),
    //                 Token::new(TokenType::RightParen, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Expression {
    //                     expr: Expr::Grouping(
    //                         Expr::Unary(
    //                             Token::new(TokenType::Minus, 1),
    //                             Expr::number_literal(456.2, 1).boxed(),
    //                         )
    //                         .boxed(),
    //                     ),
    //                 },],
    //             ),
    //         ),
    //     error_grouping_no_right_paren:
    //         (
    //             vec![
    //                 Token::new(TokenType::LeftParen, 1),
    //                 Token::new_number(123.1, 1),
    //                 Token::new(TokenType::Plus, 1),
    //                 Token::new_number(456.2, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Err(
    //                 vec![LoxError {
    //                     kind: LoxErrorKind::Syntax,
    //                     message: "Expected ')' after expression".to_owned(),
    //                     line: 1,
    //                 },],
    //             ),
    //         ),
    //     error_unary_no_operand:
    //         (
    //             vec![
    //                 Token::new(TokenType::Minus, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Err(
    //                 vec![LoxError {
    //                     kind: LoxErrorKind::Syntax,
    //                     message: "Expected expression.".to_owned(),
    //                     line: 1,
    //                 },],
    //             ),
    //         ),
    //     var_declaration:
    //         (
    //             vec![
    //                 Token::new(TokenType::Var, 1),
    //                 Token::new(TokenType::Identifier("test".to_owned()), 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Var {
    //                     name: "test".to_owned(),
    //                     initializer: None,
    //                 },],
    //             ),
    //         ),
    //     var_declaration_with_initializer:
    //         (
    //             vec![
    //                 Token::new(TokenType::Var, 1),
    //                 Token::new(TokenType::Identifier("test".to_owned()), 1),
    //                 Token::new(TokenType::Equal, 1),
    //                 Token::new_number(123.1, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Var {
    //                     name: "test".to_owned(),
    //                     initializer: Some(Expr::number_literal(123.1, 1)),
    //                 },],
    //             ),
    //         ),
    //     var_declaration_with_initializer_expression:
    //         (
    //             vec![
    //                 Token::new(TokenType::Var, 1),
    //                 Token::new(TokenType::Identifier("test".to_owned()), 1),
    //                 Token::new(TokenType::Equal, 1),
    //                 Token::new_number(1.0, 1),
    //                 Token::new(TokenType::Plus, 1),
    //                 Token::new_number(2.0, 1),
    //                 Token::new(TokenType::Star, 1),
    //                 Token::new_number(3.0, 1),
    //                 Token::new(TokenType::Semicolon, 1),
    //             ],
    //             Ok(
    //                 vec![Statement::Var {
    //                     name: "test".to_owned(),
    //                     initializer: Some(Expr::Binary(
    //                         Expr::number_literal(1.0, 1).boxed(),
    //                         Token::new(TokenType::Plus, 1),
    //                         Expr::Binary(
    //                             Expr::number_literal(2.0, 1).boxed(),
    //                             Token::new(TokenType::Star, 1),
    //                             Expr::number_literal(3.0, 1).boxed(),
    //                         )
    //                         .boxed(),
    //                     )),
    //                 },],
    //             ),
    //         ),
    //     print_statement_with_string_sum: (
    //         vec![
    //             Token::new(TokenType::Print, 1),
    //             Token::new_string("abc", 1),
    //             Token::new(TokenType::Plus, 1),
    //             Token::new_string("def", 1),
    //             Token::new(TokenType::Semicolon, 1),
    //         ],
    //         Ok(
    //             vec![Statement::Print {
    //                 expr: Expr::Binary(
    //                     Expr::string_literal("abc", 1).boxed(),
    //                     Token::new(TokenType::Plus, 1),
    //                     Expr::string_literal("def", 1).boxed(),
    //                 ),
    //             }],
    //         )
    //     ),
    // );
}
