use crate::ast::expr::Expr;
use crate::error::{LoxError, LoxErrorKind};
use crate::position::{Position, PositionRange};
use crate::token::{Token, TokenType};

use super::expr::{BinaryOp, ExprType, UnaryOp};
use super::token_iterator::TokenIterator;
use super::Statement;

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
                            position: PositionRange::from_bounds(
                                &var_keyword.position,
                                &token.position,
                            ),
                        });
                    }
                }
            }
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
        let semicolon = self.consume_semicolon(&var_keyword.position, "variable declaration")?;
        Ok(Statement::Var {
            name: name.clone(),
            initializer,
            position: PositionRange {
                start: var_keyword.position.start.clone(),
                end: semicolon.position.end.clone(),
            },
        })
    }

    fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Print => self.print_statement(),
            Some(token) if token.type_ == TokenType::LeftBrace => self.block(),
            _ => self.expression_statement(),
        }
    }

    fn block(&mut self) -> Result<Statement, LoxError> {
        let left_brace = self.token_iterator.next().unwrap();
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(peek) = self.token_iterator.peek() {
            if peek.type_ == TokenType::RightBrace {
                break;
            }
            statements.push(self.declaration()?);
        }
        let right_brace = self.consume_if_matches(&[TokenType::RightBrace]);

        match right_brace {
            Some(rb) => Ok(Statement::Block {
                statements,
                position: PositionRange::from_bounds(&left_brace.position, &rb.position),
            }),
            _ => Err(LoxError {
                kind: LoxErrorKind::Syntax,
                position: PositionRange::from_bounds(&left_brace.position, &self.token_iterator.last_position().unwrap()),
                message: "Expected '}' after block.".to_owned(),
            }),
        }
    }

    fn print_statement(&mut self) -> Result<Statement, LoxError> {
        // consume 'print' keyword
        let print_keyword = self.token_iterator.next().unwrap();
        let expr = self.expression()?;
        let semicolon = self.consume_semicolon(&expr.position, "value")?;
        let end_position = semicolon.position.end.clone();
        Ok(Statement::Print {
            expr,
            position: PositionRange {
                start: print_keyword.position.start.clone(),
                end: end_position,
            },
        })
    }

    fn expression_statement(&mut self) -> Result<Statement, LoxError> {
        let expr = self.expression()?;
        let semicolon = self.consume_semicolon(&expr.position, "expression")?;
        let statement_start = expr.position.start.clone();
        Ok(Statement::Expression {
            expr,
            position: PositionRange {
                start: statement_start,
                end: semicolon.position.end.clone(),
            },
        })
    }

    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, LoxError> {
        let expr = self.equality()?;

        if let Some(_equals) = self.consume_if_matches(&[TokenType::Equal]) {
            let value = self.assignment()?;
            match expr.expr_type {
                ExprType::Variable { name } => {
                    Ok(Expr {
                        expr_type: ExprType::Assignment { name, value: value.boxed() },
                        position: PositionRange::from_bounds(&expr.position, &value.position)
                    })
                }
                _ =>  Err(LoxError {
                    kind: LoxErrorKind::Syntax,
                    position: PositionRange::from_bounds(&expr.position, &value.position),
                    message: format!("Invalid assignment target: {}", expr.expr_type),
                })
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut left = self.comparison()?;
        while let Some(operator) =
            self.consume_if_matches(&[TokenType::BangEqual, TokenType::EqualEqual])
        {
            let right = self.comparison()?;
            left = Expr {
                expr_type: ExprType::Binary(left.boxed(), BinaryOp::from(operator), right.boxed()),
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
                expr_type: ExprType::Binary(left.boxed(), BinaryOp::from(operator), right.boxed()),
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
                expr_type: ExprType::Binary(left.boxed(), BinaryOp::from(operator), right.boxed()),
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
                expr_type: ExprType::Binary(left.boxed(), BinaryOp::from(operator), right.boxed()),
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
                position: self
                    .token_iterator
                    .last_position()
                    .unwrap_or(PositionRange {
                        start: Position { line: 0, column: 0 },
                        end: Position { line: 0, column: 0 },
                    }),
                message: "Expected expression.".to_owned(),
            }),
            Some(t) => match &t.type_ {
                TokenType::Nil => {
                    self.token_iterator.next();
                    Ok(Expr::nil(t.position.clone()))
                }
                TokenType::False => {
                    self.token_iterator.next();
                    Ok(Expr::boolean_literal(false, t.position.clone()))
                }
                TokenType::True => {
                    self.token_iterator.next();
                    Ok(Expr::boolean_literal(true, t.position.clone()))
                }
                TokenType::Number(n) => {
                    self.token_iterator.next();
                    Ok(Expr::number_literal(*n, t.position.clone()))
                }
                TokenType::String(s) => {
                    self.token_iterator.next();
                    Ok(Expr::string_literal(s, t.position.clone()))
                }
                TokenType::Identifier(name) => {
                    let position = t.position.clone();
                    self.token_iterator.next();
                    Ok(Expr {
                        expr_type: ExprType::Variable { name: name.clone() },
                        position,
                    })
                }
                TokenType::LeftParen => {
                    let left_paren = self.token_iterator.next().unwrap();
                    let expr = self.expression()?;
                    match self.token_iterator.peek() {
                        Some(tt) if tt.type_ == TokenType::RightParen => {
                            let right_paren = self.token_iterator.next().unwrap();
                            Ok(Expr {
                                expr_type: ExprType::Grouping(expr.boxed()),
                                position: PositionRange::from_bounds(
                                    &left_paren.position,
                                    &right_paren.position,
                                ),
                            })
                        }
                        _ => Err(LoxError {
                            kind: LoxErrorKind::Syntax,
                            position: PositionRange::from_bounds(
                                &left_paren.position,
                                &self.token_iterator.last_position().unwrap(),
                            ),
                            message: "Expected ')' after expression".to_owned(),
                        }),
                    }
                }
                _ => Err(LoxError {
                    kind: LoxErrorKind::Syntax,
                    position: self.token_iterator.last_position().unwrap_or({
                        PositionRange {
                            start: Position { line: 0, column: 0 },
                            end: Position { line: 0, column: 0 },
                        }
                    }),
                    message: format!("Expected expression, got {}.", t.type_),
                }),
            },
        };
        result
    }

    fn consume_if_matches(&mut self, token_types: &[TokenType]) -> Option<Token> {
        let peek = self.token_iterator.peek();
        match peek {
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

    fn consume_semicolon(
        &mut self,
        expression_start: &PositionRange,
        error_message_suffix: &str,
    ) -> Result<&Token, LoxError> {
        let next_token_type = self
            .token_iterator
            .peek()
            .map(|t| t.type_.to_string())
            .unwrap_or_else(|| "<EOF>".to_owned());
        match self.token_iterator.peek() {
            Some(token) if token.type_ == TokenType::Semicolon => {
                let semicolon = self.token_iterator.next().unwrap();
                Ok(semicolon)
            }
            _ => Err(LoxError {
                kind: LoxErrorKind::Syntax,
                position: PositionRange::from_bounds(
                    expression_start,
                    &self.token_iterator.last_position().unwrap(),
                ),
                message: format!(
                    "Expected ';' after {}, got {}",
                    error_message_suffix, next_token_type
                ),
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

pub fn parse(tokens: &[Token]) -> Result<Vec<Statement>, Vec<LoxError>> {
    let mut parser = Parser {
        token_iterator: TokenIterator::new(tokens),
    };
    parser.parse()
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let expr = parse(&input);
                assert_eq!(expr, expected);
            }
        )*
        }
    }

    fn range(line: usize, start_column: usize, end_column: usize) -> PositionRange {
        PositionRange {
            start: Position {
                line,
                column: start_column,
            },
            end: Position {
                line,
                column: end_column,
            },
        }
    }

    parametrized_tests!(
        empty_string: (vec![], Ok(vec![]),),
        number_literal:
            (
                vec![
                    Token::new_number(123.1, range(1, 1, 5)),
                    Token::new(TokenType::Semicolon, range(1, 6, 6)),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr::number_literal(123.1, range(1, 1, 5)),
                        position: range(1, 1, 6),
                    }],
                )
            ),
        number_sum:
            (
                vec![
                    Token::new_number(123.1, range(1, 1, 5)),
                    Token::new(TokenType::Plus, range(1, 6, 6)),
                    Token::new_number(456.2, range(1, 7, 11)),
                    Token::new(TokenType::Semicolon, range(1, 12, 12)),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr {
                            expr_type: ExprType::Binary(
                                Expr::number_literal(123.1, range(1, 1, 5)).boxed(),
                                BinaryOp::Plus,
                                Expr::number_literal(456.2, range(1, 7, 11)).boxed(),
                            ),
                            position: range(1, 1, 11),
                        },
                        position: range(1, 1, 12),
                    }],
                ),
            ),
        binary_priorities:
            (
                vec![
                    Token::new_number(1.0, range(1, 1, 3)),
                    Token::new(TokenType::Plus, range(1, 4, 4)),
                    Token::new_number(2.0, range(1, 5, 7)),
                    Token::new(TokenType::Star, range(1, 8, 8)),
                    Token::new_number(3.0, range(1, 9, 11)),
                    Token::new(TokenType::Semicolon, range(1, 12, 12)),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr {
                            expr_type: ExprType::Binary(
                                Expr::number_literal(1.0, range(1, 1, 3)).boxed(),
                                BinaryOp::Plus,
                                Expr {
                                    expr_type: ExprType::Binary(
                                        Expr::number_literal(2.0, range(1, 5, 7)).boxed(),
                                        BinaryOp::Star,
                                        Expr::number_literal(3.0, range(1, 9, 11)).boxed(),
                                    ),
                                    position: range(1, 5, 11),
                                }.boxed(),
                            ),
                            position: range(1, 1, 11),
                        },
                        position: range(1, 1, 12),
                    }],
                ),
            ),
        error_sum_without_second_operand:
            (
                vec![
                    Token::new_number(123.1, range(1, 1, 5)),
                    Token::new(TokenType::Plus, range(1, 6, 6)),
                    Token::new(TokenType::Semicolon, range(1, 7, 7)),
                ],
                Err(
                    // TODO: report expression start in the error position
                    vec![LoxError {
                        kind: LoxErrorKind::Syntax,
                        position: range(1, 6, 6),
                        message: "Expected expression, got ;.".to_owned()
                    },],
                ),
            ),
        grouping_with_binary_inside:
            (
                vec![
                    Token::new(TokenType::LeftParen, range(1, 1, 1)),
                    Token::new_number(123.1, range(1, 2, 6)),
                    Token::new(TokenType::Plus, range(1, 7, 7)),
                    Token::new_number(456.2, range(1, 8, 12)),
                    Token::new(TokenType::RightParen, range(1, 13, 13)),
                    Token::new(TokenType::Semicolon, range(1, 14, 14)),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr {
                            expr_type: ExprType::Grouping(
                                Expr {
                                    expr_type: ExprType::Binary(
                                        Expr {
                                            expr_type: ExprType::NumberLiteral(123.1),
                                            position: range(1, 2, 6),
                                        }.boxed(),
                                        BinaryOp::Plus,
                                        Expr {
                                            expr_type: ExprType::NumberLiteral(456.2),
                                            position: range(1, 8, 12),
                                        }.boxed(),
                                    ),
                                    position: range(1, 2, 12),
                                }.boxed(),
                            ),
                            position: range(1, 1, 13),
                        },
                        position: range(1, 1, 14),
                    },],
                ),
            ),
        grouping_with_unary_minus:
            (
                vec![
                    Token::new(TokenType::LeftParen, range(1, 1, 1)),
                    Token::new(TokenType::Minus, range(1, 2, 2)),
                    Token::new_number(456.2, range(1, 3, 7)),
                    Token::new(TokenType::RightParen, range(1, 8, 8)),
                    Token::new(TokenType::Semicolon, range(1, 9, 9)),
                ],
                Ok(
                    vec![Statement::Expression {
                        expr: Expr {
                            expr_type: ExprType::Grouping(
                                Expr {
                                    expr_type: ExprType::Unary(
                                        UnaryOp::Minus,
                                        Expr {
                                            expr_type: ExprType::NumberLiteral(456.2),
                                            position: range(1, 3, 7),
                                        }.boxed(),
                                    ),
                                    position: range(1, 2, 7),
                                }.boxed(),
                            ),
                            position: range(1, 1, 8),
                        },
                        position: range(1, 1, 9),
                    },],
                ),
            ),
        error_grouping_no_right_paren:
            (
                vec![
                    Token::new(TokenType::LeftParen, range(1, 1, 1)),
                    Token::new_number(123.1, range(1, 2, 6)),
                    Token::new(TokenType::Plus, range(1, 7, 7)),
                    Token::new_number(456.2, range(1, 8, 12)),
                    Token::new(TokenType::Semicolon, range(1, 13, 13)),
                ],
                Err(
                    vec![LoxError {
                        kind: LoxErrorKind::Syntax,
                        message: "Expected ')' after expression".to_owned(),
                        position: range(1, 1, 12),
                    },],
                ),
            ),
        error_unary_no_operand:
            (
                vec![
                    Token::new(TokenType::Minus, range(1, 1, 1)),
                    Token::new(TokenType::Semicolon, range(1, 2, 2)),
                ],
                Err(
                    vec![LoxError {
                        kind: LoxErrorKind::Syntax,
                        message: "Expected expression, got ;.".to_owned(),
                        position: range(1, 1, 1),
                    },],
                ),
            ),
        var_declaration:
            (
                vec![
                    Token::new(TokenType::Var, range(1, 1, 3)),
                    Token::new(TokenType::Identifier("test".to_owned()), range(1, 5, 8)),
                    Token::new(TokenType::Semicolon, range(1, 9, 9)),
                ],
                Ok(
                    vec![Statement::Var {
                        name: "test".to_owned(),
                        initializer: None,
                        position: range(1, 1, 9),
                    },],
                ),
            ),
        var_declaration_with_initializer:
            (
                // var test = 123.1;
                vec![
                    Token::new(TokenType::Var, range(1, 1, 3)),
                    Token::new(TokenType::Identifier("test".to_owned()), range(1, 4, 8)),
                    Token::new(TokenType::Equal, range(1, 10, 10)),
                    Token::new_number(123.1, range(1, 12, 16)),
                    Token::new(TokenType::Semicolon, range(1, 17, 17)),
                ],
                Ok(
                    vec![Statement::Var {
                        name: "test".to_owned(),
                        initializer: Some(Expr {
                            expr_type: ExprType::NumberLiteral(123.1),
                            position: range(1, 12, 16),
                        }),
                        position: range(1, 1, 17),
                    },],
                ),
            ),
        var_declaration_with_initializer_expression:
            (
                // var test=1+2*3
                vec![
                    Token::new(TokenType::Var, range(1, 1, 3)),
                    Token::new(TokenType::Identifier("test".to_owned()), range(1, 5, 8)),
                    Token::new(TokenType::Equal, range(1, 9, 9)),
                    Token::new_number(1.0, range(1, 10, 10)),
                    Token::new(TokenType::Plus, range(1, 11, 11)),
                    Token::new_number(2.0, range(1, 12, 12)),
                    Token::new(TokenType::Star, range(1, 13, 13)),
                    Token::new_number(3.0, range(1, 14, 14)),
                    Token::new(TokenType::Semicolon, range(1, 15, 15)),
                ],
                Ok(
                    vec![Statement::Var {
                        name: "test".to_owned(),
                        initializer: Some(Expr {
                            expr_type: ExprType::Binary(
                                Expr {
                                    expr_type: ExprType::NumberLiteral(1.0),
                                    position: range(1, 10, 10),
                                }.boxed(),
                                BinaryOp::Plus,
                                Expr {
                                    expr_type: ExprType::Binary(
                                        Expr {
                                            expr_type: ExprType::NumberLiteral(2.0),
                                            position: range(1, 12, 12),
                                        }.boxed(),
                                        BinaryOp::Star,
                                        Expr {
                                            expr_type: ExprType::NumberLiteral(3.0),
                                            position: range(1, 14, 14),
                                        }.boxed(),
                                    ),
                                    position: range(1, 12, 14),
                                }.boxed(),
                            ),
                            position: range(1, 10, 14),
                        }),
                        position: range(1, 1, 15),
                    },],
                ),
            ),
        print_statement_with_string_sum: (
            vec![
                Token::new(TokenType::Print, range(1, 1, 5)),
                Token::new_string("abc", range(1, 6, 8)),
                Token::new(TokenType::Plus, range(1, 9, 9)),
                Token::new_string("def", range(1, 10, 12)),
                Token::new(TokenType::Semicolon, range(1, 13, 13)),
            ],
            Ok(
                vec![Statement::Print {
                    expr: Expr {
                        expr_type: ExprType::Binary(
                            Expr {
                                expr_type: ExprType::StringLiteral("abc".to_owned()),
                                position: range(1, 6, 8),
                            }.boxed(),
                            BinaryOp::Plus,
                            Expr {
                                expr_type: ExprType::StringLiteral("def".to_owned()),
                                position: range(1, 10, 12),
                            }.boxed(),
                        ),
                        position: range(1, 6, 12),
                    },
                    position: range(1, 1, 13),
                }],
            )
        ),

        assignment: (
            vec![
                Token::new(TokenType::Identifier("test".to_owned()), range(1, 1, 4)),
                Token::new(TokenType::Equal, range(1, 5, 5)),
                Token::new(TokenType::Number(123.0), range(1, 6, 8)),
                Token::new(TokenType::Semicolon, range(1, 9, 9)),
            ],
            Ok(vec![
                Statement::Expression {
                    expr: Expr {
                        expr_type: ExprType::Assignment {
                            name: "test".to_owned(),
                            value: Expr {
                                expr_type: ExprType::NumberLiteral(123.0),
                                position: range(1, 6, 8),
                            }.boxed(),
                        },
                        position: range(1, 1, 8),
                    },
                    position: range(1, 1, 9)
                }
            ]),
        ),
        assignment_throws_error_if_no_expression: (
            vec![
                Token::new(TokenType::Identifier("test".to_owned()), range(1, 1, 4)),
                Token::new(TokenType::Equal, range(1, 5, 5)),
                Token::new(TokenType::Semicolon, range(1, 9, 9)),
            ],
            Err(vec![LoxError {
                kind: LoxErrorKind::Syntax,
                message: "Expected expression, got ;.".to_owned(),
                position: range(1, 5, 5),
            }]),
        ),
    );
}
