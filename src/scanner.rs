use std::{iter::Peekable, str::Chars, vec::Vec};

use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumberLiteral,
}

type SourceIterator<'a> = Peekable<Chars<'a>>;

fn peek_match(it: &mut SourceIterator, c: char) -> bool {
    if it.peek() == Some(&c) {
        it.next();
        true
    } else {
        false
    }
}

fn peek_match_equals(
    it: &mut SourceIterator,
    true_result: TokenType,
    false_result: TokenType,
) -> TokenType {
    if peek_match(it, '=') {
        true_result
    } else {
        false_result
    }
}

fn char_to_f64(c: char) -> f64 {
    ((c as u32) - ('0' as u32)) as f64
}

pub fn scan(source: &String) -> Result<Vec<Token>, LexicalError> {
    let mut tokens: Vec<Token> = Vec::new();
    let (mut line, mut start, mut current): (u32, usize, usize) = (1, 0, 0);
    let mut it = source.chars().peekable();
    while let Some(c) = it.next() {
        let token_type_opt = match c {
            '(' => Ok(Some(TokenType::LeftParen)),
            ')' => Ok(Some(TokenType::RightParen)),
            '{' => Ok(Some(TokenType::LeftBrace)),
            '}' => Ok(Some(TokenType::RightBrace)),
            ',' => Ok(Some(TokenType::Comma)),
            '.' => {
                let peek = it.peek();
                match peek {
                    Some(cc) if cc.is_ascii_digit() => Err(LexicalError::InvalidNumberLiteral),
                    _ => Ok(Some(TokenType::Dot)),
                }
            }
            '-' => Ok(Some(TokenType::Minus)),
            '+' => Ok(Some(TokenType::Plus)),
            ';' => Ok(Some(TokenType::Semicolon)),
            '*' => Ok(Some(TokenType::Star)),
            '!' => Ok(Some(peek_match_equals(
                &mut it,
                TokenType::BangEqual,
                TokenType::Bang,
            ))),
            '=' => Ok(Some(peek_match_equals(
                &mut it,
                TokenType::EqualEqual,
                TokenType::Equal,
            ))),
            '<' => Ok(Some(peek_match_equals(
                &mut it,
                TokenType::LessEqual,
                TokenType::Less,
            ))),
            '>' => Ok(Some(peek_match_equals(
                &mut it,
                TokenType::GreaterEqual,
                TokenType::Greater,
            ))),
            '/' if peek_match(&mut it, '/') => {
                loop {
                    match it.peek() {
                        Some('\n') | None => break,
                        _ => it.next(),
                    };
                }
                Ok(None)
            }
            '"' => {
                let mut accumulator = String::new();
                let mut error: Option<LexicalError> = None;
                loop {
                    let cc = it.next();
                    match cc {
                        Some('"') => break,
                        Some('\n') => line += 1,
                        None => {
                            error = Some(LexicalError::UnterminatedString);
                            break;
                        }
                        Some(cc) => accumulator.push(cc),
                    };
                }
                if let Some(e) = error {
                    Err(e)
                } else {
                    Ok(Some(TokenType::String(accumulator)))
                }
            }
            cc if cc.is_ascii_digit() => {
                let mut n: f64 = char_to_f64(cc);
                let mut seen_dot = false;
                let mut decimal_place: f64 = 0.0;
                loop {
                    let peek = it.peek();
                    match peek {
                        Some(cc) if cc.is_ascii_digit() => {
                            let current_digit = char_to_f64(it.next().unwrap());
                            if seen_dot {
                                decimal_place += 1.0;
                                n = n + current_digit * (0.1 as f64).powf(decimal_place);
                            } else {
                                n = n * 10.0 + current_digit;
                            }
                        }
                        Some('.') => {
                            it.next();
                            if !seen_dot {
                                match it.peek() {
                                    None => return Err(LexicalError::InvalidNumberLiteral),
                                    Some(cc) if !cc.is_ascii_digit() => {
                                        return Err(LexicalError::InvalidNumberLiteral);
                                    }
                                    _ => {}
                                }
                                seen_dot = true;
                            } else {
                                return Err(LexicalError::InvalidNumberLiteral);
                            }
                        }
                        None => break,
                        _ => {}
                    }
                }
                Ok(Some(TokenType::Number(n)))
            }
            '/' => Ok(Some(TokenType::Slash)),
            '\n' => {
                line += 1;
                Ok(None)
            }
            cc if cc.is_whitespace() => Ok(None),
            _ => Err(LexicalError::UnexpectedCharacter(c)),
        }?;
        if let Some(token_type) = token_type_opt {
            tokens.push(Token {
                type_: token_type,
                line,
            });
        }
    }
    tokens.push(Token {
        type_: TokenType::EOF,
        line: line,
    });
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                assert_eq!(expected, scan(&input.to_owned()));
            }
        )*
        }
    }

    parametrized_tests! {
        empty_string: (
            "",
            Ok(vec![
                Token{ type_: TokenType::EOF, line: 1 },
            ])
        ),
        comment: (
            "// test",
            Ok(vec![
                Token{ type_: TokenType::EOF, line: 1, },
            ])
        ),
        comment_with_line_break: (
            "// test
            ",
            Ok(vec![
                Token{ type_: TokenType::EOF, line: 2, },
            ])
        ),
        braces_and_parenthesis: (
            "({})",
            Ok(vec![
                Token{ type_: TokenType::LeftParen, line: 1, },
                Token{ type_: TokenType::LeftBrace, line: 1, },
                Token{ type_: TokenType::RightBrace, line: 1, },
                Token{ type_: TokenType::RightParen, line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ])
        ),
        string_literal: (
            "\"abc\"",
            Ok(vec![
                Token{ type_: TokenType::String("abc".to_owned()), line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ])
        ),
        unterminated_string_literal: (
            "\"abc",
            Err(LexicalError::UnterminatedString),
        ),
        int_literal: (
            "123",
            Ok(vec![
                Token{ type_: TokenType::Number(123.0), line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ])
        ),
        float_literal: (
            "123.5",
            Ok(vec![
                Token{ type_: TokenType::Number(123.5), line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ])
        ),
        error_leading_decimal_point: (
            ".5",
            Err(LexicalError::InvalidNumberLiteral),
        ),
        error_trailing_decimal_point: (
            "5.",
            Err(LexicalError::InvalidNumberLiteral),
        ),
    }
}
