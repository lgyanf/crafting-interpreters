use std::{iter::Peekable, str::Chars, vec::Vec};

use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexicalErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumberLiteral,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexicalError {
    pub line: u32,
    pub kind: LexicalErrorKind,
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

fn consume_number(
    it: &mut SourceIterator,
    first_char: char,
    line: u32,
) -> Result<Option<TokenType>, LexicalError> {
    let mut n: f64 = char_to_f64(first_char);
    loop {
        let peek = it.peek();
        match peek {
            Some(cc) if cc.is_ascii_digit() => {
                n = n * 10.0 + char_to_f64(*cc);
                it.next();
            }
            Some(cc) if *cc == '\n' || cc.is_whitespace() || *cc == '.' => {
                break;
            }
            None => break,
            _ => {
                return Err(LexicalError {
                    kind: LexicalErrorKind::InvalidNumberLiteral,
                    line,
                })
            }
        }
    }
    if it.peek() == Some(&'.') {
        let mut current_char = it.next();
        let mut decimal_place: f64 = 0.0;
        loop {
            match it.peek() {
                Some(cc) if cc.is_ascii_digit() => {
                    decimal_place += 1.0;
                    n += 0.1_f64.powf(decimal_place) * char_to_f64(*cc);
                    current_char = it.next();
                }
                Some(cc) if *cc == '\n' || cc.is_whitespace() => {
                    break;
                }
                None if current_char != Some('.') => break,
                _ => {
                    return Err(LexicalError {
                        kind: LexicalErrorKind::InvalidNumberLiteral,
                        line,
                    })
                }
            }
        }
    }
    Ok(Some(TokenType::Number(n)))
}

fn try_match_keyword(s: &String) -> TokenType {
    match s.as_str() {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        _ => TokenType::Identifier(s.to_string()),
    }
}

fn consume_identifier_or_keyword(
    it: &mut SourceIterator,
    first_char: char,
) -> Result<Option<TokenType>, LexicalError> {
    let mut accumulator = String::new();
    accumulator.push(first_char);
    loop {
        match it.peek() {
            Some(c) if c.is_alphanumeric() => {
                accumulator.push(it.next().unwrap());
            }
            _ => break,
        }
    }
    Ok(Some(try_match_keyword(&accumulator)))
}

pub fn scan(source: &str) -> Result<Vec<Token>, LexicalError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut line = 1u32;
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
                    Some(cc) if cc.is_ascii_digit() => Err(LexicalError {
                        kind: LexicalErrorKind::InvalidNumberLiteral,
                        line,
                    }),
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
                            error = Some(LexicalError {
                                line,
                                kind: LexicalErrorKind::UnterminatedString,
                            });
                            break;
                        }
                        Some(cc) => accumulator.push(cc),
                    };
                }
                if let Some(e) = error {
                    return Err(e);
                } else {
                    Ok(Some(TokenType::String(accumulator)))
                }
            }
            cc if cc.is_ascii_digit() => consume_number(&mut it, cc, line),
            cc if cc.is_alphabetic() => consume_identifier_or_keyword(&mut it, cc),
            '/' => Ok(Some(TokenType::Slash)),
            '\n' => {
                line += 1;
                Ok(None)
            }
            cc if cc.is_whitespace() => Ok(None),
            _ => Err(LexicalError {
                kind: LexicalErrorKind::UnexpectedCharacter(c),
                line,
            }),
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
        line,
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
            Err(LexicalError {
                kind: LexicalErrorKind::UnterminatedString,
                line: 1,
            }),
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
        float_literal_with_line_break: (
            "123.5
            ",
            Ok(vec![
                Token{ type_: TokenType::Number(123.5), line: 1, },
                Token{ type_: TokenType::EOF, line: 2, },
            ])
        ),
        error_leading_decimal_point: (
            ".5",
            Err(LexicalError { kind: LexicalErrorKind::InvalidNumberLiteral, line: 1}),
        ),
        error_trailing_decimal_point: (
            "5.",
            Err(LexicalError { kind: LexicalErrorKind::InvalidNumberLiteral, line: 1}),
        ),
        error_number_with_trailing_letters: (
            "123ff",
            Err(LexicalError { kind: LexicalErrorKind::InvalidNumberLiteral, line: 1}),
        ),
        error_float_number_with_trailing_letters: (
            "123.1ff",
            Err(LexicalError { kind: LexicalErrorKind::InvalidNumberLiteral, line: 1}),
        ),
        keyword_class: (
            "class",
            Ok(vec![
                Token{ type_: TokenType::Class, line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ]),
        ),
        identifier_classs: (
            "classs",
            Ok(vec![
                Token{ type_: TokenType::Identifier("classs".to_owned()), line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ]),
        ),
        identifier_alphanumeric: (
            "test123",
            Ok(vec![
                Token{ type_: TokenType::Identifier("test123".to_owned()), line: 1, },
                Token{ type_: TokenType::EOF, line: 1, },
            ]),
        ),
    }
}
