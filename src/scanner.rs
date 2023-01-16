use std::{iter::Peekable, str::Chars, vec::Vec};

use crate::{
    error::{LoxError, LoxErrorKind},
    token::{Token, TokenType},
};

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

fn is_allowed_after_number(c: char) -> bool {
    match c {
        '/' | '*' | '+' | '-' | ')' | ';' => true,
        _ if c.is_whitespace() => true,
        _ => false,
    }
}

fn consume_number(
    it: &mut SourceIterator,
    first_char: char,
    line: u32,
) -> Result<Option<TokenType>, LoxError> {
    let mut n: f64 = char_to_f64(first_char);
    loop {
        let peek = it.peek();
        match peek {
            Some(cc) if cc.is_ascii_digit() => {
                n = n * 10.0 + char_to_f64(*cc);
                it.next();
            }
            Some(cc) if is_allowed_after_number(*cc) || *cc == '.' => break,
            None => break,
            _ => {
                return Err(LoxError {
                    kind: LoxErrorKind::Lexical,
                    message: format!("Invalid number literal: {}", peek.unwrap()),
                    line,
                })
            }
        }
    }
    match it.peek() {
        Some(c) if *c == '.' => {
            let mut decimal_place: f64 = 0.0;
            let mut consumed_at_least_one_digit_after_dot = false;
            loop {
                // consume dot
                it.next();
                let peek = it.peek();
                match peek {
                    Some(cc) if cc.is_ascii_digit() => {
                        decimal_place += 1.0;
                        n += 0.1_f64.powf(decimal_place) * char_to_f64(*cc);
                        consumed_at_least_one_digit_after_dot = true;
                    }
                    Some(cc)
                        if is_allowed_after_number(*cc)
                            && consumed_at_least_one_digit_after_dot =>
                    {
                        break
                    }
                    None if consumed_at_least_one_digit_after_dot => break,
                    _ => {
                        return Err(LoxError {
                            kind: LoxErrorKind::Lexical,
                            // TODO: fix error message
                            message: "Invalid number literal".to_owned(),
                            line,
                        });
                    }
                }
            }
        }
        None => {}
        Some(c) if is_allowed_after_number(*c) => {}
        _ => {
            return Err(LoxError {
                kind: LoxErrorKind::Lexical,
                line,
                message: "Invalid number literal".to_owned(),
            })
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
) -> Result<Option<TokenType>, LoxError> {
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

pub fn scan(source: &str) -> Result<Vec<Token>, LoxError> {
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
                    Some(cc) if cc.is_ascii_digit() => Err(LoxError {
                        kind: LoxErrorKind::Lexical,
                        message: format!("Invalid number literal: {}", peek.unwrap()),
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
                let mut error: Option<LoxError> = None;
                loop {
                    let cc = it.next();
                    match cc {
                        Some('"') => break,
                        Some('\n') => line += 1,
                        None => {
                            error = Some(LoxError {
                                kind: LoxErrorKind::Lexical,
                                line,
                                message: "Unterminated string".to_owned(),
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
            _ => Err(LoxError {
                kind: LoxErrorKind::Lexical,
                message: format!("Unexpected character: {}", c),
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
            Ok(vec![])
        ),
        comment: (
            "// test",
            Ok(vec![])
        ),
        comment_with_line_break: (
            "// test
            ",
            Ok(vec![])
        ),
        braces_and_parenthesis: (
            "({})",
            Ok(vec![
                Token{ type_: TokenType::LeftParen, line: 1, },
                Token{ type_: TokenType::LeftBrace, line: 1, },
                Token{ type_: TokenType::RightBrace, line: 1, },
                Token{ type_: TokenType::RightParen, line: 1, },
            ])
        ),
        string_literal: (
            "\"abc\"",
            Ok(vec![
                Token{ type_: TokenType::String("abc".to_owned()), line: 1, },
            ])
        ),
        string_literal_with_numbers: (
            "\"123\"",
            Ok(vec![
                Token{ type_: TokenType::String("123".to_owned()), line: 1, },
            ])
        ),
        unterminated_string_literal: (
            "\"abc",
            Err(LoxError {
                kind: LoxErrorKind::Lexical,
                message: "Unterminated string".to_owned(),
                line: 1,
            }),
        ),
        int_literal: (
            "123",
            Ok(vec![
                Token{ type_: TokenType::Number(123.0), line: 1, },
            ])
        ),
        float_literal: (
            "123.567",
            Ok(vec![
                Token{ type_: TokenType::Number(123.567), line: 1, },
            ])
        ),
        float_literal_with_line_break: (
            "123.5
            ",
            Ok(vec![
                Token{ type_: TokenType::Number(123.5), line: 1, },
            ])
        ),
        error_leading_decimal_point: (
            ".5",
            Err(LoxError { kind: LoxErrorKind::Lexical, line: 1, message: "Invalid number literal: 5".to_owned()}),
        ),
        error_trailing_decimal_point: (
            "5.",
            Err(LoxError { kind: LoxErrorKind::Lexical, line: 1, message: "Invalid number literal".to_owned()}),
        ),
        error_number_with_trailing_letters: (
            "123ff",
            Err(LoxError { kind: LoxErrorKind::Lexical, line: 1, message: "Invalid number literal: f".to_owned()}),
        ),
        error_float_number_with_trailing_letters: (
            "123.1ff",
            Err(LoxError { kind: LoxErrorKind::Lexical, line: 1, message: "Invalid number literal".to_owned()}),
        ),
        keyword_class: (
            "class",
            Ok(vec![
                Token{ type_: TokenType::Class, line: 1, },
            ]),
        ),
        identifier_classs: (
            "classs",
            Ok(vec![
                Token{ type_: TokenType::Identifier("classs".to_owned()), line: 1, },
            ]),
        ),
        identifier_alphanumeric: (
            "test123",
            Ok(vec![
                Token{ type_: TokenType::Identifier("test123".to_owned()), line: 1, },
            ]),
        ),
        int_sum: (
            "1+2",
            Ok(vec![
                Token { type_: TokenType::Number(1.0), line: 1 },
                Token { type_: TokenType::Plus, line: 1 },
                Token { type_: TokenType::Number(2.0), line: 1 },
            ]),
        ),
        int_float_sum: (
            "1 + 2.0
            ",
            Ok(vec![
                Token { type_: TokenType::Number(1.0), line: 1 },
                Token { type_: TokenType::Plus, line: 1 },
                Token { type_: TokenType::Number(2.0), line: 1 },
            ]),
        ),
        float_sum: (
            "1.1+2.0
            ",
            Ok(vec![
                Token { type_: TokenType::Number(1.1), line: 1 },
                Token { type_: TokenType::Plus, line: 1 },
                Token { type_: TokenType::Number(2.0), line: 1 },
            ]),
        ),
        sum_statement: (
            "1+2;",
            Ok(vec![
                Token::new_number(1.0, 1),
                Token::new(TokenType::Plus, 1),
                Token::new_number(2.0, 1),
                Token::new(TokenType::Semicolon, 1),
            ])
        ),
    }
}
