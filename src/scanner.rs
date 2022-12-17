use std::{iter::Peekable, str::Chars, vec::Vec};

use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct LexicalError {
    character: char,
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
            '.' => Ok(Some(TokenType::Dot)),
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
            '/' => Ok(Some(TokenType::Slash)),
            '\n' => {
                line += 1;
                Ok(None)
            }
            cc if cc.is_whitespace() => Ok(None),
            _ => Err(LexicalError { character: c }),
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
    }
}
