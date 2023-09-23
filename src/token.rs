use float_cmp::approx_eq;
use std::fmt::Display;

use crate::position::{PositionRange};

#[derive(Debug, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    String(String),
    Number(f64),

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l0), Self::Identifier(r0)) => *l0 == *r0,
            (Self::String(l0), Self::String(r0)) => *l0 == *r0,
            (Self::Number(l0), Self::Number(r0)) => approx_eq!(f64, *l0, *r0),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ch = match self {
            TokenType::LeftParen => "(".to_owned(),
            TokenType::RightParen => ")".to_owned(),
            TokenType::LeftBrace => "{".to_owned(),
            TokenType::RightBrace => "}".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::Dot => ".".to_owned(),
            TokenType::Minus => "-".to_owned(),
            TokenType::Plus => "+".to_owned(),
            TokenType::Semicolon => ";".to_owned(),
            TokenType::Slash => "/".to_owned(),
            TokenType::Star => "*".to_owned(),
            TokenType::Bang => "!".to_owned(),
            TokenType::BangEqual => "!=".to_owned(),
            TokenType::Equal => "=".to_owned(),
            TokenType::EqualEqual => "==".to_owned(),
            TokenType::Greater => ">".to_owned(),
            TokenType::GreaterEqual => ">=".to_owned(),
            TokenType::Less => "<".to_owned(),
            TokenType::LessEqual => "<=".to_owned(),
            TokenType::Identifier(s) => s.clone(),
            TokenType::String(s) => s.clone(),
            TokenType::Number(n) => n.to_string(),
            TokenType::And => "and".to_owned(),
            TokenType::Class => "class".to_owned(),
            TokenType::Else => "else".to_owned(),
            TokenType::False => "false".to_owned(),
            TokenType::Fun => "fun".to_owned(),
            TokenType::For => "for".to_owned(),
            TokenType::If => "if".to_owned(),
            TokenType::Nil => "nil".to_owned(),
            TokenType::Or => "or".to_owned(),
            TokenType::Print => "print".to_owned(),
            TokenType::Return => "return".to_owned(),
            TokenType::Super => "super".to_owned(),
            TokenType::This => "this".to_owned(),
            TokenType::True => "true".to_owned(),
            TokenType::Var => "var".to_owned(),
            TokenType::While => "while".to_owned(),
        };
        write!(f, "{}", ch)
    }
}

impl TokenType {
    pub fn size(&self) -> usize {
        match self {
            Self::String(s) => s.len() + 2usize,
            _ => self.to_string().len(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub type_: TokenType,
    pub position: PositionRange,
}

impl Token {
    pub fn new(type_: TokenType, position: PositionRange) -> Token {
        Token { type_, position, }
    }

    pub fn new_number(n: f64, position: PositionRange) -> Token {
        Token::new(TokenType::Number(n), position)
    }

    pub fn new_string(value: &str, position: PositionRange) -> Token {
        Token::new(TokenType::String(value.to_owned()), position)
    }
}
