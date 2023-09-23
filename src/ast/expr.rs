use std::fmt::Display;

use crate::{
    position::PositionRange,
    token::{Token, TokenType},
};

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    EqualEqual,
    BangEqual,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOp::Plus => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Star => "*",
            BinaryOp::Slash => "/",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::EqualEqual => "==",
            BinaryOp::BangEqual => "!=",
        };
        write!(f, "{}", s)
    }
}

impl From<Token> for BinaryOp {
    fn from(t: Token) -> Self {
        match t.type_ {
            TokenType::Plus => Self::Plus,
            TokenType::Minus => Self::Minus,
            TokenType::Star => Self::Star,
            TokenType::Slash => Self::Slash,
            TokenType::Greater => Self::Greater,
            TokenType::GreaterEqual => Self::GreaterEqual,
            TokenType::Less => Self::Less,
            TokenType::LessEqual => Self::LessEqual,
            _ => unreachable!(
                "Cannot create binary operator from {} at {:?}",
                t.type_, t.position
            ),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOp::Minus => "-",
            UnaryOp::Bang => "!",
        };
        write!(f, "{}", s)
    }
}

impl From<Token> for UnaryOp {
    fn from(t: Token) -> Self {
        match t.type_ {
            TokenType::Minus => Self::Minus,
            TokenType::Bang => Self::Bang,
            _ => unreachable!(
                "Cannot create unary operator from {} at {:?}",
                t.type_, t.position
            ),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExprType {
    StringLiteral(String),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    Nil,
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
    Variable { name: String },
}

impl Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::StringLiteral(s) => write!(f, "\"{}\"", s),
            ExprType::NumberLiteral(n) => write!(f, "{}", n),
            ExprType::BooleanLiteral(b) => write!(f, "{}", b),
            ExprType::Nil => write!(f, "Nil"),
            ExprType::Unary(op, right) => write!(f, "{}{}", op, right),
            ExprType::Binary(left, op, right) => write!(f, "{}{}{}", left, op, right),
            ExprType::Grouping(expr) => write!(f, "({})", expr),
            ExprType::Variable { name } => write!(f, "{}", name),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Expr {
    pub expr_type: ExprType,
    pub position: PositionRange,
}

impl Expr {
    pub fn nil(position: PositionRange) -> Self {
        Self {
            expr_type: ExprType::Nil,
            position,
        }
    }

    pub fn boolean_literal(value: bool, position: PositionRange) -> Self {
        Self {
            expr_type: ExprType::BooleanLiteral(value),
            position,
        }
    }

    pub fn number_literal(value: f64, position: PositionRange) -> Self {
        Self {
            expr_type: ExprType::NumberLiteral(value),
            position,
        }
    }

    pub fn string_literal(value: &str, position: PositionRange) -> Self {
        Self {
            expr_type: ExprType::StringLiteral(value.to_owned()),
            position,
        }
    }

    pub fn boxed(&self) -> Box<Self> {
        Box::new(self.clone())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr_type)
    }
}
