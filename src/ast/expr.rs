use crate::token::{Token, TokenType};

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Literal(Token),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Variable { name: String }
}

impl Expr {

    pub fn number_literal(value: f64, line: u32) -> Self {
        Self::Literal(
            Token {
                type_: TokenType::Number(value),
                line,
            }
        )
    }

    pub fn boxed(&self) -> Box<Self> {
        Box::new(self.clone())
    }

}
