use crate::token::{Token, TokenType};

#[derive(PartialEq, Debug)]
enum Expr {
    Literal(TokenType),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
}

mod visit {
    use super::*;

    pub trait Visitor {
        type Result;

        fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
    }
}

struct AstPrinter {}

impl visit::Visitor for AstPrinter {
    type Result = String;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match expr {
            Expr::Literal(token) => token.to_string(),
            Expr::Unary(op, e) => {
                format!("({} {})", op.type_.to_string(), self.visit_expr(&e))
            }
            Expr::Binary(left, op, right) => format!(
                "({} {} {})",
                op.type_.to_string(),
                self.visit_expr(&left),
                self.visit_expr(&right)
            ),
            Expr::Grouping(e) => format!("({})", self.visit_expr(&e)),
        }
    }
}
