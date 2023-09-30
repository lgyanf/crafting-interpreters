use crate::position::PositionRange;

use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression {
        expr: Expr,
        position: PositionRange,
    },
    Print {
        expr: Expr,
        position: PositionRange,
    },
    Var {
        name: String,
        initializer: Option<Expr>,
        position: PositionRange,
    },
}

impl Statement {
    fn position(&self) -> &PositionRange {
        match &self {
            Self::Expression { expr: _, position } => position,
            Self::Print { expr: _, position } => position,
            Self::Var {
                name: _,
                initializer: _,
                position,
            } => position,
        }
    }
}
