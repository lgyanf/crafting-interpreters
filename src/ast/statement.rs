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
    Block {
        statements: Vec<Statement>,
        position: PositionRange,
    }
}

impl Statement {
    fn position(&self) -> &PositionRange {
        match &self {
            // Self::Assignment { name: _, expr: _, position } => position,
            Statement::Expression { expr: _, position } => position,
            Statement::Print { expr: _, position } => position,
            Statement::Var {
                name: _,
                initializer: _,
                position,
            } => position,
            Statement::Block { statements: _, position } => position,
        }
    }
}
