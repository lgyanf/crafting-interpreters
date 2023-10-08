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
    },
    IfElse {
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
        position: PositionRange,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
        position: PositionRange,
    }
}

impl Statement {
    pub fn position(&self) -> &PositionRange {
        match self {
            // Self::Assignment { name: _, expr: _, position } => position,
            Statement::Expression { expr: _, position } => position,
            Statement::Print { expr: _, position } => position,
            Statement::Var {
                name: _,
                initializer: _,
                position,
            } => position,
            Statement::Block { statements: _, position } => position,
            Statement::IfElse { condition: _, then_branch: _, else_branch: _, position } => {
                position
            },
            Statement::While { condition: _, body: _, position } => position,
        }
    }

    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
