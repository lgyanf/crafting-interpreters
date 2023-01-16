use super::{Expr, Statement};

pub trait Visitor {
    type Result;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
}

pub trait StatementVisitor {
    type Result;

    fn visit_statement(&mut self, statement: &Statement) -> Self::Result;
}