use super::expr::Expr;

pub enum Statement {
    Var(String, Option<Expr>),
}
