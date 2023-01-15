use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression { expr: Expr },
    Print { expr: Expr },
    Var { name: String, initializer: Option<Expr> },
}
