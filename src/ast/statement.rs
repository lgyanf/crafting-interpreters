use super::expr::Expr;

pub enum Statement {
    Expression { expr: Expr },
    Print { expr: Expr },
    Var { name: String, initializer: Option<Expr> },
}
