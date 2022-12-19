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
            Expr::Grouping(e) => format!("(group {})", self.visit_expr(&e)),
        }
    }
}

#[cfg(test)]

mod tests {
    use super::*;
    use visit::*;

    macro_rules! parametrized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let mut printer = AstPrinter{};
                let printed_ast = printer.visit_expr(&input);
                assert_eq!(printed_ast, expected);
            }
        )*
        }
    }

    parametrized_tests!(
        example1: (
            Expr::Binary(
                Box::new(
                    Expr::Unary(
                        Token { type_: TokenType::Minus, line: 1 },
                        Box::new(Expr::Literal(TokenType::Number(123.0)))
                    )
                ),
                Token { type_: TokenType::Star, line: 1},
                Box::new(
                    Expr::Grouping(
                        Box::new(Expr::Literal(TokenType::Number(45.67))),
                    ),
                ),
            ),
            "(* (- 123) (group 45.67))"
        ),
    );
}
