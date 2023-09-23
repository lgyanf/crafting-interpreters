use super::{
    expr::{Expr, ExprType},
    visitor::Visitor,
};

pub struct AstPrinter {}

impl Visitor for AstPrinter {
    type Result = String;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match &expr.expr_type {
            ExprType::StringLiteral(s) => s.clone(),
            ExprType::BooleanLiteral(b) => b.to_string(),
            ExprType::NumberLiteral(n) => n.to_string(),
            ExprType::Nil => "Nil".to_owned(),
            ExprType::Unary(op, e) => {
                format!("({} {})", op, self.visit_expr(&e))
            }
            ExprType::Binary(left, op, right) => format!(
                "({} {} {})",
                op,
                self.visit_expr(&left),
                self.visit_expr(&right)
            ),
            ExprType::Grouping(e) => format!("(group {})", self.visit_expr(&e)),
            ExprType::Variable { name } => name.clone(),
        }
    }
}

// #[cfg(test)]

// mod ast_printer_tests {
//     use crate::token::{Token, TokenType};

//     use super::*;

//     macro_rules! parametrized_tests {
//         ($($name:ident: $value:expr,)*) => {
//         $(
//             #[test]
//             fn $name() {
//                 let (input, expected) = $value;
//                 let mut printer = AstPrinter{};
//                 let printed_ast = printer.visit_expr(&input);
//                 assert_eq!(printed_ast, expected);
//             }
//         )*
//         }
//     }

//     parametrized_tests!(
//         example1: (
//             Expr::Binary(
//                 Box::new(
//                     Expr::Unary(
//                         Token { type_: TokenType::Minus, line: 1 },
//                         Box::new(Expr::Literal(
//                             Token{ type_: TokenType::Number(123.0), line: 1}
//                         ))
//                     )
//                 ),
//                 Token { type_: TokenType::Star, line: 1},
//                 Box::new(
//                     Expr::Grouping(
//                         Box::new(Expr::Literal(
//                             Token { type_: TokenType::Number(45.67), line: 1 }
//                         )),
//                     ),
//                 ),
//             ),
//             "(* (- 123) (group 45.67))"
//         ),
//     );
// }
