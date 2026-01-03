use crate::expressions::Expr;
use crate::parser::Parser;

impl Parser<'_> {
    pub fn parse_expr(&mut self)-> Expr {
        Expr::dummy()
    }
}