use zirael_parser::*;
use zirael_utils::prelude::*;

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub ty: Type,
    pub span: Span,
    pub id: AstId,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    Literal(Literal),
    Symbol(SymbolId),
    Binary { left: Box<HirExpr>, op: BinaryOp, right: Box<HirExpr> },
    Unary { op: UnaryOp, operand: Box<HirExpr> },
    Block(Vec<HirStmt>),
    Assign { lhs: Box<HirExpr>, rhs: Box<HirExpr> },
    Call { callee: Box<HirExpr>, args: Vec<HirExpr>, call_info: Option<CallInfo> },
    StructInit { name: Box<HirExpr>, fields: HashMap<Identifier, HirExpr> },
    FieldAccess { receiver: Box<HirExpr>, field_symbol: SymbolId },
    IndexAccess { object: Box<HirExpr>, index: Box<HirExpr> },
    Error,
}

#[derive(Debug, Clone)]
pub enum HirStmt {
    Expr(HirExpr),
    Var { symbol_id: SymbolId, init: HirExpr },
    Return(Option<HirExpr>),
}
