use std::fmt::Display;
use zirael_parser::*;
use zirael_utils::prelude::*;

#[derive(Debug, Clone)]
pub struct HirMatchArm {
    pub pattern: HirPattern,
    pub body: HirExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirPattern {
    Wildcard,
    Identifier(SymbolId),
    Literal(Literal),
    EnumVariant {
        symbol_id: SymbolId,
        fields: Option<Vec<HirPatternField>>,
    },
    Struct {
        symbol_id: SymbolId,
        fields: Vec<HirPatternField>,
    },
}

#[derive(Debug, Clone)]
pub struct HirPatternField {
    pub name: Identifier,
    pub symbol_id: Option<SymbolId>,
    pub pattern: Option<Box<HirPattern>>,
    pub span: Span,
}

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
    Binary {
        left: Box<HirExpr>,
        op: BinaryOp,
        right: Box<HirExpr>,
    },
    Ternary {
        condition: Box<HirExpr>,
        true_expr: Box<HirExpr>,
        false_expr: Box<HirExpr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<HirExpr>,
    },
    Block(Vec<HirStmt>),
    Assign {
        lhs: Box<HirExpr>,
        rhs: Box<HirExpr>,
    },
    Call {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
        call_info: Option<CallInfo>,
    },
    StructInit {
        name: Box<HirExpr>,
        fields: HashMap<Identifier, HirExpr>,
        call_info: Option<CallInfo>,
    },
    FieldAccess {
        base_field: FieldSymbol,
        main_access: AccessKind,
        fields: Vec<(Identifier, AccessKind)>,
    },
    IndexAccess {
        object: Box<HirExpr>,
        index: Box<HirExpr>,
    },
    Match {
        scrutinee: Box<HirExpr>,
        arms: Vec<HirMatchArm>,
    },
    Error,
}

#[derive(Debug, Clone)]
pub enum FieldSymbol {
    Symbol(SymbolId),
    Expr(Box<HirExpr>),
}

#[derive(Debug, Clone)]
pub enum AccessKind {
    Value,
    Pointer,
}

impl Display for AccessKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value => write!(f, "."),
            Self::Pointer => write!(f, "->"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum HirStmt {
    Expr(HirExpr),
    Var { symbol_id: SymbolId, init: HirExpr },
    Return(Option<HirExpr>),
}
