// use crate::expr::{Block, Expr, FieldInit, Local, MatchArm, Stmt};
// use crate::generics::GenericParam;
// use crate::ids::HirId;
// use crate::item::{Const, Enum, Field, Function, Method, Module, Param, Struct};
// use crate::pat::{FieldPat, Pat};
// use crate::ty::Ty;
// use zirael_utils::prelude::Span;
//
// /// Any HIR node - unified enum for looking up and traversing the HIR tree.
// #[derive(Debug, Clone)]
// pub enum Node<'hir> {
//   // Items
//   Function(&'hir Function),
//   Struct(&'hir Struct),
//   Enum(&'hir Enum),
//   Const(&'hir Const),
//   Module(&'hir Module),
//
//   // Item parts
//   Param(&'hir Param),
//   Field(&'hir Field),
//   Method(&'hir Method),
//   Variant(&'hir crate::item::Variant),
//   GenericParam(&'hir GenericParam),
//
//   // Expressions
//   Expr(&'hir Expr),
//   Block(&'hir Block),
//   FieldInit(&'hir FieldInit),
//   MatchArm(&'hir MatchArm),
//
//   // Statements
//   Stmt(&'hir Stmt),
//   Local(&'hir Local),
//
//   // Types
//   Ty(&'hir Ty),
//
//   // Patterns
//   Pat(&'hir Pat),
//   FieldPat(&'hir FieldPat),
// }
//
// impl<'hir> Node<'hir> {
//   /// Get the HirId of this node.
//   pub fn hir_id(&self) -> HirId {
//     match self {
//       Node::Function(f) => f.hir_id,
//       Node::Struct(s) => s.hir_id,
//       Node::Enum(e) => e.hir_id,
//       Node::Const(c) => c.hir_id,
//       Node::Module(m) => m.hir_id,
//       Node::Param(p) => p.hir_id,
//       Node::Field(f) => f.hir_id,
//       Node::Method(m) => m.hir_id,
//       Node::Variant(v) => v.hir_id,
//       Node::GenericParam(g) => g.hir_id,
//       Node::Expr(e) => e.hir_id,
//       Node::Block(b) => b.hir_id,
//       Node::FieldInit(f) => f.hir_id,
//       Node::MatchArm(m) => m.hir_id,
//       Node::Stmt(s) => s.hir_id,
//       Node::Local(l) => l.hir_id,
//       Node::Ty(t) => t.hir_id,
//       Node::Pat(p) => p.hir_id,
//       Node::FieldPat(f) => f.hir_id,
//     }
//   }
//
//   /// Get the span of this node.
//   pub fn span(&self) -> Span {
//     match self {
//       Node::Function(f) => f.span,
//       Node::Struct(s) => s.span,
//       Node::Enum(e) => e.span,
//       Node::Const(c) => c.span,
//       Node::Module(m) => m.span,
//       Node::Param(p) => p.span,
//       Node::Field(f) => f.span,
//       Node::Method(m) => m.span,
//       Node::Variant(v) => v.span,
//       Node::GenericParam(g) => g.span,
//       Node::Expr(e) => e.span,
//       Node::Block(b) => b.span,
//       Node::FieldInit(f) => f.span,
//       Node::MatchArm(m) => m.span,
//       Node::Stmt(s) => s.span,
//       Node::Local(l) => l.span,
//       Node::Ty(t) => t.span,
//       Node::Pat(p) => p.span,
//       Node::FieldPat(f) => f.span,
//     }
//   }
//
//   // Convenience casting methods
//
//   pub fn as_function(&self) -> Option<&'hir Function> {
//     match self {
//       Node::Function(f) => Some(f),
//       _ => None,
//     }
//   }
//
//   pub fn as_struct(&self) -> Option<&'hir Struct> {
//     match self {
//       Node::Struct(s) => Some(s),
//       _ => None,
//     }
//   }
//
//   pub fn as_enum(&self) -> Option<&'hir Enum> {
//     match self {
//       Node::Enum(e) => Some(e),
//       _ => None,
//     }
//   }
//
//   pub fn as_const(&self) -> Option<&'hir Const> {
//     match self {
//       Node::Const(c) => Some(c),
//       _ => None,
//     }
//   }
//
//   pub fn as_expr(&self) -> Option<&'hir Expr> {
//     match self {
//       Node::Expr(e) => Some(e),
//       _ => None,
//     }
//   }
//
//   pub fn as_block(&self) -> Option<&'hir Block> {
//     match self {
//       Node::Block(b) => Some(b),
//       _ => None,
//     }
//   }
//
//   pub fn as_stmt(&self) -> Option<&'hir Stmt> {
//     match self {
//       Node::Stmt(s) => Some(s),
//       _ => None,
//     }
//   }
//
//   pub fn as_ty(&self) -> Option<&'hir Ty> {
//     match self {
//       Node::Ty(t) => Some(t),
//       _ => None,
//     }
//   }
//
//   pub fn as_pat(&self) -> Option<&'hir Pat> {
//     match self {
//       Node::Pat(p) => Some(p),
//       _ => None,
//     }
//   }
// }
