use crate::lower::context::LoweringContext;
use crate::ty::{ArrayLen, PathSegment, Ty, TyKind};
use zirael_parser::Path;
use zirael_parser::ast::types::Type;
use zirael_source::prelude::Span;
use zirael_utils::prelude::{Identifier, get_or_intern};

impl LoweringContext<'_> {
  pub fn lower_path(&mut self, path: &Path) -> Vec<PathSegment> {
    let mut segments = vec![];
    if let Some(root) = path.root {
      segments.push(PathSegment {
        name: Identifier::new(&root.to_string(), Span::dummy()),
        args: vec![],
      })
    }

    for seg in &path.segments {
      segments.push(PathSegment {
        name: seg.identifier,
        args: seg.args.iter().map(|s| self.lower_type(s)).collect(),
      })
    }

    segments
  }

  pub fn lower_type(&mut self, ty: &Type) -> Ty {
    let kind = match ty {
      Type::Primitive(p) => TyKind::Primitive(p.kind),

      Type::Path(path) => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          TyKind::Path {
            def_id,
            segments: self.lower_path(path),
          }
        } else {
          TyKind::Err
        }
      }

      Type::Pointer(ptr) => TyKind::Ptr {
        mutability: ptr.mutability,
        ty: Box::new(self.lower_type(&ptr.inner)),
      },

      Type::Optional(opt) => {
        TyKind::Optional(Box::new(self.lower_type(&opt.inner)))
      }

      Type::Array(arr) => TyKind::Array {
        ty: Box::new(self.lower_type(&arr.element)),
        len: ArrayLen::ConstExpr(Box::new(self.lower_expr(&arr.size))),
      },

      Type::Tuple(tup) => {
        TyKind::Tuple(tup.elements.iter().map(|t| self.lower_type(t)).collect())
      }

      Type::Function(func) => TyKind::Fn {
        params: func.params.iter().map(|t| self.lower_type(t)).collect(),
        ret: Box::new(self.lower_type(&func.return_type)),
      },

      Type::Unit(_) => TyKind::Unit,

      Type::Never(_) => TyKind::Never,

      Type::Invalid => TyKind::Err,
    };

    Ty {
      hir_id: self.next_hir_id(),
      kind,
      span: get_type_span(ty),
    }
  }
}

fn get_type_span(ty: &Type) -> Span {
  match ty {
    Type::Primitive(p) => p.span,
    Type::Path(p) => p.span,
    Type::Pointer(p) => p.span,
    Type::Optional(o) => o.span,
    Type::Array(a) => a.span,
    Type::Tuple(t) => t.span,
    Type::Function(f) => f.span,
    Type::Unit(u) => u.span,
    Type::Never(n) => n.span,
    Type::Invalid => Span::default(),
  }
}
