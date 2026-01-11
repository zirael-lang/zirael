use crate::lower::context::LoweringContext;
use crate::pat::{FieldPat, Pat, PatKind};
use zirael_parser::ast::expressions::{Pattern, StructPatternField};

impl<'a> LoweringContext<'a> {
  pub fn lower_ast_pattern(&mut self, pat: &Pattern) -> Pat {
    let (kind, span) = match pat {
      Pattern::Wildcard(w) => (PatKind::Wild, w.span),

      Pattern::Ident(ident) => {
        (
          PatKind::Binding {
            def_id: zirael_resolver::DefId(0), // TODO: get from resolver
            name: *ident,
            is_mut: false, // TODO: handle `mut x` patterns
            subpat: None,
          },
          *ident.span(),
        )
      }

      Pattern::Literal(lit) => {
        let span = self.get_literal_span(lit);
        (PatKind::Literal(self.lower_literal(lit)), span)
      }

      Pattern::Tuple(tup) => (
        PatKind::Tuple(
          tup
            .patterns
            .iter()
            .map(|p| self.lower_ast_pattern(p))
            .collect(),
        ),
        tup.span,
      ),

      Pattern::Struct(s) => {
        let def_id = self.get_def_id(s.path.id);

        let fields = s
          .fields
          .iter()
          .map(|f| match f {
            StructPatternField::Full { name, pattern } => FieldPat {
              hir_id: self.next_hir_id(),
              name: *name,
              pat: self.lower_ast_pattern(pattern),
              span: *name.span(),
            },
            StructPatternField::Shorthand(ident) => {
              let hir_id = self.next_hir_id();
              FieldPat {
                hir_id,
                name: *ident,
                pat: Pat {
                  hir_id: self.next_hir_id(),
                  kind: PatKind::Binding {
                    def_id: zirael_resolver::DefId(0),
                    name: *ident,
                    is_mut: false,
                    subpat: None,
                  },
                  span: *ident.span(),
                },
                span: *ident.span(),
              }
            }
          })
          .collect();

        (
          PatKind::Struct {
            def_id: def_id.unwrap_or(zirael_resolver::DefId(0)),
            fields,
            rest: false, // TODO: handle `..` rest pattern
          },
          s.span,
        )
      }

      Pattern::Enum(e) => {
        let def_id = self.get_def_id(e.path.id);

        (
          PatKind::TupleStruct {
            def_id: def_id.unwrap_or(zirael_resolver::DefId(0)),
            pats: e
              .patterns
              .iter()
              .map(|p| self.lower_ast_pattern(p))
              .collect(),
          },
          e.span,
        )
      }
    };

    Pat {
      hir_id: self.next_hir_id(),
      kind,
      span,
    }
  }
}
