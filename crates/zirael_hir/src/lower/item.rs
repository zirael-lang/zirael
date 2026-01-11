use crate::generics::{GenericParam, GenericParamKind, Generics, TypeBound};
use crate::item::{
  Const, Enum, Field, Function, Method, Param, ParamKind, SelfKind, Struct,
  Variant, VariantField, VariantKind,
};
use crate::lower::context::LoweringContext;
use crate::ty::PathSegment;
use zirael_parser::ast::ProgramNode;
use zirael_parser::ast::items::{
  ConstItem, EnumItem, FunctionItem, Item as AstItem, ItemKind, MethodItem,
  StructField, StructItem, StructMember, Variant as AstVariant,
  VariantPayload as AstVariantPayload, Visibility,
};
use zirael_parser::ast::params;
use zirael_parser::ast::{
  GenericParam as AstGenericParam, GenericParams as AstGenericParams,
  TypeBound as AstTypeBound, VariantField as AstVariantField,
};
use zirael_resolver::DefId;
use zirael_source::span::Span;
use zirael_utils::prelude::Identifier;

impl LoweringContext<'_> {
  pub fn lower_module(&mut self, node: &ProgramNode) {
    for item in &node.items {
      self.lower_item(item);
    }
    self.finalize_module();
  }

  fn lower_item(&mut self, item: &AstItem) {
    match &item.kind {
      ItemKind::Function(func) => {
        self.lower_function(func, item.visibility);
      }
      ItemKind::Struct(s) => {
        self.lower_struct(s, item.visibility);
      }
      ItemKind::Enum(e) => {
        self.lower_enum(e, item.visibility);
      }
      ItemKind::Const(c) => {
        self.lower_const(c, item.visibility);
      }
      ItemKind::Mod(_m) => todo!(),
    }
  }

  fn lower_function(&mut self, func: &FunctionItem, visibility: Visibility) {
    let Some(def_id) = self.get_def_id(func.id) else {
      return;
    };

    self.module_items.push(def_id);

    let hir_func = self.with_owner(def_id, |ctx| {
      let hir_id = ctx.owner_hir_id(def_id);
      let generics = ctx.lower_generics(&func.generics);
      let params = func.params.iter().map(|p| ctx.lower_param(p)).collect();
      let return_type = ctx.lower_type(&func.return_type);
      let body = func.body.as_ref().map(|b| ctx.lower_block(b));

      Function {
        hir_id,
        def_id,
        name: func.name,
        visibility,
        is_const: func.is_const,
        generics,
        params,
        return_type,
        body,
        span: func.span,
      }
    });

    self.hir.functions.insert(def_id, hir_func);
  }

  fn lower_struct(&mut self, s: &StructItem, visibility: Visibility) {
    let Some(def_id) = self.get_def_id(s.id) else {
      return;
    };

    self.module_items.push(def_id);

    let hir_struct = self.with_owner(def_id, |ctx| {
      let hir_id = ctx.owner_hir_id(def_id);
      let generics = ctx.lower_generics(&s.generics);

      let mut fields = Vec::new();
      let mut methods = Vec::new();

      for member in &s.members {
        match member {
          StructMember::Field(f) => {
            fields.push(ctx.lower_field(f));
          }
          StructMember::Method(m) => {
            methods.push(ctx.lower_method(m));
          }
        }
      }

      Struct {
        hir_id,
        def_id,
        name: s.name,
        visibility,
        generics,
        fields,
        methods,
        span: s.span,
      }
    });

    self.hir.structs.insert(def_id, hir_struct);
  }

  /// Lower a struct field.
  fn lower_field(&mut self, f: &StructField) -> Field {
    Field {
      hir_id: self.next_hir_id(),
      name: f.name,
      visibility: f.visibility,
      ty: self.lower_type(&f.ty),
      span: f.span,
    }
  }

  fn lower_method(&mut self, m: &MethodItem) -> Method {
    let def_id = self.get_def_id(m.id);

    Method {
      hir_id: self.next_hir_id(),
      def_id: def_id.unwrap_or(DefId(0)), // TODO: handle missing def_id
      name: m.name,
      visibility: m.visibility,
      params: m.params.iter().map(|p| self.lower_param(p)).collect(),
      return_type: m.return_type.as_ref().map(|t| self.lower_type(t)),
      body: self.lower_block(&m.body),
      span: m.span,
    }
  }

  fn lower_enum(&mut self, e: &EnumItem, visibility: Visibility) {
    let Some(def_id) = self.get_def_id(e.id) else {
      return;
    };

    self.module_items.push(def_id);

    let hir_enum = self.with_owner(def_id, |ctx| {
      let hir_id = ctx.owner_hir_id(def_id);
      let generics = ctx.lower_generics(&e.generics);
      let variants = e.variants.iter().map(|v| ctx.lower_variant(v)).collect();

      Enum {
        hir_id,
        def_id,
        name: e.name,
        visibility,
        generics,
        variants,
        span: e.span,
      }
    });

    self.hir.enums.insert(def_id, hir_enum);
  }

  fn lower_variant(&mut self, v: &AstVariant) -> Variant {
    let def_id = self.get_def_id(v.id);

    let kind = match &v.payload {
      None => VariantKind::Unit,
      Some(AstVariantPayload::Tuple(fields)) => VariantKind::Tuple(
        fields.iter().map(|f| self.lower_variant_field(f)).collect(),
      ),
      Some(AstVariantPayload::Discriminant(expr)) => {
        VariantKind::Discriminant(self.lower_expr(expr))
      }
    };

    Variant {
      hir_id: self.next_hir_id(),
      def_id: def_id.unwrap_or(zirael_resolver::DefId(0)),
      name: v.name,
      kind,
      span: v.span,
    }
  }

  fn lower_variant_field(&mut self, f: &AstVariantField) -> VariantField {
    todo!()
  }

  fn lower_const(&mut self, c: &ConstItem, visibility: Visibility) {
    let Some(def_id) = self.get_def_id(c.id) else {
      return;
    };

    self.module_items.push(def_id);

    let hir_const = self.with_owner(def_id, |ctx| {
      let hir_id = ctx.owner_hir_id(def_id);

      Const {
        hir_id,
        def_id,
        name: c.name,
        visibility,
        ty: ctx.lower_type(&c.ty),
        value: ctx.lower_expr(&c.value),
        span: c.span,
      }
    });

    self.hir.consts.insert(def_id, hir_const);
  }

  /// Lower a parameter.
  fn lower_param(&mut self, p: &params::Param) -> Param {
    let (kind, span, id) = match p {
      params::Param::Regular(r) => {
        let def_id = self.get_def_id(r.id);
        (
          ParamKind::Regular {
            name: r.name,
            ty: self.lower_type(&r.ty),
            default: r.default.as_ref().map(|e| self.lower_expr(e)),
          },
          r.span,
          def_id,
        )
      }
      params::Param::SelfParam(s) => {
        let kind = match s.kind {
          params::SelfKind::Value => SelfKind::Value,
          params::SelfKind::Mut => SelfKind::Mut,
          params::SelfKind::Ptr => SelfKind::Ptr,
          params::SelfKind::PtrMut => SelfKind::PtrMut,
        };
        (ParamKind::SelfParam { kind }, s.span, self.get_def_id(s.id))
      }
      params::Param::Variadic(v) => (
        ParamKind::Variadic {
          name: v.name,
          ty: self.lower_type(&v.ty),
        },
        v.span,
        self.get_def_id(v.id),
      ),
    };

    Param {
      hir_id: self.next_hir_id(),
      def_id: id.unwrap_or(zirael_resolver::DefId(0)),
      kind,
      span,
    }
  }

  pub fn lower_generics(
    &mut self,
    generics: &Option<AstGenericParams>,
  ) -> Generics {
    let Some(g) = generics else {
      return Generics::empty();
    };

    Generics {
      params: g
        .params
        .iter()
        .map(|p| self.lower_generic_param(p))
        .collect(),
      span: g.span,
    }
  }

  fn lower_generic_param(&mut self, p: &AstGenericParam) -> GenericParam {
    let def_id = self.get_def_id(p.id);

    GenericParam {
      hir_id: self.next_hir_id(),
      def_id: def_id.unwrap_or(DefId(0)),
      name: p.name,
      kind: GenericParamKind::Type {
        bounds: p.bounds.iter().map(|b| self.lower_type_bound(b)).collect(),
      },
      span: p.span,
    }
  }

  fn lower_type_bound(&mut self, b: &AstTypeBound) -> TypeBound {
    let def_id = self.get_def_id(b.path.id);

    TypeBound {
      hir_id: self.next_hir_id(),
      def_id: def_id.unwrap_or(DefId(0)),
      path: vec![PathSegment {
        name: Identifier::new(
          &b.path
            .path
            .segments
            .iter()
            .map(|s| s.text())
            .collect::<Vec<_>>()
            .join("::"),
          Span::dummy(), // TODO: correct span
        ),
        args: b
          .path
          .args
          .as_ref()
          .map(|args| args.iter().map(|t| self.lower_type(t)).collect())
          .unwrap_or_default(),
      }],
      span: b.span,
    }
  }
}
