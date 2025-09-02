use crate::ir::{
  HirLowering, IrBlock, IrExpr, IrExprKind, IrField, IrFunction, IrItem, IrItemKind, IrModule,
  IrParam, IrStmt, IrStruct, IrVariant, IrVariantData,
};
use itertools::Itertools as _;
use std::{
  collections::{HashMap, HashSet},
  hash::{DefaultHasher, Hash as _},
};
use zirael_parser::{
  FunctionSignature, GenericParameter, MonomorphizationId, ScopeType, Symbol, SymbolKind,
  SymbolRelationNode, Type, monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_type_checker::monomorphization::{MonomorphizationData, MonomorphizationEntry};
use zirael_utils::prelude::{Identifier, debug, get_or_intern, resolve};

impl<'reports> HirLowering<'reports> {
  pub fn process_monomorphization_entries(&mut self, module: &mut IrModule) {
    // TODO: find a way to do that without cloning the entries
    let entries: Vec<_> = self.mono_table.entries.clone().into_iter().collect();

    let (struct_entries, func_entries): (Vec<_>, Vec<_>) =
      entries.into_iter().partition(|(_, entry)| {
        let symbol = self.symbol_table.get_symbol_unchecked(&entry.original_id);
        matches!(
          symbol.kind,
          SymbolKind::Struct { .. } | SymbolKind::Enum { .. } | SymbolKind::EnumVariant { .. }
        )
      });

    self.push_scope(ScopeType::Module(module.id));
    for (id, entry) in struct_entries {
      if let Some(monomorphized_item) = self.create_monomorphized_item(&id, &entry, module) {
        module.mono_items.push(monomorphized_item);
      }
    }

    for (id, entry) in func_entries {
      if let Some(monomorphized_item) = self.create_monomorphized_item(&id, &entry, module) {
        module.mono_items.push(monomorphized_item);
      }
    }
    self.pop_scope();
  }

  fn get_type_arguments(
    &mut self,
    generics: &Vec<GenericParameter>,
    concrete_types: &HashMap<Identifier, Type>,
  ) -> Option<Vec<Type>> {
    let all_generics_mapped = generics.iter().all(|g| concrete_types.contains_key(&g.name));

    if !all_generics_mapped {
      return None;
    }

    generics
      .iter()
      .map(|param| concrete_types.get(&param.name).cloned())
      .collect::<Option<Vec<_>>>()
  }

  fn create_monomorphized_item(
    &mut self,
    id: &MonomorphizationId,
    entry: &MonomorphizationEntry,
    module: &IrModule,
  ) -> Option<IrItem> {
    let original_id = entry.original_id;
    let concrete_types = &entry.concrete_types;

    let original_item = module.items.iter().find(|item| item.sym_id == original_id)?;
    let original_symbol = self.symbol_table.get_symbol_unchecked(&original_id);

    self.current_mono_id = Some(*id);
    match (&original_symbol.kind, &original_item.kind) {
      (SymbolKind::Function { signature: sig, .. }, IrItemKind::Function(func)) => {
        self.push_scope(ScopeType::Function(func.id));
        let signature = if let Some(data) = &entry.data
          && let MonomorphizationData::Signature(signature) = data
        {
          signature
        } else {
          sig
        };

        let mangled_name = self.get_monomorphized_name(*id);
        let monomorphized_function =
          self.monomorphize_function(mangled_name.clone(), signature, func, concrete_types);
        self.pop_scope();

        Some(IrItem {
          name: mangled_name,
          kind: IrItemKind::Function(monomorphized_function),
          sym_id: original_id,
          mono_id: Some(*id),
        })
      }
      (SymbolKind::Struct { generics, .. }, IrItemKind::Struct(struct_def)) => {
        self.push_scope(ScopeType::Struct(struct_def.id));
        let Some(type_arguments) = self.get_type_arguments(generics, concrete_types) else {
          return None;
        };

        let mangled_name = self.mangle_monomorphized_symbol(original_id, *id, &type_arguments);
        let monomorphized_struct =
          self.monomorphize_struct(mangled_name.clone(), struct_def, concrete_types);

        self.pop_scope();
        Some(IrItem {
          name: mangled_name,
          kind: IrItemKind::Struct(monomorphized_struct),
          sym_id: original_id,
          mono_id: Some(*id),
        })
      }
      (SymbolKind::EnumVariant { parent_enum, data: _ }, IrItemKind::EnumVariant(variant)) => {
        let parent_enum = self.symbol_table.get_symbol_unchecked(parent_enum);
        let SymbolKind::Enum { generics, id, .. } = &parent_enum.kind else { unreachable!() };
        self.push_scope(ScopeType::Enum(*id));

        let Some(type_arguments) = self.get_type_arguments(generics, concrete_types) else {
          return None;
        };

        let mangled_name = self.mangle_monomorphized_symbol(original_id, *id, &type_arguments);
        let mono_enum = self.monomorphize_enum(mangled_name.clone(), variant, concrete_types);
        self.pop_scope();

        Some(IrItem {
          name: mangled_name,
          kind: IrItemKind::EnumVariant(mono_enum),
          sym_id: original_id,
          mono_id: Some(*id),
        })
      }
      _ => {
        debug!(
          "missing implementation for monomorphization of {original_symbol:?} {original_item:?}"
        );
        None
      }
    }
  }

  fn monomorphize_enum(
    &mut self,
    name: String,
    variant: &IrVariant,
    type_map: &HashMap<Identifier, Type>,
  ) -> IrVariant {
    IrVariant {
      symbol_id: variant.symbol_id,
      name,
      data: match &variant.data {
        IrVariantData::Struct(fields) => {
          let fields = &mut fields.clone();
          for field in fields.iter_mut() {
            field.ty = self.substitute_type(&field.ty, type_map);
          }

          IrVariantData::Struct(fields.clone())
        }
        _ => IrVariantData::Unit,
      },
    }
  }

  fn monomorphize_struct(
    &mut self,
    name: String,
    struct_def: &IrStruct,
    type_map: &HashMap<Identifier, Type>,
  ) -> IrStruct {
    let fields = struct_def
      .fields
      .iter()
      .map(|field| {
        let substituted_ty = self.substitute_type(&field.ty, type_map);
        IrField { name: field.name.clone(), ty: substituted_ty }
      })
      .collect_vec();
    IrStruct { name, fields, id: struct_def.id }
  }

  fn monomorphize_function(
    &mut self,
    name: String,
    signature: &FunctionSignature,
    original: &IrFunction,
    type_map: &HashMap<Identifier, Type>,
  ) -> IrFunction {
    let parameters = signature
      .parameters
      .iter()
      .map(|param| {
        let substituted_ty = self.substitute_type(&param.ty, type_map);
        IrParam { name: self.mangle_symbol(param.symbol_id.unwrap()), ty: substituted_ty }
      })
      .collect();

    let mut return_type = self.substitute_type(&signature.return_type, type_map);
    return_type = self.lower_type(return_type.clone());

    let body = original.body.as_ref().map(|body| self.monomorphize_block(body, type_map));

    IrFunction {
      name,
      parameters,
      return_type,
      body,
      is_async: original.is_async,
      is_const: original.is_const,
      is_extern: original.is_extern,
      abi: original.abi.clone(),
      id: original.id,
    }
  }

  fn monomorphize_block(
    &mut self,
    original: &IrBlock,
    type_map: &HashMap<Identifier, Type>,
  ) -> IrBlock {
    let stmts = original.stmts.iter().map(|stmt| self.monomorphize_stmt(stmt, type_map)).collect();

    IrBlock { stmts }
  }

  fn monomorphize_stmt(
    &mut self,
    original: &IrStmt,
    type_map: &HashMap<Identifier, Type>,
  ) -> IrStmt {
    match original {
      IrStmt::Var(name, init) => IrStmt::Var(name.clone(), self.monomorphize_expr(init, type_map)),
      IrStmt::Expr(expr) => IrStmt::Expr(self.monomorphize_expr(expr, type_map)),
      IrStmt::Return(expr) => {
        if let Some(expr) = expr {
          IrStmt::Return(Some(self.monomorphize_expr(expr, type_map)))
        } else {
          IrStmt::Return(None)
        }
      }
    }
  }

  pub fn monomorphize_expr(
    &mut self,
    original: &IrExpr,
    type_map: &HashMap<Identifier, Type>,
  ) -> IrExpr {
    let ty = self.substitute_type(&original.ty, type_map);

    let kind = match &original.kind {
      IrExprKind::Symbol(sym) => IrExprKind::Symbol(sym.clone()),

      IrExprKind::Block(block) => IrExprKind::Block(self.monomorphize_block(block, type_map)),

      IrExprKind::Literal(lit) => IrExprKind::Literal(lit.clone()),

      IrExprKind::Call(func, args) => {
        let mono_args = args.iter().map(|arg| self.monomorphize_expr(arg, type_map)).collect();

        IrExprKind::Call(func.clone(), mono_args)
      }

      IrExprKind::CCall(func, args) => {
        let mono_args = args.iter().map(|arg| self.monomorphize_expr(arg, type_map)).collect();

        IrExprKind::CCall(func.clone(), mono_args)
      }

      IrExprKind::Assign(left, right) => IrExprKind::Assign(
        Box::new(self.monomorphize_expr(left, type_map)),
        Box::new(self.monomorphize_expr(right, type_map)),
      ),

      IrExprKind::Unary(op, expr) => {
        IrExprKind::Unary(op.clone(), Box::new(self.monomorphize_expr(expr, type_map)))
      }

      IrExprKind::Binary(left, op, right) => IrExprKind::Binary(
        Box::new(self.monomorphize_expr(left, type_map)),
        op.clone(),
        Box::new(self.monomorphize_expr(right, type_map)),
      ),

      IrExprKind::StructInit(struct_name, fields) => {
        let mut new_fields = HashMap::new();
        for (field_name, field_expr) in fields {
          let new_field_expr = self.monomorphize_expr(field_expr, type_map);
          new_fields.insert(field_name.clone(), new_field_expr);
        }

        IrExprKind::StructInit(struct_name.clone(), new_fields)
      }

      IrExprKind::Type(ty) => IrExprKind::Type(self.substitute_type(ty, type_map)),

      IrExprKind::FieldAccess(fields) => {
        let mono_fields = fields.clone();
        IrExprKind::FieldAccess(mono_fields)
      }

      IrExprKind::Ternary(cond, tr, fl) => IrExprKind::Ternary(
        Box::new(self.monomorphize_expr(cond, type_map)),
        Box::new(self.monomorphize_expr(tr, type_map)),
        Box::new(self.monomorphize_expr(fl, type_map)),
      ),

      IrExprKind::If { condition, then_branch, else_branch } => IrExprKind::If {
        condition: Box::new(self.monomorphize_expr(condition, type_map)),
        then_branch: Box::new(self.monomorphize_expr(then_branch, type_map)),
        else_branch: else_branch.as_ref().map(|e| Box::new(self.monomorphize_expr(e, type_map))),
      },

      IrExprKind::Match { scrutinee, arms } => IrExprKind::Match {
        scrutinee: Box::new(self.monomorphize_expr(scrutinee, type_map)),
        arms: arms
          .iter()
          .map(|arm| crate::ir::IrMatchArm {
            pattern: arm.pattern.clone(),
            body: self.monomorphize_expr(&arm.body, type_map),
          })
          .collect(),
      },
    };

    IrExpr { ty, kind }
  }

  fn substitute_type(&mut self, ty: &Type, type_map: &HashMap<Identifier, Type>) -> Type {
    self.substitute_type_with_visited(ty, type_map, &mut HashSet::new())
  }

  fn substitute_type_with_visited(
    &mut self,
    ty: &Type,
    type_map: &HashMap<Identifier, Type>,
    visited: &mut HashSet<Identifier>,
  ) -> Type {
    let new_ty = match ty {
      Type::Variable { id: _, name } => {
        if let Some(concrete_type) = type_map.get(name) {
          concrete_type.clone()
        } else {
          ty.clone()
        }
      }

      Type::Pointer(inner) => {
        Type::Pointer(Box::new(self.substitute_type_with_visited(inner, type_map, visited)))
      }

      Type::Reference(inner) => {
        Type::Reference(Box::new(self.substitute_type_with_visited(inner, type_map, visited)))
      }

      Type::Array(inner, size) => {
        Type::Array(Box::new(self.substitute_type_with_visited(inner, type_map, visited)), *size)
      }

      Type::Named { name, generics } => {
        if type_map.contains_key(name) {
          if visited.contains(name) {
            return ty.clone();
          }

          visited.insert(*name);
          let result =
            self.substitute_type_with_visited(type_map.get(name).unwrap(), type_map, visited);
          visited.remove(name);
          return result;
        }

        if generics.is_empty() {
          ty.clone()
        } else {
          let substituted_generics = generics
            .iter()
            .map(|g| self.substitute_type_with_visited(g, type_map, visited))
            .collect();

          Type::Named { name: *name, generics: substituted_generics }
        }
      }

      Type::Function { params, return_type } => {
        let substituted_params =
          params.iter().map(|p| self.substitute_type_with_visited(p, type_map, visited)).collect();

        let substituted_return =
          Box::new(self.substitute_type_with_visited(return_type, type_map, visited));

        Type::Function { params: substituted_params, return_type: substituted_return }
      }

      Type::MonomorphizedSymbol(sym) => self.handle_monomorphized_symbol(sym, true),

      _ => ty.clone(),
    };

    self.lower_type(new_ty)
  }

  pub fn handle_monomorphized_symbol(
    &mut self,
    sym: &MonomorphizedSymbol,
    add_struct: bool,
  ) -> Type {
    let name = self.get_monomorphized_name(sym.id);
    let sym_id = self.mono_table.entries[&sym.id].original_id;
    let original_symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
    self.new_relation(SymbolRelationNode::Monomorphization(sym.id));

    if let SymbolKind::Struct { .. } = &original_symbol.kind {
      Type::Named {
        name: get_or_intern(&if add_struct { format!("struct {name}") } else { name }),
        generics: vec![],
      }
    } else if let SymbolKind::EnumVariant { parent_enum, .. } = &original_symbol.kind {
      Type::Named { name: get_or_intern(&self.mangle_symbol(*parent_enum)), generics: vec![] }
    } else if let SymbolKind::Enum { .. } = &original_symbol.kind {
      Type::Named {
        name: get_or_intern(&self.mangle_symbol(original_symbol.canonical_symbol)),
        generics: vec![],
      }
    } else {
      Type::Named { name: get_or_intern(&name), generics: vec![] }
    }
  }

  pub(crate) fn hash_type(&mut self, ty: &Type, hasher: &mut DefaultHasher) {
    match ty {
      Type::String => "string".hash(hasher),
      Type::Char => "char".hash(hasher),
      Type::Int => "int".hash(hasher),
      Type::Uint => "uint".hash(hasher),
      Type::Float => "float".hash(hasher),
      Type::Bool => "bool".hash(hasher),
      Type::Void => "void".hash(hasher),

      Type::Pointer(inner) => {
        "ptr".hash(hasher);
        self.hash_type(inner, hasher);
      }

      Type::Reference(inner) => {
        "ref".hash(hasher);
        self.hash_type(inner, hasher);
      }

      Type::Array(inner, size) => {
        "array".hash(hasher);
        if let Some(s) = size {
          s.hash(hasher);
        }
        self.hash_type(inner, hasher);
      }

      Type::Named { name, generics } => {
        resolve(name).hash(hasher);
        for g in generics {
          self.hash_type(g, hasher);
        }
      }

      Type::Function { params, return_type } => {
        "function".hash(hasher);
        for p in params {
          self.hash_type(p, hasher);
        }
        self.hash_type(return_type, hasher);
      }

      Type::Variable { id, name } => {
        id.hash(hasher);
        resolve(name).hash(hasher);
      }

      _ => "unknown".hash(hasher),
    }
  }

  pub fn get_monomorphized_name(&mut self, mono_id: MonomorphizationId) -> String {
    if let Some(entry) = self.mono_table.entries.get(&mono_id) {
      let original_symbol = self.symbol_table.get_symbol_unchecked(&entry.original_id);
      let generics = self.symbol_table.get_generics_for_symbol(&original_symbol);

      if let Some(generics) = generics {
        let type_arguments: Vec<Type> = generics
          .iter()
          .filter_map(|param| entry.concrete_types.get(&param.name))
          .cloned()
          .collect();

        return self.mangle_monomorphized_symbol(entry.original_id, mono_id, &type_arguments);
      } else {
        debug!("no generics for {original_symbol:?}");
        return self.mangle_symbol(entry.original_id);
      }
    }

    panic!("Monomorphization ID {mono_id:?} not found")
  }
}
