use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{Expr, ExprKind, GenericParameter, Stmt, StmtKind, Symbol, SymbolKind, Type};
use zirael_utils::prelude::Identifier;

#[derive(Debug, Clone)]
pub enum UnificationResult {
    Identical(Type),
    Unified(Type),
    Incompatible,
}

impl<'reports> TypeInference<'reports> {
    pub fn unify_types(&mut self, left_ty: &Type, right_ty: &Type) -> UnificationResult {
        if self.eq(left_ty, right_ty) {
            return UnificationResult::Identical(left_ty.clone());
        }

        let left_info = self.extract_generic_info(left_ty);
        let right_info = self.extract_generic_info(right_ty);

        let (left_name, left_generics) = match left_info {
            Some(info) => info,
            None => return UnificationResult::Incompatible,
        };

        let (right_name, right_generics) = match right_info {
            Some(info) => info,
            None => return UnificationResult::Incompatible,
        };

        if left_name != right_name || left_generics.len() != right_generics.len() {
            return UnificationResult::Incompatible;
        }

        let mut unified_generics = Vec::new();
        let mut substitutions: HashMap<Identifier, Type> = HashMap::new();

        for (left_gen, right_gen) in left_generics.iter().zip(right_generics.iter()) {
            match self.unify_generic_params(left_gen, right_gen, &mut substitutions) {
                Some(unified_param) => unified_generics.push(unified_param),
                None => return UnificationResult::Incompatible,
            }
        }

        for param in &mut unified_generics {
            self.apply_substitutions(param, &substitutions);
        }

        let unified_type = Type::Named { name: left_name, generics: unified_generics };

        UnificationResult::Unified(unified_type)
    }

    fn extract_generic_info<'a>(&self, ty: &'a Type) -> Option<(Identifier, &'a [Type])> {
        match ty {
            Type::Named { name, generics } => Some((*name, generics.as_slice())),
            Type::MonomorphizedSymbol(mono_sym) => {
                if let Type::Named { name, generics } = &*mono_sym.display_ty {
                    Some((*name, generics.as_slice()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn unify_generic_params(
        &mut self,
        left: &Type,
        right: &Type,
        substitutions: &mut HashMap<Identifier, Type>,
    ) -> Option<Type> {
        match (left, right) {
            (Type::Inferred, concrete) => Some(concrete.clone()),
            (concrete, Type::Inferred) => Some(concrete.clone()),
            (a, b) if self.eq(a, b) => Some(a.clone()),
            (Type::Variable { id: _, name: var_name }, concrete) => {
                if let Some(existing) = substitutions.get(var_name) {
                    if self.eq(existing, concrete) { Some(concrete.clone()) } else { None }
                } else {
                    substitutions.insert(*var_name, concrete.clone());
                    Some(concrete.clone())
                }
            }
            (concrete, Type::Variable { id: _, name: var_name }) => {
                if let Some(existing) = substitutions.get(var_name) {
                    if self.eq(existing, concrete) { Some(concrete.clone()) } else { None }
                } else {
                    substitutions.insert(*var_name, concrete.clone());
                    Some(concrete.clone())
                }
            }
            _ => {
                if self.eq(left, right) {
                    Some(left.clone())
                } else {
                    None
                }
            }
        }
    }

    fn apply_substitutions(&self, ty: &mut Type, substitutions: &HashMap<Identifier, Type>) {
        match ty {
            Type::Variable { id: _, name } => {
                if let Some(substitution) = substitutions.get(name) {
                    *ty = substitution.clone();
                }
            }
            Type::Named { generics, .. } => {
                for generic in generics {
                    self.apply_substitutions(generic, substitutions);
                }
            }
            Type::MonomorphizedSymbol(mono_sym) => {
                if let Type::Named { generics, .. } = &mut *mono_sym.display_ty {
                    for generic in generics {
                        self.apply_substitutions(generic, substitutions);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn update_monomorphization(&mut self, expr: &mut Expr, resolved_ty: &Type) {
        if let Type::Named { generics, .. } = resolved_ty {
            if !generics.is_empty() && generics.iter().any(|g| !matches!(g, Type::Inferred)) {
                match &mut expr.kind {
                    ExprKind::Call { call_info: Some(call_info), .. }
                    | ExprKind::StaticCall { call_info: Some(call_info), .. } => {
                        if let Some(mono_id) = call_info.monomorphized_id {
                            let (original_id, should_update) = {
                                let entry = match self.mono_table.entries.get(&mono_id) {
                                    Some(entry) => entry,
                                    None => return,
                                };
                                let has_inferred = entry
                                    .concrete_types
                                    .values()
                                    .any(|ty| matches!(ty, Type::Inferred));
                                (entry.original_id, has_inferred)
                            };

                            if should_update {
                                let symbol = self.symbol_table.get_symbol_unchecked(&original_id);
                                let generics = self.get_generics_for_symbol(&symbol);

                                if let (Some(generics), Some(entry)) =
                                    (generics, self.mono_table.entries.get_mut(&mono_id))
                                {
                                    if let Type::Named { generics: resolved_generics, .. } =
                                        resolved_ty
                                    {
                                        for (param, resolved_type) in
                                            generics.iter().zip(resolved_generics.iter())
                                        {
                                            if !matches!(resolved_type, Type::Inferred) {
                                                entry
                                                    .concrete_types
                                                    .insert(param.name, resolved_type.clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    pub fn update_expr_recursively(&mut self, expr: &mut Expr, resolved_ty: &Type) {
        self.update_monomorphization(expr, resolved_ty);

        match &mut expr.kind {
            ExprKind::Call { args, .. } | ExprKind::StaticCall { args, .. } => {
                self.update_exprs(args);
            }
            ExprKind::MethodCall { chain, args, .. } => {
                self.update_exprs(chain);
                self.update_exprs(args);
            }
            ExprKind::Binary { left, right, .. }
            | ExprKind::Assign(left, right)
            | ExprKind::AssignOp(left, _, right)
            | ExprKind::IndexAccess(left, right) => {
                self.update_expr_with_own_type(left);
                self.update_expr_with_own_type(right);
            }
            ExprKind::Unary(_, inner) | ExprKind::Paren(inner) => {
                self.update_expr_with_own_type(inner);
            }
            ExprKind::Ternary { condition, true_expr, false_expr } => {
                self.update_expr_with_own_type(condition);
                self.update_expr_recursively(true_expr, resolved_ty);
                self.update_expr_recursively(false_expr, resolved_ty);
            }
            ExprKind::FieldAccess(exprs) => {
                self.update_exprs(exprs);
            }
            ExprKind::StructInit { name, fields, .. } => {
                self.update_expr_with_own_type(name);
                for field_expr in fields.values_mut() {
                    self.update_expr_with_own_type(field_expr);
                }
            }
            ExprKind::Block(stmts) => {
                for stmt in stmts {
                    self.update_stmt_recursively(stmt);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.update_expr_with_own_type(scrutinee);
                for arm in arms {
                    self.update_expr_recursively(&mut arm.body, resolved_ty);
                }
            }
            ExprKind::Literal(_) | ExprKind::Identifier(_, _) | ExprKind::CouldntParse(_) => {}
        }
    }

    fn update_expr_with_own_type(&mut self, expr: &mut Expr) {
        let expr_ty = expr.ty.clone();
        if self.has_generic_types(&expr_ty) {
            self.update_expr_recursively(expr, &expr_ty);
        }
    }

    fn update_exprs(&mut self, exprs: &mut [Expr]) {
        for expr in exprs {
            self.update_expr_with_own_type(expr);
        }
    }

    fn has_generic_types(&self, ty: &Type) -> bool {
        matches!(ty, Type::Named { generics, .. } if !generics.is_empty())
    }

    fn update_stmt_recursively(&mut self, stmt: &mut Stmt) {
        match &mut stmt.0 {
            StmtKind::Expr(expr) => {
                self.update_expr_with_own_type(expr);
            }
            StmtKind::Var(var_decl) => {
                self.update_expr_with_own_type(&mut var_decl.value);
            }
            StmtKind::Return(return_stmt) => {
                if let Some(value) = &mut return_stmt.value {
                    self.update_expr_with_own_type(value);
                }
            }
        }
    }

    fn get_generics_for_symbol(&self, symbol: &Symbol) -> Option<Vec<GenericParameter>> {
        match &symbol.kind {
            SymbolKind::Function { signature, .. } => {
                let mut generics = vec![];

                if let Some(parent_struct) =
                    self.symbol_table.is_a_child_of_symbol(symbol.canonical_symbol)
                {
                    let parent_struct = self.symbol_table.get_symbol_unchecked(&parent_struct);
                    if let SymbolKind::Struct { generics: gens, .. }
                    | SymbolKind::Enum { generics: gens, .. } = &parent_struct.kind
                    {
                        generics.extend(gens.clone());
                    }
                }
                generics.extend(signature.generics.clone());
                Some(generics)
            }
            SymbolKind::Struct { generics, .. } | SymbolKind::Enum { generics, .. } => {
                Some(generics.clone())
            }
            SymbolKind::EnumVariant { parent_enum, .. } => {
                let sym = self.symbol_table.get_symbol_unchecked(parent_enum);
                self.get_generics_for_symbol(&sym)
            }
            _ => None,
        }
    }
}
