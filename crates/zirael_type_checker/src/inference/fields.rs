use crate::{MonomorphizationData, TypeInference, inference::ctx::TypeInferenceContext};
use std::{fmt::format, rc::Rc};
use zirael_parser::{AstWalker, CallInfo, Expr, ExprKind, SymbolId, SymbolKind, Type};
use zirael_utils::prelude::{Colorize, Span, get_or_intern, resolve};

impl<'reports> TypeInference<'reports> {
    fn traverse_chain<F>(&mut self, chain: &mut [Expr], mut validator: F) -> Type
    where
        F: FnMut(&mut Self, &Type, &str, Span) -> Type,
    {
        if chain.is_empty() {
            return Type::Error;
        }

        let base_type = match self.get_base_type(&chain[0]) {
            Ok(ty) => ty,
            Err(_) => return Type::Error,
        };

        chain[0].ty = base_type.clone();

        self.traverse_chain_from_type(base_type, &mut chain[1..], validator)
    }

    fn traverse_chain_from_type<F>(
        &mut self,
        mut current_type: Type,
        chain: &mut [Expr],
        mut validator: F,
    ) -> Type
    where
        F: FnMut(&mut Self, &Type, &str, Span) -> Type,
    {
        for expr in chain {
            let (name, _) = match expr.as_identifier() {
                Some(id) => id,
                None => {
                    self.non_struct_type(expr.span.clone(), file!(), line!());
                    return Type::Error;
                }
            };

            let span = expr.span.clone();
            current_type = validator(self, &current_type, &resolve(&name), span);

            expr.ty = current_type.clone();

            if matches!(current_type, Type::Error) {
                return Type::Error;
            }
        }

        current_type
    }

    fn get_base_type(&mut self, expr: &Expr) -> Result<Type, ()> {
        let base = expr.as_identifier();
        let first_span = expr.span.clone();

        if let Some((_, Some(sym_id))) = base {
            match self.ctx.get_variable(*sym_id) {
                Some(var) => Ok(var.clone()),
                None => Err(()),
            }
        } else {
            self.non_struct_type(first_span, file!(), line!());
            Err(())
        }
    }

    pub fn infer_field_access(&mut self, fields: &mut [Expr]) -> Type {
        self.traverse_chain(fields, |checker, current_type, field_name, field_span| {
            checker.get_field_type(current_type, field_name, field_span)
        })
    }

    pub fn infer_method_call(
        &mut self,
        chain: &mut [Expr],
        args: &mut Vec<Expr>,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        if chain.len() < 2 {
            return Type::Error;
        }

        let (method_expr, receiver_chain) = chain.split_last_mut().unwrap();

        let receiver_type = if receiver_chain.len() == 1 {
            match self.get_base_type(&receiver_chain[0]) {
                Ok(ty) => ty,
                Err(_) => return Type::Error,
            }
        } else {
            self.traverse_chain(receiver_chain, |checker, current_type, field_name, field_span| {
                checker.get_field_type(current_type, field_name, field_span)
            })
        };

        if matches!(receiver_type, Type::Error) {
            return Type::Error;
        }

        let (method_name, _) = match method_expr.as_identifier() {
            Some(id) => id,
            None => {
                self.non_struct_type(method_expr.span.clone(), file!(), line!());
                return Type::Error;
            }
        };

        self.resolve_method_on_type(
            &receiver_type,
            &resolve(&method_name),
            method_expr,
            args,
            method_expr.span.clone(),
            call_info,
        )
    }

    pub fn infer_index_access(&mut self, expr: &mut Box<Expr>, index: &mut Box<Expr>) -> Type {
        let container_type = self.infer_expr(expr);
        let index_type = self.infer_expr(index);

        if matches!(container_type, Type::Error) || matches!(index_type, Type::Error) {
            return Type::Error;
        }

        self.resolve_index_access(&container_type, &index_type, expr.span.clone())
    }

    pub fn infer_static_call(
        &mut self,
        callee: &mut Box<Expr>,
        args: &mut Vec<Expr>,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        self.resolve_static_call(callee, args, call_info)
    }

    fn get_field_type(&mut self, current_type: &Type, field_name: &str, field_span: Span) -> Type {
        let resolved_type = match current_type {
            Type::Reference(inner) => inner.as_ref(),
            other => other,
        };

        let struct_fields = match resolved_type {
            Type::Named { name, .. } => {
                let sym = match self.symbol_table.lookup_symbol(name) {
                    Some(sym) => sym,
                    None => return Type::Error,
                };

                match sym.kind.clone() {
                    SymbolKind::Struct { fields, .. } => fields,
                    _ => {
                        self.non_struct_type(field_span, file!(), line!());
                        return Type::Error;
                    }
                }
            }
            Type::MonomorphizedSymbol(sym) => {
                if let Some(entry) = self.mono_table.get_entry(sym.id) {
                    let sym = self.symbol_table.get_symbol_unchecked(&entry.original_id);
                    match sym.kind.clone() {
                        SymbolKind::Struct { fields: struct_fields, .. } => {
                            if let Some(data) = &entry.data {
                                if let MonomorphizationData::Fields(fields) = data {
                                    fields.clone()
                                } else {
                                    struct_fields
                                }
                            } else {
                                struct_fields
                            }
                        }
                        _ => {
                            self.non_struct_type(field_span, file!(), line!());
                            return Type::Error;
                        }
                    }
                } else {
                    self.non_struct_type(field_span, file!(), line!());
                    return Type::Error;
                }
            }
            _ => {
                self.non_struct_type(field_span, file!(), line!());
                return Type::Error;
            }
        };

        match struct_fields.iter().find(|f| resolve(&f.name) == field_name) {
            Some(field) => field.ty.clone(),
            None => {
                self.field_not_found_error(field_name, current_type, field_span);
                Type::Error
            }
        }
    }

    fn get_methods(&mut self, current_type: &Type) -> Vec<SymbolId> {
        let resolved_type = match current_type {
            Type::Reference(inner) => inner.as_ref(),
            other => other,
        };

        match resolved_type {
            Type::Named { name, .. } => {
                let sym = match self.symbol_table.lookup_symbol(name) {
                    Some(sym) => sym,
                    None => return Vec::new(),
                };

                match sym.kind.clone() {
                    SymbolKind::Struct { methods, .. } => methods,
                    SymbolKind::Enum { .. } => todo!(),
                    _ => Vec::new(),
                }
            }
            Type::MonomorphizedSymbol(sym) => {
                if let Some(entry) = self.mono_table.get_entry(sym.id) {
                    let sym = self.symbol_table.get_symbol_unchecked(&entry.original_id);

                    match sym.kind.clone() {
                        SymbolKind::Struct { methods, .. } => methods,
                        SymbolKind::Enum { .. } => todo!(),
                        _ => Vec::new(),
                    }
                } else {
                    Vec::new()
                }
            }
            _ => Vec::new(),
        }
    }

    fn resolve_method_on_type(
        &mut self,
        receiver_type: &Type,
        method_name: &str,
        callee: &mut Expr,
        args: &mut Vec<Expr>,
        span: Span,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        let methods = self.get_methods(receiver_type);

        let mut found = None;
        for method in methods {
            let sym = self.symbol_table.get_symbol_unchecked(&method);

            if sym.name == get_or_intern(method_name) {
                found = Some(method);
                break;
            }
        }

        if let Some(method_id) = found {
            let sym = self.symbol_table.get_symbol_unchecked(&method_id);

            if let SymbolKind::Function { signature, .. } = &sym.kind {
                if signature.is_static() {
                    self.error(
                        &format!(
                            "cannot call static method {} on type {}",
                            method_name,
                            self.format_type(receiver_type)
                        ),
                        vec![("in this field access".to_string(), span.clone())],
                        vec![format!(
                            "consider using {}::{}(...) instead",
                            self.format_type(receiver_type),
                            method_name
                        )],
                    )
                }

                let (_, sym_id) = match callee.as_identifier_mut() {
                    Some(id) => id,
                    None => {
                        return Type::Error;
                    }
                };
                *sym_id = Some(method_id);

                self.infer_call(callee, args, call_info)
            } else {
                Type::Error
            }
        } else {
            self.error(
                &format!(
                    "no method named {} found on type {}",
                    method_name,
                    self.format_type(receiver_type)
                ),
                vec![("in this field access".to_string(), span)],
                vec![],
            );
            Type::Error
        }
    }

    fn resolve_index_access(
        &mut self,
        container_type: &Type,
        index_type: &Type,
        span: Span,
    ) -> Type {
        todo!("Implement index access type resolution")
    }

    fn resolve_static_call(
        &mut self,
        callee: &mut Box<Expr>,
        args: &mut Vec<Expr>,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        let ExprKind::FieldAccess(fields) = &mut callee.kind else {
            return Type::Error;
        };

        let Some((_, sym_id)) = fields[0].as_identifier_unchecked() else {
            self.non_struct_type(fields[0].span.clone(), file!(), line!());
            return Type::Error;
        };

        let Some((ident, call_id)) = fields[1].as_identifier_mut() else {
            self.error(
                "right now, only one level of static calls are supported",
                vec![("in this field access".to_string(), fields[1].span.clone())],
                vec![],
            );
            return Type::Error;
        };

        let sym = self.symbol_table.get_symbol_unchecked(&sym_id);
        if let SymbolKind::Struct { methods, .. } = &sym.kind {
            let mut found = None;

            for method in methods {
                let sym = self.symbol_table.get_symbol_unchecked(method);

                if sym.name == *ident {
                    found = Some(sym.id);
                    break;
                }
            }

            if let Some(method_id) = found {
                *call_id = Some(method_id);
                self.infer_call(&mut fields[1], args, call_info)
            } else {
                self.error(
                    &format!(
                        "no static method named {}, on struct {}",
                        resolve(ident).dimmed().bold(),
                        resolve(&sym.name).dimmed().bold()
                    ),
                    vec![("in this field access".to_string(), fields[1].span.clone())],
                    vec![],
                );
                Type::Error
            }
        } else {
            self.non_struct_type(fields[0].span.clone(), file!(), line!());
            Type::Error
        }
    }

    pub fn non_struct_type(&mut self, span: Span, file: &str, line: u32) {
        println!("{}:{}", file, line);
        self.error(
            "cannot access field of non-struct type",
            vec![("in this field access".to_string(), span)],
            vec![],
        )
    }

    fn field_not_found_error(&mut self, field_name: &str, ty: &Type, span: Span) {
        self.error(
            &format!("couldn't find field {field_name} on type {}", self.format_type(ty)),
            vec![("in this field access".to_string(), span)],
            vec![],
        )
    }
}
