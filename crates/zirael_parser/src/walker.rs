use crate::ast::{
    Abi, Ast, Attribute, BinaryOp, ClassDeclaration, ClassField, EnumDeclaration, EnumVariant,
    EnumVariantData, Expr, ExprKind, Function, FunctionModifiers, FunctionSignature, GenericArg,
    GenericParameter, ImportKind, Item, ItemKind, Literal, Parameter, ParameterKind, ReturnType,
    Stmt, StmtKind, TraitBound, Type, UnaryOp, VarDecl,
};
use zirael_utils::prelude::*;

pub trait AstWalker {
    fn walk_ast(&mut self, ast: &mut Ast) {
        for item in &mut ast.items {
            self.walk_item(item);
        }
    }

    fn walk_item(&mut self, item: &mut Item) {
        self.visit_item(item);

        for attr in &mut item.attributes {
            self.walk_attribute(attr);
        }

        self.walk_identifier(&mut item.name);
        self.walk_item_kind(&mut item.kind);
    }

    fn walk_item_kind(&mut self, kind: &mut ItemKind) {
        match kind {
            ItemKind::Function(func) => self.walk_function(func),
            ItemKind::Class(class) => self.walk_class_declaration(class),
            ItemKind::Enum(enum_decl) => self.walk_enum_declaration(enum_decl),
            ItemKind::Import(import, _) => self.walk_import_kind(import),
        }
    }

    fn walk_import_kind(&mut self, import: &mut ImportKind) {
        self.visit_import_kind(import);
        match import {
            ImportKind::Path(_) => {}
            ImportKind::ExternalModule(identifiers) => {
                for id in identifiers {
                    self.walk_identifier(id);
                }
            }
        }
    }

    fn walk_function(&mut self, func: &mut Function) {
        self.visit_function(func);
        self.walk_identifier(&mut func.name);
        self.walk_function_modifiers(&mut func.modifiers);
        self.walk_function_signature(&mut func.signature);

        if let Some(body) = &mut func.body {
            self.walk_expr(body);
        }
    }

    fn walk_function_modifiers(&mut self, modifiers: &mut FunctionModifiers) {
        self.visit_function_modifiers(modifiers);
        if let Some(abi) = &mut modifiers.abi {
            self.walk_abi(abi);
        }
    }

    fn walk_abi(&mut self, abi: &mut Abi) {
        self.visit_abi(abi);
    }

    fn walk_function_signature(&mut self, sig: &mut FunctionSignature) {
        self.visit_function_signature(sig);

        for generic in &mut sig.generics {
            self.walk_generic_parameter(generic);
        }

        for param in &mut sig.parameters {
            self.walk_parameter(param);
        }

        self.walk_return_type(&mut sig.return_type);
    }

    fn walk_parameter(&mut self, param: &mut Parameter) {
        self.visit_parameter(param);
        self.walk_identifier(&mut param.name);
        self.walk_type(&mut param.ty);
        self.walk_parameter_kind(&mut param.kind);

        if let Some(default) = &mut param.default_value {
            self.walk_expr(default);
        }
    }

    fn walk_parameter_kind(&mut self, kind: &mut ParameterKind) {
        self.visit_parameter_kind(kind);
    }

    fn walk_attribute(&mut self, attr: &mut Attribute) {
        self.visit_attribute(attr);
        self.walk_identifier(&mut attr.name);

        if let Some(args) = &mut attr.args {
            for arg in args {
                self.walk_expr(arg);
            }
        }
    }

    fn walk_class_declaration(&mut self, class: &mut ClassDeclaration) {
        self.visit_class_declaration(class);
        self.walk_identifier(&mut class.name);

        for generic in &mut class.generics {
            self.walk_generic_parameter(generic);
        }

        for field in &mut class.fields {
            self.walk_class_field(field);
        }
    }

    fn walk_class_field(&mut self, field: &mut ClassField) {
        self.visit_class_field(field);
        self.walk_identifier(&mut field.name);
        self.walk_type(&mut field.field_type);

        for attr in &mut field.attributes {
            self.walk_attribute(attr);
        }
    }

    fn walk_enum_declaration(&mut self, enum_decl: &mut EnumDeclaration) {
        self.visit_enum_declaration(enum_decl);
        self.walk_identifier(&mut enum_decl.name);

        if let Some(generics) = &mut enum_decl.generics {
            for generic in generics {
                self.walk_generic_parameter(generic);
            }
        }

        for variant in &mut enum_decl.variants {
            self.walk_enum_variant(variant);
        }
    }

    fn walk_enum_variant(&mut self, variant: &mut EnumVariant) {
        self.visit_enum_variant(variant);
        self.walk_identifier(&mut variant.name);
        self.walk_enum_variant_data(&mut variant.data);

        for attr in &mut variant.attributes {
            self.walk_attribute(attr);
        }
    }

    fn walk_enum_variant_data(&mut self, data: &mut EnumVariantData) {
        self.visit_enum_variant_data(data);
        match data {
            EnumVariantData::Unit => {}
            EnumVariantData::Tuple(types) => {
                for ty in types {
                    self.walk_type(ty);
                }
            }
            EnumVariantData::Class(fields) => {
                for field in fields {
                    self.walk_class_field(field);
                }
            }
        }
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        self.visit_expr(expr);
        self.walk_expr_kind(&mut expr.kind);
    }

    fn walk_expr_kind(&mut self, kind: &mut ExprKind) {
        match kind {
            ExprKind::Literal(lit) => self.walk_literal(lit),
            ExprKind::Identifier(id, _) => self.walk_identifier(id),
            ExprKind::Binary { left, op, right } => {
                self.walk_expr(left);
                self.visit_binary_op(op);
                self.walk_expr(right);
            }
            ExprKind::Block(stmts) => {
                for stmt in stmts {
                    self.walk_stmt(stmt);
                }
            }
            ExprKind::Assign(lhs, rhs) => {
                self.walk_expr(lhs);
                self.walk_expr(rhs);
            }
            ExprKind::AssignOp(lhs, op, rhs) => {
                self.walk_expr(lhs);
                self.visit_binary_op(op);
                self.walk_expr(rhs);
            }
            ExprKind::Unary(op, expr) => {
                self.visit_unary_op(op);
                self.walk_expr(expr);
            }
            ExprKind::Paren(expr) => {
                self.walk_expr(expr);
            }
            ExprKind::Call { callee, args } => {
                self.visit_function_call(callee, args);
                self.walk_expr(callee);
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            ExprKind::FieldAccess(exprs) => {
                for expr in exprs {
                    self.walk_expr(expr);
                }
            }
            ExprKind::CouldntParse(_) => {}
        }
    }

    fn walk_literal(&mut self, lit: &mut Literal) {
        self.visit_literal(lit);
    }

    fn walk_generic_parameter(&mut self, generic: &mut GenericParameter) {
        self.visit_generic_parameter(generic);
        self.walk_identifier(&mut generic.name);

        for constraint in &mut generic.constraints {
            self.walk_trait_bound(constraint);
        }

        if let Some(default) = &mut generic.default_type {
            self.walk_type(default);
        }
    }

    fn walk_trait_bound(&mut self, bound: &mut TraitBound) {
        self.visit_trait_bound(bound);
        self.walk_identifier(&mut bound.name);

        for arg in &mut bound.generic_args {
            self.walk_generic_arg(arg);
        }
    }

    fn walk_generic_arg(&mut self, arg: &mut GenericArg) {
        self.visit_generic_arg(arg);
        match arg {
            GenericArg::Type(ty) => self.walk_type(ty),
            GenericArg::Named { name, ty } => {
                self.walk_identifier(name);
                self.walk_type(ty);
            }
        }
    }

    fn walk_type(&mut self, ty: &mut Type) {
        self.visit_type(ty);
        match ty {
            Type::String
            | Type::Char
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::Void
            | Type::Inferred => {}
            Type::Pointer(inner) | Type::Reference(inner) | Type::MutableReference(inner) => {
                self.walk_type(inner);
            }
            Type::Array(inner, _) => {
                self.walk_type(inner);
            }
            Type::Function { params, return_type } => {
                for param in params {
                    self.walk_type(param);
                }
                self.walk_return_type(return_type);
            }
            Type::Named { name, generics } => {
                self.walk_identifier(name);
                for generic in generics {
                    self.walk_type(generic);
                }
            }
        }
    }

    fn walk_return_type(&mut self, ret_ty: &mut ReturnType) {
        self.visit_return_type(ret_ty);
        match ret_ty {
            ReturnType::Default => {}
            ReturnType::Type(ty) => self.walk_type(ty),
        }
    }

    fn walk_identifier(&mut self, id: &mut Identifier) {
        self.visit_identifier(id);
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        self.visit_stmt(stmt);
        self.walk_stmt_kind(&mut stmt.0);
    }

    fn walk_stmt_kind(&mut self, kind: &mut StmtKind) {
        self.visit_stmt_kind(kind);
        match kind {
            StmtKind::Expr(expr) => self.walk_expr(expr),
            StmtKind::Var(var_decl) => self.walk_var_decl(var_decl),
        }
    }

    fn walk_var_decl(&mut self, var_decl: &mut VarDecl) {
        self.visit_var_decl(var_decl);
        self.walk_identifier(&mut var_decl.name);
        self.walk_type(&mut var_decl.ty);
        self.walk_expr(&mut var_decl.value);
    }

    fn visit_item(&mut self, _item: &mut Item) {}
    fn visit_import_kind(&mut self, _import: &mut ImportKind) {}
    fn visit_function(&mut self, _func: &mut Function) {}
    fn visit_function_modifiers(&mut self, _modifiers: &mut FunctionModifiers) {}
    fn visit_abi(&mut self, _abi: &mut Abi) {}
    fn visit_function_signature(&mut self, _sig: &mut FunctionSignature) {}
    fn visit_parameter(&mut self, _param: &mut Parameter) {}
    fn visit_parameter_kind(&mut self, _kind: &mut ParameterKind) {}
    fn visit_attribute(&mut self, _attr: &mut Attribute) {}
    fn visit_class_declaration(&mut self, _class: &mut ClassDeclaration) {}
    fn visit_class_field(&mut self, _field: &mut ClassField) {}
    fn visit_enum_declaration(&mut self, _enum_decl: &mut EnumDeclaration) {}
    fn visit_enum_variant(&mut self, _variant: &mut EnumVariant) {}
    fn visit_enum_variant_data(&mut self, _data: &mut EnumVariantData) {}
    fn visit_expr(&mut self, _expr: &mut Expr) {}
    fn visit_literal(&mut self, _lit: &mut Literal) {}
    fn visit_binary_op(&mut self, _op: &mut BinaryOp) {}
    fn visit_unary_op(&mut self, _op: &mut UnaryOp) {}
    fn visit_stmt_kind(&mut self, _kind: &mut StmtKind) {}
    fn visit_generic_parameter(&mut self, _param: &mut GenericParameter) {}
    fn visit_trait_bound(&mut self, _bound: &mut TraitBound) {}
    fn visit_generic_arg(&mut self, _arg: &mut GenericArg) {}
    fn visit_type(&mut self, _ty: &mut Type) {}
    fn visit_return_type(&mut self, _ret_ty: &mut ReturnType) {}
    fn visit_identifier(&mut self, _id: &mut Identifier) {}
    fn visit_stmt(&mut self, _stmt: &mut Stmt) {}
    fn visit_var_decl(&mut self, _var_decl: &mut VarDecl) {}
    fn visit_function_call(&mut self, _callee: &mut Expr, _args: &mut [Expr]) {}
}
