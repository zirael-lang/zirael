use crate::ast::{
    Abi, Ast, Attribute, BinaryOp, ClassDeclaration, ClassField, EnumDeclaration, EnumVariant,
    EnumVariantData, Expr, ExprKind, Function, FunctionModifiers, FunctionSignature, GenericArg,
    GenericParameter, ImportKind, Item, ItemKind, Literal, Parameter, ParameterKind, ReturnType,
    Stmt, StmtKind, TraitBound, Type, UnaryOp, VarDecl,
};
use zirael_utils::prelude::*;

pub trait AstWalker {
    fn walk_ast(&mut self, ast: &Ast) {
        for item in &ast.items {
            self.walk_item(item);
        }
    }

    fn walk_item(&mut self, item: &Item) {
        self.visit_item(item);

        for attr in &item.attributes {
            self.walk_attribute(attr);
        }

        self.walk_identifier(&item.name);
        self.walk_item_kind(&item.kind);
    }

    fn walk_item_kind(&mut self, kind: &ItemKind) {
        match kind {
            ItemKind::Function(func) => self.walk_function(func),
            ItemKind::Class(class) => self.walk_class_declaration(class),
            ItemKind::Enum(enum_decl) => self.walk_enum_declaration(enum_decl),
            ItemKind::Import(import) => self.walk_import_kind(import),
        }
    }

    fn walk_import_kind(&mut self, import: &ImportKind) {
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

    fn walk_function(&mut self, func: &Function) {
        self.visit_function(func);
        self.walk_identifier(&func.name);
        self.walk_function_modifiers(&func.modifiers);
        self.walk_function_signature(&func.signature);

        if let Some(body) = &func.body {
            self.walk_expr(body);
        }
    }

    fn walk_function_modifiers(&mut self, modifiers: &FunctionModifiers) {
        self.visit_function_modifiers(modifiers);
        if let Some(abi) = &modifiers.abi {
            self.walk_abi(abi);
        }
    }

    fn walk_abi(&mut self, abi: &Abi) {
        self.visit_abi(abi);
    }

    fn walk_function_signature(&mut self, sig: &FunctionSignature) {
        self.visit_function_signature(sig);

        for generic in &sig.generics {
            self.walk_generic_parameter(generic);
        }

        for param in &sig.parameters {
            self.walk_parameter(param);
        }

        self.walk_return_type(&sig.return_type);
    }

    fn walk_parameter(&mut self, param: &Parameter) {
        self.visit_parameter(param);
        self.walk_identifier(&param.name);
        self.walk_type(&param.ty);
        self.walk_parameter_kind(&param.kind);

        if let Some(default) = &param.default_value {
            self.walk_expr(default);
        }
    }

    fn walk_parameter_kind(&mut self, kind: &ParameterKind) {
        self.visit_parameter_kind(kind);
    }

    fn walk_attribute(&mut self, attr: &Attribute) {
        self.visit_attribute(attr);
        self.walk_identifier(&attr.name);

        if let Some(args) = &attr.args {
            for arg in args {
                self.walk_expr(arg);
            }
        }
    }

    fn walk_class_declaration(&mut self, class: &ClassDeclaration) {
        self.visit_class_declaration(class);
        self.walk_identifier(&class.name);

        for generic in &class.generics {
            self.walk_generic_parameter(generic);
        }

        for field in &class.fields {
            self.walk_class_field(field);
        }
    }

    fn walk_class_field(&mut self, field: &ClassField) {
        self.visit_class_field(field);
        self.walk_identifier(&field.name);
        self.walk_type(&field.field_type);

        for attr in &field.attributes {
            self.walk_attribute(attr);
        }
    }

    fn walk_enum_declaration(&mut self, enum_decl: &EnumDeclaration) {
        self.visit_enum_declaration(enum_decl);
        self.walk_identifier(&enum_decl.name);

        if let Some(generics) = &enum_decl.generics {
            for generic in generics {
                self.walk_generic_parameter(generic);
            }
        }

        for variant in &enum_decl.variants {
            self.walk_enum_variant(variant);
        }
    }

    fn walk_enum_variant(&mut self, variant: &EnumVariant) {
        self.visit_enum_variant(variant);
        self.walk_identifier(&variant.name);
        self.walk_enum_variant_data(&variant.data);

        for attr in &variant.attributes {
            self.walk_attribute(attr);
        }
    }

    fn walk_enum_variant_data(&mut self, data: &EnumVariantData) {
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

    fn walk_expr(&mut self, expr: &Expr) {
        self.visit_expr(expr);
        self.walk_expr_kind(&expr.0);
    }

    fn walk_expr_kind(&mut self, kind: &ExprKind) {
        match kind {
            ExprKind::Literal(lit) => self.walk_literal(lit),
            ExprKind::Identifier(id) => self.walk_identifier(id),
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

    fn walk_literal(&mut self, lit: &Literal) {
        self.visit_literal(lit);
    }

    fn walk_generic_parameter(&mut self, generic: &GenericParameter) {
        self.visit_generic_parameter(generic);
        self.walk_identifier(&generic.name);

        for constraint in &generic.constraints {
            self.walk_trait_bound(constraint);
        }

        if let Some(default) = &generic.default_type {
            self.walk_type(default);
        }
    }

    fn walk_trait_bound(&mut self, bound: &TraitBound) {
        self.visit_trait_bound(bound);
        self.walk_identifier(&bound.name);

        for arg in &bound.generic_args {
            self.walk_generic_arg(arg);
        }
    }

    fn walk_generic_arg(&mut self, arg: &GenericArg) {
        self.visit_generic_arg(arg);
        match arg {
            GenericArg::Type(ty) => self.walk_type(ty),
            GenericArg::Named { name, ty } => {
                self.walk_identifier(name);
                self.walk_type(ty);
            }
        }
    }

    fn walk_type(&mut self, ty: &Type) {
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

    fn walk_return_type(&mut self, ret_ty: &ReturnType) {
        self.visit_return_type(ret_ty);
        match ret_ty {
            ReturnType::Default => {}
            ReturnType::Type(ty) => self.walk_type(ty),
        }
    }

    fn walk_identifier(&mut self, id: &Identifier) {
        self.visit_identifier(id);
    }

    fn walk_stmt(&mut self, stmt: &Stmt) {
        self.visit_stmt(stmt);
        self.walk_stmt_kind(&stmt.0);
    }

    fn walk_stmt_kind(&mut self, kind: &StmtKind) {
        self.visit_stmt_kind(kind);
        match kind {
            StmtKind::Expr(expr) => self.walk_expr(expr),
            StmtKind::Var(var_decl) => self.walk_var_decl(var_decl),
        }
    }

    fn walk_var_decl(&mut self, var_decl: &VarDecl) {
        self.visit_var_decl(var_decl);
        self.walk_identifier(&var_decl.name);
        self.walk_type(&var_decl.ty);
        self.walk_expr(&var_decl.value);
    }

    fn visit_item(&mut self, _item: &Item) {}
    fn visit_import_kind(&mut self, _import: &ImportKind) {}
    fn visit_function(&mut self, _func: &Function) {}
    fn visit_function_modifiers(&mut self, _modifiers: &FunctionModifiers) {}
    fn visit_abi(&mut self, _abi: &Abi) {}
    fn visit_function_signature(&mut self, _sig: &FunctionSignature) {}
    fn visit_parameter(&mut self, _param: &Parameter) {}
    fn visit_parameter_kind(&mut self, _kind: &ParameterKind) {}
    fn visit_attribute(&mut self, _attr: &Attribute) {}
    fn visit_class_declaration(&mut self, _class: &ClassDeclaration) {}
    fn visit_class_field(&mut self, _field: &ClassField) {}
    fn visit_enum_declaration(&mut self, _enum_decl: &EnumDeclaration) {}
    fn visit_enum_variant(&mut self, _variant: &EnumVariant) {}
    fn visit_enum_variant_data(&mut self, _data: &EnumVariantData) {}
    fn visit_expr(&mut self, _expr: &Expr) {}
    fn visit_literal(&mut self, _lit: &Literal) {}
    fn visit_binary_op(&mut self, _op: &BinaryOp) {}
    fn visit_unary_op(&mut self, _op: &UnaryOp) {}
    fn visit_stmt_kind(&mut self, _kind: &StmtKind) {}
    fn visit_generic_parameter(&mut self, _param: &GenericParameter) {}
    fn visit_trait_bound(&mut self, _bound: &TraitBound) {}
    fn visit_generic_arg(&mut self, _arg: &GenericArg) {}
    fn visit_type(&mut self, _ty: &Type) {}
    fn visit_return_type(&mut self, _ret_ty: &ReturnType) {}
    fn visit_identifier(&mut self, _id: &Identifier) {}
    fn visit_stmt(&mut self, _stmt: &Stmt) {}
    fn visit_var_decl(&mut self, _var_decl: &VarDecl) {}
}
