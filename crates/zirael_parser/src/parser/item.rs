use crate::{
    AstId, StructDeclaration, StructField, TokenKind, Type,
    ast::{
        Abi, Function, FunctionModifiers, FunctionSignature, ImportKind, Item, ItemKind, Keyword,
        Parameter, ParameterKind,
    },
    parser::Parser,
    span::SpanUtils as _,
};
use colored::Colorize as _;
use convert_case::{Case, Casing as _};
use ordinal::ToOrdinal as _;
use std::{collections::HashSet, path::PathBuf};
use zirael_utils::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_item(&mut self) -> Option<Item> {
        let span = self.peek_span();
        let attrs = self.parse_attrs();

        let id = self.fresh_id();
        let (kind, name) = if self.match_keyword(Keyword::Fn) {
            self.parse_fn(id)
        } else if self.match_keyword(Keyword::Struct) {
            self.parse_struct(id)
        } else if self.match_keyword(Keyword::Import) {
            self.parse_import()
        } else {
            if let Some(token) = self.peek() {
                self.error_at_peek(format!(
                    "expected item declaration (fn, struct, or import), found {:?}",
                    token.kind
                ));
            } else {
                self.error_at_peek("expected item declaration, found end of file");
            }

            self.synchronize(&[
                TokenKind::Keyword(Keyword::Fn),
                TokenKind::Keyword(Keyword::Struct),
                TokenKind::Keyword(Keyword::Import),
                TokenKind::Semicolon,
                TokenKind::BraceClose,
            ]);

            return None;
        };

        Some(Item {
            attributes: attrs,
            name,
            kind,
            id,
            span: span.to(self.prev_span()),
            symbol_id: None,
        })
    }

    pub fn parse_struct(&mut self, id: AstId) -> (ItemKind, Identifier) {
        let span = self.prev_span();
        let name = self.expect_identifier().unwrap_or_else(|| {
            self.error_at_current("struct name is required");
            default_ident()
        });
        self.validate_struct_name(&name);
        let generics = self.parse_generics();

        if self.match_token(TokenKind::Semicolon) {
            // struct with no data: struct Person;
            return (
                ItemKind::Struct(StructDeclaration {
                    id,
                    name,
                    generics,
                    fields: vec![],
                    methods: vec![],
                    span: span.to(self.prev_span()),
                }),
                name,
            );
        }

        self.expect(TokenKind::BraceOpen);
        let mut fields = vec![];
        let mut methods = vec![];

        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            let attrs = self.parse_attrs();

            if self.check_keyword(Keyword::Fn) {
                self.advance();
                let method_id = self.fresh_id();
                let (method_kind, method_name) = self.parse_fn(method_id);

                if let ItemKind::Function(func) = method_kind {
                    let method_item = Item {
                        attributes: attrs,
                        name: method_name,
                        kind: ItemKind::Function(func),
                        id: method_id,
                        span: self.prev_span(),
                        symbol_id: None,
                    };
                    methods.push(method_item);
                } else {
                    self.error_at_current("expected function after 'fn' keyword");
                }
            } else if let Some(field_name) = self.expect_identifier() {
                self.expect(TokenKind::Colon);
                let field_type = self.parse_type().unwrap_or_else(|| {
                    self.error_at_current("expected type after ':'");
                    Type::Error
                });

                fields.push(StructField {
                    name: field_name,
                    ty: field_type,
                    is_public: true,
                    attributes: attrs,
                });

                if !self.check(&TokenKind::BraceClose)
                    && !self.match_token(TokenKind::Comma)
                    && !self.match_token(TokenKind::Semicolon)
                {
                    self.error_at_current("expected ',' or ';' after field");
                }
            } else {
                self.error_at("struct body expected a method or field", self.prev_span());
                self.synchronize(&[TokenKind::Semicolon, TokenKind::Comma, TokenKind::BraceClose]);
                if self.check(&TokenKind::BraceClose) {
                    break;
                }
            }
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("expected '}' to close struct");
        }

        (
            ItemKind::Struct(StructDeclaration {
                id,
                name,
                generics,
                fields,
                methods,
                span: span.to(self.prev_span()),
            }),
            name,
        )
    }

    pub fn parse_import(&mut self) -> (ItemKind, Identifier) {
        let string = self.expect_string().unwrap_or_default();
        let span = self.prev_span();
        let current_file = self.source.path();
        let path = current_file.parent().unwrap_or(&PathBuf::new()).join(string.clone());

        let kind = if path.is_file() && path.extension().is_some_and(|ext| ext == "zr") {
            self.discover_queue.push((path.clone(), span.clone()));
            ImportKind::Path(path)
        } else {
            let parts = string.split('/').map(get_or_intern).collect::<Vec<_>>();
            ImportKind::ExternalModule(parts)
        };
        let span = span.to(self.prev_span());

        (ItemKind::Import(kind, span), default_ident())
    }

    pub fn parse_fn(&mut self, id: AstId) -> (ItemKind, Identifier) {
        let span = self.prev_span();
        let async_ = self.match_keyword(Keyword::Async);
        let const_ = self.match_keyword(Keyword::Const);
        let extern_ = self.match_keyword(Keyword::Extern);

        let abi = if extern_ {
            if let Some(abi_name) = self.expect_string() {
                self.advance();
                Some(Abi(abi_name))
            } else {
                None
            }
        } else {
            None
        };

        let name = self.expect_identifier().unwrap_or_else(|| {
            self.error_at_current("function name is required");
            default_ident()
        });
        self.validate_fn_name(&name);

        let generics = self.parse_generics();
        let parameters = self.parse_parameters();

        let return_type = if self.match_token(TokenKind::Colon) {
            if let Some(ty) = self.parse_type() { ty } else { Type::Void }
        } else {
            Type::Void
        };
        let span = span.to(self.prev_span());

        let body =
            if self.match_token(TokenKind::BraceOpen) { Some(self.parse_block()) } else { None };

        let signature = FunctionSignature { generics, parameters, return_type };
        let function = Function {
            id,
            name,
            modifiers: FunctionModifiers {
                is_async: async_,
                is_const: const_,
                is_extern: extern_,
                abi,
            },
            signature,
            body,
            span,
        };

        (ItemKind::Function(function), name)
    }

    pub fn validate_fn_name(&mut self, name: &Identifier) {
        let name = resolve(name);
        if name.is_empty() {
            return;
        }

        let camel = name.to_case(Case::Camel);

        if name != camel {
            self.add_report(
                ReportBuilder::builder("function names must be camel case", ReportKind::Warning)
                    .label("here", self.prev_span())
                    .note(&format!("Suggested name {}", camel.dimmed().bold())),
            );
        }
    }

    pub fn validate_struct_name(&mut self, name: &Identifier) {
        let name = resolve(name);
        if name.is_empty() {
            return;
        }

        let pascal = name.to_case(Case::Pascal);

        if name != pascal {
            self.add_report(
                ReportBuilder::builder("struct names must be pascal case", ReportKind::Warning)
                    .label("here", self.prev_span())
                    .note(&format!("Suggested name {}", pascal.dimmed().bold())),
            );
        }
    }

    pub fn parse_parameters(&mut self) -> Vec<Parameter> {
        let span_start = self.peek_span();
        self.expect(TokenKind::ParenOpen);
        let mut params = vec![];

        if self.match_token(TokenKind::ParenClose) {
            return params;
        }

        loop {
            if let Some(param) = self.parse_single_parameter() {
                params.push(param);
            } else {
                break;
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            if self.check(&TokenKind::ParenClose) {
                break;
            }
        }
        self.expect(TokenKind::ParenClose);
        self.validate_parameters(&params, span_start.to(self.prev_span()));

        params
    }

    pub fn validate_parameters(&mut self, params: &[Parameter], span: Span) {
        let mut seen_names = HashSet::new();
        let mut duplicate_params = Vec::new();

        for param in params {
            if !seen_names.insert(&param.name) {
                duplicate_params.push(param);
            }
        }

        if !duplicate_params.is_empty() {
            let mut report = ReportBuilder::builder(
                "Found multiple parameters with the same name",
                ReportKind::Error,
            );

            for (i, param) in duplicate_params.iter().enumerate() {
                report = report.label(
                    &format!("{} parameter found here", (i + 1).to_ordinal_string()),
                    param.span.clone(),
                );
            }

            self.add_report(report);
        }

        self.validate_variadic(params, span.clone());
        self.validate_self(params, span);
    }

    pub fn validate_self(&mut self, params: &[Parameter], span: Span) {
        let mut seen_self = false;
        for param in params {
            if param.name == get_or_intern("self") {
                if seen_self {
                    self.error_at("self parameter can only be used once", param.span.clone());
                } else {
                    seen_self = true;
                }
            }
        }
    }

    pub fn validate_variadic(&mut self, params: &[Parameter], span: Span) {
        let variadic_params: Vec<(usize, &Parameter)> =
            params.iter().enumerate().filter(|(_, p)| p.kind == ParameterKind::Variadic).collect();

        match variadic_params.len() {
            0 => {}
            1 => {
                let (index, param) = variadic_params[0];
                if index != params.len() - 1 {
                    self.error_at("variadic parameter must be last.", param.span.clone());
                }
            }
            _ => {
                self.error_at("only one variadic parameter allowed per function.", span);
            }
        }
    }

    pub fn parse_single_parameter(&mut self) -> Option<Parameter> {
        let span = self.peek_span();
        let variadic = self.match_triple_dot();

        let is_ref = self.match_token(TokenKind::BitwiseAnd);
        let name = self.expect_identifier()?;
        let ty = if name == get_or_intern("self") {
            if is_ref { Type::Reference(Box::new(Type::Inferred)) } else { Type::Inferred }
        } else {
            self.expect_message(TokenKind::Colon, "every parameter requires a type");
            let ty = self.parse_type();

            let Some(ty) = ty else {
                self.error_at_current("every parameter requires a type");
                return None;
            };

            ty
        };

        let default_value =
            if self.match_token(TokenKind::Equals) { Some(self.parse_expr()) } else { None };

        Some(Parameter {
            name,
            kind: if variadic { ParameterKind::Variadic } else { ParameterKind::Plain },
            ty,
            default_value,
            span: span.to(self.prev_span()),
            symbol_id: None,
        })
    }
}
