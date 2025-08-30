use crate::{
    AstId, Attribute, StructDeclaration, StructField, TokenKind, Type, TypeExtension,
    ast::{
        Abi, EnumDeclaration, EnumVariant, EnumVariantData, Function, FunctionModifiers,
        FunctionSignature, ImportKind, Item, ItemKind, Keyword, Parameter, ParameterKind,
    },
    parser::Parser,
    span::SpanUtils as _,
};
use colored::Colorize as _;
use convert_case::{Case, Casing as _};
use ordinal::ToOrdinal as _;
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};
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
        } else if self.match_keyword(Keyword::Enum) {
            self.parse_enum(id)
        } else if self.match_keyword(Keyword::Import) {
            self.parse_import()
        } else if self.match_keyword(Keyword::Extension) {
            self.parse_extension(id)
        } else if self.match_keyword(Keyword::Mod) {
            self.parse_mod();
            return None;
        } else {
            if let Some(token) = self.peek() {
                self.error_at_peek(format!(
                    "expected item declaration (fn, struct, enum, extension, or import), found {}",
                    token.kind
                ));
            } else {
                self.error_at_peek("expected item declaration, found end of file");
            }

            self.synchronize(&[
                TokenKind::Keyword(Keyword::Fn),
                TokenKind::Keyword(Keyword::Struct),
                TokenKind::Keyword(Keyword::Enum),
                TokenKind::Keyword(Keyword::Extension),
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

    pub fn parse_mod(&mut self) {
        let string = self.expect_string().unwrap_or_default();
        let span = self.prev_span();
        let current_file = self.source.path();
        let base_path = current_file.parent().unwrap_or(Path::new(""));

        if string.contains('*') {
            self.handle_mod_glob(string, base_path, span);
        } else {
            self.handle_mod_single(string, base_path, span);
        }
    }

    fn handle_mod_glob(&mut self, pattern: String, base_path: &Path, span: Span) {
        use glob::glob;

        let full_pattern = base_path.join(&pattern);
        let pattern_str = full_pattern.to_string_lossy();

        match glob(&pattern_str) {
            Ok(paths) => {
                let mut found_any = false;

                for entry in paths {
                    match entry {
                        Ok(path) => {
                            found_any = true;

                            if path.is_file() && path.extension().is_some_and(|ext| ext == "zr") {
                                self.discover_queue.push((path, span.clone()));
                            } else if path.is_dir() {
                                let index_file = path.join("index.zr");

                                if index_file.exists() {
                                    self.discover_queue.push((index_file, span.clone()));
                                }
                            }
                        }
                        Err(e) => {
                            self.error_at(format!("error reading glob entry: {e}"), span.clone());
                        }
                    }
                }

                if !found_any {
                    self.error_at(format!("no files found matching pattern: {pattern}"), span);
                }
            }
            Err(e) => {
                self.error_at(format!("invalid glob pattern '{pattern}': {e}"), span);
            }
        }
    }

    fn handle_mod_single(&mut self, string: String, base_path: &Path, span: Span) {
        let path = base_path.join(&string);

        if path.is_file() && path.extension().is_some_and(|ext| ext == "zr") {
            self.discover_queue.push((path, span));
        } else if path.is_dir() {
            let index_file = path.join("index.zr");

            if index_file.exists() {
                self.discover_queue.push((index_file, span));
            } else {
                self.error_at(
                    format!("couldn't find index.zr in directory: {}", path.display()),
                    span,
                );
            }
        } else {
            let path_with_ext = path.with_extension("zr");
            if path_with_ext.is_file() {
                self.discover_queue.push((path_with_ext, span));
            } else {
                self.error_at(
                    format!(
                        "couldn't find module: {} (tried {}, {}, and {})",
                        string,
                        path.display(),
                        path_with_ext.display(),
                        path.join("index.zr").display()
                    ),
                    span,
                );
            }
        }
    }

    pub fn parse_import(&mut self) -> (ItemKind, Identifier) {
        let string = self.expect_string().unwrap_or_default();
        let span = self.prev_span();
        let current_file = self.source.path();
        let path = current_file.parent().unwrap_or(&PathBuf::new()).join(string.clone());

        let is_path_import =
            string.starts_with("./") || string.starts_with('/') || string.starts_with('\\');

        let kind = if is_path_import {
            if path.is_file() && path.extension().is_some_and(|ext| ext == "zr") {
                self.discover_queue.push((path.clone(), span.clone()));
                ImportKind::Path(path)
            } else {
                let error_msg = if !path.exists() {
                    format!("couldn't find file: {}", path.display())
                } else if path.extension().is_none_or(|ext| ext != "zr") {
                    format!("import file must have .zr extension: {}", path.display())
                } else {
                    format!("import path is not a file: {}", path.display())
                };

                self.error_at(error_msg, span.clone());

                ImportKind::Path(path)
            }
        } else {
            let parts = string.split('/').map(get_or_intern).collect::<Vec<_>>();
            ImportKind::ExternalModule(parts)
        };

        let span = span.to(self.prev_span());

        (ItemKind::Import(kind, span), default_ident())
    }

    pub fn parse_single_method(&mut self, attrs: Vec<Attribute>) -> Option<Item> {
        if !self.check_keyword(Keyword::Fn) {
            return None;
        }

        self.advance();
        let method_id = self.fresh_id();
        let (method_kind, method_name) = self.parse_fn(method_id);

        if let ItemKind::Function(func) = method_kind {
            Some(Item {
                attributes: attrs,
                name: method_name,
                kind: ItemKind::Function(func),
                id: method_id,
                span: self.prev_span(),
                symbol_id: None,
            })
        } else {
            self.error_at_current("expected function after 'fn' keyword");
            None
        }
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
                if let Some(method) = self.parse_single_method(attrs) {
                    methods.push(method);
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

    pub fn parse_enum(&mut self, id: AstId) -> (ItemKind, Identifier) {
        let span = self.prev_span();
        let name = self.expect_identifier().unwrap_or_else(|| {
            self.error_at_current("enum name is required");
            default_ident()
        });
        self.validate_enum_name(&name);
        let generics = self.parse_generics();

        self.expect(TokenKind::BraceOpen);

        let mut variants = vec![];
        let mut methods = vec![];

        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            let attrs = self.parse_attrs();

            if self.check_keyword(Keyword::Fn) {
                if let Some(method) = self.parse_single_method(attrs) {
                    methods.push(method);
                }
            } else if let Some(variant_name) = self.expect_identifier() {
                let variant_id = self.fresh_id();
                let variant_span = self.prev_span();

                let data = if self.match_token(TokenKind::BraceOpen) {
                    let fields = self.parse_struct_fields();
                    self.expect(TokenKind::BraceClose);
                    EnumVariantData::Struct(fields)
                } else {
                    EnumVariantData::Unit
                };

                variants.push(EnumVariant {
                    id: variant_id,
                    name: variant_name,
                    data,
                    attributes: attrs,
                    span: variant_span.to(self.prev_span()),
                    symbol_id: None,
                });

                if !self.check(&TokenKind::BraceClose)
                    && !self.match_token(TokenKind::Comma)
                    && !self.match_token(TokenKind::Semicolon)
                {
                    self.error_at_current("expected ',' or ';' after variant");
                }
            } else {
                self.error_at("enum body expected a method or variant", self.prev_span());
                self.synchronize(&[TokenKind::Semicolon, TokenKind::Comma, TokenKind::BraceClose]);
                if self.check(&TokenKind::BraceClose) {
                    break;
                }
            }
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("expected '}' to close enum");
        }

        (
            ItemKind::Enum(EnumDeclaration {
                id,
                name,
                generics,
                variants,
                methods,
                span: span.to(self.prev_span()),
            }),
            name,
        )
    }

    pub fn parse_extension(&mut self, id: AstId) -> (ItemKind, Identifier) {
        let span = self.prev_span();

        let extended_type = self.parse_type().unwrap_or_else(|| {
            self.error_at_current("expected type after 'extension' keyword");
            Type::Error
        });

        self.expect(TokenKind::BraceOpen);

        let mut items = vec![];
        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            let attrs = self.parse_attrs();

            if self.check_keyword(Keyword::Fn) {
                if let Some(method) = self.parse_single_method(attrs) {
                    items.push(method);
                }
            } else {
                self.error_at("extension body can only contain methods", self.prev_span());
                self.synchronize(&[TokenKind::Semicolon, TokenKind::Comma, TokenKind::BraceClose]);
                if self.check(&TokenKind::BraceClose) {
                    break;
                }
            }
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("expected '}' to close extension");
        }

        (
            ItemKind::TypeExtension(TypeExtension {
                id,
                ty: extended_type,
                items,
                span: span.to(self.prev_span()),
            }),
            default_ident(),
        )
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

    pub fn validate_enum_name(&mut self, name: &Identifier) {
        let name = resolve(name);
        if name.is_empty() {
            return;
        }

        let pascal = name.to_case(Case::Pascal);

        if name != pascal {
            self.add_report(
                ReportBuilder::builder("enum names must be pascal case", ReportKind::Warning)
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

    pub fn validate_self(&mut self, params: &[Parameter], _span: Span) {
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

    pub fn parse_struct_fields(&mut self) -> Vec<StructField> {
        let mut fields = vec![];

        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            let attrs = self.parse_attrs();

            if let Some(field_name) = self.expect_identifier() {
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
                self.error_at("expected field name", self.prev_span());
                self.synchronize(&[TokenKind::Semicolon, TokenKind::Comma, TokenKind::BraceClose]);
                if self.check(&TokenKind::BraceClose) {
                    break;
                }
            }
        }

        fields
    }
}
