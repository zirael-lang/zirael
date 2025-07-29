use crate::{
    TokenKind,
    ast::{
        Abi, Function, FunctionModifiers, FunctionSignature, ImportKind, Item, ItemKind, Keyword,
        Parameter, ParameterKind, ReturnType,
    },
    parser::Parser,
};
use colored::Colorize;
use convert_case::{Case, Casing};
use ordinal::ToOrdinal;
use std::{collections::HashSet, path::PathBuf};
use zirael_utils::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_item(&mut self) -> Option<Item> {
        let attrs = self.parse_attrs();

        let (kind, name) = if self.match_keyword(Keyword::Fn) {
            self.parse_fn()
        } else if self.match_keyword(Keyword::Import) {
            self.parse_import()
        } else {
            self.error_at_peek(format!("no valid item found for {:?}", self.peek()));
            self.synchronize(&[TokenKind::Semicolon, TokenKind::BraceClose]);
            return None;
        };

        Some(Item { attributes: attrs, name, kind })
    }

    pub fn parse_import(&mut self) -> (ItemKind, Identifier) {
        let string = self.expect_string().unwrap_or_default();
        let span = self.prev_span();
        let current_file = self.source.path();
        let path = current_file.parent().unwrap_or(&PathBuf::new()).join(string.clone());

        let kind = if path.is_file() && path.extension().is_some_and(|ext| ext == "zr") {
            self.discover_queue.push((path.clone(), span));
            ImportKind::Path(path)
        } else {
            let parts = string.split('/').map(get_or_intern).collect::<Vec<_>>();
            ImportKind::ExternalModule(parts)
        };

        (ItemKind::Import(kind), default_ident())
    }

    pub fn parse_fn(&mut self) -> (ItemKind, Identifier) {
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
            if let Some(ty) = self.parse_type() {
                ReturnType::Type(ty)
            } else {
                ReturnType::Default
            }
        } else {
            ReturnType::Default
        };

        let body =
            if self.match_token(TokenKind::BraceOpen) { Some(self.parse_block()) } else { None };
        self.match_token(TokenKind::BraceClose);

        let signature = FunctionSignature { generics, parameters, return_type };
        let function = Function {
            name,
            modifiers: FunctionModifiers {
                is_async: async_,
                is_const: const_,
                is_extern: extern_,
                abi,
            },
            signature,
            body,
            span: span.start..self.prev_span().end,
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
            )
        }
    }

    pub fn parse_parameters(&mut self) -> Vec<Parameter> {
        let span_start = self.peek_span().end;
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
        let span = span_start..self.prev_span().start;
        self.validate_parameters(&params, span);

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

        self.validate_variadic(params, span);
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

        let name = self.expect_identifier()?;
        self.expect_message(TokenKind::Colon, "every parameter requires a type");
        let ty = self.parse_type();

        let Some(ty) = ty else {
            self.error_at_current("every parameter requires a type");
            return None;
        };

        let default_value =
            if self.match_token(TokenKind::Equals) { Some(self.parse_expr()) } else { None };

        let span = span.start..self.prev_span().end;
        Some(Parameter {
            name,
            kind: if variadic { ParameterKind::Variadic } else { ParameterKind::Plain },
            ty,
            default_value,
            span,
        })
    }
}
