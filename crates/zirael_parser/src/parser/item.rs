use crate::{
    TokenKind,
    ast::{
        Abi, Function, FunctionModifiers, FunctionSignature, ImportKind, Item, ItemKind, Parameter,
        ParameterKind, ReturnType, keyword::Keyword,
    },
    parser::Parser,
};
use std::path::PathBuf;
use zirael_utils::{
    ident_table::default_ident,
    prelude::{Identifier, get_or_intern},
};

impl<'a> Parser<'a> {
    pub fn parse_item(&mut self) -> Option<Item> {
        let attrs = self.parse_attrs();

        let (kind, name) = if self.match_keyword(Keyword::Fn) {
            self.parse_fn()
        } else if self.match_keyword(Keyword::Import) {
            self.parse_import()
        } else {
            self.error_at_peek(format!("No valid item found for {:?}", self.peek()));
            self.synchronize(&[TokenKind::Semicolon, TokenKind::BraceClose]);
            return None;
        };

        Some(Item { attributes: attrs, name, kind })
    }

    pub fn parse_import(&mut self) -> (ItemKind, Identifier) {
        let string = self.expect_string().unwrap_or_default();
        let span = self.prev_span();
        let current_file = self.source.path().expect("No source file");
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
            self.error_at_current("Function name is required");
            default_ident()
        });

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
        };

        (ItemKind::Function(function), name)
    }

    pub fn parse_parameters(&mut self) -> Vec<Parameter> {
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
        params
    }

    pub fn parse_single_parameter(&mut self) -> Option<Parameter> {
        let variadic = self.match_triple_dot();

        let name = self.expect_identifier()?;
        self.expect_message(TokenKind::Colon, "Every parameter requires a type");
        let ty = self.parse_type();

        let Some(ty) = ty else {
            self.error_at_current("Every parameter requires a type");
            return None;
        };

        let default_value =
            if self.match_token(TokenKind::Equals) { Some(self.parse_expr()) } else { None };

        Some(Parameter {
            name,
            kind: if variadic { ParameterKind::Variadic } else { ParameterKind::Plain },
            ty,
            default_value,
        })
    }
}
