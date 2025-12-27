use crate::ast::{
    AssociatedType, AssociatedTypeImpl, ConstItem, EnumItem, FunctionItem, GenericParam,
    GenericParams, ImplItem, ImplMember, InterfaceItem, InterfaceMember, InterfaceMethod,
    InterfaceMethodImpl, ItemKind, MethodItem, Param, ProgramNode, RegularParam,
    SelfKind, SelfParam, StructField, StructItem, StructMember, TypeBound, VariadicParam,
    Variant, VariantField, VariantPayload, Visibility,
};
use crate::items::Item;
use crate::lexer::TokenType;
use crate::parser::{ParseResult, Parser, ParserError, ParserErrorKind};

impl Parser {
    pub fn parse_program(&mut self) -> ParseResult<ProgramNode> {
        let start = self.current_span();

        // Parse attributes
        let attributes = self.parse_attributes()?;

        // Parse imports
        let mut imports = Vec::new();
        while self.check(&TokenType::Import) {
            match self.parse_import() {
                Ok(import) => imports.push(import),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }
        }

        // Parse items
        let mut items = Vec::new();
        while !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }
        }

        Ok(ProgramNode {
            attributes,
            imports,
            items,
            span: self.span_from(start),
        })
    }

    pub fn parse_item(&mut self) -> ParseResult<Item> {
        let start = self.current_span();

        let attributes = self.parse_attributes()?;

        let visibility = if self.eat(TokenType::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        // Parse item kind
        let kind = if self.check(&TokenType::Const) {
            self.parse_const_item().map(ItemKind::Const)?
        } else if self.check(&TokenType::Func) {
            self.parse_function_item().map(ItemKind::Function)?
        } else if self.check(&TokenType::Struct) {
            self.parse_struct_item().map(ItemKind::Struct)?
        } else if self.check(&TokenType::Enum) {
            self.parse_enum_item().map(ItemKind::Enum)?
        } else if self.check(&TokenType::Interface) {
            self.parse_interface_item().map(ItemKind::Interface)?
        } else if self.check(&TokenType::Impl) {
            self.parse_impl_item().map(ItemKind::Impl)?
        } else {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken,
                span: self.current_span(),
                message: format!("Expected item (func, struct, enum, interface, impl, or const), found {}", self.peek().token_type),
            });
        };

        Ok(Item {
            attributes,
            visibility,
            kind,
            span: self.span_from(start),
        })
    }

    fn parse_const_item(&mut self) -> ParseResult<ConstItem> {
        let start = self.expect(TokenType::Const, "at start of const item")?.span;
        let name = self.parse_identifier()?;
        self.expect(TokenType::Colon, "after const name")?;
        let ty = self.parse_type()?;
        self.expect(TokenType::Assign, "in const item")?;
        let value = self.parse_expr()?;
        self.expect(TokenType::Semicolon, "after const item")?;

        Ok(ConstItem {
            name,
            ty,
            value,
            span: self.span_from(start),
        })
    }

    fn parse_function_item(&mut self) -> ParseResult<FunctionItem> {
        let start = self.current_span();

        let is_const = self.eat(TokenType::Const);
        self.expect(TokenType::Func, "at start of function")?;
        let name = self.parse_identifier()?;

        let generics = if self.check(&TokenType::Lt) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(TokenType::LeftParen, "after function name")?;
        let params = self.parse_params()?;
        self.expect(TokenType::RightParen, "after function parameters")?;

        let return_type = if self.eat(TokenType::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(FunctionItem {
            is_const,
            name,
            generics,
            params,
            return_type,
            body,
            span: self.span_from(start),
        })
    }

    fn parse_params(&mut self) -> ParseResult<Vec<Param>> {
        let mut params = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                params.push(self.parse_param()?);

                if !self.eat(TokenType::Comma) {
                    break;
                }

                if self.check(&TokenType::RightParen) {
                    break;
                }
            }
        }

        Ok(params)
    }

    fn parse_param(&mut self) -> ParseResult<Param> {
        let start = self.current_span();

        // Check for self parameter variants
        // &mut self
        if self.check(&TokenType::Amp) {
            if let Some(next) = self.peek_ahead(1) {
                if matches!(next.token_type, TokenType::Mut) {
                    if let Some(after_mut) = self.peek_ahead(2) {
                        if matches!(after_mut.token_type, TokenType::SelfValue) {
                            self.advance(); // consume &
                            self.advance(); // consume mut
                            self.advance(); // consume self
                            return Ok(Param::SelfParam(SelfParam {
                                kind: SelfKind::RefMut,
                                span: self.span_from(start),
                            }));
                        }
                    }
                } else if matches!(next.token_type, TokenType::SelfValue) {
                    // &self
                    self.advance(); // consume &
                    self.advance(); // consume self
                    return Ok(Param::SelfParam(SelfParam {
                        kind: SelfKind::Ref,
                        span: self.span_from(start),
                    }));
                }
            }
        }

        // mut self
        if self.check(&TokenType::Mut) {
            if let Some(next) = self.peek_ahead(1) {
                if matches!(next.token_type, TokenType::SelfValue) {
                    self.advance(); // consume mut
                    self.advance(); // consume self
                    return Ok(Param::SelfParam(SelfParam {
                        kind: SelfKind::Mut,
                        span: self.span_from(start),
                    }));
                }
            }
        }

        // self (by value)
        if self.check(&TokenType::SelfValue) {
            self.advance();
            return Ok(Param::SelfParam(SelfParam {
                kind: SelfKind::Value,
                span: self.span_from(start),
            }));
        }

        // Regular parameter: name: Type or name: Type = default or name: Type...
        let name = self.parse_identifier()?;
        self.expect(TokenType::Colon, "after parameter name")?;
        let ty = self.parse_type()?;

        // Check for variadic: name: Type...
        if self.eat(TokenType::DotDotDot) {
            return Ok(Param::Variadic(VariadicParam {
                name,
                ty,
                span: self.span_from(start),
            }));
        }

        let default = if self.eat(TokenType::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Param::Regular(RegularParam {
            name,
            ty,
            default,
            span: self.span_from(start),
        }))
    }

    fn parse_generic_params(&mut self) -> ParseResult<GenericParams> {
        let start = self.expect(TokenType::Lt, "at start of generic parameters")?.span;

        let mut params = Vec::new();

        if !self.check(&TokenType::Gt) {
            loop {
                params.push(self.parse_generic_param()?);

                if !self.eat(TokenType::Comma) {
                    break;
                }

                if self.check(&TokenType::Gt) {
                    break;
                }
            }
        }

        self.expect(TokenType::Gt, "after generic parameters")?;

        Ok(GenericParams {
            params,
            span: self.span_from(start),
        })
    }

    fn parse_generic_param(&mut self) -> ParseResult<GenericParam> {
        let start = self.current_span();
        let name = self.parse_identifier()?;

        let mut bounds = Vec::new();

        // Parse bounds: T: Bound1 & Bound2
        if self.eat(TokenType::Colon) {
            loop {
                let bound_start = self.current_span();
                let path = self.parse_path()?;
                let type_path = crate::ast::TypePath {
                    path: path.clone(),
                    args: None,
                    span: path.span.clone(),
                };

                bounds.push(TypeBound {
                    path: type_path,
                    span: self.span_from(bound_start),
                });

                if !self.eat(TokenType::Amp) {
                    break;
                }
            }
        }

        Ok(GenericParam {
            name,
            bounds,
            span: self.span_from(start),
        })
    }

    fn parse_struct_item(&mut self) -> ParseResult<StructItem> {
        let start = self.expect(TokenType::Struct, "at start of struct")?.span;
        let name = self.parse_identifier()?;

        let generics = if self.check(&TokenType::Lt) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(TokenType::LeftBrace, "after struct name")?;

        let mut members = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_struct_member() {
                Ok(member) => members.push(member),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }
        }

        self.expect(TokenType::RightBrace, "after struct members")?;

        Ok(StructItem {
            name,
            generics,
            members,
            span: self.span_from(start),
        })
    }

    fn parse_struct_member(&mut self) -> ParseResult<StructMember> {
        let attributes = self.parse_attributes()?;
        let visibility = if self.eat(TokenType::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        // Check if this is a method
        if self.check(&TokenType::Func) {
            let method = self.parse_method_item(attributes, visibility)?;
            return Ok(StructMember::Method(method));
        }

        // Otherwise, it's a field
        let field = self.parse_struct_field(attributes, visibility)?;
        Ok(StructMember::Field(field))
    }

    fn parse_struct_field(
        &mut self,
        attributes: Vec<crate::ast::Attribute>,
        visibility: Visibility,
    ) -> ParseResult<StructField> {
        let start = self.current_span();
        let name = self.parse_identifier()?;
        self.expect(TokenType::Colon, "after field name")?;
        let ty = self.parse_type()?;
        self.expect(TokenType::Comma, "after struct field")?;

        Ok(StructField {
            attributes,
            visibility,
            name,
            ty,
            span: self.span_from(start),
        })
    }

    fn parse_method_item(
        &mut self,
        attributes: Vec<crate::ast::Attribute>,
        visibility: Visibility,
    ) -> ParseResult<MethodItem> {
        let start = self.expect(TokenType::Func, "at start of method")?.span;
        let name = self.parse_identifier()?;

        self.expect(TokenType::LeftParen, "after method name")?;
        let params = self.parse_params()?;
        self.expect(TokenType::RightParen, "after method parameters")?;

        let return_type = if self.eat(TokenType::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(MethodItem {
            attributes,
            visibility,
            name,
            params,
            return_type,
            body,
            span: self.span_from(start),
        })
    }

    fn parse_enum_item(&mut self) -> ParseResult<EnumItem> {
        let start = self.expect(TokenType::Enum, "at start of enum")?.span;
        let name = self.parse_identifier()?;

        let generics = if self.check(&TokenType::Lt) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(TokenType::LeftBrace, "after enum name")?;

        let mut variants = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_variant() {
                Ok(variant) => variants.push(variant),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }

            self.eat(TokenType::Comma);
        }

        self.expect(TokenType::RightBrace, "after enum variants")?;

        Ok(EnumItem {
            name,
            generics,
            variants,
            span: self.span_from(start),
        })
    }

    fn parse_variant(&mut self) -> ParseResult<Variant> {
        let start = self.current_span();
        let attributes = self.parse_attributes()?;
        let name = self.parse_identifier()?;

        let payload = if self.eat(TokenType::LeftParen) {
            // Tuple variant
            let mut fields = Vec::new();

            if !self.check(&TokenType::RightParen) {
                loop {
                    fields.push(self.parse_variant_field()?);

                    if !self.eat(TokenType::Comma) {
                        break;
                    }

                    if self.check(&TokenType::RightParen) {
                        break;
                    }
                }
            }

            self.expect(TokenType::RightParen, "after variant fields")?;
            Some(VariantPayload::Tuple(fields))
        } else if self.eat(TokenType::Assign) {
            // Discriminant
            let expr = self.parse_expr()?;
            Some(VariantPayload::Discriminant(expr))
        } else {
            None
        };

        Ok(Variant {
            attributes,
            name,
            payload,
            span: self.span_from(start),
        })
    }

    fn parse_variant_field(&mut self) -> ParseResult<VariantField> {
        // Check if this is a named field: name: Type
        if let TokenType::Identifier(_) = &self.peek().token_type {
            if let Some(next) = self.peek_ahead(1) {
                if matches!(next.token_type, TokenType::Colon) {
                    let name = self.parse_identifier()?;
                    self.expect(TokenType::Colon, "in named variant field")?;
                    let ty = self.parse_type()?;
                    return Ok(VariantField::Named { name, ty });
                }
            }
        }

        let ty = self.parse_type()?;
        Ok(VariantField::Unnamed(ty))
    }

    fn parse_interface_item(&mut self) -> ParseResult<InterfaceItem> {
        let start = self.expect(TokenType::Interface, "at start of interface")?.span;
        let name = self.parse_identifier()?;

        let generics = if self.check(&TokenType::Lt) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        self.expect(TokenType::LeftBrace, "after interface name")?;

        let mut members = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_interface_member() {
                Ok(member) => members.push(member),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }
        }

        self.expect(TokenType::RightBrace, "after interface members")?;

        Ok(InterfaceItem {
            name,
            generics,
            members,
            span: self.span_from(start),
        })
    }

    fn parse_interface_member(&mut self) -> ParseResult<InterfaceMember> {
        let start = self.current_span();

        // Type member: type Name;
        if self.eat(TokenType::Type) {
            let name = self.parse_identifier()?;
            self.expect(TokenType::Semicolon, "after associated type")?;

            return Ok(InterfaceMember::AssociatedType(AssociatedType {
                name,
                span: self.span_from(start),
            }));
        }

        // Method signature
        self.expect(TokenType::Func, "at start of method signature in interface")?;
        let name = self.parse_identifier()?;

        self.expect(TokenType::LeftParen, "after method name")?;
        let params = self.parse_params()?;
        self.expect(TokenType::RightParen, "after method parameters")?;

        let return_type = if self.eat(TokenType::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenType::Semicolon, "after interface method")?;

        Ok(InterfaceMember::Method(InterfaceMethod {
            name,
            params,
            return_type,
            span: self.span_from(start),
        }))
    }

    fn parse_impl_item(&mut self) -> ParseResult<ImplItem> {
        let start = self.expect(TokenType::Impl, "at start of impl")?.span;

        let generics = if self.check(&TokenType::Lt) {
            Some(self.parse_generic_params()?)
        } else {
            None
        };

        // Parse first type (could be interface or target)
        let first_type = self.parse_type()?;

        // Check if this is an interface implementation: impl Interface for Type
        let (interface_ref, target_type) = if self.eat(TokenType::For) {
            let target = self.parse_type()?;
            // Convert first_type to TypePath
            let interface_ref = if let crate::ast::Type::Path(type_path) = first_type {
                Some(type_path)
            } else {
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidSyntax,
                    span: self.current_span(),
                    message: "Expected interface path in impl".to_string(),
                });
            };
            (interface_ref, target)
        } else {
            (None, first_type)
        };

        self.expect(TokenType::LeftBrace, "after impl target")?;

        let mut members = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_impl_member(interface_ref.is_some()) {
                Ok(member) => members.push(member),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }
        }

        self.expect(TokenType::RightBrace, "after impl members")?;

        Ok(ImplItem {
            generics,
            interface_ref,
            target_type,
            members,
            span: self.span_from(start),
        })
    }

    fn parse_impl_member(&mut self, is_interface_impl: bool) -> ParseResult<ImplMember> {
        let start = self.current_span();

        // Associated type implementation: type Name = Type;
        if self.eat(TokenType::Type) {
            let name = self.parse_identifier()?;
            self.expect(TokenType::Assign, "in associated type")?;
            let ty = self.parse_type()?;
            self.expect(TokenType::Semicolon, "after associated type")?;

            return Ok(ImplMember::AssociatedType(AssociatedTypeImpl {
                name,
                ty,
                span: self.span_from(start),
            }));
        }

        // Method implementation
        self.expect(TokenType::Func, "at start of method definition in impl block")?;
        let name = self.parse_identifier()?;

        self.expect(TokenType::LeftParen, "after method name")?;
        let params = self.parse_params()?;
        self.expect(TokenType::RightParen, "after method parameters")?;

        let return_type = if self.eat(TokenType::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        if is_interface_impl {
            Ok(ImplMember::Method(InterfaceMethodImpl {
                name,
                params,
                return_type,
                body,
                span: self.span_from(start),
            }))
        } else {
            Ok(ImplMember::InherentMethod(MethodItem {
                attributes: Vec::new(),
                visibility: Visibility::Public,
                name,
                params,
                return_type,
                body,
                span: self.span_from(start),
            }))
        }
    }
}
