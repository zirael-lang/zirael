use crate::{
    Token, TokenKind,
    ast::{Keyword, Type},
    parser::Parser,
};
use zirael_utils::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_type(&mut self) -> Option<Type> {
        self.parse_type_with_precedence()
    }

    fn parse_type_with_precedence(&mut self) -> Option<Type> {
        self.parse_primary_type().and_then(|ty| self.parse_type_postfix(ty))
    }

    fn parse_primary_type(&mut self) -> Option<Type> {
        if self.match_keyword(Keyword::Int) {
            return Some(Type::Int);
        } else if self.match_keyword(Keyword::Uint) {
            return Some(Type::Uint);
        } else if self.match_keyword(Keyword::Bool) {
            return Some(Type::Bool);
        } else if self.match_keyword(Keyword::Void) {
            return Some(Type::Void);
        } else if self.match_keyword(Keyword::String) {
            return Some(Type::String);
        } else if self.match_keyword(Keyword::Char) {
            return Some(Type::Char);
        } else if self.match_keyword(Keyword::Float) {
            return Some(Type::Float);
        }

        if self.match_token(TokenKind::BracketOpen) {
            if let Some(element_type) = self.parse_type() {
                if self.match_token(TokenKind::Semicolon) {
                    // Fixed-size array [T; N]
                    if let Some(Token { kind: TokenKind::Integer(size), .. }) = self.advance() {
                        if self.expect(TokenKind::BracketClose).is_some() {
                            return Some(Type::Array(Box::new(element_type), Some(size as usize)));
                        }
                    } else {
                        self.error_at_current("expected array size");
                        self.synchronize(&[TokenKind::BracketClose]);
                        self.match_token(TokenKind::BracketClose);
                    }
                } else if self.match_token(TokenKind::BracketClose) {
                    // Dynamic array [T]
                    return Some(Type::Array(Box::new(element_type), None));
                } else {
                    self.error_at_current("expected ';' or ']' in array type");
                    self.synchronize(&[TokenKind::BracketClose]);
                    self.match_token(TokenKind::BracketClose);
                }
            } else {
                self.error_at_current("expected element type in array");
                self.synchronize(&[TokenKind::BracketClose]);
                self.match_token(TokenKind::BracketClose);
            }
            return Some(Type::Error);
        }

        if self.match_token(TokenKind::BitwiseAnd) {
            if let Some(inner_type) = self.parse_type() {
                return Some(Type::Reference(Box::new(inner_type)));
            }
            self.error_at_current("expected type after '&'");
            return Some(Type::Error);
        }

        if self.match_token(TokenKind::Multiply) {
            if let Some(inner_type) = self.parse_type() {
                return Some(Type::Pointer(Box::new(inner_type)));
            }
            self.error_at_current("expected type after '*'");
            return Some(Type::Error);
        }

        if self.match_keyword(Keyword::Fn) {
            return self.parse_function_type();
        }

        if self.match_token(TokenKind::Underscore) {
            return Some(Type::Inferred);
        }

        if let Some(ident) = self.try_parse(|parser| {
            parser.expect_identifier().ok_or_else(|| {
                ReportBuilder::builder("expected type identifier", ReportKind::Error)
                    .label("here", parser.eof_span())
            })
        }) {
            return Some(Type::Named { name: ident, generics: vec![] });
        }

        if let Some(token) = self.peek() {
            self.error_at_peek(format!("unexpected token in type context: {:?}", token.kind));
        } else {
            self.error_at_peek("unexpected end of input while parsing type");
        }

        None
    }

    fn parse_type_postfix(&mut self, mut base_type: Type) -> Option<Type> {
        loop {
            if self.match_token(TokenKind::LessThan) {
                if let Type::Named { name, .. } = base_type {
                    let mut generics = vec![];

                    if !self.match_token(TokenKind::GreaterThan) {
                        loop {
                            if let Some(ty) = self.parse_type() {
                                generics.push(ty);
                            } else {
                                self.error_at_current("expected type in generic argument list");
                                self.synchronize(&[TokenKind::Comma, TokenKind::GreaterThan]);
                                if self.check(&TokenKind::GreaterThan) {
                                    break;
                                }
                                if !self.check(&TokenKind::Comma) {
                                    break;
                                }
                            }

                            if self.match_token(TokenKind::Comma) {
                                if self.check(&TokenKind::GreaterThan) {
                                    break;
                                }
                                continue;
                            } else if self.match_token(TokenKind::GreaterThan) {
                                break;
                            } else {
                                self.error_at_current(
                                    "expected ',' or '>' in type generic arguments",
                                );
                                self.synchronize(&[TokenKind::GreaterThan]);
                                self.match_token(TokenKind::GreaterThan);
                                break;
                            }
                        }
                    }

                    base_type = Type::Named { name, generics };
                } else {
                    self.error_at_current("generic arguments can only be applied to named types");
                    return None;
                }
            } else {
                break;
            }
        }

        Some(base_type)
    }

    fn parse_function_type(&mut self) -> Option<Type> {
        self.expect(TokenKind::ParenOpen)?;

        let mut params = vec![];

        if !self.check(&TokenKind::ParenClose) {
            loop {
                if let Some(param_type) = self.parse_type() {
                    params.push(param_type);
                } else {
                    self.error_at_current("expected parameter type in function type");
                    return None;
                }

                if self.match_token(TokenKind::Comma) {
                    if self.check(&TokenKind::ParenClose) {
                        break;
                    }
                    continue;
                } else {
                    break;
                }
            }
        }

        self.expect(TokenKind::ParenClose)?;

        let return_type = (|| {
            if self.match_token(TokenKind::Arrow) {
                let Some(ty) = self.parse_type() else {
                    return Box::new(Type::Void);
                };
                Box::new(ty)
            } else {
                Box::new(Type::Void)
            }
        })();

        Some(Type::Function { params, return_type })
    }
}
