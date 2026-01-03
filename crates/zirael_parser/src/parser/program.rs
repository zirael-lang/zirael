use crate::items::Item;
use crate::parser::Parser;
use crate::parser::errors::ModStringLit;
use crate::parser::parser::ITEM_TOKENS;
use crate::{
  ItemKind, ModItem, NodeId, ProgramNode, TokenType, Visibility,
  log_parse_failure,
};
use zirael_utils::prelude::debug;

impl Parser<'_> {
  pub fn parse_program(&mut self) -> Option<ProgramNode> {
    let mut items: Vec<Item> = vec![];

    while !self.is_at_end() {
      if let Some(item) = self.parse_item() {
        items.push(item);
      } else {
        self.synchronize_to_next_item();
        self.eat_semis();

        if !self.is_at_end() && !ITEM_TOKENS.contains(&self.peek().kind) {
          self.advance();
        }
      }
    }

    Some(ProgramNode {
      id: NodeId::new(),
      attributes: vec![],
      imports: vec![],
      items,
    })
  }

  fn check_possible_comment(&mut self) {
    while let TokenType::DocComment(comment) = &self.peek().kind {
      self.push_comment(comment.clone());
      self.advance();
    }
  }

  /// tries to find next item to start parsing from.
  fn synchronize_to_next_item(&mut self) {
    self.advance_until_one_of(ITEM_TOKENS);
  }

  fn parse_mod(&mut self) -> Option<ModItem> {
    let span_start = self.previous().span;
    if let TokenType::StringLiteral(_) = &self.peek().kind {
      self.emit(ModStringLit {
        span: self.peek().span,
      });

      return None;
    }
    let path = self.parse_path();

    Some(ModItem {
      id: NodeId::new(),
      span: self.span_from(span_start),
      path,
    })
  }

  fn parse_item(&mut self) -> Option<Item> {
    self.doc_comment = None;
    self.check_possible_comment();

    let span_start = self.peek().span;
    let visibility = if let Some(t) = self.is(TokenType::Pub) {
      Visibility::Public(t.span)
    } else {
      Visibility::Private
    };

    let token = self.expect_any(ITEM_TOKENS, "as an item beginning")?.kind;
    let kind = match token {
      TokenType::Mod => {
        ItemKind::Mod(log_parse_failure!(self.parse_mod(), "module item")?)
      }
      _ => unreachable!(),
    };

    self.eat_semis();

    Some(Item {
      id: NodeId::new(),
      kind,
      // TODO: attributes parsing
      attributes: vec![],
      span: self.span_from(span_start),
      visibility,
      doc_comments: self.doc_comment.clone(),
    })
  }
}
