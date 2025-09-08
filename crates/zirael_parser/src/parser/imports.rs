use crate::parser::Parser;
use crate::{ImportKind, ItemKind, TokenKind};
use std::path::{Component, Path, PathBuf};
use zirael_utils::ident_table::{Identifier, default_ident, get_or_intern};
use zirael_utils::prelude::Span;

impl<'reports> Parser<'reports> {
  pub fn parse_import(&mut self) -> (ItemKind, Identifier) {
    let string = self.expect_string().unwrap_or_default();
    let span = self.prev_span();

    let kind = if Self::is_path_import(&string) {
      self.handle_path_import(string, &span)
    } else {
      self.handle_external_module_import(string)
    };

    self.match_token(TokenKind::Semicolon);

    let final_span = span.to(self.prev_span());
    (ItemKind::Import(kind, final_span), default_ident())
  }

  fn is_path_import(import_string: &str) -> bool {
    let path = Path::new(import_string);

    path.is_absolute()
      || path
        .components()
        .any(|component| matches!(component, Component::CurDir | Component::ParentDir))
      || import_string.starts_with("./")
      || import_string.starts_with("../")
      || import_string.starts_with(".\\")
      || import_string.starts_with("..\\")
  }

  fn handle_path_import(&mut self, string: String, span: &Span) -> ImportKind {
    let current_file = self.source.path();
    let base_dir = current_file.parent().unwrap_or(Path::new(""));
    let path = self.canonicalize_path(&base_dir.join(&string), span.clone());

    if self.is_valid_zr_file(&path) {
      self.discover_queue.push((path.clone(), span.clone()));
      ImportKind::Path(path)
    } else {
      let error_msg = self.get_path_error_message(&path);
      self.error_at(error_msg, span.clone());
      ImportKind::Path(path)
    }
  }

  fn handle_external_module_import(&mut self, string: String) -> ImportKind {
    println!("external module import: {}", string);
    let parts = string.split('/').map(|x| get_or_intern(x, None)).collect::<Vec<_>>();
    ImportKind::ExternalModule(parts)
  }

  fn is_valid_zr_file(&self, path: &Path) -> bool {
    path.is_file() && path.extension().is_some_and(|ext| ext == "zr")
  }

  fn get_path_error_message(&self, path: &Path) -> String {
    if !path.exists() {
      format!("couldn't find file: {}", path.display())
    } else if !path.is_file() {
      format!("import path is not a file: {}", path.display())
    } else {
      format!("import file must have .zr extension: {}", path.display())
    }
  }
}
