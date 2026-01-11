use crate::ast::NodeId;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use zirael_utils::prelude::{Identifier, Span};

#[derive(Debug, Clone)]
pub struct ImportDecl {
  pub id: NodeId,
  pub path: Path,
  pub tail: Option<ImportTail>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportTail {
  Wildcard,
  Items(Vec<ImportSpec>),
}

#[derive(Debug, Clone)]
pub struct ImportSpec {
  pub id: NodeId,
  pub name: ImportName,
  pub alias: Option<Identifier>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportName {
  SelfValue,
  Ident(Identifier),
}

#[derive(Debug, Clone)]
pub struct Path {
  pub id: NodeId,
  pub root: Option<PathRoot>,
  pub segments: Vec<Identifier>,
  pub span: Span,
}

impl Path {
  pub fn construct_file(
    &self,
    root: PathBuf,
    current: PathBuf,
  ) -> Option<PathBuf> {
    let mut path = match self.root {
      Some(PathRoot::Package) => root,
      Some(PathRoot::SelfMod) | None => current.parent()?.to_path_buf(),
      Some(PathRoot::Super) => current.parent()?.parent()?.to_path_buf(),
    };

    for part in &self.segments {
      let text = part.text();

      if text == "super" {
        path = path.parent()?.to_path_buf();
      } else {
        path = path.join(text);
      }
    }

    Some(path.with_extension("zr"))
  }
}

impl Display for Path {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(root) = &self.root {
      match root {
        PathRoot::Super => write!(f, "super")?,
        PathRoot::SelfMod => write!(f, "self")?,
        PathRoot::Package => write!(f, "package")?,
      }
    }

    for segment in &self.segments {
      write!(f, "::{}", segment.text())?;
    }

    Ok(())
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathRoot {
  /// Absolute from the package root
  Package,
  /// Relative from current directory
  SelfMod,
  /// Relative from the parent
  Super,
}
