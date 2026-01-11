use crate::ast::NodeId;
use crate::{GenericParams, Type};
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use zirael_diagnostics::DiagnosticCtx;
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
  pub segments: Vec<PathSegment>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
  pub identifier: Identifier,
  pub args: Vec<Type>,
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
      if !part.args.is_empty() {
        break;
      }
      
      let text = part.identifier.text();

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
      write!(f, "{root}")?;
    }

    for segment in &self.segments {
      write!(f, "::{}", segment.identifier.text())?;
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

impl Display for PathRoot {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      PathRoot::Super => write!(f, "super"),
      PathRoot::SelfMod => write!(f, "self"),
      PathRoot::Package => write!(f, "package"),
    }
  }
}
