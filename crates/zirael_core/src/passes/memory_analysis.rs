use crate::prelude::WalkerWithAst;
use zirael_parser::{AstWalker, SymbolTable, impl_ast_pass};
use zirael_utils::prelude::{Reports, SourceFileId, Sources};

impl_ast_pass!(MemoryAnalysis);

impl<'reports> AstWalker for MemoryAnalysis<'reports> {}
