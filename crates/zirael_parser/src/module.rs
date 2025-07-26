use crate::{ast::Ast, get_tokens, parser::Parser};
use zirael_utils::{
    prelude::{ReportBuilder, Reports},
    sources::{SourceFileId, Sources},
};

#[derive(Clone, Debug)]
/// This is only used during lexing and parsing process to determine the basic AST and relations between files.
pub struct LexedModule {
    pub file: SourceFileId,
    pub ast: Ast,
}

impl LexedModule {
    pub fn new(file: SourceFileId, ast: Ast) -> Self {
        Self { file, ast }
    }
}

pub fn determine_lexed_modules<'a>(
    entrypoint: SourceFileId,
    sources: Sources,
    reports: Reports<'a>,
) -> Vec<LexedModule> {
    let mut mods = vec![];

    let mut process = |source_file_id: SourceFileId| {
        let mut parser = Parser::new(sources.get_unchecked(source_file_id).content());
        mods.push(parser.parse(source_file_id));
        for report in parser.errors {
            reports.add(source_file_id, report.clone())
        }
    };

    process(entrypoint);
    mods
}
