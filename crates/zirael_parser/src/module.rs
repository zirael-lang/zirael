use crate::{ast::Ast, parser::Parser};
use ariadne::ReportKind;
use std::collections::HashSet;
use zirael_utils::prelude::*;

#[derive(Clone, Debug)]
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
    sources: &Sources,
    reports: &Reports<'a>,
) -> Vec<LexedModule> {
    debug!("starting module discovery from entrypoint: {:?}", entrypoint);

    let mut all_modules = Vec::new();
    let processed = Mutex::new(HashSet::new());
    let mut current_wave = vec![entrypoint];
    let mut wave_number = 1;

    while !current_wave.is_empty() {
        let wave_results: Vec<_> = current_wave
            .into_par_iter()
            .filter_map(|file| {
                {
                    let mut proc = processed.lock();
                    if proc.contains(&file) {
                        debug!("file {:?} already processed, skipping", file);
                        return None;
                    }
                    proc.insert(file);
                }
                let mut parser = Parser::new(sources.get_unchecked(file));
                let result = parser.parse(file);

                let mut discovered_files = Vec::new();
                for (path, span) in parser.discover_queue {
                    let contents = fs_err::read_to_string(path.clone());
                    let Ok(contents) = contents else {
                        let err = contents.unwrap_err();
                        debug!("failed to read dependency file: {}", path.display());
                        reports.add(
                            file,
                            ReportBuilder::builder(
                                format!(
                                    "Failed to read contents of {} while discovering dependencies: {}",
                                    path.display().to_string().dimmed(),
                                    err
                                ),
                                ReportKind::Error,
                            )
                                .label("failed while resolving this file", span),
                        );
                        continue;
                    };
                    let file_id = sources.add_owned(contents, Some(path.clone()));
                    debug!("discovered dependency: {} -> {:?}", path.display(), file_id);
                    discovered_files.push(file_id);
                }

                for report in parser.errors {
                    reports.add(file, report.clone());
                }

                Some((result, discovered_files))
            })
            .collect();

        let mut next_wave = Vec::new();
        for (module, discovered) in wave_results {
            all_modules.push(module);
            next_wave.extend(discovered);
        }

        next_wave.sort_unstable();
        next_wave.dedup();

        debug!(
            "Wave {} completed. Processed {} modules, discovered {} new files for next wave",
            wave_number,
            all_modules.len(),
            next_wave.len()
        );

        current_wave = next_wave;
        wave_number += 1;
    }

    debug!("Module discovery completed. Total modules processed: {}", all_modules.len());
    all_modules
}
