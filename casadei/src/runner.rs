use crate::app::AppState;
use crate::directives::{Directive, LineDirection};
use crate::output::{FailureType, TestResult, TestStatus};
use crate::test::{Test, TestId};
use parking_lot::Mutex;
use std::io::{Cursor, Write};
use std::sync::Arc;
use zirael_core::prelude::*;

pub struct TestRunner<'tests> {
  pub tests: &'tests Vec<Test>,
  pub state: Arc<Mutex<AppState>>,
}

impl<'tests> TestRunner<'tests> {
  pub fn new(tests: &'tests Vec<Test>, state: Arc<Mutex<AppState>>) -> Self {
    Self { tests, state }
  }

  pub fn run_with_progress(&mut self) -> color_eyre::Result<()> {
    self.tests.par_iter().for_each(|test| {
      let result = self.run_single_test(test);

      let lock = &mut self.state.lock();
      lock.increment_completed();
      lock.add_result(result);
    });

    Ok(())
  }

  fn run_single_test(&'tests self, test: &'tests Test) -> TestResult {
    let project_type = test
      .directives
      .iter()
      .find_map(|directive| match directive {
        Directive::PackageType(package_type) => Some(*package_type),
        _ => None,
      })
      .unwrap_or(PackageType::Binary);

    let output = Arc::new(Mutex::new(Cursor::new(vec![])));
    let result = check_project(
      &ProjectConfig {
        entrypoint: test.path.clone(),
        project_type,
        packages: vec![],
        mode: Mode::Debug,
        name: format!("test-{}", test.id.0),
        lib_type: LibType::Static,
        output: Default::default(),
        root: test.path.parent().unwrap().to_path_buf(),
        diagnostic_output_type: DiagnosticOutputType::HumanReadable,
        color: true,
      },
      output.clone(),
    );

    let result = if let Ok(sess) = result {
      let sources = sess.dcx().sources();

      if sess.dcx().has_errors() {
        let mut failures = vec![];

        let error_directives: Vec<_> = test
          .directives
          .iter()
          .filter_map(|d| match d {
            Directive::Error { line, direction, pattern } => {
              Some((*line, direction.clone(), pattern.clone()))
            }
            _ => None,
          })
          .collect();

        let mut matched_directives = vec![false; error_directives.len()];

        for diagnostic in sess.dcx().diagnostics.iter() {
          let diag = diagnostic.value();

          for (idx, (directive_line, direction, pattern)) in error_directives.iter().enumerate() {
            if matched_directives[idx] {
              continue;
            }

            if matches_directive(diag, *directive_line, direction, pattern, &sources) {
              matched_directives[idx] = true;
              break;
            }
          }
        }

        for (idx, matched) in matched_directives.iter().enumerate() {
          if !*matched {
            let (line, direction, pattern) = &error_directives[idx];
            failures.push(FailureType::ExpectedErrorNotFound {
              line: *line,
              direction: direction.clone(),
              pattern: pattern.clone(),
            });
          }
        }

        if failures.is_empty() { TestStatus::Passed } else { TestStatus::Failed(failures) }
      } else {
        let has_error_directives =
          test.directives.iter().any(|d| matches!(d, Directive::Error { .. }));

        if has_error_directives {
          TestStatus::Failed(vec![FailureType::ExpectedErrorsButCompiled])
        } else {
          TestStatus::Passed
        }
      }
    } else {
      TestStatus::Failed(vec![FailureType::OtherCompilerError])
    };

    TestResult { result, output: output.lock().get_ref().clone(), test_id: test.id }
  }
}

fn matches_directive(
  diagnostic: &Diagnostic,
  directive_line: usize,
  direction: &LineDirection,
  pattern: &str,
  sources: &Sources,
) -> bool {
  let Some(diag_line) = get_diagnostic_line(diagnostic, sources) else {
    return false;
  };

  let line_matches = match direction {
    LineDirection::Up => diag_line <= directive_line,
    LineDirection::Down => diag_line >= directive_line,
  };

  if !line_matches {
    return false;
  }

  matches_pattern(&diagnostic.diag.message, pattern)
}

fn get_diagnostic_line(diagnostic: &Diagnostic, sources: &Sources) -> Option<usize> {
  diagnostic.diag.labels.iter().find_map(|label| {
    let file_id = label.file();
    let src = sources.get(file_id)?;
    src.get_byte_line(label.span.start).map(|(_, line_idx, _)| line_idx)
  })
}

fn matches_pattern(message: &str, pattern: &str) -> bool {
  let parts: Vec<&str> = pattern.split("{}").collect();

  if parts.len() == 1 {
    return message.to_lowercase().contains(&pattern.to_lowercase());
  }

  let mut pos = 0;
  for (i, part) in parts.iter().enumerate() {
    if part.is_empty() {
      continue;
    }

    if i == 0 {
      if !message[pos..].starts_with(part) {
        return false;
      }
      pos += part.len();
    } else if i == parts.len() - 1 {
      if !message[pos..].ends_with(part) {
        return false;
      }
    } else {
      if let Some(found_pos) = message[pos..].find(part) {
        pos += found_pos + part.len();
      } else {
        return false;
      }
    }
  }

  true
}
