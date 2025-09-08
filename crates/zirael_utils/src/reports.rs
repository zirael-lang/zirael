use crate::prelude::{SourceFileId, Sources, Span};
use ariadne::{CharSet, Color, Config, IndexType, Label, LabelAttach, ReportKind, sources};
use log::error;
use parking_lot::RwLock;
use std::{collections::HashMap, ops::Range, path::PathBuf, process::exit, sync::Arc};

#[derive(Debug, Clone, Default)]
pub struct Reports<'a>(Arc<RwLock<ReportsImpl<'a>>>);

pub type Report<'a> = ariadne::Report<'a, (String, Range<usize>)>;
#[derive(Debug, Default)]
pub struct ReportsImpl<'a> {
  pub reports: HashMap<SourceFileId, Vec<ReportBuilder<'a>>>,
}

impl<'a> Reports<'a> {
  pub fn new() -> Self {
    Default::default()
  }

  fn read<R>(&self, reader: impl FnOnce(&ReportsImpl<'a>) -> R) -> R {
    reader(&self.0.read())
  }

  fn write<R>(&self, writer: impl FnOnce(&mut ReportsImpl<'a>) -> R) -> R {
    writer(&mut self.0.write())
  }

  pub fn add(&self, source_id: SourceFileId, report: ReportBuilder<'a>) {
    self.write(|reports| {
      reports.reports.entry(source_id).or_default().push(report);
    });
  }

  pub fn print(&self, srcs: &Sources) {
    self.read(|reports| {
      let mut collected_sources = vec![];

      for (_, source) in srcs.all() {
        collected_sources.push((source.path().display().to_string(), source.content().to_owned()));
      }

      for (id, report) in &reports.reports {
        let file = srcs.get_unchecked(*id);
        let path = file.path();
        let path = &path.display().to_string();

        for report in report {
          let report = report.clone().build(path);
          report.eprint(sources(collected_sources.clone())).unwrap();
        }
      }
    });

    if self.has_errors() {
      error!("exiting early due to compiler errors");
      exit(1);
    } else {
      self.write(|reports| {
        reports.reports.clear();
      });
    }
  }

  pub fn has_errors(&self) -> bool {
    self.read(|reports| {
      reports.reports.iter().any(|(_, report)| report.iter().any(|r| r.kind == ReportKind::Error))
    })
  }
}

#[derive(Debug, Clone)]
pub struct LocalLabel {
  msg: String,
  span: Span,
  color: Color,
  custom_file: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct ReportBuilder<'a> {
  kind: ReportKind<'a>,
  message: String,
  labels: Vec<LocalLabel>,
  notes: Vec<String>,
}

impl<'a> ReportBuilder<'a> {
  pub fn builder(message: impl Into<String>, kind: ReportKind<'a>) -> Self {
    Self { kind, message: message.into(), labels: Vec::new(), notes: Vec::new() }
  }

  fn label_color(&self) -> Color {
    match self.kind {
      ReportKind::Error => Color::BrightRed,
      ReportKind::Warning => Color::BrightYellow,
      ReportKind::Advice => Color::BrightCyan,
      ReportKind::Custom(..) => Color::BrightWhite,
    }
  }

  pub fn label(mut self, msg: &str, span: Span) -> Self {
    let label =
      LocalLabel { msg: msg.to_owned(), span, color: self.label_color(), custom_file: None };
    self.labels.push(label);
    self
  }

  pub fn label_color_custom(mut self, msg: &str, span: Span, color: Color) -> Self {
    let label = LocalLabel { msg: msg.to_owned(), span, color, custom_file: None };
    self.labels.push(label);
    self
  }

  pub fn label_custom(
    mut self,
    msg: &str,
    span: Span,
    file: &PathBuf,
    color: Color,
  ) -> Self {
    let label = LocalLabel { msg: msg.to_owned(), span, color, custom_file: Some(file.to_owned()) };
    self.labels.push(label);
    self
  }

  pub fn note(mut self, note: &str) -> Self {
    self.notes.push(note.to_owned());
    self
  }

  pub fn custom_kind(&self) -> ReportKind<'a> {
    match self.kind {
      ReportKind::Error => ReportKind::Custom("error", Color::BrightRed),
      ReportKind::Warning => ReportKind::Custom("warning", Color::BrightYellow),
      ReportKind::Advice => ReportKind::Custom("advice", Color::BrightCyan),
      ReportKind::Custom(..) => self.kind,
    }
  }

  pub fn message(mut self, message: impl Into<String>) -> Self {
    self.message = message.into();
    self
  }

  pub fn build(self, path: &str) -> Report<'a> {
    let mut report = Report::build(self.custom_kind(), (path.to_owned(), 0usize..0usize))
      .with_message(self.message);

    let labels = self
      .labels
      .into_iter()
      .map(|l| {
        let path =
          l.custom_file.map(|p| p.display().to_string()).unwrap_or_else(|| path.to_owned());

        Label::new((path, l.span.into())).with_message(l.msg).with_color(l.color)
      })
      .collect::<Vec<_>>();

    report = report.with_labels(labels);
    report.with_notes(self.notes);

    report
      .with_config(
        Config::default()
          .with_char_set(CharSet::Ascii)
          .with_index_type(IndexType::Char)
          .with_label_attach(LabelAttach::End),
      )
      .finish()
  }
}
