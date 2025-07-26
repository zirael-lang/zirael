use crate::prelude::{SourceFileId, Sources, SourcesImpl};
use ariadne::{Color, Label, ReportKind, Source};
use parking_lot::RwLock;
use std::{collections::HashMap, ops::Range, path::PathBuf, sync::Arc};

#[derive(Debug, Clone, Default)]
pub struct Reports<'a>(Arc<RwLock<ReportsImpl<'a>>>);

pub type Report<'a> = ariadne::Report<'a, (String, Range<usize>)>;
#[derive(Debug, Default)]
pub struct ReportsImpl<'a> {
    pub reports: HashMap<SourceFileId, ReportBuilder<'a>>,
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
            reports.reports.insert(source_id, report);
        });
    }

    pub fn print(&self, sources: &Sources) {
        self.read(|reports| {
            for (id, report) in &reports.reports {
                let file = sources.get_unchecked(*id);
                let path = file.path().unwrap_or(&PathBuf::new()).clone();
                let path = path.display().to_string();
                let source = Source::from(file.content());

                let report = report.clone().build(path.clone());
                report.eprint((path, source)).unwrap();
            }
        })
    }

    pub fn has_errors(&self) -> bool {
        self.read(|reports| {
            reports.reports.iter().any(|(_, report)| report.kind == ReportKind::Error)
        })
    }
}

#[derive(Debug, Clone)]
pub struct LocalLabel {
    msg: String,
    span: Range<usize>,
    color: Color,
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
            ReportKind::Advice => Color::BrightGreen,
            _ => Color::BrightWhite,
        }
    }

    pub fn label(mut self, msg: &str, span: Range<usize>) -> Self {
        let label = LocalLabel { msg: msg.to_string(), span, color: self.label_color() };
        self.labels.push(label);
        self
    }

    pub fn note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    pub fn build(self, path: String) -> Report<'a> {
        let mut report =
            Report::build(self.kind, (path.clone(), 0usize..0usize)).with_message(self.message);

        let labels = self
            .labels
            .into_iter()
            .map(|l| Label::new((path.clone(), l.span)).with_message(l.msg).with_color(l.color))
            .collect::<Vec<_>>();

        report = report.with_labels(labels);
        report.with_notes(self.notes);

        report.finish()
    }
}
