use crate::fmt::Fmt;
use crate::show::Show;
use crate::{Diag, DiagnosticLevel, Label};
use std::io;
use std::io::{Stderr, Write, stderr};
use std::ops::Range;
use std::sync::Arc;
use unicode_width::UnicodeWidthChar;
use yansi::Color;
use zirael_source::arena::source_file::SourceFileId;
use zirael_source::prelude::Sources;
use zirael_source::span::Span;

pub trait Emitter: std::fmt::Debug {
  fn emit_diagnostic(&self, diag: &Diag) -> anyhow::Result<()>;
}

#[derive(Debug)]
pub struct Characters {
  pub hbar: char,
  pub vbar: char,
  pub xbar: char,
  pub vbar_break: char,
  pub vbar_gap: char,

  pub uarrow: char,
  pub rarrow: char,

  pub ltop: char,
  pub mtop: char,
  pub rtop: char,
  pub lbot: char,
  pub rbot: char,
  pub mbot: char,

  pub lbox: char,
  pub rbox: char,

  pub lcross: char,
  pub rcross: char,

  pub underbar: char,
  pub underline: char,
}

pub fn ascii() -> Characters {
  Characters {
    hbar: '-',
    vbar: '|',
    xbar: '+',
    vbar_break: '*',
    vbar_gap: ':',
    uarrow: '^',
    rarrow: '>',
    ltop: ',',
    mtop: 'v',
    rtop: '.',
    lbot: '`',
    mbot: '^',
    rbot: '\'',
    lbox: '[',
    rbox: ']',
    lcross: '|',
    rcross: '|',
    underbar: '|',
    underline: '^',
  }
}

#[derive(Debug)]
pub struct HumanReadableEmitter {
  characters: Characters,
  color: bool,
  sources: Arc<Sources>,
}

impl HumanReadableEmitter {
  pub fn new(sources: Arc<Sources>, color: bool) -> Self {
    Self { characters: ascii(), color, sources }
  }

  fn warning_color(&self) -> Option<Color> {
    Some(Color::Yellow).filter(|_| self.color)
  }
  fn advice_color(&self) -> Option<Color> {
    Some(Color::Fixed(147)).filter(|_| self.color)
  }
  fn margin_color(&self) -> Option<Color> {
    Some(Color::Fixed(246)).filter(|_| self.color)
  }
  fn skipped_margin_color(&self) -> Option<Color> {
    Some(Color::Fixed(240)).filter(|_| self.color)
  }
  fn unimportant_color(&self) -> Option<Color> {
    Some(Color::Fixed(249)).filter(|_| self.color)
  }
  fn note_color(&self) -> Option<Color> {
    Some(Color::Fixed(115)).filter(|_| self.color)
  }
  fn filter_color(&self, color: Option<Color>) -> Option<Color> {
    color.filter(|_| self.color)
  }

  fn char_width(&self, c: char, col: usize) -> (char, usize) {
    match c {
      '\t' => {
        // Find the column that the tab should end at
        let tab_end = (col / TAB_WIDTH + 1) * TAB_WIDTH;
        (' ', tab_end - col)
      }
      c if c.is_whitespace() => (' ', 1),
      _ => (c, c.width().unwrap_or(1)),
    }
  }

  fn get_source_groups<'a>(&self, diag: &'a Diag) -> Vec<SourceGroup<'a>> {
    let mut labels = Vec::new();
    for label in diag.labels.iter() {
      let label_source = label.span.file_id;

      let src_display = self.sources.display(label_source);
      let src = match self.sources.get(label_source) {
        Some(src) => src,
        None => {
          eprintln!("Unable to fetch source '{}'", Show(src_display));
          continue;
        }
      };

      let given_label_span = label.span.start()..label.span.end();

      let (label_char_span, start_line, end_line) = {
        let Some((start_line_obj, start_line, start_byte_col)) =
          src.get_byte_line(given_label_span.start)
        else {
          continue;
        };
        let line_text = src.get_line_text(start_line_obj).unwrap();

        let num_chars_before_start =
          line_text[..start_byte_col.min(line_text.len())].chars().count();
        let start_char_offset = start_line_obj.offset() + num_chars_before_start;

        if given_label_span.start >= given_label_span.end {
          (start_char_offset..start_char_offset, start_line, start_line)
        } else {
          // We can subtract 1 from end, because get_byte_line doesn't actually index into the text.
          let end_pos = given_label_span.end - 1;
          let Some((end_line_obj, end_line, end_byte_col)) = src.get_byte_line(end_pos) else {
            continue;
          };
          let end_line_text = src.get_line_text(end_line_obj).unwrap();
          // Have to add 1 back now, so we don't cut a char in two.
          let num_chars_before_end = end_line_text[..end_byte_col + 1].chars().count();
          let end_char_offset = end_line_obj.offset() + num_chars_before_end;

          (start_char_offset..end_char_offset, start_line, end_line)
        }
      };

      let label_info = LabelInfo {
        kind: if start_line == end_line { LabelKind::Inline } else { LabelKind::Multiline },
        char_span: label_char_span,
        start_line,
        end_line,
        info: label,
      };

      labels.push((label_info, label_source));
    }
    labels.sort_by_key(|(l, _)| (l.info.order, l.end_line, l.start_line));
    let mut groups: Vec<SourceGroup<'a>> = Vec::new();
    for (label, src_id) in labels {
      match groups.last_mut() {
        Some(group)
          if group.src_id == src_id
            && group.labels.last().map_or(true, |last| last.end_line <= label.end_line) =>
        {
          group.char_span.start = group.char_span.start.min(label.char_span.start);
          group.char_span.end = group.char_span.end.max(label.char_span.end);
          let display_range = label.display_range();
          group.display_range.start = group.display_range.start.min(display_range.start);
          group.display_range.end = group.display_range.end.max(display_range.end);
          group.labels.push(label);
        }
        _ => {
          groups.push(SourceGroup {
            src_id,
            char_span: label.char_span.clone(),
            display_range: label.display_range(),
            labels: vec![label],
          });
        }
      }
    }
    groups
  }
}

enum LabelKind {
  Inline,
  Multiline,
}

struct LabelInfo<'a> {
  kind: LabelKind,
  char_span: Range<usize>,
  start_line: usize,
  end_line: usize,
  info: &'a Label,
}

impl LabelInfo<'_> {
  fn last_offset(&self) -> usize {
    self.char_span.end.saturating_sub(1).max(self.char_span.start)
  }

  fn display_range(&self) -> Range<usize> {
    self.start_line.saturating_sub(CONTEXT_LINES)..self.end_line + CONTEXT_LINES + 1
  }
}

const CONTEXT_LINES: usize = 0;
const TAB_WIDTH: usize = 4;
const MULTILINE_ARROWS: bool = true;
const CROSS_GAPS: bool = true;
const UNDERLINES: bool = true;

struct SourceGroup<'a> {
  src_id: SourceFileId,
  char_span: Range<usize>,
  display_range: Range<usize>,
  labels: Vec<LabelInfo<'a>>,
}

impl Emitter for HumanReadableEmitter {
  fn emit_diagnostic(&self, diag: &Diag) -> anyhow::Result<()> {
    let draw = &self.characters;
    let mut w = stderr();

    // --- Header ---

    let code = diag.code.as_ref().map(|c| format!("[{}] ", c));
    let id = format!("{}{}:", Show(code), diag.level.name());
    let kind_color = diag.level.color();
    writeln!(w, "{} {}", id.fg(kind_color), diag.message)?;

    let groups = self.get_source_groups(diag);

    // Line number maximum width
    let line_no_width = groups
      .iter()
      .filter_map(|SourceGroup { char_span, src_id, .. }| {
        let src_name = self
          .sources
          .display(*src_id)
          .map(|d| d.to_string())
          .unwrap_or_else(|| "<unknown>".to_string());

        let src = match self.sources.get(*src_id) {
          Some(src) => src,
          None => {
            eprintln!("Unable to fetch source {}", src_name);
            return None;
          }
        };

        let line_range = src.get_line_range(char_span.clone());
        Some((1..).map(|x| 10u32.pow(x)).take_while(|x| line_range.end as u32 / x != 0).count() + 1)
      })
      .max()
      .unwrap_or(0);

    // --- Source sections ---
    let groups_len = groups.len();
    for (group_idx, SourceGroup { src_id, char_span, labels, .. }) in groups.into_iter().enumerate()
    {
      let src_name = self
        .sources
        .display(src_id)
        .map(|d| d.to_string())
        .unwrap_or_else(|| "<unknown>".to_string());

      let src = match self.sources.get(src_id) {
        Some(src) => src,
        None => {
          eprintln!("Unable to fetch source {}", src_name);
          continue;
        }
      };

      let line_range = src.get_line_range(char_span);

      // File name & reference
      let location = labels[0].char_span.start;
      let line_and_col = src.get_byte_line(location).map(|(line_obj, idx, col)| {
        let line_text = src.get_line_text(line_obj).unwrap();

        let col = line_text[..col.min(line_text.len())].chars().count();

        (line_obj, idx, col)
      });

      let (line_no, col_no) = line_and_col
        .map(|(_, idx, col)| {
          (format!("{}", idx + 1 + src.display_line_offset()), format!("{}", col + 1))
        })
        .unwrap_or_else(|| ('?'.to_string(), '?'.to_string()));
      let line_ref = format!("{}:{}:{}", src_name, line_no, col_no);

      writeln!(
        w,
        "{}{}{}{} {} {}",
        Show((' ', line_no_width + 2)),
        if group_idx == 0 { draw.ltop } else { draw.lcross }.fg(self.margin_color()),
        draw.hbar.fg(self.margin_color()),
        draw.lbox.fg(self.margin_color()),
        line_ref,
        draw.rbox.fg(self.margin_color()),
      )?;

      writeln!(w, "{}{}", Show((' ', line_no_width + 2)), draw.vbar.fg(self.margin_color()))?;

      struct LineLabel<'a> {
        col: usize,
        label: &'a LabelInfo<'a>,
        multi: bool,
        draw_msg: bool,
      }

      // Generate a list of multi-line labels
      let mut multi_labels = Vec::new();
      let mut multi_labels_with_message = Vec::new();
      for label_info in &labels {
        if matches!(label_info.kind, LabelKind::Multiline) {
          multi_labels.push(label_info);
          if label_info.info.message.is_some() {
            multi_labels_with_message.push(label_info);
          }
        }
      }

      // Sort multiline labels by length
      multi_labels
        .sort_by_key(|m| -(Span::no_file(m.char_span.start, m.char_span.end).len() as isize));
      multi_labels_with_message
        .sort_by_key(|m| -(Span::no_file(m.char_span.start, m.char_span.end).len() as isize));

      let write_margin = |w: &mut Stderr,
                          idx: usize,
                          is_line: bool,
                          is_ellipsis: bool,
                          draw_labels: bool,
                          report_row: Option<(usize, bool)>,
                          line_labels: &[LineLabel<'_>],
                          margin_label: &Option<LineLabel<'_>>|
       -> std::io::Result<()> {
        let line_no_margin = if is_line && !is_ellipsis {
          let line_no = format!("{}", idx + 1);
          format!(
            "{}{} {}",
            Show((' ', line_no_width - line_no.chars().count())),
            line_no,
            draw.vbar,
          )
          .fg(self.margin_color())
        } else {
          format!(
            "{}{}",
            Show((' ', line_no_width + 1)),
            if is_ellipsis { draw.vbar_gap } else { draw.vbar }
          )
          .fg(self.skipped_margin_color())
        };

        write!(w, " {}{}", line_no_margin, ' ')?;

        // Multi-line margins
        if draw_labels {
          for col in
            0..multi_labels_with_message.len() + (!multi_labels_with_message.is_empty()) as usize
          {
            let mut corner = None;
            let mut hbar: Option<&LabelInfo> = None;
            let mut vbar: Option<&LabelInfo> = None;
            let mut margin_ptr = None;

            let multi_label = multi_labels_with_message.get(col);
            let line_span = src.line(idx).unwrap().span();

            for (i, label) in multi_labels_with_message
              [0..(col + 1).min(multi_labels_with_message.len())]
              .iter()
              .enumerate()
            {
              let margin = margin_label.as_ref().filter(|m| std::ptr::eq(*label, m.label));

              if label.char_span.start <= line_span.end && label.char_span.end > line_span.start {
                let is_parent = i != col;
                let is_start = line_span.contains(label.char_span.start);
                let is_end = line_span.contains(label.last_offset());

                if let Some(margin) = margin.filter(|_| is_line) {
                  margin_ptr = Some((margin, is_start));
                } else if !is_start && (!is_end || is_line) {
                  vbar = vbar.or(Some(*label).filter(|_| !is_parent));
                } else if let Some((report_row, is_arrow)) = report_row {
                  let label_row = line_labels
                    .iter()
                    .enumerate()
                    .find(|(_, l)| std::ptr::eq(*label, l.label))
                    .map_or(0, |(r, _)| r);
                  if report_row == label_row {
                    if let Some(margin) = margin {
                      vbar = Some(margin.label).filter(|_| col == i);
                      if is_start {
                        continue;
                      }
                    }

                    if is_arrow {
                      hbar = Some(*label);
                      if !is_parent {
                        corner = Some((label, is_start));
                      }
                    } else if !is_start {
                      vbar = vbar.or(Some(*label).filter(|_| !is_parent));
                    }
                  } else {
                    vbar = vbar.or(
                      Some(*label).filter(|_| !is_parent && (is_start ^ (report_row < label_row))),
                    );
                  }
                }
              }
            }

            if let (Some((margin, _is_start)), true) = (margin_ptr, is_line) {
              let is_col = multi_label.map_or(false, |ml| std::ptr::eq(*ml, margin.label));
              let is_limit = col + 1 == multi_labels_with_message.len();
              if !is_col && !is_limit {
                hbar = hbar.or(Some(margin.label));
              }
            }

            hbar = hbar.filter(|l| {
              margin_label.as_ref().map_or(true, |margin| !std::ptr::eq(margin.label, *l))
                || !is_line
            });

            let (a, b) = if let Some((label, is_start)) = corner {
              (
                if is_start { draw.ltop } else { draw.lbot }.fg(label.info.color()),
                draw.hbar.fg(label.info.color()),
              )
            } else if let Some(label) = hbar.filter(|_| vbar.is_some() && !CROSS_GAPS) {
              (draw.xbar.fg(label.info.color()), draw.hbar.fg(label.info.color()))
            } else if let Some(label) = hbar {
              (draw.hbar.fg(label.info.color()), draw.hbar.fg(label.info.color()))
            } else if let Some(label) = vbar {
              (
                if is_ellipsis { draw.vbar_gap } else { draw.vbar }.fg(label.info.color()),
                ' '.fg(None),
              )
            } else if let (Some((margin, is_start)), true) = (margin_ptr, is_line) {
              let is_col = multi_label.map_or(false, |ml| std::ptr::eq(*ml, margin.label));
              let is_limit = col == multi_labels_with_message.len();
              (
                if is_limit {
                  draw.rarrow
                } else if is_col {
                  if is_start { draw.ltop } else { draw.lcross }
                } else {
                  draw.hbar
                }
                .fg(margin.label.info.color()),
                if !is_limit { draw.hbar } else { ' ' }.fg(margin.label.info.color()),
              )
            } else {
              (' '.fg(None), ' '.fg(None))
            };
            write!(w, "{}", a)?;
            write!(w, "{}", b)?;
          }
        }

        Ok(())
      };

      let mut is_ellipsis = false;
      for idx in line_range {
        let line = if let Some(line) = src.line(idx) {
          line
        } else {
          continue;
        };

        let margin_label = multi_labels_with_message
          .iter()
          .enumerate()
          .filter_map(|(_i, label)| {
            let is_start = line.span().contains(label.char_span.start);
            let is_end = line.span().contains(label.last_offset());
            if is_start {
              // TODO: Check to see whether multi is the first on the start line or first on the end line
              Some(LineLabel {
                col: label.char_span.start - line.offset(),
                label,
                multi: true,
                draw_msg: false, // Multi-line spans don;t have their messages drawn at the start
              })
            } else if is_end {
              Some(LineLabel {
                col: label.last_offset() - line.offset(),
                label,
                multi: true,
                draw_msg: true, // Multi-line spans have their messages drawn at the end
              })
            } else {
              None
            }
          })
          .min_by_key(|ll| (ll.col, !ll.label.char_span.start));

        // Generate a list of labels for this line, along with their label columns
        let mut line_labels = multi_labels_with_message
          .iter()
          .enumerate()
          .filter_map(|(_i, label)| {
            let is_start = line.span().contains(label.char_span.start);
            let is_end = line.span().contains(label.last_offset());
            if is_start && margin_label.as_ref().map_or(true, |m| !std::ptr::eq(*label, m.label)) {
              // TODO: Check to see whether multi is the first on the start line or first on the end line
              Some(LineLabel {
                col: label.char_span.start - line.offset(),
                label,
                multi: true,
                draw_msg: false, // Multi-line spans don;t have their messages drawn at the start
              })
            } else if is_end {
              Some(LineLabel {
                col: label.last_offset() - line.offset(),
                label,
                multi: true,
                draw_msg: true, // Multi-line spans have their messages drawn at the end
              })
            } else {
              None
            }
          })
          .collect::<Vec<_>>();

        for label_info in labels
          .iter()
          .filter(|l| l.char_span.start >= line.span().start && l.char_span.end <= line.span().end)
        {
          if matches!(label_info.kind, LabelKind::Inline) {
            line_labels.push(LineLabel {
              col: ((label_info.char_span.start + label_info.char_span.end) / 2)
                .max(label_info.char_span.start)
                - line.offset(),
              label: label_info,
              multi: false,
              draw_msg: true,
            });
          }
        }

        // Skip this line if we don't have labels for it
        if line_labels.is_empty() && margin_label.is_none() {
          let within_label =
            multi_labels.iter().any(|label| label.char_span.contains(&line.span().start()));
          if !is_ellipsis && within_label {
            is_ellipsis = true;
          } else {
            if !is_ellipsis {
              write_margin(&mut w, idx, false, is_ellipsis, false, None, &[], &None)?;
              writeln!(w)?;
            }
            is_ellipsis = true;
            continue;
          }
        } else {
          is_ellipsis = false;
        }

        // Sort the labels by their columns
        line_labels.sort_by_key(|ll| (ll.label.info.order, ll.col, !ll.label.char_span.start));

        // Determine label bounds so we know where to put error messages
        let arrow_end_space = 2;
        let arrow_len = line_labels.iter().fold(0, |l, ll| {
          if ll.multi {
            line.len()
          } else {
            l.max(ll.label.char_span.end.saturating_sub(line.offset()))
          }
        }) + arrow_end_space;

        // Should we draw a vertical bar as part of a label arrow on this line?
        let get_vbar = |col, row| {
          line_labels
            .iter()
            // Only labels with notes get an arrow
            .enumerate()
            .filter(|(_, ll)| {
              ll.label.info.message.is_some()
                && margin_label.as_ref().map_or(true, |m| !std::ptr::eq(ll.label, m.label))
            })
            .find(|(j, ll)| ll.col == col && row <= *j)
            .map(|(_, ll)| ll)
        };

        let get_highlight = |col| {
          margin_label
            .iter()
            .map(|ll| &ll.label)
            .chain(multi_labels.iter())
            .chain(line_labels.iter().map(|l| &l.label))
            .filter(|l| l.char_span.contains(&(line.offset() + col)))
            // Prioritise displaying smaller spans
            .min_by_key(|l| (-l.info.priority, ExactSizeIterator::len(&l.char_span)))
        };

        let get_underline = |col| {
          line_labels
            .iter()
            .filter(|ll| {
              UNDERLINES

                                // Underlines only occur for inline spans (highlighting can occur for all spans)
                                && !ll.multi
                                && ll.label.char_span.contains(&(line.offset() + col))
            })
            // Prioritise displaying smaller spans
            .min_by_key(|ll| (-ll.label.info.priority, ExactSizeIterator::len(&ll.label.char_span)))
        };

        // Margin

        write_margin(&mut w, idx, true, is_ellipsis, true, None, &line_labels, &margin_label)?;

        // Line
        if !is_ellipsis {
          for (col, c) in src.get_line_text(line).unwrap().trim_end().chars().enumerate() {
            let color = if let Some(highlight) = get_highlight(col) {
              highlight.info.color().filter(|_| !self.color)
            } else {
              self.unimportant_color()
            };
            let (c, width) = self.char_width(c, col);
            if c.is_whitespace() {
              for _ in 0..width {
                write!(w, "{}", c.fg(color))?;
              }
            } else {
              write!(w, "{}", c.fg(color))?;
            };
          }
        }
        writeln!(w)?;

        // Arrows
        for row in 0..line_labels.len() {
          let line_label = &line_labels[row];
          //No message to draw thus no arrow to draw
          if line_label.label.info.message.is_none() {
            continue;
          }
          // Margin alternate
          write_margin(
            &mut w,
            idx,
            false,
            is_ellipsis,
            true,
            Some((row, false)),
            &line_labels,
            &margin_label,
          )?;
          // Lines alternate
          let mut chars = src.get_line_text(line).unwrap().trim_end().chars();
          for col in 0..arrow_len {
            let width = chars.next().map_or(1, |c| self.char_width(c, col).1);

            let vbar = get_vbar(col, row);
            let underline = get_underline(col).filter(|_| row == 0);
            let [c, tail] = if let Some(vbar_ll) = vbar {
              let [c, tail] = if underline.is_some() {
                // TODO: Is this good?
                // The `true` is used here because it's temporarily disabling a
                // feature that might be reenabled later.
                #[allow(clippy::overly_complex_bool_expr)]
                if ExactSizeIterator::len(&vbar_ll.label.char_span) <= 1 || true {
                  [draw.underbar, draw.underline]
                } else if line.offset() + col == vbar_ll.label.char_span.start {
                  [draw.ltop, draw.underbar]
                } else if line.offset() + col == vbar_ll.label.last_offset() {
                  [draw.rtop, draw.underbar]
                } else {
                  [draw.underbar, draw.underline]
                }
              } else if vbar_ll.multi && row == 0 && MULTILINE_ARROWS {
                [draw.uarrow, ' ']
              } else {
                [draw.vbar, ' ']
              };
              [c.fg(vbar_ll.label.info.color()), tail.fg(vbar_ll.label.info.color())]
            } else if let Some(underline_ll) = underline {
              [draw.underline.fg(underline_ll.label.info.color()); 2]
            } else {
              [' '.fg(None); 2]
            };

            for i in 0..width {
              write!(w, "{}", if i == 0 { c } else { tail })?;
            }
          }
          writeln!(w)?;

          // Margin
          write_margin(
            &mut w,
            idx,
            false,
            is_ellipsis,
            true,
            Some((row, true)),
            &line_labels,
            &margin_label,
          )?;
          // Lines
          let mut chars = src.get_line_text(line).unwrap().trim_end().chars();
          for col in 0..arrow_len {
            let width = chars.next().map_or(1, |c| self.char_width(c, col).1);

            let is_hbar = (((col > line_label.col) ^ line_label.multi)
              || (line_label.label.info.message.is_some()
                && line_label.draw_msg
                && col > line_label.col))
              && line_label.label.info.message.is_some();
            let [c, tail] = if col == line_label.col
              && line_label.label.info.message.is_some()
              && margin_label.as_ref().map_or(true, |m| !std::ptr::eq(line_label.label, m.label))
            {
              [
                if line_label.multi {
                  if line_label.draw_msg { draw.mbot } else { draw.rbot }
                } else {
                  draw.lbot
                }
                .fg(line_label.label.info.color()),
                draw.hbar.fg(line_label.label.info.color()),
              ]
            } else if let Some(vbar_ll) = get_vbar(col, row)
              .filter(|_| col != line_label.col || line_label.label.info.message.is_some())
            {
              if !CROSS_GAPS && is_hbar {
                [draw.xbar.fg(line_label.label.info.color()), ' '.fg(line_label.label.info.color())]
              } else if is_hbar {
                [draw.hbar.fg(line_label.label.info.color()); 2]
              } else {
                [
                  if vbar_ll.multi && row == 0 { draw.uarrow } else { draw.vbar }
                    .fg(vbar_ll.label.info.color()),
                  ' '.fg(line_label.label.info.color()),
                ]
              }
            } else if is_hbar {
              [draw.hbar.fg(line_label.label.info.color()); 2]
            } else {
              [' '.fg(None); 2]
            };

            if width > 0 {
              write!(w, "{}", c)?;
            }
            for _ in 1..width {
              write!(w, "{}", tail)?;
            }
          }
          if line_label.draw_msg {
            write!(w, " {}", Show(line_label.label.info.message.as_ref()))?;
          }
          writeln!(w)?;
        }
      }

      let is_final_group = group_idx + 1 == groups_len;

      // Help
      if is_final_group {
        for (i, help) in diag.helps.iter().enumerate() {
          write_margin(&mut w, 0, false, false, true, Some((0, false)), &[], &None)?;
          writeln!(w)?;

          let help_prefix = format!("{} {}", "Help", i + 1);
          let help_prefix_len = if diag.helps.len() > 1 { help_prefix.len() } else { 4 };
          let mut lines = help.lines();
          if let Some(line) = lines.next() {
            write_margin(&mut w, 0, false, false, true, Some((0, false)), &[], &None)?;
            if diag.helps.len() > 1 {
              writeln!(w, "{}: {}", help_prefix.fg(self.note_color()), line)?;
            } else {
              writeln!(w, "{}: {}", "Help".fg(self.note_color()), line)?;
            }
          }
          for line in lines {
            write_margin(&mut w, 0, false, false, true, Some((0, false)), &[], &None)?;
            writeln!(w, "{:>pad$}{}", "", line, pad = help_prefix_len + 2)?;
          }
        }
      }

      // Note
      if is_final_group {
        for (i, note) in diag.notes.iter().enumerate() {
          write_margin(&mut w, 0, false, false, true, Some((0, false)), &[], &None)?;
          writeln!(w)?;

          let note_prefix = format!("{} {}", "Note", i + 1);
          let note_prefix_len = if diag.notes.len() > 1 { note_prefix.len() } else { 4 };
          let mut lines = note.lines();
          if let Some(line) = lines.next() {
            write_margin(&mut w, 0, false, false, true, Some((0, false)), &[], &None)?;
            if diag.notes.len() > 1 {
              writeln!(w, "{}: {}", note_prefix.fg(self.note_color()), line)?;
            } else {
              writeln!(w, "{}: {}", "Note".fg(self.note_color()), line)?;
            }
          }
          for line in lines {
            write_margin(&mut w, 0, false, false, true, Some((0, false)), &[], &None)?;
            writeln!(w, "{:>pad$}{}", "", line, pad = note_prefix_len + 2)?;
          }
        }
      }

      if is_final_group {
        let final_margin = format!("{}{}", Show((draw.hbar, line_no_width + 2)), draw.rbot);
        writeln!(w, "{}", final_margin.fg(self.margin_color()))?;
      } else {
        writeln!(w, "{}{}", Show((' ', line_no_width + 2)), draw.vbar.fg(self.margin_color()))?;
      }
    }
    Ok(())
  }
}
