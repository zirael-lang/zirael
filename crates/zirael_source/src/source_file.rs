use crate::line::Line;
use crate::new_id;
use crate::span::Span;
use std::ops::Range;
use std::path::PathBuf;

new_id!(SourceFileId);

#[derive(Debug, Clone)]
pub struct SourceFile {
  content: String,
  path: PathBuf,
  pub file_id: SourceFileId,
  lines: Vec<Line>,
  len: usize,
  byte_len: usize,
  display_line_offset: usize,
}

impl AsRef<str> for SourceFile {
  fn as_ref(&self) -> &str {
    self.content.as_str()
  }
}

impl SourceFile {
  pub fn new(input: String, path: PathBuf, file_id: SourceFileId) -> Self {
    if input.is_empty() {
      return Self {
        content: input,
        lines: vec![Line::new(0, 0, 0, 0)],
        len: 0,
        byte_len: 0,
        display_line_offset: 0,
        path,
        file_id,
      };
    }

    let mut char_offset = 0;
    let mut byte_offset = 0;
    let mut lines = Vec::new();

    const SEPARATORS: [char; 7] = [
      '\r',       // Carriage return
      '\n',       // Line feed
      '\x0B',     // Vertical tab
      '\x0C',     // Form feed
      '\u{0085}', // Next line
      '\u{2028}', // Line separator
      '\u{2029}', // Paragraph separator
    ];
    let mut remaining = input.split_inclusive(SEPARATORS).peekable();
    while let Some(line) = remaining.next() {
      let mut byte_len = line.len();
      let mut char_len = line.chars().count();
      // Handle CRLF as a single terminator.
      if line.ends_with('\r') && remaining.next_if_eq(&"\n").is_some() {
        byte_len += 1;
        char_len += 1;
      }
      lines.push(Line::new(char_offset, char_len, byte_offset, byte_len));

      char_offset += char_len;
      byte_offset += byte_len;
    }

    Self {
      content: input,
      lines,
      len: char_offset,
      byte_len: byte_offset,
      display_line_offset: 0,
      path,
      file_id,
    }
  }

  pub fn content(&self) -> &str {
    self.content.as_str()
  }

  pub fn path(&self) -> PathBuf {
    self.path.clone()
  }

  pub fn with_display_line_offset(mut self, offset: usize) -> Self {
    self.display_line_offset = offset;
    self
  }

  /// Get the offset added to printed line numbers
  pub fn display_line_offset(&self) -> usize {
    self.display_line_offset
  }

  /// Get the length of the total number of characters in the source.
  pub fn len(&self) -> usize {
    self.len
  }

  /// Returns `true` if this source contains no characters.
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Return an iterator over the characters in the source.
  pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
    self.as_ref().chars()
  }

  /// Get access to a specific, zero-indexed [`Line`].
  pub fn line(&self, idx: usize) -> Option<Line> {
    self.lines.get(idx).copied()
  }

  /// Return an iterator over the [`Line`]s in this source.
  pub fn lines(&self) -> impl ExactSizeIterator<Item = Line> + '_ {
    self.lines.iter().copied()
  }

  /// Get the line that the given offset appears on, and the line/column numbers of the offset.
  ///
  /// Note that the line/column numbers are zero-indexed.
  pub fn get_offset_line(&self, offset: usize) -> Option<(Line, usize, usize)> {
    if offset <= self.len {
      let idx = self
        .lines
        .binary_search_by_key(&offset, |line| line.offset)
        .unwrap_or_else(|idx| idx.saturating_sub(1));
      let line = self.line(idx)?;
      assert!(
        offset >= line.offset,
        "offset = {}, line.offset = {}",
        offset,
        line.offset
      );
      Some((line, idx, offset - line.offset))
    } else {
      None
    }
  }

  /// Get the line that the given byte offset appears on, and the line/byte column numbers of the offset.
  ///
  /// Note that the line/column numbers are zero-indexed.
  pub fn get_byte_line(
    &self,
    byte_offset: usize,
  ) -> Option<(Line, usize, usize)> {
    if byte_offset <= self.byte_len {
      let idx = self
        .lines
        .binary_search_by_key(&byte_offset, |line| line.byte_offset)
        .unwrap_or_else(|idx| idx.saturating_sub(1));
      let line = self.line(idx)?;
      assert!(
        byte_offset >= line.byte_offset,
        "byte_offset = {}, line.byte_offset = {}",
        byte_offset,
        line.byte_offset
      );
      Some((line, idx, byte_offset - line.byte_offset))
    } else {
      None
    }
  }

  /// Get the range of lines that this span runs across.
  ///
  /// The resulting range is guaranteed to contain valid line indices (i.e: those that can be used for
  /// [`Source::line`]).
  pub fn get_line_range(&self, span: Range<usize>) -> Range<usize> {
    let start = self.get_offset_line(span.start).map_or(0, |(_, l, _)| l);
    let end = self
      .get_offset_line(span.end.saturating_sub(1).max(span.start))
      .map_or(self.lines.len(), |(_, l, _)| l + 1);
    start..end
  }

  /// Get the source text for a line, includes trailing whitespace and the newline
  pub fn get_line_text(&self, line: Line) -> Option<&'_ str> {
    self.as_ref().get(line.byte_span(self.file_id).range())
  }
}
