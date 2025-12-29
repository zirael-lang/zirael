use crate::arena::source_file::SourceFileId;
use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
  pub start: usize,
  pub end: usize,

  pub file_id: SourceFileId,
}

impl Span {
  pub fn new(start: usize, end: usize, file_id: SourceFileId) -> Self {
    Self { start, end, file_id }
  }

  pub fn no_file(start: usize, end: usize) -> Self {
    Self { start, end, file_id: SourceFileId::dummy() }
  }

  pub fn from_len(start: usize, len: usize, file_id: SourceFileId) -> Self {
    Self { start, end: start + len, file_id }
  }

  pub fn len(&self) -> usize {
    self.end.saturating_sub(self.start)
  }

  pub fn is_empty(&self) -> bool {
    self.start >= self.end
  }

  pub fn contains(&self, index: usize) -> bool {
    index >= self.start && index < self.end
  }

  pub fn intersect(&self, other: &Self) -> Option<Self> {
    let start = self.start.max(other.start);
    let end = self.end.min(other.end);

    if start < end { Some(Self { start, end, file_id: self.file_id }) } else { None }
  }

  pub fn overlaps(&self, other: &Self) -> bool {
    self.start < other.end && other.start < self.end
  }

  pub fn to_start(&self) -> Self {
    Self::new(self.start, self.start + 1, self.file_id)
  }

  pub fn to_end(&self) -> Self {
    if self.end > 0 {
      Self::new(self.end - 1, self.end, self.file_id)
    } else {
      Self::new(self.end, self.end, self.file_id)
    }
  }

  pub fn move_by(&self, offset: usize) -> Self {
    Self::new(self.start + offset, self.end + offset, self.file_id)
  }

  pub fn move_back_by(&self, offset: usize) -> Self {
    Self::new(self.start - offset, self.end - offset, self.file_id)
  }

  pub fn to(&self, other: Self) -> Self {
    Self::new(self.start, other.end, self.file_id)
  }

  pub fn source(&self) -> SourceFileId {
    self.file_id
  }

  pub fn dummy() -> Self {
    Self::default()
  }

  pub fn start(&self) -> usize {
    self.start
  }

  pub fn end(&self) -> usize {
    self.end
  }

  pub fn range(&self) -> Range<usize> {
    self.start..self.end
  }
}

impl From<Span> for Range<usize> {
  fn from(val: Span) -> Self {
    val.start..val.end
  }
}

impl From<Range<usize>> for Span {
  fn from(value: Range<usize>) -> Self {
    Span::no_file(value.start, value.end)
  }
}

impl Iterator for Span {
  type Item = usize;

  fn next(&mut self) -> Option<Self::Item> {
    if self.start < self.end {
      let current = self.start;
      self.start += 1;
      Some(current)
    } else {
      None
    }
  }
}

impl Default for Span {
  fn default() -> Self {
    Self { start: 0, end: 0, file_id: SourceFileId(usize::MAX - 1) }
  }
}
