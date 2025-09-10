#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

impl Span {
  pub fn new(start: usize, end: usize) -> Self {
    Self { start, end }
  }

  pub fn from_len(start: usize, len: usize) -> Self {
    Self { start, end: start + len }
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

    if start < end { Some(Self { start, end }) } else { None }
  }

  pub fn overlaps(&self, other: &Self) -> bool {
    self.start < other.end && other.start < self.end
  }

  pub fn to_start(&self) -> Self {
    Self::new(self.start, self.start + 1)
  }

  pub fn to_end(&self) -> Self {
    if self.end > 0 { Self::new(self.end - 1, self.end) } else { Self::new(self.end, self.end) }
  }

  pub fn move_by(&self, offset: usize) -> Self {
    Self::new(self.start + offset, self.end + offset)
  }

  pub fn move_back_by(&self, offset: usize) -> Self {
    Self::new(self.start - offset, self.end - offset)
  }

  pub fn to(&self, other: Self) -> Self {
    Self::new(self.start, other.end)
  }
}

impl From<std::ops::Range<usize>> for Span {
  fn from(range: std::ops::Range<usize>) -> Self {
    Self { start: range.start, end: range.end }
  }
}

impl From<Span> for std::ops::Range<usize> {
  fn from(val: Span) -> Self {
    val.start..val.end
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
