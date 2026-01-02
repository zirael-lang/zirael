use crate::source_file::SourceFileId;
use crate::span::Span;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Line {
  pub offset: usize,
  pub char_len: usize,
  pub byte_offset: usize,
  pub byte_len: usize,
}

impl Line {
  pub fn new(offset: usize, char_len: usize, byte_offset: usize, byte_len: usize) -> Self {
    Self { offset, char_len, byte_offset, byte_len }
  }

  /// Get the offset of this line in the original [`Source`] (i.e: the number of characters that precede it).
  pub fn offset(&self) -> usize {
    self.offset
  }

  /// Get the character length of this line.
  pub fn len(&self) -> usize {
    self.char_len
  }

  /// Returns `true` if this line contains no characters.
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Get the offset span of this line in the original [`Source`].
  pub fn span(&self) -> Span {
    Span::no_file(self.offset, self.offset + self.char_len)
  }

  /// Get the byte offset span of this line in the original [`Source`]. This can be used to
  /// directly slice into its source text.
  pub fn byte_span(&self, file_id: SourceFileId) -> Span {
    Span::new(self.byte_offset, self.byte_offset + self.byte_len, file_id)
  }
}
