use std::ops::Range;

pub trait SpanUtils {
    fn to_start(&self) -> Range<usize>;
    fn to_end(&self) -> Range<usize>;

    /// moves both points by the given amount
    fn move_by(&self, offset: usize) -> Range<usize>;
    fn move_back_by(&self, offset: usize) -> Range<usize>;
    fn to(&self, other: Self) -> Range<usize>;
}

impl SpanUtils for Range<usize> {
    fn to_start(&self) -> Range<usize> {
        self.start..self.start + 1
    }

    fn to_end(&self) -> Range<usize> {
        if self.end > 0 { (self.end - 1)..self.end } else { self.end..self.end }
    }

    fn move_by(&self, offset: usize) -> Range<usize> {
        self.start + offset..self.end + offset
    }

    fn move_back_by(&self, offset: usize) -> Range<usize> {
        self.start - offset..self.end - offset
    }

    fn to(&self, other: Self) -> Range<usize> {
        self.start..other.end
    }
}
