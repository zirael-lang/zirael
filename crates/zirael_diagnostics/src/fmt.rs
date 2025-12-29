use std::fmt;
use std::fmt::{Display, Formatter};
use yansi::{Color, Paint};

pub trait Fmt: Sized {
  fn fg<C: Into<Option<Color>>>(self, color: C) -> Foreground<Self>
  where
    Self: Display,
  {
    Foreground(self, color.into())
  }

  fn bg<C: Into<Option<Color>>>(self, color: C) -> Background<Self>
  where
    Self: Display,
  {
    Background(self, color.into())
  }
}

impl<T: Display> Fmt for T {}

#[derive(Copy, Clone, Debug)]
pub struct Foreground<T>(T, Option<Color>);
impl<T: Display> Display for Foreground<T> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    if let Some(col) = self.1 {
      write!(f, "{}", Paint::new(&self.0).fg(col))
    } else {
      write!(f, "{}", self.0)
    }
  }
}

#[derive(Copy, Clone, Debug)]
pub struct Background<T>(T, Option<Color>);
impl<T: Display> Display for Background<T> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    if let Some(col) = self.1 {
      write!(f, "{}", Paint::new(&self.0).bg(col))
    } else {
      write!(f, "{}", self.0)
    }
  }
}
