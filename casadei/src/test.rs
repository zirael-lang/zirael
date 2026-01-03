use crate::directives::{Directive, parse_directive};
use color_eyre::Result;
use itertools::Itertools;
use std::env;
use std::path::PathBuf;
use zirael_core::prelude::fs::read_to_string;
use zirael_core::prelude::strip_same_root;
use zirael_source::new_id;

new_id!(TestId);

#[derive(Debug)]
pub struct Test {
  pub id: TestId,
  pub path: PathBuf,
  pub short_path: PathBuf,

  pub directives: Vec<Directive>,
}

impl Test {
  pub fn try_new(path: PathBuf) -> Result<Test> {
    let contents = read_to_string(path.clone())?;
    let lines = contents.lines().collect_vec();

    let directives = lines
      .iter()
      .enumerate()
      .filter(|(_, line)| line.starts_with("//#"))
      .map(|(line_num, line)| {
        parse_directive(line.to_string(), line_num, &path)
      })
      .collect_vec();

    Ok(Test {
      id: TestId::new(),
      path: path.clone(),
      directives,
      short_path: strip_same_root(&path, &env::current_dir()?),
    })
  }
}
