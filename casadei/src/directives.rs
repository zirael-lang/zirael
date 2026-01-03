use itertools::Itertools;
use log::warn;
use std::path::PathBuf;
use zirael_core::prelude::PackageType;

#[derive(Clone, Debug)]
pub enum Directive {
  PackageType(PackageType),
  // the pattern can include {} that means that any value matches this part.
  Error {
    line: usize,
    direction: LineDirection,
    pattern: String,
  },
  Invalid,
}

#[derive(Clone, Debug)]
pub enum LineDirection {
  Up,
  Down,
}

pub fn parse_directive(
  directive: String,
  line: usize,
  file: &PathBuf,
) -> Directive {
  let stripped = directive.strip_prefix("//#").unwrap_or(&directive);

  let (name_with_attrs, value) = match stripped.split_once(':') {
    Some((n, v)) => (n, v.trim()),
    None => (stripped, ""),
  };

  let (name, attrs) = if let Some(start) = name_with_attrs.find('(') {
    let end = name_with_attrs.find(')').unwrap_or(name_with_attrs.len());
    let name = &name_with_attrs[..start];
    let attrs_str = &name_with_attrs[start + 1..end];
    (name, parse_attributes(attrs_str))
  } else {
    (name_with_attrs, vec![])
  };

  match name {
    "package-type" => {
      if value.is_empty() {
        Directive::PackageType(PackageType::Binary)
      } else {
        Directive::PackageType(value.parse().unwrap())
      }
    }
    "error" => {
      if value.is_empty() {
        panic!("Error directive must have a pattern message")
      } else {
        let direction = attrs
          .iter()
          .find(|(k, _)| k == "direction")
          .map(|(_, v)| match v.as_str() {
            "up" => LineDirection::Up,
            "down" => LineDirection::Down,
            _ => LineDirection::Down,
          })
          .unwrap_or(LineDirection::Down);

        Directive::Error {
          line,
          direction,
          pattern: value.to_string(),
        }
      }
    }
    _ => {
      println!(
        "warn: {name} found in {} is not a valid directive",
        file.display()
      );
      Directive::Invalid
    }
  }
}

fn parse_attributes(attrs_str: &str) -> Vec<(String, String)> {
  attrs_str
    .split(',')
    .filter_map(|attr| {
      let parts: Vec<&str> = attr.split('=').map(|s| s.trim()).collect();
      if parts.len() == 2 {
        Some((parts[0].to_string(), parts[1].to_string()))
      } else {
        None
      }
    })
    .collect()
}
