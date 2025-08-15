use clap::ValueEnum;
use std::fmt::Display;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum ProjectType {
    Binary,
    Library,
}

impl Display for ProjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            match self {
                ProjectType::Binary => "binary",
                ProjectType::Library => "library",
            }
        )
    }
}
