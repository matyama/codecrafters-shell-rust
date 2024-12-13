use std::path::PathBuf;

use crate::parser::Quote;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Quote(Quote),

    #[error("{0}: command not found")]
    CommandNotFound(String),

    #[error("{0}: too many arguments")]
    TooManyArgs(String),

    #[error("{}: No such file or directory", .0.display())]
    NoSuchFileOrDir(PathBuf),

    #[error("No home directory")]
    NoHomeDir,

    #[error(transparent)]
    IO(#[from] std::io::Error),
}
