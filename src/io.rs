use std::collections::HashMap;
use std::fs;
use std::io;
use std::process::Stdio;

pub(crate) use std::io::prelude::*;
pub(crate) use std::io::{stderr, stdin, stdout, Result};

use crate::parser::OutputMode;

pub const STDOUT: i32 = 1;
pub const STDERR: i32 = 2;

// TODO: support file descriptors other than STDOUT and STDERR
// FIXME: non-existing fd is treated as an arg (maybe this should be part of parsing)
pub fn redirect(outputs: HashMap<i32, OutputMode>) -> Result<(impl OutputIO, impl OutputIO)> {
    // TODO: lift this restriction
    for &fd in outputs.keys() {
        if !matches!(fd, STDOUT | STDERR) {
            return Err(io::Error::other(format!(
                "only stdout/strerr redirects are supported, got {fd}"
            )));
        }
    }

    // TODO: set options instead
    const NOCLOBBER: bool = false;

    macro_rules! redirect_fd {
        ($fd:expr, $stdio:expr) => {{
            match outputs.get(&$fd) {
                Some(OutputMode::Redirect { path, force, .. }) => {
                    if let Some(prefix) = path.parent() {
                        fs::create_dir_all(prefix)?;
                    }

                    let mut file = fs::OpenOptions::new();
                    file.write(true);

                    if NOCLOBBER && !force {
                        file.create_new(true);
                    } else {
                        file.create(true);
                    }

                    file.truncate(true).open(path).map(OutputWriter::File)?
                }

                None => OutputWriter::Std($stdio),
            }
        }};
    }

    let output = redirect_fd!(STDOUT, io::stdout());
    let error = redirect_fd!(STDERR, io::stderr());

    Ok((output, error))
}

pub trait DynWriter {
    fn writer(&mut self) -> &mut dyn Write;
}

impl DynWriter for fs::File {
    #[inline]
    fn writer(&mut self) -> &mut dyn Write {
        self
    }
}

impl DynWriter for io::Stdout {
    #[inline]
    fn writer(&mut self) -> &mut dyn Write {
        self
    }
}

impl DynWriter for io::Stderr {
    #[inline]
    fn writer(&mut self) -> &mut dyn Write {
        self
    }
}

pub trait OutputIO: DynWriter + Into<Stdio> {}

impl<T: DynWriter + Into<Stdio>> OutputIO for T {}

#[derive(Debug)]
pub enum OutputWriter<T> {
    Std(T),
    File(fs::File),
}

impl<T: DynWriter> DynWriter for OutputWriter<T> {
    #[inline]
    fn writer(&mut self) -> &mut dyn Write {
        match self {
            Self::Std(stdio) => stdio.writer(),
            Self::File(file) => file,
        }
    }
}

impl<T: OutputIO> From<OutputWriter<T>> for Stdio {
    #[inline]
    fn from(writer: OutputWriter<T>) -> Self {
        match writer {
            OutputWriter::Std(stdio) => stdio.into(),
            OutputWriter::File(file) => file.into(),
        }
    }
}
