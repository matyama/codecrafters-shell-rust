use std::io::{self, Write};
use std::str::FromStr;

use anyhow::Result;

const PROMPT: char = '$';

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("{0}: command not found")]
    CommandNotFound(String),
}

#[derive(Debug)]
enum Command {}

impl std::fmt::Display for Command {
    #[inline]
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl FromStr for Command {
    type Err = Error;

    fn from_str(cmd: &str) -> Result<Self, Self::Err> {
        Err(Error::CommandNotFound(cmd.to_string()))
    }
}

fn main() -> Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    loop {
        write!(stdout, "{PROMPT} ")?;
        stdout.flush()?;

        let mut input = String::new();
        stdin.read_line(&mut input)?;

        match input.trim().parse::<Command>() {
            Ok(cmd) => unimplemented!("handle {cmd}"),
            Err(e) => {
                writeln!(stderr, "{e}")?;
                stderr.flush()?;
            }
        }
    }
}
