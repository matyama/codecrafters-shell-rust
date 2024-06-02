use std::io::{self, Write as _};
use std::process::ExitCode;
use std::str::FromStr;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("{0}: command not found")]
    CommandNotFound(String),

    #[error("{0}: too many arguments")]
    TooManyArgs(String),
}

#[derive(Debug)]
enum Command {
    Noop,
    Exit(ExitCode),
}

impl std::fmt::Display for Command {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Noop => Ok(()),
            Self::Exit(_) => write!(f, "exit"),
        }
    }
}

impl FromStr for Command {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        // TODO: no alloc
        let args: Vec<_> = input.split_whitespace().collect();

        let Some((&cmd, args)) = args.split_first() else {
            return Ok(Command::Noop);
        };

        match cmd {
            "exit" => match *args {
                [] => Ok(Command::Exit(ExitCode::SUCCESS)),
                [arg] => {
                    let code = arg.parse::<u8>().map_or(ExitCode::FAILURE, ExitCode::from);
                    Ok(Command::Exit(code))
                }
                _ => Err(Error::TooManyArgs(cmd.to_string())),
            },
            cmd => Err(Error::CommandNotFound(cmd.to_string())),
        }
    }
}

fn main() -> Result<ExitCode, Box<dyn std::error::Error + 'static>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    loop {
        write!(stdout, "$ ")?;
        stdout.flush()?;

        let mut input = String::new();
        stdin.read_line(&mut input)?;

        let cmd = match input.parse() {
            Ok(cmd) => cmd,
            Err(e) => {
                writeln!(stderr, "{e}")?;
                stderr.flush()?;
                continue;
            }
        };

        match cmd {
            Command::Noop => continue,
            Command::Exit(code) => break Ok(code),
        }
    }
}
