use std::io::{self, Write as _};
use std::ops::ControlFlow;
use std::process::ExitCode;
use std::str::FromStr;

// TODO: <'a>, possibly Cow<'a, str>

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
    Echo(Box<[String]>),
    Type(Box<[String]>),
}

impl Command {
    pub fn exec(
        self,
        stdout: &mut io::Stdout,
        _stderr: &mut io::Stderr,
    ) -> io::Result<ControlFlow<ExitCode>> {
        match self {
            Self::Noop => Ok(ControlFlow::Continue(())),

            Self::Exit(code) => Ok(ControlFlow::Break(code)),

            Self::Echo(args) => {
                let args = args.join(" ");
                writeln!(stdout, "{args}")?;
                stdout.flush()?;
                Ok(ControlFlow::Continue(()))
            }

            Self::Type(args) => {
                for arg in args.iter() {
                    match arg.as_str() {
                        name @ ("exit" | "echo" | "type") => {
                            writeln!(stdout, "{name} is a shell builtin")?
                        }
                        cmd => writeln!(stdout, "{cmd} not found")?,
                    }
                }
                stdout.flush()?;
                Ok(ControlFlow::Continue(()))
            }
        }
    }
}

impl std::fmt::Display for Command {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Noop => Ok(()),
            Self::Exit(_) => write!(f, "exit"),
            Self::Echo(_) => write!(f, "echo"),
            Self::Type(_) => write!(f, "type"),
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

            // TODO: support operands (https://manned.org/echo)
            "echo" => Ok(Command::Echo(args.iter().map(|s| s.to_string()).collect())),

            // TODO: support operands (https://manned.org/type)
            "type" => Ok(Command::Type(args.iter().map(|s| s.to_string()).collect())),

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

        // TODO: reuse single BytesMut (as a bytes pool) outside the loop
        let mut input = String::new();
        stdin.read_line(&mut input)?;

        let cmd = match input.parse::<Command>() {
            Ok(cmd) => cmd,
            Err(err) => {
                writeln!(stderr, "{err}")?;
                stderr.flush()?;
                continue;
            }
        };

        match cmd.exec(&mut stdout, &mut stderr)? {
            ControlFlow::Break(code) => break Ok(code),
            ControlFlow::Continue(_) => continue,
        }
    }
}
