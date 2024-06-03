use std::io::{self, Write as _};
use std::ops::ControlFlow;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::str::FromStr;
use std::{env, fs};

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
        use ControlFlow::*;
        match self {
            Self::Noop => Ok(Continue(())),

            Self::Exit(code) => Ok(Break(code)),

            Self::Echo(args) => {
                let args = args.join(" ");
                writeln!(stdout, "{args}")?;
                stdout.flush().map(Continue)
            }

            Self::Type(args) => {
                let path = env::var("PATH").unwrap_or_default();
                let path = path.split(':').map(Path::new).collect::<Vec<_>>();

                for arg in args.iter() {
                    Self::exec_type(arg, &path, stdout)?;
                }

                stdout.flush().map(Continue)
            }
        }
    }

    fn exec_type(arg: &str, env: &[&Path], stdout: &mut io::Stdout) -> io::Result<()> {
        if matches!(arg, "exit" | "echo" | "type") {
            return writeln!(stdout, "{arg} is a shell builtin");
        }

        for path in env {
            let mut finder = FindExecutable { target: arg };
            if let Some(exe) = finder.visit(path)? {
                return writeln!(stdout, "{arg} is {}", exe.display());
            }
        }

        writeln!(stdout, "{arg} not found")
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

trait FSVisitor {
    type Output;

    fn visit_file(&mut self, file: &Path) -> ControlFlow<Self::Output>;

    fn visit_entry(&mut self, entry: &fs::DirEntry) -> ControlFlow<Self::Output>;

    #[inline]
    fn visit(&mut self, path: &Path) -> io::Result<Option<Self::Output>> {
        self.visit_rec(path).map(|res| match res {
            ControlFlow::Break(res) => Some(res),
            ControlFlow::Continue(_) => None,
        })
    }

    fn visit_rec(&mut self, path: &Path) -> io::Result<ControlFlow<Self::Output>> {
        match path {
            dir if dir.is_dir() => {
                for entry in fs::read_dir(dir)? {
                    let entry = entry?;

                    if let res @ ControlFlow::Break(_) = self.visit_entry(&entry) {
                        return Ok(res);
                    }

                    let path = entry.path();

                    // prevent cycles by excluding symlinks
                    if path.is_dir() && !path.is_symlink() {
                        if let res @ ControlFlow::Break(_) = self.visit_rec(&path)? {
                            return Ok(res);
                        }
                    }
                }

                Ok(ControlFlow::Continue(()))
            }

            file if file.is_file() => Ok(self.visit_file(file)),

            _ => Ok(ControlFlow::Continue(())),
        }
    }
}

struct FindExecutable<'a> {
    target: &'a str,
}

impl FSVisitor for FindExecutable<'_> {
    // TODO: try to do this without alloc
    type Output = PathBuf;

    fn visit_file(&mut self, file: &Path) -> ControlFlow<Self::Output> {
        debug_assert!(file.is_file());
        if file.file_name().map_or(false, |f| f == self.target) {
            return ControlFlow::Break(file.to_path_buf());
        }
        ControlFlow::Continue(())
    }

    fn visit_entry(&mut self, entry: &fs::DirEntry) -> ControlFlow<Self::Output> {
        let path = entry.path();
        if path.is_file() {
            return self.visit_file(path.as_path());
        }
        ControlFlow::Continue(())
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
