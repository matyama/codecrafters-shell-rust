use std::io::{self, Write as _};
use std::ops::ControlFlow;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::str::FromStr;
use std::{env, fs};

use crate::error::Error;
use crate::parser::{ArgParser, Quote};

mod error;
mod parser;

#[inline]
fn home_dir() -> Option<PathBuf> {
    // NOTE: there are easy drop-in replacements (e.g., https://crates.io/crates/home)
    #[allow(deprecated)]
    env::home_dir()
}

#[derive(Debug)]
enum ShellCmd {
    Noop,
    Pwd,
    Exit(ExitCode),
    Cd(PathBuf),
    Echo(Box<[String]>),
    Type(Box<[String]>),
    Exec(String, PathBuf, Vec<String>),
}

impl ShellCmd {
    pub fn exec(
        self,
        stdout: &mut io::Stdout,
        stderr: &mut io::Stderr,
    ) -> io::Result<ControlFlow<ExitCode>> {
        use ControlFlow::*;
        match self {
            Self::Noop => Ok(Continue(())),

            Self::Pwd => {
                let pwd = env::current_dir()?;
                writeln!(stdout, "{}", pwd.display())?;
                Ok(Continue(()))
            }

            Self::Cd(path) if path.exists() && path.is_dir() => {
                env::set_current_dir(path).map(Continue)
            }

            Self::Cd(path) => {
                writeln!(stderr, "{}", Error::NoSuchFileOrDir(path))?;
                stderr.flush().map(Continue)
            }

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
                    match find_type(arg, &path)? {
                        Some(Type::Builtin) => writeln!(stdout, "{arg} is a shell builtin")?,
                        Some(Type::Executable(path)) => {
                            writeln!(stdout, "{arg} is {}", path.display())?
                        }
                        None => writeln!(stdout, "{arg}: not found")?,
                    }
                }

                stdout.flush().map(Continue)
            }

            Self::Exec(prog, _path, args) => Command::new(prog)
                .args(args)
                .spawn()?
                .wait()
                .map(|_status| Continue(())),
        }
    }
}

impl std::fmt::Display for ShellCmd {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Noop => Ok(()),
            Self::Pwd => write!(f, "pwd"),
            Self::Cd(_) => write!(f, "cd"),
            Self::Exit(_) => write!(f, "exit"),
            Self::Echo(_) => write!(f, "echo"),
            Self::Type(_) => write!(f, "type"),
            Self::Exec(prog, ..) => write!(f, "{prog}"),
        }
    }
}

impl FromStr for ShellCmd {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut args = ArgParser::new(input).into_iter();

        let cmd = match args.next() {
            Some(Ok(cmd)) => cmd,
            Some(Err(e)) => return Err(e),
            None => return Ok(ShellCmd::Noop),
        };

        // XXX: ShellCmd<'a> => `arg.map(Cow::Borrowed)` or even directly `&'a str`
        let args = args
            .map(|arg| arg.map(String::from))
            .collect::<Result<Vec<_>, _>>()?;

        match cmd.as_ref() {
            // TODO: support options (https://manned.org/pwd)
            "pwd" => Ok(ShellCmd::Pwd),

            // TODO: support options (https://manned.org/cd)
            "cd" => match args.first().map(Path::new) {
                Some(path) if path.as_os_str() == "~" => {
                    let home = home_dir().ok_or(Error::NoHomeDir)?;
                    Ok(ShellCmd::Cd(home))
                }
                Some(path) if path.exists() && path.is_dir() => {
                    Ok(ShellCmd::Cd(path.to_path_buf()))
                }
                Some(path) => Err(Error::NoSuchFileOrDir(path.to_path_buf())),
                None => {
                    let home = home_dir().ok_or(Error::NoHomeDir)?;
                    Ok(ShellCmd::Cd(home))
                }
            },

            "exit" => match args.as_slice() {
                [] => Ok(ShellCmd::Exit(ExitCode::SUCCESS)),
                [arg] => {
                    let code = arg.parse::<u8>().map_or(ExitCode::FAILURE, ExitCode::from);
                    Ok(ShellCmd::Exit(code))
                }
                _ => Err(Error::TooManyArgs(cmd.to_string())),
            },

            // TODO: support operands (https://manned.org/echo)
            "echo" => Ok(ShellCmd::Echo(args.into_boxed_slice())),

            // TODO: support operands (https://manned.org/type)
            "type" => Ok(ShellCmd::Type(args.into_boxed_slice())),

            cmd => {
                let path = env::var("PATH").unwrap_or_default();
                let path = path.split(':').map(Path::new).collect::<Vec<_>>();

                let Some(Type::Executable(path)) = find_type(cmd, &path)? else {
                    return Err(Error::CommandNotFound(cmd.to_string()));
                };

                Ok(ShellCmd::Exec(cmd.to_string(), path, args))
            }
        }
    }
}

#[derive(Debug)]
enum Type {
    Builtin,
    Executable(PathBuf),
}

fn find_type(arg: &str, env: &[&Path]) -> io::Result<Option<Type>> {
    if matches!(arg, "exit" | "echo" | "pwd" | "type") {
        return Ok(Some(Type::Builtin));
    }

    for path in env {
        let mut finder = FindExecutable { target: arg };
        if let Some(exe) = finder.visit(path)? {
            return Ok(Some(Type::Executable(exe)));
        }
    }

    Ok(None)
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

#[derive(Debug, Default)]
enum Prompt {
    #[default]
    User,
    //Root(String),
    Quote(Quote),
}

impl std::fmt::Display for Prompt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::User => write!(f, "$"),
            //Self::Root(name) => write!(f, "{name}#"),
            Self::Quote(quote) => write!(f, "{quote}>"),
        }
    }
}

fn main() -> Result<ExitCode, Box<dyn std::error::Error + 'static>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    let mut prompt = Prompt::default();
    let mut input = String::new();

    loop {
        write!(stdout, "{prompt} ")?;
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let cmd = match input.parse::<ShellCmd>() {
            Ok(cmd) => {
                input.clear();
                prompt = Prompt::default();
                cmd
            }
            Err(Error::IO(err)) => Err(err)?,
            Err(Error::Quote(quote)) => {
                // NOTE: don't clear the buffer to preserve previous inputs
                prompt = Prompt::Quote(quote);
                continue;
            }
            Err(err) => {
                writeln!(stderr, "{err}")?;
                stderr.flush()?;
                input.clear();
                prompt = Prompt::default();
                continue;
            }
        };

        match cmd.exec(&mut stdout, &mut stderr)? {
            ControlFlow::Break(code) => break Ok(code),
            ControlFlow::Continue(_) => continue,
        }
    }
}
