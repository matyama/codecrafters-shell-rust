use std::io::{self, Write as _};
use std::iter::Peekable;
use std::ops::ControlFlow;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::str::{Chars, FromStr};
use std::{env, fs};

#[inline]
fn home_dir() -> Option<PathBuf> {
    // NOTE: there are easy drop-in replacements (e.g., https://crates.io/crates/home)
    #[allow(deprecated)]
    env::home_dir()
}

// TODO: <'a>, possibly Cow<'a, str>

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("quote")]
    Quote,

    #[error("{0}: command not found")]
    CommandNotFound(String),

    #[error("{0}: too many arguments")]
    TooManyArgs(String),

    #[error("{}: No such file or directory", .0.display())]
    NoSuchFileOrDir(PathBuf),

    #[error("No home directory")]
    NoHomeDir,

    #[error(transparent)]
    IO(#[from] io::Error),
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

        match cmd {
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

struct ArgParser<'a> {
    args: &'a str,
}

impl<'a> ArgParser<'a> {
    #[inline]
    fn new(args: &'a str) -> Self {
        Self { args }
    }
}

impl<'a> IntoIterator for ArgParser<'a> {
    type Item = <ArgsIter<'a> as Iterator>::Item;
    type IntoIter = ArgsIter<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        ArgsIter {
            args: self.args,
            iter: self.args.chars().peekable(),
            pos: 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Quote {
    Single,
    Double,
}

impl From<Quote> for char {
    #[inline]
    fn from(quote: Quote) -> Self {
        match quote {
            Quote::Single => '\'',
            Quote::Double => '"',
        }
    }
}

struct ArgsIter<'a> {
    args: &'a str,
    iter: Peekable<Chars<'a>>,
    pos: usize,
}

impl<'a> Iterator for ArgsIter<'a> {
    // XXX: might have to return Cow<'a, str> here due to quoting
    type Item = Result<&'a str, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut start = self.pos;
        let mut quote = None;

        loop {
            let Some(next) = self.iter.next() else {
                return quote.map(|_| Err(Error::Quote));
            };

            self.pos += 1;

            match next {
                // handle single quotes
                '\'' => match quote {
                    // found a closing quote
                    Some(Quote::Single) => {
                        // remove the quote from the literal argument value
                        return Some(Ok(&self.args[start..self.pos - 1]));
                    }
                    // treat the single quote as a regular character
                    Some(Quote::Double) => continue,
                    // found an opening quote
                    None => {
                        let _ = quote.insert(Quote::Single);
                        // remove the quote from the literal argument value
                        start = self.pos;
                    }
                },

                // handle double quotes
                '"' => match quote {
                    // found a closing quote
                    Some(Quote::Double) => {
                        // remove the quote from the literal argument value
                        return Some(Ok(&self.args[start..self.pos - 1]));
                    }
                    // treat the double quote as a regular character
                    Some(Quote::Single) => continue,
                    // found an opening single quote
                    None => {
                        let _ = quote.insert(Quote::Double);
                        // remove the quote from the literal argument value
                        start = self.pos;
                    }
                },

                '\\' => match quote {
                    // TODO: backslash inside single quotes
                    Some(Quote::Single) => continue,
                    // TODO: backslash inside double quotes
                    Some(Quote::Double) => continue,
                    // ignore backslash outside of quotes
                    // FIXME: `echo foo x\\y bar` should output `foo x\y bar`, not `foo \y bar`
                    None if matches!(self.iter.peek(), Some('\\')) => start = self.pos,
                    None => continue,
                },

                // handle whitespaces
                c if c.is_whitespace() => {
                    if quote.is_some() {
                        // preserve literal whitespace inside quotes
                        continue;
                    }

                    let arg = &self.args[start..self.pos - 1];
                    if arg.is_empty() {
                        // ignore non-literal whitespace
                        start = self.pos;
                    } else {
                        // yield next argument separated by this whitespace
                        return Some(Ok(arg));
                    }
                }

                // implicitly add this character to the current argument
                _ => continue,
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
    Quote,
}

impl std::fmt::Display for Prompt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::User => write!(f, "$"),
            //Self::Root(name) => write!(f, "{name}#"),
            Self::Quote => write!(f, "quote>"),
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
            Err(Error::Quote) => {
                // NOTE: don't clear the buffer to preserve previous inputs
                prompt = Prompt::Quote;
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
