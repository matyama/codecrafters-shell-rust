use std::borrow::Cow;
use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::ControlFlow;
use std::path::{Path, PathBuf};
use std::str::Chars;

use crate::error::Error;

pub type Arg<'a> = Cow<'a, str>;

pub struct ArgParser<'a> {
    args: &'a str,
}

impl<'a> ArgParser<'a> {
    #[inline]
    pub fn new(args: &'a str) -> Self {
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
pub enum Quote {
    Single,
    Double,
}

impl std::fmt::Display for Quote {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single => f.write_str("quote"),
            Self::Double => f.write_str("dquote"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Token {
    Escape,
    Literal(char),
}

pub struct ArgsIter<'a> {
    args: &'a str,
    iter: Peekable<Chars<'a>>,
    pos: usize,
}

impl<'a> ArgsIter<'a> {
    #[inline]
    fn arg<'b>(&'b self, start: usize) -> &'a str {
        &self.args[start..self.pos]
    }

    fn add(&self, arg: Arg<'a>, start: usize, c: char) -> Arg<'a> {
        match arg {
            Cow::Borrowed(_) => Cow::Borrowed(self.arg(start)),
            Cow::Owned(mut arg) => {
                arg.push(c);
                Cow::Owned(arg)
            }
        }
    }

    fn close_quote(&mut self, arg: Arg<'a>) -> ControlFlow<Arg<'a>, Arg<'a>> {
        match self.iter.peek() {
            None => ControlFlow::Break(arg),
            Some(c) if c.is_whitespace() => ControlFlow::Break(arg),
            Some(_) => ControlFlow::Continue(Cow::Owned(arg.into_owned())),
        }
    }
}

impl<'a> Iterator for ArgsIter<'a> {
    type Item = Result<Arg<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut start = self.pos;
        let mut quote = None;
        let mut prev = None;
        let mut arg = Cow::Borrowed(self.arg(start));

        loop {
            let Some(next) = self.iter.next() else {
                return match quote {
                    // error on non-terminated quote
                    Some(quote) => Some(Err(Error::Quote(quote))),
                    // stop the iteration when there's no argument
                    None if arg.is_empty() => None,
                    // yield final argument
                    None => Some(Ok(arg)),
                };
            };

            self.pos += 1;

            match next {
                // handle single quotes
                c @ '\'' => match quote {
                    // found a closing quote
                    Some(Quote::Single) => match self.close_quote(arg) {
                        ControlFlow::Break(arg) => return Some(Ok(arg)),
                        ControlFlow::Continue(new_arg) => {
                            arg = new_arg;
                            quote = None;
                        }
                    },

                    // treat the single quote as a regular character
                    Some(Quote::Double) => arg = self.add(arg, start, c),

                    // found an opening quote
                    None => {
                        let _ = quote.insert(Quote::Single);
                        // remove the quote from the literal argument value
                        arg = if prev.is_some() {
                            Cow::Owned(arg.into_owned())
                        } else {
                            start = self.pos;
                            Cow::Borrowed(self.arg(start))
                        };
                    }
                },

                // handle double quotes
                c @ '"' => match quote {
                    // escaped double quote inside a double quote is treated as literal
                    Some(Quote::Double) if matches!(prev, Some(Token::Escape)) => {
                        match self.iter.peek() {
                            None => return Some(Ok(arg)),
                            Some(c) if c.is_whitespace() => return Some(Ok(arg)),
                            Some(_) => arg = self.add(arg, start, c),
                        }
                    }

                    // found a closing quote
                    Some(Quote::Double) => match self.close_quote(arg) {
                        ControlFlow::Break(arg) => return Some(Ok(arg)),
                        ControlFlow::Continue(new_arg) => {
                            arg = new_arg;
                            quote = None;
                        }
                    },

                    // treat the double quote as a regular character
                    Some(Quote::Single) => arg = self.add(arg, start, c),

                    // escaped double quote outside a double quote is treated as literal
                    None if matches!(prev, Some(Token::Escape)) => arg = self.add(arg, start, c),

                    // found an opening single quote
                    None => {
                        let _ = quote.insert(Quote::Double);
                        // remove the quote from the literal argument value
                        arg = if prev.is_some() {
                            Cow::Owned(arg.into_owned())
                        } else {
                            start = self.pos;
                            Cow::Borrowed(self.arg(start))
                        };
                    }
                },

                c @ '\\' => match quote {
                    // backslash inside single quotes preserves the literal value
                    Some(Quote::Single) => arg = self.add(arg, start, next),

                    // backslash inside double quotes preserves literal values of some characters
                    Some(Quote::Double)
                        if matches!(self.iter.peek(), Some('\\' | '$' | '"' | '\n')) =>
                    {
                        arg = match prev {
                            Some(Token::Escape) => {
                                arg = self.add(arg, start, c);
                                prev = Some(Token::Literal(c));
                                continue;
                            }
                            Some(Token::Literal('\\')) => {
                                prev = Some(Token::Literal(c));
                                continue;
                            }
                            Some(_) => Cow::Owned(arg.into_owned()),
                            None => Cow::Owned(arg.into_owned()),
                        };
                    }

                    // other occurrences of a backslash inside a double quote is treated as literal
                    Some(Quote::Double) => arg = self.add(arg, start, c),

                    // backslash outside of quotes serving as an escape character
                    None if matches!(self.iter.peek(), Some('\\')) => {
                        arg = match prev {
                            Some(Token::Escape) => {
                                arg = self.add(arg, start, c);
                                prev = Some(Token::Literal(c));
                                continue;
                            }
                            Some(Token::Literal('\\')) => {
                                prev = Some(Token::Literal(c));
                                continue;
                            }
                            Some(_) => Cow::Owned(arg.into_owned()),
                            None => Cow::Owned(arg.into_owned()),
                        };
                    }

                    // backslash outside of quotes
                    None if matches!(prev, Some(Token::Escape)) => arg = self.add(arg, start, c),

                    None => arg = Cow::Owned(arg.into_owned()),
                },

                // handle whitespaces
                c if c.is_whitespace() && quote.is_none() => {
                    match prev {
                        // escaped whitespace outside of quotes is treated as literal
                        Some(Token::Escape) => arg = self.add(arg, start, c),

                        // non-escaped whitespace outside of quotes is ignored
                        _ if arg.is_empty() => start = self.pos,

                        // yield next argument separated by this whitespace
                        _ => return Some(Ok(arg)),
                    }
                }

                // other characters are added to the current argument
                // (this includes literal whitespace inside quotes)
                c => arg = self.add(arg, start, c),
            }

            // backslash is an escape character by default, literal backslash is handled above
            prev = Some(match next {
                '\\' => Token::Escape,
                c => Token::Literal(c),
            });
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OutputMode {
    Redirect { fd: i32, path: PathBuf, force: bool },
    // TODO: Append
}

pub fn parse_output_mode<'a, I>(args: I) -> Result<(Vec<Arg<'a>>, HashMap<i32, OutputMode>), Error>
where
    I: IntoIterator<Item = Result<Cow<'a, str>, Error>>,
{
    let mut arguments = Vec::new();
    let mut outputs = HashMap::new();
    let mut output = None;

    // NOTE: `$ > test` will actually redirect stdin to `test` file, so ([], [<mode>]) is valid
    for arg in args {
        let arg = arg?;

        // check if arg belongs to the previously parsed redirect operator
        match output.take() {
            Some(OutputMode::Redirect { fd, force, .. }) => {
                let path = match arg {
                    Cow::Borrowed(path) => PathBuf::from(path),
                    Cow::Owned(path) => PathBuf::from(path),
                };
                outputs.insert(fd, OutputMode::Redirect { fd, path, force });
                continue;
            }

            None => match parse_redirect(arg) {
                Ok(Redirect {
                    input: _,
                    fd,
                    fd_arg,
                    op: op @ (">" | ">|"),
                    word,
                }) => {
                    if let Some(arg) = fd_arg {
                        arguments.push(Cow::Owned(arg));
                    }

                    let incomplete = word.is_empty();

                    // TODO: path could be a nested command
                    let redirect = OutputMode::Redirect {
                        fd,
                        path: PathBuf::from(word),
                        force: op == ">|",
                    };

                    if incomplete {
                        output = Some(redirect);
                    } else {
                        outputs.insert(fd, redirect);
                    }
                }

                Ok(Redirect { op, .. }) if !op.is_empty() => unimplemented!("'{op}' output mode"),

                Ok(Redirect { input, .. }) | Err(input) => arguments.push(input),
            },
        }
    }

    // check for trailing redirect operator without an argument
    if output.is_some() {
        Err(Error::Syntax(String::from("unexpected token `newline'")))
    } else {
        Ok((arguments, outputs))
    }
}

struct Redirect<'a> {
    input: Arg<'a>,
    fd: i32,
    fd_arg: Option<String>,
    op: &'static str,
    word: String,
}

fn parse_redirect(input: Arg<'_>) -> Result<Redirect<'_>, Arg<'_>> {
    let mut chars = input.chars().peekable();

    let mut fd = 1;
    let mut fd_arg = None;
    let mut op = None;
    let mut pos = 0;

    while let Some(c) = chars.next() {
        match c {
            // file descriptor (an optional leading integer)
            c if pos == 0 && c.is_ascii_digit() => {
                pos += 1;
                while chars.next_if(char::is_ascii_digit).is_some() {
                    pos += 1;
                }
                // NOTE: bash treats large numbers not as a fd, but rather as an extra arg for echo
                match input[..pos].parse() {
                    Ok(n) => fd = n,
                    Err(_) => fd_arg = Some(input[..pos].to_string()),
                };
            }

            '>' => match chars.peek() {
                // append
                Some('>') => {
                    op = Some(">>");
                    pos += 2;
                    let _ = chars.next();
                }

                // redirect as if noclobber was not set
                Some('|') => {
                    op = Some(">|");
                    pos += 2;
                    let _ = chars.next();
                }

                // `>&word` should be semantically equivalent to `>word 2>&1`
                Some('&') => unimplemented!("redirecting with >&word"),

                // simple redirect
                Some(_) | None => {
                    op = Some(">");
                    pos += 1;
                }
            },

            // TODO matches one of the following
            //  - appending stdout and stderr: &>> which is equivalent to >>word 2>&1
            //  - here strings (and documents): [n]<<< word
            //  - duplicating file descriptors: [n]<&word and [n]>&word
            //  - moving fds: [n]<&digit- and [n]>&digit-
            //  - opening fds to R/W: [n]<>word
            _ => break,
        }
    }

    if let Some(op) = op {
        let word = input[pos..].to_string();
        Ok(Redirect {
            input,
            fd,
            fd_arg,
            op,
            word,
        })
    } else {
        Err(input)
    }
}

pub fn parse_shell_name(executable: String) -> String {
    Path::new(&executable)
        .file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Result<Vec<Cow<'_, str>>, Error> {
        let parser = ArgParser::new(input);
        parser.into_iter().collect::<Result<Vec<_>, _>>()
    }

    #[test]
    fn single_quotes() {
        let args = parse("echo 'shell hello'").expect("no parsing error");
        assert_eq!(args, vec!["echo", "shell hello"]);

        let args = parse("echo 'world    test'").expect("no parsing error");
        assert_eq!(args, vec!["echo", "world    test"]);

        let args = parse("echo 'hello'world").expect("no parsing error");
        assert_eq!(args, vec!["echo", "helloworld"]);

        let args = parse("echo hello'world'!").expect("no parsing error");
        assert_eq!(args, vec!["echo", "helloworld!"]);
    }

    #[test]
    fn double_quotes() {
        let args = parse(r#"echo "quz  hello"  "bar""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", "quz  hello", "bar"]);

        let args = parse(r#"echo "bar"  "shell's"  "foo""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", "bar", "shell's", "foo"]);

        let args = parse(r#"echo "hello"world"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", "helloworld"]);

        let args = parse(r#"echo hello"world"!"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", "helloworld!"]);
    }

    #[test]
    fn backslash_outside_quotes() {
        let args = parse(r#"echo "before\   after""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"before\   after"#]);

        let args = parse(r#"echo x\\y bar"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"x\y"#, "bar"]);

        let args = parse(r#"echo hello\\\\world"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"hello\world"#]);

        let args = parse(r#"echo world\ \ \ \ \ \ script"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", "world      script"]);
    }

    #[test]
    fn backslash_within_single_quotes() {
        let args = parse(r#"echo 'shell\\\nscript'"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"shell\\\nscript"#]);

        let args = parse(r#"echo 'example\"testhello\"shell'"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"example\"testhello\"shell"#]);
    }

    #[test]
    fn backslash_within_double_quotes() {
        let args = parse(r#"echo "\\""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"\"#]);

        let args = parse(r#"echo "\\"hello"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"\hello"#]);

        let args = parse(r#"echo "\\\\"hello"#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"\hello"#]);

        let args = parse(r#"echo "hello'script'\\n'world""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"hello'script'\n'world"#]);

        let args = parse(r#"echo "hello\"insidequotes"script\""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"hello"insidequotesscript""#]);

        let args = parse(r#"echo "mixed\"quote'hello'\\""#).expect("no parsing error");
        assert_eq!(args, vec!["echo", r#"mixed"quote'hello'\"#]);
    }

    macro_rules! args {
        ($($arg:literal),*) => {
            vec![$(Cow::Owned($arg.to_string()),)*]
        }
    }

    fn test_parse_output_mode(
        args: Vec<Cow<'_, str>>,
        expected_args: Vec<Cow<'_, str>>,
        expected_mode: Vec<OutputMode>,
    ) {
        let args = args.into_iter().map(Ok).collect::<Vec<_>>();

        let expected_mode = expected_mode
            .into_iter()
            .map(|mode| match mode {
                mode @ OutputMode::Redirect { fd, .. } => (fd, mode),
            })
            .collect::<HashMap<_, _>>();

        let (actual_args, actual_mode) = parse_output_mode(args).expect("redirect parsing error");
        assert_eq!(expected_args, actual_args);
        assert_eq!(expected_mode, actual_mode);
    }

    #[test]
    fn parse_redirect_no_output() {
        test_parse_output_mode(args![], args![], vec![]);
        test_parse_output_mode(args!["echo", "foo"], args!["echo", "foo"], vec![]);
    }

    #[test]
    fn parse_redirect_trailing() {
        test_parse_output_mode(
            args!["echo", "foo", ">", "/tmp/bar"],
            args!["echo", "foo"],
            vec![OutputMode::Redirect {
                fd: 1,
                path: PathBuf::from("/tmp/bar"),
                force: false,
            }],
        );

        test_parse_output_mode(
            args!["echo", "foo", ">/tmp/bar"],
            args!["echo", "foo"],
            vec![OutputMode::Redirect {
                fd: 1,
                path: PathBuf::from("/tmp/bar"),
                force: false,
            }],
        );
    }

    #[test]
    fn parse_redirect_non_trailing() {
        test_parse_output_mode(
            args!["echo", "foo", ">", "test", "echo", "bar"],
            args!["echo", "foo", "echo", "bar"],
            vec![OutputMode::Redirect {
                fd: 1,
                path: PathBuf::from("test"),
                force: false,
            }],
        );
    }

    #[test]
    fn parse_redirect_explicit_fd() {
        test_parse_output_mode(
            args!["echo", "foo", "2>", "test"],
            args!["echo", "foo"],
            vec![OutputMode::Redirect {
                fd: 2,
                path: PathBuf::from("test"),
                force: false,
            }],
        );

        test_parse_output_mode(
            args!["echo", "foo", "22>", "test"],
            args!["echo", "foo"],
            vec![OutputMode::Redirect {
                fd: 22,
                path: PathBuf::from("test"),
                force: false,
            }],
        );
    }

    #[test]
    fn parse_redirect_preceding_non_fd_number() {
        test_parse_output_mode(
            args![
                "echo",
                "foo",
                "2000000000000000000000000000000000000000>test",
                "bar"
            ],
            args![
                "echo",
                "foo",
                "2000000000000000000000000000000000000000",
                "bar"
            ],
            vec![OutputMode::Redirect {
                fd: 1,
                path: PathBuf::from("test"),
                force: false,
            }],
        );
    }

    #[test]
    fn parse_redirect_force_overwrite() {
        test_parse_output_mode(
            args!["echo", "foo", ">|", "test"],
            args!["echo", "foo"],
            vec![OutputMode::Redirect {
                fd: 1,
                path: PathBuf::from("test"),
                force: true,
            }],
        );

        test_parse_output_mode(
            args!["echo", "foo", ">|test"],
            args!["echo", "foo"],
            vec![OutputMode::Redirect {
                fd: 1,
                path: PathBuf::from("test"),
                force: true,
            }],
        );
    }

    #[test]
    fn parse_redirect_muliple_outputs() {
        test_parse_output_mode(
            args!["echo", "hello", ">", "test1", "echo", "world", ">test2"],
            args!["echo", "hello", "echo", "world"],
            vec![
                OutputMode::Redirect {
                    fd: 1,
                    path: PathBuf::from("test1"),
                    force: false,
                },
                OutputMode::Redirect {
                    fd: 1,
                    path: PathBuf::from("test2"),
                    force: false,
                },
            ],
        );
    }

    #[test]
    fn parse_redirect_syntax_error() {
        let args = args!["echo", "foo", ">"]
            .into_iter()
            .map(Ok)
            .collect::<Vec<_>>();

        let err = parse_output_mode(args).expect_err("redirect parsing error");
        assert!(matches!(err, Error::Syntax(_)), "expected syntax error");
    }
}
