use std::borrow::Cow;
use std::iter::Peekable;
use std::ops::ControlFlow;
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
}
