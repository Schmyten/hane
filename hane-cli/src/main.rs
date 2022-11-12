use hane_kernel::Global;
use hane_syntax::{
    eval::EvalError,
    lower::LoweringError,
    parser::{parse, ParseError},
    Ident, Span, SpanError, print::Print,
};
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
    io::stdin,
};

enum Error<'a> {
    ParseError(Option<&'a str>, &'a str, ParseError),
    SingleCommand,
    LoweringError(Option<&'a str>, &'a str, SpanError<LoweringError>),
    EvalError(Option<&'a str>, &'a str, SpanError<EvalError>),
}

impl<'a> From<(Option<&'a str>, &'a str, ParseError)> for Error<'a> {
    fn from((input, path, err): (Option<&'a str>, &'a str, ParseError)) -> Error<'a> {
        Error::ParseError(input, path, err)
    }
}

impl<'a> From<(Option<&'a str>, &'a str, SpanError<LoweringError>)> for Error<'a> {
    fn from((input, path, err): (Option<&'a str>, &'a str, SpanError<LoweringError>)) -> Error<'a> {
        Error::LoweringError(input, path, err)
    }
}

impl<'a> From<(Option<&'a str>, &'a str, SpanError<EvalError>)> for Error<'a> {
    fn from((input, path, err): (Option<&'a str>, &'a str, SpanError<EvalError>)) -> Error<'a> {
        Error::EvalError(input, path, err)
    }
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::ParseError(path, input, err) => err.write(path.as_ref().cloned(), input, f),
            Error::SingleCommand => write!(f, "The cli only accepts a single command at a time"),
            Error::LoweringError(path, input, err) => err.write(path.as_ref().cloned(), input, f),
            Error::EvalError(path, input, err) => err.write(path.as_ref().cloned(), input, f),
        }
    }
}

fn main() {
    let mut names = HashSet::new();
    let mut global = Global::new();

    for line in stdin().lines() {
        let line = line.unwrap();

        if let Err(err) = eval_line(&line, &mut names, &mut global) {
            eprintln!("{err}")
        }
    }
}

fn eval_line<'a>(
    line: &'a str,
    names: &mut HashSet<String>,
    global: &mut Global<Span, Ident>,
) -> Result<(), Error<'a>> {
    let mut commands = parse(&line).map_err(|err| (None, line, err))?;

    if commands.len() != 1 {
        return Err(Error::SingleCommand);
    }

    let command = commands.pop().unwrap();
    let command = command.lower(names).map_err(|err| (None, line, err))?;

    command.eval(global, |out|print!("{}", Print(out))).map_err(|(span, err)|(None, line, SpanError { span, err: EvalError(err) }))?;
    Ok(())
}
