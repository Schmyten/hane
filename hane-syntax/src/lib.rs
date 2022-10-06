pub mod print;
pub mod parser;
pub mod lower;

use std::fmt::{self, Write, Display};

#[derive(Clone, Copy)]
pub struct Location {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}

impl Location {
    fn from_pest(pos: pest::Position) -> Self {
        let (line, col) = pos.line_col();
        Location { pos: pos.pos(), line, col }
    }
}

#[derive(Clone)]
pub struct Span {
    start: Location,
    end: Location,
}

impl Span {
    fn from_pest(span: pest::Span) -> Self {
        Span {
            start: Location::from_pest(span.start_pos()),
            end: Location::from_pest(span.end_pos()),
        }
    }
}

pub struct Command {
    pub span: Span,
    pub variant: CommandVariant,
}

pub enum CommandVariant {
    Definition(String, Expr, Expr),
    Axiom(String, Expr),
}

pub struct Expr {
    pub span: Span,
    pub variant: Box<ExprVariant>,
}

pub enum ExprVariant {
    Prop,
    Var(String),
    App(Expr, Expr),
    Product(String, Expr, Expr),
    Abstract(String, Expr, Expr),
    Bind(String, Expr, Expr, Expr),
}

pub struct SpanError<E> {
    pub span: Span,
    pub err: E,
}

impl Span {
    pub fn write(&self, path: &str, input: &str, f: &mut impl Write) -> fmt::Result {
        if self.start.line == self.end.line {
            let len = format!("{}", self.start.line).len();
            let line = input.lines().nth(self.start.line-1).unwrap();
            writeln!(f, "{0: >len$}--> {1}:{2}:{3}", "", path, self.start.line, self.start.col)?;
            writeln!(f, "{0: >len$} |", "")?;
            writeln!(f, "{0} | {1}", self.start.line, line)?;
            writeln!(f, "{0: >len$} | {0: >col$}{0:^>span$}", "", col=self.start.col-1, span=1.max(self.end.col-self.start.col))?;
            writeln!(f, "{0: >len$} |", "")?;
            write!(f, "{0: >len$} = ", "")?;
        } else {
            
        }
        Ok(())
    }
}

impl<E: Display> SpanError<E> {
    pub fn write(&self, path: &str, input: &str, f: &mut impl Write) -> fmt::Result {
        self.span.write(path, input, f)?;
        write!(f, "{}", self.err)
    }

    pub fn print(&self, path: &str, input: &str) -> String {
        let mut buf = String::new();
        self.write(path, input, &mut buf).unwrap();
        buf
    }
}
