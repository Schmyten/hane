pub mod eval;
pub mod lower;
pub mod parser;
pub mod print;

use std::fmt::{self, Display, Write};

use hane_kernel::entry::Binder as LoweredBinder;
use hane_kernel::Term;

#[derive(Clone, Copy)]
pub struct Location {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}

impl Location {
    fn from_pest(pos: pest::Position) -> Self {
        let (line, col) = pos.line_col();
        Location {
            pos: pos.pos(),
            line,
            col,
        }
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
    Inductive(Vec<IndBody>),
}

pub struct IndBody {
    pub name: String,
    pub params: Vec<Binder>,
    pub ttype: Expr,
    pub constructors: Vec<IndConstructor>,
}

pub struct IndConstructor {
    pub name: String,
    pub ttype: Expr,
}

pub struct LoweredCommand {
    pub span: Span,
    pub variant: LoweredCommandVariant,
}

pub enum LoweredCommandVariant {
    Definition(String, Term<Span, String>, Term<Span, String>),
    Axiom(String, Term<Span, String>),
    Inductive(Vec<LoweredBinder<Span, String>>, Vec<LoweredIndBody>),
}

pub struct LoweredIndBody {
    pub name: String,
    pub ttype: Term<Span, String>,
    pub constructors: Vec<LoweredIndConstructor>,
}

pub struct LoweredIndConstructor {
    pub name: String,
    pub ttype: Term<Span, String>,
}

#[derive(PartialEq, Eq)]
pub enum Sort {
    Prop,
    Set,
    Type(usize),
}

pub struct Expr {
    pub span: Span,
    pub variant: Box<ExprVariant>,
}

impl Eq for Expr {}
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.variant == other.variant
    }
}

#[derive(PartialEq, Eq)]
pub struct Binder {
    pub name: String,
    pub ttype: Expr,
}

#[derive(PartialEq, Eq)]
pub enum ExprVariant {
    Sort(Sort),
    Var(String),
    App(Expr, Expr),
    Product(Vec<Binder>, Expr),
    Abstract(Vec<Binder>, Expr),
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
            let line = input.lines().nth(self.start.line - 1).unwrap();
            writeln!(
                f,
                "{0: >len$}--> {1}:{2}:{3}",
                "", path, self.start.line, self.start.col
            )?;
            writeln!(f, "{0: >len$} |", "")?;
            writeln!(f, "{0} | {1}", self.start.line, line)?;
            writeln!(
                f,
                "{0: >len$} | {0: >col$}{0:^>span$}",
                "",
                col = self.start.col - 1,
                span = 1.max(self.end.col - self.start.col)
            )?;
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
