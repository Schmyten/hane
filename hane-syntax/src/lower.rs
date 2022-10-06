use std::fmt::{self, Display, Formatter};

use hane_kernel::{term::Term, stack::Stack, global::{Global, CommandError}};
use crate::{Expr, ExprVariant, SpanError, Command, CommandVariant, print::write_term};

pub enum LoweringError {
    UnknownVariable(String),
    CommandError(CommandError<String>)
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::UnknownVariable(x) => write!(f, "Unknown variable `{x}`"),
            LoweringError::CommandError(CommandError::NameAlreadyExists(name)) => write!(f, "The name `{name}` has already been defined"),
            LoweringError::CommandError(CommandError::TypeError) => write!(f, "Type error"),
            LoweringError::CommandError(CommandError::IncompatibleType(expected, actual)) => {
                let mut names = Stack::new();
                write!(f, "Expected `")?;
                write_term(f, expected, &mut names, 200)?;
                write!(f, "`, but found `")?;
                write_term(f, actual, &mut names, 200)?;
                write!(f, "`")
            },
            LoweringError::CommandError(CommandError::ExpectedSort(term, ttype)) => {
                let mut names = Stack::new();
                write!(f, "`")?;
                write_term(f, term, &mut names, 200)?;
                write!(f, "` has type `")?;
                write_term(f, ttype, &mut names, 200)?;
                write!(f, "`, but was expected to be a sort")
            }
        }
    }
}

impl Command {
    pub fn lower(self, global: &mut Global<String>) -> Result<(), SpanError<LoweringError>> {
        let mut names = Stack::new();
        match self.variant {
            CommandVariant::Definition(name, ttype, value) => {
                let ttype = ttype.lower(global, &mut names)?;
                let value = value.lower(global, &mut names)?;
                global.definition(name, ttype, value).map_err(|err|
                    SpanError { span: self.span.clone(), err: LoweringError::CommandError(err) }
                )
            },
            CommandVariant::Axiom(name, ttype) => {
                let ttype = ttype.lower(global, &mut names)?;
                global.axiom(name, ttype).map_err(|err|
                    SpanError { span: self.span.clone(), err: LoweringError::CommandError(err) }
                )
            },
        }
    }
}

impl Expr {
    pub fn lower(self, global: &Global<String>, names: &mut Stack<String>) -> Result<Term<String>, SpanError<LoweringError>> {
        Ok(match *self.variant {
            ExprVariant::Prop => Term::Prop,
            ExprVariant::Var(x) => {
                if let Some((i, _)) = names.iter().enumerate().find(|(_, y)| x==**y) {
                    Term::Var(i)
                } else if global.free(&x).is_err() {
                    Term::Const(x)
                } else {
                    return Err(SpanError { span: self.span.clone(), err: LoweringError::UnknownVariable(x.to_owned()) })
                }
            },
            ExprVariant::App(f, v) => Term::App(Box::new(f.lower(global, names)?), Box::new(v.lower(global, names)?)),
            ExprVariant::Product(x, x_tp, t) => {
                let x_tp = x_tp.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                Term::Product(x, Box::new(x_tp), Box::new(t))
            },
            ExprVariant::Abstract(x, x_tp, t) => {
                let x_tp = x_tp.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                Term::Abstract(x, Box::new(x_tp), Box::new(t))
            },
            ExprVariant::Bind(x, x_tp, x_val, t) => {
                let x_tp = x_tp.lower(global, names)?;
                let x_val = x_val.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                Term::Bind(x, Box::new(x_tp), Box::new(x_val), Box::new(t))
            },
        })
    }
}