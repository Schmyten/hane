use std::fmt::{self, Display, Formatter};

use hane_kernel::term::Term;
use crate::{Expr, ExprVariant, SpanError};

pub enum LoweringError {
    UnknownVariable(String),
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::UnknownVariable(x) => write!(f, "Unknown variable `{x}`"),
        }
    }
}

impl Expr {
    pub fn lower(self, names: &mut Vec<String>) -> Result<Term<String>, SpanError<LoweringError>> {
        Ok(match *self.variant {
            ExprVariant::Prop => Term::Prop,
            ExprVariant::Var(x) => {
                if let Some((i, _)) = names.iter().rev().enumerate().find(|(_, y)| x==**y) {
                    Term::Var(i)
                } else {
                    return Err(SpanError { span: self.span.clone(), err: LoweringError::UnknownVariable(x.to_owned()) })
                }
            },
            ExprVariant::App(f, v) => Term::App(Box::new(f.lower(names)?), Box::new(v.lower(names)?)),
            ExprVariant::Product(x, x_tp, t) => {
                let x_tp = x_tp.lower(names)?;
                names.push(x);
                let t = t.lower(names);
                let x = names.pop().unwrap();
                let t = t?;
                Term::Product(x, Box::new(x_tp), Box::new(t))
            },
            ExprVariant::Abstract(x, x_tp, t) => {
                let x_tp = x_tp.lower(names)?;
                names.push(x);
                let t = t.lower(names);
                let x = names.pop().unwrap();
                let t = t?;
                Term::Abstract(x, Box::new(x_tp), Box::new(t))
            },
            ExprVariant::Bind(x, x_tp, x_val, t) => {
                let x_tp = x_tp.lower(names)?;
                let x_val = x_val.lower(names)?;
                names.push(x);
                let t = t.lower(names);
                let x = names.pop().unwrap();
                let t = t?;
                Term::Bind(x, Box::new(x_tp), Box::new(x_val), Box::new(t))
            },
        })
    }
}