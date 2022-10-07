use std::fmt::{self, Display, Formatter};

use hane_kernel::{Stack, Global, Term, TermVariant, CommandError, TypeErrorVariant, Sort};
use crate::{Expr, ExprVariant, SpanError, Command, CommandVariant, print::write_term, Span};

pub enum LoweringError {
    UnknownVariable(String),
    CommandError(CommandError<Span, String>)
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::UnknownVariable(x) => write!(f, "Unknown variable `{x}`"),
            LoweringError::CommandError(CommandError::NameAlreadyExists(name)) => write!(f, "The name `{name}` has already been defined"),
            LoweringError::CommandError(CommandError::TypeError(err)) => {
                let mut names = err.bindings();
                match &err.variant {
                    TypeErrorVariant::NotSubtypeType(expected, actual) => {
                        writeln!(f, "Invalid Subtype")?;
                        write!(f, "Expected: ")?;
                        write_term(f, expected, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, &mut names, 200)
                    },
                    TypeErrorVariant::IncompatibleTypes(expected, actual) => {
                        writeln!(f, "Incompatible Types")?;
                        write!(f, "Expected: ")?;
                        write_term(f, expected, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, &mut names, 200)
                    },
                    TypeErrorVariant::NotAProduct(ttype) => {
                        writeln!(f, "Expected a product")?;
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    },
                    TypeErrorVariant::NotASort(ttype) => {
                        writeln!(f, "Expected a sort")?;
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    },
                    TypeErrorVariant::DebruijnOutOfScope(n) => write!(f, "Found debruijn index {n}, but there are only {} names in scope", names.len()),
                    TypeErrorVariant::UndefinedConst(name) => write!(f, "Unknown constant {name}"),
                }
            },
        }
    }
}

impl Command {
    pub fn lower(self, global: &mut Global<Span, String>) -> Result<(), SpanError<LoweringError>> {
        let mut names = Stack::new();
        match self.variant {
            CommandVariant::Definition(name, ttype, value) => {
                let ttype = ttype.lower(global, &mut names)?;
                let value = value.lower(global, &mut names)?;
                global.definition(self.span, name, ttype, value).map_err(|(span, err)|
                    SpanError { span, err: LoweringError::CommandError(err) }
                )
            },
            CommandVariant::Axiom(name, ttype) => {
                let ttype = ttype.lower(global, &mut names)?;
                global.axiom(self.span, name, ttype).map_err(|(span, err)|
                    SpanError { span, err: LoweringError::CommandError(err) }
                )
            },
        }
    }
}

impl Expr {
    pub fn lower(self, global: &Global<Span, String>, names: &mut Stack<String>) -> Result<Term<Span, String>, SpanError<LoweringError>> {
        let variant = match *self.variant {
            ExprVariant::Sort(sort) => TermVariant::Sort(sort.lower()),
            ExprVariant::Var(x) => {
                if let Some((i, _)) = names.iter().enumerate().find(|(_, y)| x==**y) {
                    TermVariant::Var(i)
                } else if global.free(&x).is_err() {
                    TermVariant::Const(x)
                } else {
                    return Err(SpanError { span: self.span.clone(), err: LoweringError::UnknownVariable(x.to_owned()) })
                }
            },
            ExprVariant::App(f, v) => TermVariant::App(f.lower(global, names)?, v.lower(global, names)?),
            ExprVariant::Product(x, x_tp, t) => {
                let x_tp = x_tp.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                TermVariant::Product(x, x_tp, t)
            },
            ExprVariant::Abstract(x, x_tp, t) => {
                let x_tp = x_tp.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                TermVariant::Abstract(x, x_tp, t)
            },
            ExprVariant::Bind(x, x_tp, x_val, t) => {
                let x_tp = x_tp.lower(global, names)?;
                let x_val = x_val.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                TermVariant::Bind(x, x_tp, x_val, t)
            },
        };
        Ok(Term { meta: self.span, variant: Box::new(variant) })
    }
}

impl crate::Sort {
    pub fn lower(self) -> Sort {
        match self {
            crate::Sort::Prop => Sort::Prop,
            crate::Sort::Set => Sort::Set,
            crate::Sort::Type(n) => Sort::Type(n),
        }
    }
}