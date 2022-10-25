use std::fmt::{self, Display, Formatter};

use crate::{
    print::{write_local, write_term},
    Ident, Span,
};
use hane_kernel::{entry::Entry, CommandError, Stack, TypeError, TypeErrorVariant};

pub struct EvalError(pub CommandError<Span, Ident>);

fn write_cause(
    err: &TypeError<Span, Ident>,
    local: &Stack<Entry<Span, Ident>>,
    f: &mut Formatter,
) -> fmt::Result {
    match &err.variant {
        TypeErrorVariant::NotSubtypeType(_, _) => write!(f, "Invalid Subtype"),
        TypeErrorVariant::IncompatibleTypes(_, _) => write!(f, "Incompatible Types"),
        TypeErrorVariant::NotAProduct(_) => write!(f, "Expected a product"),
        TypeErrorVariant::NotASort(_) => write!(f, "Expected a sort"),
        TypeErrorVariant::DebruijnOutOfScope(n) => write!(
            f,
            "Debruijn index {n} out of scope. The local environment only contains {} names.",
            local.len()
        ),
        TypeErrorVariant::UndefinedConst(name) => write!(f, "Unknown constant {name}"),
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0 {
            CommandError::NameAlreadyExists(name) => {
                write!(f, "The name `{name}` has already been defined")
            }
            CommandError::TypeError(err) => {
                write_cause(err, &err.local, f)?;
                writeln!(f)?;
                let mut names = Stack::new();
                let mut names = write_local(f, &err.local, &mut names)?;
                writeln!(f)?;
                match &err.variant {
                    TypeErrorVariant::NotSubtypeType(expected, actual) => {
                        write!(f, "Expected: ")?;
                        write_term(f, expected, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, &mut names, 200)
                    }
                    TypeErrorVariant::IncompatibleTypes(expected, actual) => {
                        write!(f, "Expected: ")?;
                        write_term(f, expected, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, &mut names, 200)
                    }
                    TypeErrorVariant::NotAProduct(ttype) => {
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    }
                    TypeErrorVariant::NotASort(ttype) => {
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    }
                    TypeErrorVariant::DebruijnOutOfScope(_) => Ok(()),
                    TypeErrorVariant::UndefinedConst(_) => Ok(()),
                }
            }
        }
    }
}
