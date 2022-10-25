use std::fmt::{self, Display, Formatter};

use crate::{
    print::{write_local, write_term},
    Ident, Span,
};
use hane_kernel::{entry::Entry, CommandError, Global, Stack, TypeError, TypeErrorVariant};

pub struct EvalError<'a>(pub &'a Global<Span, Ident>, pub CommandError<Span, Ident>);

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
        TypeErrorVariant::NotAnInductiveType(name) => {
            write!(f, "Expected an inductive type, Found {name}")
        }
        TypeErrorVariant::IncorrectParameterCount(expected, found) => {
            write!(f, "Expected {expected} parameters, found {found}")
        }
        TypeErrorVariant::IncorrectConstructorCount(expected, found) => {
            write!(f, "Expected {expected} constructors, found {found}")
        }
        TypeErrorVariant::NotOfExpectedInducitve(ind, _) => {
            write!(f, "Expected a term of type {ind}")
        }
        TypeErrorVariant::DisallowedEleminationSort(s1, s2) => {
            write!(f, "{s1} cannot elemintate into {s2}")
        }
        TypeErrorVariant::DebruijnOutOfScope(n) => write!(
            f,
            "Debruijn index {n} out of scope. The local environment only contains {} names.",
            local.len()
        ),
        TypeErrorVariant::UndefinedConst(name) => write!(f, "Unknown constant {name}"),
    }
}

impl<'a> Display for EvalError<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let global = self.0;
        match &self.1 {
            CommandError::NameAlreadyExists(name) => {
                write!(f, "The name `{name}` has already been defined")
            }
            CommandError::TypeError(err) => {
                write_cause(err, &err.local, f)?;
                writeln!(f)?;
                let mut names = Stack::new();
                let mut names = write_local(f, &err.local, global, &mut names)?;
                writeln!(f)?;
                match &err.variant {
                    TypeErrorVariant::NotSubtypeType(expected, actual) => {
                        write!(f, "Expected: ")?;
                        write_term(f, expected, global, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, global, &mut names, 200)
                    }
                    TypeErrorVariant::IncompatibleTypes(expected, actual) => {
                        write!(f, "Expected: ")?;
                        write_term(f, expected, global, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, global, &mut names, 200)
                    }
                    TypeErrorVariant::NotAProduct(ttype) => {
                        write!(f, "Found: ")?;
                        write_term(f, ttype, global, &mut names, 200)
                    }
                    TypeErrorVariant::NotASort(ttype) => {
                        write!(f, "Found: ")?;
                        write_term(f, ttype, global, &mut names, 200)
                    }
                    TypeErrorVariant::NotAnInductiveType(_) => Ok(()),
                    TypeErrorVariant::IncorrectParameterCount(_, _) => Ok(()),
                    TypeErrorVariant::IncorrectConstructorCount(_, _) => Ok(()),
                    TypeErrorVariant::NotOfExpectedInducitve(_, ttype) => {
                        write!(f, "Found: ")?;
                        write_term(f, ttype, global, &mut names, 200)
                    }
                    TypeErrorVariant::DisallowedEleminationSort(_, _) => Ok(()),
                    TypeErrorVariant::DebruijnOutOfScope(_) => Ok(()),
                    TypeErrorVariant::UndefinedConst(_) => Ok(()),
                }
            }
        }
    }
}
