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
        TypeErrorVariant::NotAnInductiveType(name) => {
            write!(f, "Expected an inductive type, Found {name}")
        }
        TypeErrorVariant::NotAConstructor(ind, name, constructors) => {
            write!(f, "{name} is not a constructor for {ind}.")?;
            write!(f, " Constructors are: ")?;
            let mut sep = "";
            for constructor in constructors {
                write!(f, "{sep}{constructor}")?;
                sep = ", ";
            }
            Ok(())
        }
        TypeErrorVariant::IncorrectParameterCount(expected, found) => {
            write!(f, "Expected {expected} parameters, found {found}")
        }
        TypeErrorVariant::NotOfExpectedInducitve(ind, _) => {
            write!(f, "Expected a term of type {ind}")
        }
        TypeErrorVariant::DisallowedEleminationSort(s1, s2) => {
            write!(f, "{s1} cannot elemintate into {s2}")
        }
        TypeErrorVariant::DupplicateConstructor(_) => {
            write!(f, "Constructor was previously covered")
        }
        TypeErrorVariant::MissingConstructors(constructors) => {
            write!(f, "Missing the constructors: ")?;
            let mut sep = "";
            for constructor in constructors {
                write!(f, "{sep}{constructor}")?;
                sep = ", ";
            }
            Ok(())
        }
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
            CommandError::ConstructorFailsPositivityCondition => {
                write!(f, "This constructor fails the positivity condition")
            }
            CommandError::ConstructorArgsContainsType => {
                write!(f, "Types from the constructors type family is not allowed as arguments to the constructed type")
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
                    TypeErrorVariant::NotAnInductiveType(_) => Ok(()),
                    TypeErrorVariant::NotAConstructor(_, _, _) => Ok(()),
                    TypeErrorVariant::IncorrectParameterCount(_, _) => Ok(()),
                    TypeErrorVariant::NotOfExpectedInducitve(_, ttype) => {
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    }
                    TypeErrorVariant::DisallowedEleminationSort(_, _) => Ok(()),
                    TypeErrorVariant::DupplicateConstructor(_) => Ok(()),
                    TypeErrorVariant::MissingConstructors(_) => Ok(()),
                    TypeErrorVariant::DebruijnOutOfScope(_) => Ok(()),
                    TypeErrorVariant::UndefinedConst(_) => Ok(()),
                }
            }
        }
    }
}
