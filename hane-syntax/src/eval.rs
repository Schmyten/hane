use std::fmt::{self, Display, Formatter};

use crate::{print::write_term, LoweredCommand, LoweredCommandVariant, Span, SpanError};
use hane_kernel::{CommandError, Global, TypeErrorVariant};

pub enum EvalError {
    CommandError(CommandError<Span, String>),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::CommandError(CommandError::NameAlreadyExists(name)) => {
                write!(f, "The name `{name}` has already been defined")
            }
            EvalError::CommandError(CommandError::TypeError(err)) => {
                let mut names = err.bindings.clone();
                match &err.variant {
                    TypeErrorVariant::NotSubtypeType(expected, actual) => {
                        writeln!(f, "Invalid Subtype")?;
                        write!(f, "Expected: ")?;
                        write_term(f, expected, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, &mut names, 200)
                    }
                    TypeErrorVariant::IncompatibleTypes(expected, actual) => {
                        writeln!(f, "Incompatible Types")?;
                        write!(f, "Expected: ")?;
                        write_term(f, expected, &mut names, 200)?;
                        writeln!(f)?;
                        write!(f, "Actual: ")?;
                        write_term(f, actual, &mut names, 200)
                    }
                    TypeErrorVariant::NotAProduct(ttype) => {
                        writeln!(f, "Expected a product")?;
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    }
                    TypeErrorVariant::NotASort(ttype) => {
                        writeln!(f, "Expected a sort")?;
                        write!(f, "Found: ")?;
                        write_term(f, ttype, &mut names, 200)
                    }
                    TypeErrorVariant::DebruijnOutOfScope(n) => write!(
                        f,
                        "Found debruijn index {n}, but there are only {} names in scope",
                        names.len()
                    ),
                    TypeErrorVariant::UndefinedConst(name) => write!(f, "Unknown constant {name}"),
                }
            }
        }
    }
}

impl LoweredCommand {
    /// Evaluates the command, mutating the global environment acordingly.
    pub fn eval(self, global: &mut Global<Span, String>) -> Result<(), SpanError<EvalError>> {
        match self.variant {
            LoweredCommandVariant::Definition(name, ttype, value) => {
                global.definition(self.span, name, ttype, value)
            }
            LoweredCommandVariant::Axiom(name, ttype) => global.axiom(self.span, name, ttype),
            LoweredCommandVariant::Inductive(params, bodies) => global.inductive(
                self.span,
                params,
                bodies
                    .into_iter()
                    .map(|body| {
                        (
                            body.name,
                            body.ttype,
                            body.constructors
                                .into_iter()
                                .map(|constructor| (constructor.name, constructor.ttype))
                                .collect(),
                        )
                    })
                    .collect(),
            ),
        }
        .map_err(|(span, err)| SpanError {
            span,
            err: EvalError::CommandError(err),
        })
    }
}
