use crate::{entry::Entry, Sort, Stack, Term};

pub enum CommandError<M, B> {
    NameAlreadyExists(String),
    ConstructorArgsContainsType,
    TypeError(TypeError<M, B>),
}

pub struct TypeError<M, B> {
    /// the local binding context of the terms in the error
    pub local: Stack<Entry<M, B>>,
    pub variant: TypeErrorVariant<M, B>,
}

pub enum TypeErrorVariant<M, B> {
    NotSubtypeType(Term<M, B>, Term<M, B>),
    IncompatibleTypes(Term<M, B>, Term<M, B>),
    NotAProduct(Term<M, B>),
    NotASort(Term<M, B>),
    NotAnInductiveType(String),
    NotAConstructor(String, String, Vec<String>),
    IncorrectParameterCount(usize, usize),
    NotOfExpectedInducitve(String, Term<M, B>),
    DisallowedEleminationSort(Sort, Sort),
    DupplicateConstructor(String),
    MissingConstructors(Vec<String>),
    DebruijnOutOfScope(usize),
    UndefinedConst(String),
}

impl<M: Clone, B: Clone> TypeError<M, B> {
    pub fn new(local: &Stack<Entry<M, B>>, variant: TypeErrorVariant<M, B>) -> Self {
        TypeError {
            local: local.clone(),
            variant,
        }
    }
}
