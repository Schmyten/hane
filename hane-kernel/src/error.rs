use crate::{entry::Entry, Stack, Term};

pub enum CommandError<M, B> {
    NameAlreadyExists(String),
    TypeError(TypeError<M, B>),
}

pub struct TypeError<M, B> {
    pub bindings: Stack<B>,
    pub variant: TypeErrorVariant<M, B>,
}

pub enum TypeErrorVariant<M, B> {
    NotSubtypeType(Term<M, B>, Term<M, B>),
    IncompatibleTypes(Term<M, B>, Term<M, B>),
    NotAProduct(Term<M, B>),
    NotASort(Term<M, B>),
    DebruijnOutOfScope(usize),
    UndefinedConst(String),
}

impl<M, B: Clone> TypeError<M, B> {
    pub fn new(lenv: &mut Stack<Entry<M, B>>, variant: TypeErrorVariant<M, B>) -> Self {
        TypeError {
            bindings: lenv.iter().rev().map(|entry| entry.x.clone()).collect(),
            variant,
        }
    }
}
