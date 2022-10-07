use crate::{Stack, Term};

pub enum CommandError<M, B> {
    NameAlreadyExists(String),
    TypeError(TypeError<M, B>),
}

pub struct TypeError<M, B> {
    bindings: Vec<B>,
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

impl<M, B> TypeError<M, B> {
    pub fn new(variant: TypeErrorVariant<M, B>) -> Self {
        TypeError { bindings: Vec::new(), variant }
    }

    pub fn bind(mut self, bind: B) -> Self {
        self.bindings.push(bind);
        self
    }

    pub fn bindings(&self) -> Stack<B> where B: Clone {
        self.bindings.iter().rev().cloned().collect()
    }
}
