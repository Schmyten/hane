use crate::term::Term;

#[derive(Clone)]
pub struct Binder<M, B> {
    pub x: B,
    pub ttype: Term<M, B>,
}

pub struct Entry<M, B> {
    pub x: B,
    pub value: Option<Term<M, B>>,
    pub ttype: Term<M, B>,
}

pub struct EntryRef<'a, M, B> {
    pub value: Option<&'a Term<M, B>>,
    pub ttype: &'a Term<M, B>,
}

impl<M, B> Entry<M, B> {
    pub fn new(x: B, ttype: Term<M, B>) -> Self {
        Entry {
            x,
            value: None,
            ttype,
        }
    }

    pub fn with_value(x: B, value: Term<M, B>, ttype: Term<M, B>) -> Self {
        Entry {
            x,
            value: Some(value),
            ttype,
        }
    }
}

impl<'a, M, B> EntryRef<'a, M, B> {
    pub fn new(ttype: &'a Term<M, B>) -> Self {
        EntryRef { value: None, ttype }
    }

    pub fn with_value(value: &'a Term<M, B>, ttype: &'a Term<M, B>) -> Self {
        EntryRef {
            value: Some(value),
            ttype,
        }
    }
}

impl<M, B> From<Binder<M, B>> for Entry<M, B> {
    fn from(binder: Binder<M, B>) -> Self {
        Entry::new(binder.x, binder.ttype)
    }
}
