use crate::term::Term;

pub struct Entry<M, B> {
    pub value: Option<Term<M, B>>,
    pub ttype: Term<M, B>,
}

pub struct EntryRef<'a, M, B> {
    pub value: Option<&'a Term<M, B>>,
    pub ttype: &'a Term<M, B>,
}

impl<M, B> Entry<M, B> {
    pub fn new(ttype: Term<M, B>) -> Self {
        Entry { value: None, ttype }
    }
    
    pub fn with_value(value: Term<M, B>, ttype: Term<M, B>) -> Self {
        Entry { value: Some(value), ttype }
    }
}

impl<'a, M, B> EntryRef<'a, M, B> {
    pub fn new(ttype: &'a Term<M, B>) -> Self {
        EntryRef { value: None, ttype }
    }
    
    pub fn with_value(value: &'a Term<M, B>, ttype: &'a Term<M, B>) -> Self {
        EntryRef { value: Some(value), ttype }
    }
}
