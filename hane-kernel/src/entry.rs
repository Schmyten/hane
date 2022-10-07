use crate::term::Term;

pub struct Entry<M, B> {
    pub value: Option<Term<M, B>>,
    pub ttype: Term<M, B>,
}

pub struct EntryRef<'a, M, B> {
    pub value: Option<&'a Term<M, B>>,
    pub ttype: &'a Term<M, B>,
}
