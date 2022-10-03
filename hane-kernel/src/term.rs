use std::cmp::Ordering;

#[derive(Clone)]
pub enum Term<B> {
    Prop,
    Var(usize),
    App(Box<Term<B>>, Box<Term<B>>),
    Product(B, Box<Term<B>>, Box<Term<B>>),
    Abstract(B, Box<Term<B>>, Box<Term<B>>),
    Bind(B, Box<Term<B>>, Box<Term<B>>, Box<Term<B>>),
}

impl<B: Clone> Term<B> {
    //TODO: Find a more descriptive name
    pub fn push(&self, amount: usize) -> Self {
        self.push_inner(0, amount)
    }
    
    fn push_inner(&self, cut: usize, amount: usize) -> Self {
        match self {
            Term::Prop => Term::Prop,
            Term::Var(n) => Term::Var(if cut <= *n { *n + amount } else { *n }),
            Term::App(f, v) => Term::App(Box::new(f.push_inner(cut, amount)), Box::new(v.push_inner(cut, amount))),
            Term::Product(x, x_tp, t) => Term::Product(x.clone(), Box::new(x_tp.push_inner(cut, amount)), Box::new(t.push_inner(cut+1, amount))),
            Term::Abstract(x, x_tp, t) => Term::Abstract(x.clone(), Box::new(x_tp.push_inner(cut, amount)), Box::new(t.push_inner(cut+1, amount))),
            Term::Bind(x, x_tp, x_val, t) => Term::Bind(x.clone(), Box::new(x_tp.push_inner(cut, amount)), Box::new(x_val.push_inner(cut, amount)), Box::new(t.push_inner(cut+1, amount))),
        }
    }

    pub fn subst(&self, mut f: impl FnMut(usize, usize) -> Self) -> Self {
        self.subst_inner(0, &mut f)
    }
    
    fn subst_inner(&self, push: usize, f: &mut impl FnMut(usize, usize) -> Self) -> Self {
        match self {
            Term::Prop => Term::Prop,
            Term::Var(n) => f(*n, push),
            Term::App(t, v) => Term::App(Box::new(t.subst_inner(push, f)), Box::new(v.subst_inner(push, f))),
            Term::Product(x, x_tp, t) => Term::Product(x.clone(), Box::new(x_tp.subst_inner(push, f)), Box::new(t.subst_inner(push+1, f))),
            Term::Abstract(x, x_tp, t) => Term::Abstract(x.clone(), Box::new(x_tp.subst_inner(push, f)), Box::new(t.subst_inner(push+1, f))),
            Term::Bind(x, x_tp, x_val, t) => Term::Bind(x.clone(), Box::new(x_tp.subst_inner(push, f)), Box::new(x_val.subst_inner(push, f)), Box::new(t.subst_inner(push+1, f))),
        }
    }

    pub fn try_subst<E>(&self, mut f: impl FnMut(usize, usize) -> Result<Self, E>) -> Result<Self, E> {
        self.try_subst_inner(0, &mut f)
    }
    
    fn try_subst_inner<E>(&self, push: usize, f: &mut impl FnMut(usize, usize) -> Result<Self, E>) -> Result<Self, E> {
        Ok(match self {
            Term::Prop => Term::Prop,
            Term::Var(n) => f(*n, push)?,
            Term::App(t, v) => Term::App(Box::new(t.try_subst_inner(push, f)?), Box::new(v.try_subst_inner(push, f)?)),
            Term::Product(x, x_tp, t) => Term::Product(x.clone(), Box::new(x_tp.try_subst_inner(push, f)?), Box::new(t.try_subst_inner(push+1, f)?)),
            Term::Abstract(x, x_tp, t) => Term::Abstract(x.clone(), Box::new(x_tp.try_subst_inner(push, f)?), Box::new(t.try_subst_inner(push+1, f)?)),
            Term::Bind(x, x_tp, x_val, t) => Term::Bind(x.clone(), Box::new(x_tp.try_subst_inner(push, f)?), Box::new(x_val.try_subst_inner(push, f)?), Box::new(t.try_subst_inner(push+1, f)?)),
        })
    }

    pub fn subst_single(&self, n: usize, val: &Self) -> Self {
        self.subst(|x, push| {
            match (n + push).cmp(&x) {
                Ordering::Less => Term::Var(x - 1),
                Ordering::Equal => val.push(push),
                Ordering::Greater => Term::Var(x),
            }
        })
    }

    pub fn pop(&self, n: usize) -> Option<Self> {
        self.try_subst(|x, push|
            match (n + push).cmp(&x) {
                Ordering::Less => Ok(Term::Var(x - 1)),
                Ordering::Equal => Err(()),
                Ordering::Greater => Ok(Term::Var(x)),
            }
        ).ok()
    }

    pub fn normalize(&mut self) {
        loop {
            match self {
                Term::Prop => break,
                Term::Var(_) => break,
                Term::App(f, v) => {
                    f.normalize();
                    v.normalize();

                    if let Term::Abstract(_, _, t) = &**f {
                        *self = t.subst_single(0, v);
                        continue;
                    }
                },
                Term::Product(_, t1, t2) => {
                    t1.normalize();
                    t2.normalize();
                },
                Term::Abstract(_, t, b) => {
                    t.normalize();
                    b.normalize();

                    if let Term::App(f, v) = &**b {
                        if let Term::Var(0) = &**v {
                            if let Some(t) = f.pop(0) {
                                *self = t;
                                continue;
                            }
                        }
                    }
                },
                Term::Bind(_, _, val, t) => {
                    val.normalize();
                    *self = t.subst_single(0, val);
                    continue;
                },
            }
            break;
        }
    }
}
