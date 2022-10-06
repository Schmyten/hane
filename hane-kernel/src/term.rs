use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

use crate::global::Global;
use crate::stack::Stack;

#[derive(Clone)]
pub enum Term<B> {
    Prop,
    Var(usize),
    Const(String),
    App(Box<Term<B>>, Box<Term<B>>),
    Product(B, Box<Term<B>>, Box<Term<B>>),
    Abstract(B, Box<Term<B>>, Box<Term<B>>),
    Bind(B, Box<Term<B>>, Box<Term<B>>, Box<Term<B>>),
}

pub struct LEntry<B> {
    value: Option<Term<B>>,
    ttype: Term<B>,
}

impl<B> PartialEq for Term<B> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (Self::Const(l0), Self::Const(r0)) => l0 == r0,
            (Self::App(l0, l1), Self::App(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Product(_, l0, l1), Self::Product(_, r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Abstract(_, l0, l1), Self::Abstract(_, r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Bind(_, l0, l1, l2), Self::Bind(_, r0, r1, r2)) => l0 == r0 && l1 == r1 && l2 == r2,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<B> Eq for Term<B> {}

impl<B> Display for Term<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Term::Prop => write!(f, "Prop"),
            Term::Var(n) => write!(f, "'{}", n),
            Term::Const(name) => write!(f, "{}", name),
            Term::App(t1, t2) => write!(f, "({}) ({})", t1, t2),
            Term::Product(_, t1, t2) => write!(f, "forall[{}] ({})", t1, t2),
            Term::Abstract(_, t1, t2) =>  write!(f, "fun[{}] ({})", t1, t2),
            Term::Bind(_, t1, t2, t3) => write!(f, "let[{} : {}] ({})", t1, t2, t3),
        }
    }
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
            Term::Const(name) => Term::Const(name.to_owned()),
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
            Term::Const(name) => Term::Const(name.to_owned()),
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
            Term::Const(name) => Term::Const(name.to_owned()),
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

    pub fn normalize(&mut self, global: &Global<B>, lenv: &mut Stack<LEntry<B>>) {
        loop {
            match self {
                Term::Prop => break,
                Term::Var(n) => {
                    // δ reduction
                    if let Some(value) = &lenv.get(*n).unwrap().value {
                        *self = value.push(*n);
                        continue;
                    }
                },
                Term::Const(name) => {
                    // δ reduction
                    if let Some(value) = global.get(name).unwrap().0 {
                        *self = value.push(lenv.len());
                        continue;
                    }
                }
                Term::App(f, v) => {
                    f.normalize(global, lenv);
                    v.normalize(global, lenv);

                    // β reduction
                    if let Term::Abstract(_, _, t) = &**f {
                        *self = t.subst_single(0, v);
                        continue;
                    }
                },
                Term::Product(_name, input_type, output_type) => {
                    input_type.normalize(global, lenv);
                    lenv.push(LEntry { value: None, ttype: (&**input_type).clone() });
                    output_type.normalize(global, lenv);
                    lenv.pop();
                },
                Term::Abstract(_name, input_type, body) => {
                    input_type.normalize(global, lenv);
                    lenv.push(LEntry { value: None, ttype: (&**input_type).clone() });
                    body.normalize(global, lenv);
                    lenv.pop();
                },
                Term::Bind(_name, _type, val, t) => {
                    val.normalize(global, lenv);
                    // ζ reduction (Remove let binding)
                    *self = t.subst_single(0, val);
                    continue;
                },
            }
            break;
        }
    }

    fn eta(&mut self) {
        match self {
            Term::Prop => {},
            Term::Var(_) => {},
            Term::Const(_) => {},
            Term::App(f, v) => {
                f.eta();
                v.eta();
            },
            Term::Product(_, input_type, output_type) => {
                input_type.eta();
                output_type.eta();
            },
            Term::Abstract(_, input_type, body) => {
                input_type.eta();
                body.eta();
            },
            Term::Bind(_, _, _, _) => unreachable!(),
        }

        if let Term::Abstract(_, _, body) = self {
            if let Term::App(f, v) = &**body {
                if let Term::Var(0) = &**v {
                    if let Some(f) = f.pop(0) {
                        *self = f;
                    }
                }
            }
        }
    }

    pub fn convertable(&self, other: &Self, global: &Global<B>, lenv: &mut Stack<LEntry<B>>) -> bool {
        let mut this = self.clone();
        let mut other = other.clone();
        this.normalize(global, lenv);
        this.eta();
        other.normalize(global, lenv);
        other.eta();
        this == other
    }

    pub fn is_sort(&self, global: &Global<B>, lenv: &mut Stack<LEntry<B>>) -> bool {
        let mut t = self.clone();
        t.normalize(global, lenv);
        if let Term::Prop = t { true } else { false }
    }

    pub fn type_check(&self, global: &Global<B>, lenv: &mut Stack<LEntry<B>>) -> Option<Self> {
        match self {
            Term::Prop => Some(Term::Prop),
            Term::Var(n) => lenv.get(*n).map(|e|e.ttype.push(*n)),
            Term::Const(name) => global.get(name).map(|(_, ttype)|ttype.push(lenv.len())),
            Term::App(f, v) => {
                let mut f_tp = f.type_check(global, lenv)?;
                let v_tp = v.type_check(global, lenv)?;
                f_tp.normalize(global, lenv);
                if let Term::Product(_, input_tp, output_tp) = f_tp {
                    if input_tp.convertable(&v_tp, global, lenv) {
                        Some(output_tp.subst_single(0, v))
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            Term::Product(_, x_tp, t) => {
                let x_sort = x_tp.type_check(global, lenv)?;
                lenv.push(LEntry { value: None, ttype: (&**x_tp).clone() });
                let t_tp = t.type_check(global, lenv);
                lenv.pop();
                let t_tp = t_tp?;
                if x_sort.is_sort(global, lenv) && t_tp.is_sort(global, lenv) {
                    Some(Term::Prop)
                } else {
                    None
                }
            },
            Term::Abstract(x, x_tp, t) => {
                let x_sort = x_tp.type_check(global, lenv)?;
                lenv.push(LEntry { value: None, ttype: (&**x_tp).clone() });
                let t_tp = t.type_check(global, lenv);
                lenv.pop();
                let t_tp = t_tp?;
                if x_sort.is_sort(global, lenv) {
                    Some(Term::Product(x.clone(), x_tp.clone(), Box::new(t_tp)))
                } else {
                    None
                }
            },
            Term::Bind(_, x_tp, x_val, t) => {
                let x_sort = x_tp.type_check(global, lenv)?;
                if let Term::Prop = x_sort {} else { return None }
                let x_val_tp = x_val.type_check(global, lenv)?;
                if !x_tp.convertable(&x_val_tp, global, lenv) { return None }
                let t_subst = t.subst_single(0, x_val);
                lenv.push(LEntry { value: None, ttype: (&**x_tp).clone() });
                let t_tp = t_subst.type_check(global, lenv);
                lenv.pop();
                t_tp
            },
        }
    }
}
