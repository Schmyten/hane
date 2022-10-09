use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

use crate::entry::{Entry, EntryRef};
use crate::{Global, Sort, Stack, TypeError, TypeErrorVariant};

#[derive(Clone)]
pub struct Term<M, B> {
    pub meta: M,
    pub variant: Box<TermVariant<M, B>>,
}

#[derive(Clone)]
pub enum TermVariant<M, B> {
    Sort(Sort),
    Var(usize),
    Const(String),
    App(Term<M, B>, Term<M, B>),
    Product(B, Term<M, B>, Term<M, B>),
    Abstract(B, Term<M, B>, Term<M, B>),
    Bind(B, Term<M, B>, Term<M, B>, Term<M, B>),
}

impl<M, B> PartialEq for Term<M, B> {
    fn eq(&self, other: &Self) -> bool {
        self.variant == other.variant
    }
}

impl<M, B> PartialEq for TermVariant<M, B> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Sort(l0), Self::Sort(r0)) => l0 == r0,
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (Self::Const(l0), Self::Const(r0)) => l0 == r0,
            (Self::App(l0, l1), Self::App(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Product(_, l0, l1), Self::Product(_, r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Abstract(_, l0, l1), Self::Abstract(_, r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Bind(_, l0, l1, l2), Self::Bind(_, r0, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            _ => false,
        }
    }
}

impl<M, B> Eq for Term<M, B> {}
impl<M, B> Eq for TermVariant<M, B> {}

impl<M, B> Display for TermVariant<M, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TermVariant::Sort(sort) => write!(f, "{sort}"),
            TermVariant::Var(n) => write!(f, "'{}", n),
            TermVariant::Const(name) => write!(f, "{}", name),
            TermVariant::App(t1, t2) => write!(f, "({}) ({})", t1, t2),
            TermVariant::Product(_, t1, t2) => write!(f, "forall[{}] ({})", t1, t2),
            TermVariant::Abstract(_, t1, t2) => write!(f, "fun[{}] ({})", t1, t2),
            TermVariant::Bind(_, t1, t2, t3) => write!(f, "let[{} : {}] ({})", t1, t2, t3),
        }
    }
}

impl<M, B> Display for Term<M, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.variant)
    }
}

impl<M: Clone, B: Clone> TermVariant<M, B> {
    fn push_inner(&self, cut: usize, amount: usize) -> Self {
        match self {
            TermVariant::Sort(sort) => TermVariant::Sort(sort.clone()),
            TermVariant::Var(n) => TermVariant::Var(if cut <= *n { *n + amount } else { *n }),
            TermVariant::Const(name) => TermVariant::Const(name.to_owned()),
            TermVariant::App(f, v) => {
                TermVariant::App(f.push_inner(cut, amount), v.push_inner(cut, amount))
            }
            TermVariant::Product(x, x_tp, t) => TermVariant::Product(
                x.clone(),
                x_tp.push_inner(cut, amount),
                t.push_inner(cut + 1, amount),
            ),
            TermVariant::Abstract(x, x_tp, t) => TermVariant::Abstract(
                x.clone(),
                x_tp.push_inner(cut, amount),
                t.push_inner(cut + 1, amount),
            ),
            TermVariant::Bind(x, x_tp, x_val, t) => TermVariant::Bind(
                x.clone(),
                x_tp.push_inner(cut, amount),
                x_val.push_inner(cut, amount),
                t.push_inner(cut + 1, amount),
            ),
        }
    }
}

impl<M: Clone, B: Clone> Term<M, B> {
    //TODO: Find a more descriptive name
    pub fn push(&self, amount: usize) -> Self {
        self.push_inner(0, amount)
    }

    fn push_inner(&self, cut: usize, amount: usize) -> Self {
        Term {
            meta: self.meta.clone(),
            variant: Box::new(self.variant.push_inner(cut, amount)),
        }
    }

    pub fn subst(&self, mut f: impl FnMut(&M, usize, usize) -> Self) -> Self {
        self.subst_inner(0, &mut f)
    }

    fn subst_inner(&self, push: usize, f: &mut impl FnMut(&M, usize, usize) -> Self) -> Self {
        let variant = match &*self.variant {
            TermVariant::Sort(sort) => TermVariant::Sort(sort.clone()),
            TermVariant::Var(n) => return f(&self.meta, *n, push),
            TermVariant::Const(name) => TermVariant::Const(name.to_owned()),
            TermVariant::App(t, v) => {
                TermVariant::App(t.subst_inner(push, f), v.subst_inner(push, f))
            }
            TermVariant::Product(x, x_tp, t) => TermVariant::Product(
                x.clone(),
                x_tp.subst_inner(push, f),
                t.subst_inner(push + 1, f),
            ),
            TermVariant::Abstract(x, x_tp, t) => TermVariant::Abstract(
                x.clone(),
                x_tp.subst_inner(push, f),
                t.subst_inner(push + 1, f),
            ),
            TermVariant::Bind(x, x_tp, x_val, t) => TermVariant::Bind(
                x.clone(),
                x_tp.subst_inner(push, f),
                x_val.subst_inner(push, f),
                t.subst_inner(push + 1, f),
            ),
        };
        Term {
            meta: self.meta.clone(),
            variant: Box::new(variant),
        }
    }

    pub fn try_subst<E>(
        &self,
        mut f: impl FnMut(&M, usize, usize) -> Result<Self, E>,
    ) -> Result<Self, E> {
        self.try_subst_inner(0, &mut f)
    }

    fn try_subst_inner<E>(
        &self,
        push: usize,
        f: &mut impl FnMut(&M, usize, usize) -> Result<Self, E>,
    ) -> Result<Self, E> {
        let variant = match &*self.variant {
            TermVariant::Sort(sort) => TermVariant::Sort(sort.clone()),
            TermVariant::Var(n) => return f(&self.meta, *n, push),
            TermVariant::Const(name) => TermVariant::Const(name.to_owned()),
            TermVariant::App(t, v) => {
                TermVariant::App(t.try_subst_inner(push, f)?, v.try_subst_inner(push, f)?)
            }
            TermVariant::Product(x, x_tp, t) => TermVariant::Product(
                x.clone(),
                x_tp.try_subst_inner(push, f)?,
                t.try_subst_inner(push + 1, f)?,
            ),
            TermVariant::Abstract(x, x_tp, t) => TermVariant::Abstract(
                x.clone(),
                x_tp.try_subst_inner(push, f)?,
                t.try_subst_inner(push + 1, f)?,
            ),
            TermVariant::Bind(x, x_tp, x_val, t) => TermVariant::Bind(
                x.clone(),
                x_tp.try_subst_inner(push, f)?,
                x_val.try_subst_inner(push, f)?,
                t.try_subst_inner(push + 1, f)?,
            ),
        };
        Ok(Term {
            meta: self.meta.clone(),
            variant: Box::new(variant),
        })
    }

    pub fn subst_single(&self, n: usize, val: &Self) -> Self {
        self.subst(|meta, x, push| match (n + push).cmp(&x) {
            Ordering::Less => Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x - 1)),
            },
            Ordering::Equal => val.push(push),
            Ordering::Greater => Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x)),
            },
        })
    }

    pub fn pop(&self, n: usize) -> Option<Self> {
        self.try_subst(|meta, x, push| match (n + push).cmp(&x) {
            Ordering::Less => Ok(Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x - 1)),
            }),
            Ordering::Equal => Err(()),
            Ordering::Greater => Ok(Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x)),
            }),
        })
        .ok()
    }

    pub fn normalize(&mut self, global: &Global<M, B>, lenv: &mut Stack<Entry<M, B>>) {
        loop {
            match &mut *self.variant {
                TermVariant::Sort(_) => break,
                TermVariant::Var(n) => {
                    // δ reduction
                    if let Some(value) = &lenv.get(*n).unwrap().value {
                        *self = value.push(*n);
                        continue;
                    }
                }
                TermVariant::Const(name) => {
                    // δ reduction
                    if let Some(value) = global.get(name).unwrap().value {
                        *self = value.push(lenv.len());
                        continue;
                    }
                }
                TermVariant::App(f, v) => {
                    f.normalize(global, lenv);
                    v.normalize(global, lenv);

                    // β reduction
                    if let TermVariant::Abstract(_, _, t) = &*f.variant {
                        *self = t.subst_single(0, v);
                        continue;
                    }
                }
                TermVariant::Product(x, input_type, output_type) => {
                    input_type.normalize(global, lenv);
                    lenv.push(Entry::new(x.clone(), input_type.clone()));
                    output_type.normalize(global, lenv);
                    lenv.pop();
                }
                TermVariant::Abstract(x, input_type, body) => {
                    input_type.normalize(global, lenv);
                    lenv.push(Entry::new(x.clone(), input_type.clone()));
                    body.normalize(global, lenv);
                    lenv.pop();
                }
                TermVariant::Bind(_name, _type, val, t) => {
                    val.normalize(global, lenv);
                    // ζ reduction (Remove let binding)
                    *self = t.subst_single(0, val);
                    continue;
                }
            }
            break;
        }
    }

    fn eta(&mut self) {
        match &mut *self.variant {
            TermVariant::Sort(_) => {}
            TermVariant::Var(_) => {}
            TermVariant::Const(_) => {}
            TermVariant::App(f, v) => {
                f.eta();
                v.eta();
            }
            TermVariant::Product(_, input_type, output_type) => {
                input_type.eta();
                output_type.eta();
            }
            TermVariant::Abstract(_, input_type, body) => {
                input_type.eta();
                body.eta();
            }
            TermVariant::Bind(_, _, _, _) => unreachable!(),
        }

        if let TermVariant::Abstract(_, _, body) = &*self.variant {
            if let TermVariant::App(f, v) = &*body.variant {
                if let TermVariant::Var(0) = &*v.variant {
                    if let Some(f) = f.pop(0) {
                        *self = f;
                    }
                }
            }
        }
    }

    pub fn expect_convertable(
        &self,
        other: &Self,
        global: &Global<M, B>,
        lenv: &mut Stack<Entry<M, B>>,
    ) -> Result<(), TypeError<M, B>> {
        let mut this = self.clone();
        let mut other = other.clone();
        this.normalize(global, lenv);
        this.eta();
        other.normalize(global, lenv);
        other.eta();
        if this == other {
            Ok(())
        } else {
            Err(TypeError::new(
                lenv,
                TypeErrorVariant::IncompatibleTypes(other.clone(), self.clone()),
            ))
        }
    }

    fn subtype_inner(&self, other: &Self, global: &Global<M, B>) -> bool {
        match (&*self.variant, &*other.variant) {
            (TermVariant::Sort(l), TermVariant::Sort(r)) => l <= r,
            (TermVariant::Product(_, l0, l1), TermVariant::Product(_, r0, r1)) => {
                l0 == r0 && l1.subtype_inner(r1, global)
            }
            (l, r) => l == r,
        }
    }

    pub fn expect_subtype(
        &self,
        other: &Self,
        global: &Global<M, B>,
        lenv: &mut Stack<Entry<M, B>>,
    ) -> Result<(), TypeError<M, B>> {
        let mut this = self.clone();
        let mut other = other.clone();
        this.normalize(global, lenv);
        this.eta();
        other.normalize(global, lenv);
        other.eta();
        if this.subtype_inner(&other, global) {
            Ok(())
        } else {
            Err(TypeError::new(
                lenv,
                TypeErrorVariant::IncompatibleTypes(other.clone(), self.clone()),
            ))
        }
    }

    pub fn strip_products(mut self) -> (Vec<(B, Self)>, Self) {
        let mut arity = Vec::new();
        while let TermVariant::Product(x, ttype, body) = *self.variant {
            arity.push((x, ttype));
            self = body
        }
        (arity, self)
    }

    pub fn strip_args(mut self) -> (Self, Vec<Self>) {
        let mut args = Vec::new();
        while let TermVariant::App(fun, arg) = *self.variant {
            args.push(arg);
            self = fun
        }
        args.reverse();
        (self, args)
    }

    pub fn expect_sort(
        &self,
        global: &Global<M, B>,
        lenv: &mut Stack<Entry<M, B>>,
    ) -> Result<Sort, TypeError<M, B>> {
        let mut t = self.clone();
        t.normalize(global, lenv);
        if let TermVariant::Sort(sort) = *t.variant {
            Ok(sort)
        } else {
            Err(TypeError::new(
                lenv,
                TypeErrorVariant::NotASort(self.clone()),
            ))
        }
    }

    pub fn expect_product(
        mut self,
        global: &Global<M, B>,
        lenv: &mut Stack<Entry<M, B>>,
    ) -> Result<(Self, Self), TypeError<M, B>> {
        self.normalize(global, lenv);
        if let TermVariant::Product(_, input_type, output_type) = *self.variant {
            Ok((input_type, output_type))
        } else {
            Err(TypeError::new(lenv, TypeErrorVariant::NotASort(self)))
        }
    }

    pub fn type_check(
        &self,
        global: &Global<M, B>,
        lenv: &mut Stack<Entry<M, B>>,
    ) -> Result<Self, (M, TypeError<M, B>)> {
        Ok(match &*self.variant {
            TermVariant::Sort(sort) => Term {
                meta: self.meta.clone(),
                variant: Box::new(TermVariant::Sort(sort.ttype())),
            },
            TermVariant::Var(n) => {
                return lenv.get(*n).map(|e| e.ttype.push(*n)).ok_or_else(|| {
                    (
                        self.meta.clone(),
                        TypeError::new(lenv, TypeErrorVariant::DebruijnOutOfScope(*n)),
                    )
                })
            }
            TermVariant::Const(name) => {
                return global
                    .get(name)
                    .map(|EntryRef { ttype, .. }| ttype.push(lenv.len()))
                    .ok_or_else(|| {
                        (
                            self.meta.clone(),
                            TypeError::new(lenv, TypeErrorVariant::UndefinedConst(name.clone())),
                        )
                    })
            }
            TermVariant::App(f, v) => {
                let f_tp = f.type_check(global, lenv)?;
                let (input_type, output_type) = f_tp
                    .expect_product(global, lenv)
                    .map_err(|err| (f.meta.clone(), err))?;
                let v_tp = v.type_check(global, lenv)?;
                v_tp.expect_subtype(&input_type, global, lenv)
                    .map_err(|err| (self.meta.clone(), err))?;
                output_type.subst_single(0, v)
            }
            TermVariant::Product(x, x_tp, t) => {
                let x_sort = x_tp.type_check(global, lenv)?;
                let x_sort = x_sort
                    .expect_sort(global, lenv)
                    .map_err(|err| (x_tp.meta.clone(), err))?;
                lenv.push(Entry::new(x.clone(), x_tp.clone()));
                let t_tp = t.type_check(global, lenv);
                lenv.pop();
                let t_tp = t_tp?;
                let t_sort = t_tp
                    .expect_sort(global, lenv)
                    .map_err(|err| (t.meta.clone(), err))?;
                Term {
                    meta: self.meta.clone(),
                    variant: Box::new(TermVariant::Sort(x_sort.product(t_sort))),
                }
            }
            TermVariant::Abstract(x, x_tp, t) => {
                let x_sort = x_tp.type_check(global, lenv)?;
                x_sort
                    .expect_sort(global, lenv)
                    .map_err(|err| (x_tp.meta.clone(), err))?;
                lenv.push(Entry::new(x.clone(), x_tp.clone()));
                let t_tp = t.type_check(global, lenv);
                lenv.pop();
                let t_tp = t_tp?;
                Term {
                    meta: self.meta.clone(),
                    variant: Box::new(TermVariant::Product(x.clone(), x_tp.clone(), t_tp)),
                }
            }
            TermVariant::Bind(x, x_tp, x_val, t) => {
                let x_sort = x_tp.type_check(global, lenv)?;
                x_sort
                    .expect_sort(global, lenv)
                    .map_err(|err| (x_tp.meta.clone(), err))?;
                let x_val_tp = x_val.type_check(global, lenv)?;
                x_val_tp
                    .expect_subtype(x_tp, global, lenv)
                    .map_err(|err| (x_val.meta.clone(), err))?;
                let t_subst = t.subst_single(0, x_val);
                lenv.push(Entry::new(x.clone(), x_tp.clone()));
                let t_tp = t_subst.type_check(global, lenv);
                lenv.pop();
                t_tp?
            }
        })
    }
}
