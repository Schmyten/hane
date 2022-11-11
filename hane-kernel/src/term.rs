use std::cmp::Ordering;
use std::convert::Infallible;
use std::fmt::{self, Display, Formatter};

use crate::entry::{Binder, Entry, EntryRef};
use crate::global::GEntryRef;
use crate::{Global, Sort, Stack, TypeError, TypeErrorVariant};

pub trait TermLike<M: Clone, B: Clone>: Sized {
    /// Attempts to replace all occurrences of `Var(n)` with the result of `f(meta, n, push)`
    /// where `push` is the amount of local binders passed to find the variable.
    ///
    /// This function returns an error if any of the replacements fails.
    fn try_subst_inner<E>(
        &self,
        push: usize,
        f: &mut impl FnMut(&M, usize, usize) -> Result<Term<M, B>, E>,
    ) -> Result<Self, E>;

    /// Attempts to replace all occurrences of `Var(n)` with the result of `f(meta, n, push)`
    ///
    /// This function returns an error if any of the replacements fails.
    fn try_subst<E>(
        &self,
        mut f: impl FnMut(&M, usize, usize) -> Result<Term<M, B>, E>,
    ) -> Result<Self, E> {
        self.try_subst_inner(0, &mut f)
    }

    /// Replaces all occurrences of `Var(n)` with the result of `f(meta, n, push)`
    fn subst(&self, mut f: impl FnMut(&M, usize, usize) -> Term<M, B>) -> Self {
        let res = self.try_subst::<Infallible>(|meta, x, push| Ok(f(meta, x, push)));
        match res {
            Ok(t) => t,
            Err(i) => match i {},
        }
    }

    /// Changes the context of the term to include `amount` new local variables.
    fn move_in(&self, amount: usize) -> Self {
        self.subst(|meta, n, cut| {
            let n = if cut <= n { n + amount } else { n };
            Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(n)),
            }
        })
    }

    /// Attempts to remove the newest `amount` local variables from the context of the term
    ///
    /// This function return `None` if the term contains any of the removed variables
    fn move_out(&self, amount: usize) -> Option<Self> {
        self.try_subst(|meta, x, push| match push.cmp(&x) {
            Ordering::Less => Ok(Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x - amount)),
            }),
            Ordering::Equal => Err(()),
            Ordering::Greater => Ok(Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x)),
            }),
        })
        .ok()
    }

    /// Replace the `n`th newest local variable with `val`, as well as removing that variable from the context of the term.
    fn subst_single(&self, n: usize, val: &Term<M, B>) -> Self {
        self.subst(|meta, x, push| match (n + push).cmp(&x) {
            Ordering::Less => Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x - 1)),
            },
            Ordering::Equal => val.move_in(push),
            Ordering::Greater => Term {
                meta: meta.clone(),
                variant: Box::new(TermVariant::Var(x)),
            },
        })
    }

    /// Replaces a range of local variables with the results of `vals` thought of as a slice of terms.
    ///
    /// The range starts from the `n + len - 1`th newest and end with the `n`th newest  (included)
    ///
    /// The context of results of `vals` should be the context of the term without that range of variables.
    fn subst_many<'a>(
        &'a self,
        n: usize,
        len: usize,
        vals: impl Fn(usize) -> &'a Term<M, B>,
    ) -> Self
    where
        M: 'a,
        B: 'a,
    {
        self.subst(|meta, x, push| {
            if x < n + push {
                Term {
                    meta: meta.clone(),
                    variant: Box::new(TermVariant::Var(x)),
                }
            } else if x < n + push + len {
                let i = n + push + len - 1 - x;
                vals(i).move_in(push)
            } else {
                Term {
                    meta: meta.clone(),
                    variant: Box::new(TermVariant::Var(x - len)),
                }
            }
        })
    }
}

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
    Product(Binder<M, B>, Term<M, B>),
    Abstract(Binder<M, B>, Term<M, B>),
    Bind(B, Term<M, B>, Term<M, B>, Term<M, B>),
    Match(Term<M, B>, B, MatchArm<M, B>, Vec<MatchArm<M, B>>),
}

#[derive(Clone)]
pub struct MatchArm<M, B> {
    pub meta: M,
    pub constructor: String,
    pub params: Vec<B>,
    pub body: Term<M, B>,
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
            (Self::Product(l0, l1), Self::Product(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Abstract(l0, l1), Self::Abstract(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Bind(_, l0, l1, l2), Self::Bind(_, r0, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            (Self::Match(l0, _, l1, l2), Self::Match(r0, _, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            _ => false,
        }
    }
}

impl<M, B> PartialEq for MatchArm<M, B> {
    fn eq(&self, other: &Self) -> bool {
        self.constructor == other.constructor && self.body == other.body
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
            TermVariant::Product(binder, body) => write!(f, "forall[{}] ({})", binder.ttype, body),
            TermVariant::Abstract(binder, body) => write!(f, "fun[{}] ({})", binder.ttype, body),
            TermVariant::Bind(_, t1, t2, t3) => write!(f, "let[{} : {}] ({})", t1, t2, t3),
            TermVariant::Match(t, _, ret, arms) => {
                write!(
                    f,
                    "match {t} in {} return {} with",
                    ret.constructor, ret.body
                )?;
                let mut sep = "";
                for arm in arms {
                    write!(f, "{sep} {} => {}", arm.constructor, arm.body)?;
                    sep = " |";
                }
                write!(f, " end")
            }
        }
    }
}

impl<M, B> Display for Term<M, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.variant)
    }
}

impl<M: Clone, B: Clone> TermLike<M, B> for Term<M, B> {
    fn try_subst_inner<E>(
        &self,
        push: usize,
        f: &mut impl FnMut(&M, usize, usize) -> Result<Term<M, B>, E>,
    ) -> Result<Self, E> {
        let variant = match &*self.variant {
            TermVariant::Sort(sort) => TermVariant::Sort(sort.clone()),
            TermVariant::Var(n) => return f(&self.meta, *n, push),
            TermVariant::Const(name) => TermVariant::Const(name.to_owned()),
            TermVariant::App(t, v) => {
                TermVariant::App(t.try_subst_inner(push, f)?, v.try_subst_inner(push, f)?)
            }
            TermVariant::Product(binder, body) => TermVariant::Product(
                binder.try_subst_inner(push, f)?,
                body.try_subst_inner(push + 1, f)?,
            ),
            TermVariant::Abstract(binder, body) => TermVariant::Abstract(
                binder.try_subst_inner(push, f)?,
                body.try_subst_inner(push + 1, f)?,
            ),
            TermVariant::Bind(x, x_tp, x_val, t) => TermVariant::Bind(
                x.clone(),
                x_tp.try_subst_inner(push, f)?,
                x_val.try_subst_inner(push, f)?,
                t.try_subst_inner(push + 1, f)?,
            ),
            TermVariant::Match(t, x, ret, arms) => TermVariant::Match(
                t.try_subst_inner(push, f)?,
                x.clone(),
                ret.try_subst_inner(push + 1, f)?,
                arms.try_subst_inner(push, f)?,
            ),
        };
        Ok(Term {
            meta: self.meta.clone(),
            variant: Box::new(variant),
        })
    }
}

impl<M: Clone, B: Clone> TermLike<M, B> for Binder<M, B> {
    fn try_subst_inner<E>(
        &self,
        push: usize,
        f: &mut impl FnMut(&M, usize, usize) -> Result<Term<M, B>, E>,
    ) -> Result<Self, E> {
        Ok(Binder {
            x: self.x.clone(),
            ttype: self.ttype.try_subst_inner(push, f)?,
        })
    }
}

impl<M: Clone, B: Clone> TermLike<M, B> for MatchArm<M, B> {
    fn try_subst_inner<E>(
        &self,
        push: usize,
        f: &mut impl FnMut(&M, usize, usize) -> Result<Term<M, B>, E>,
    ) -> Result<Self, E> {
        Ok(MatchArm {
            meta: self.meta.clone(),
            constructor: self.constructor.clone(),
            params: self.params.clone(),
            body: self.body.try_subst_inner(push + self.params.len() + 1, f)?,
        })
    }
}

impl<M: Clone, B: Clone, T: TermLike<M, B>> TermLike<M, B> for Vec<T> {
    fn try_subst_inner<E>(
        &self,
        push: usize,
        f: &mut impl FnMut(&M, usize, usize) -> Result<Term<M, B>, E>,
    ) -> Result<Self, E> {
        self.iter().map(|x| x.try_subst_inner(push, f)).collect()
    }
}

impl<M: Clone, B: Clone> Term<M, B> {
    pub fn validate_consts<E>(
        &self,
        mut f: impl FnMut(&str) -> Result<(), E>,
    ) -> Result<(), (M, E)> {
        self.validate_consts_inner(&mut f)
    }

    fn validate_consts_inner<E>(
        &self,
        f: &mut impl FnMut(&str) -> Result<(), E>,
    ) -> Result<(), (M, E)> {
        match &*self.variant {
            TermVariant::Sort(_) => Ok(()),
            TermVariant::Var(_) => Ok(()),
            TermVariant::Const(name) => f(name).map_err(|e| (self.meta.clone(), e)),
            TermVariant::App(t1, t2) => {
                t1.validate_consts_inner(f)?;
                t2.validate_consts_inner(f)
            }
            TermVariant::Product(binder, body) => {
                binder.ttype.validate_consts_inner(f)?;
                body.validate_consts_inner(f)
            }
            TermVariant::Abstract(binder, body) => {
                binder.ttype.validate_consts_inner(f)?;
                body.validate_consts_inner(f)
            }
            TermVariant::Bind(_, t1, t2, t3) => {
                t1.validate_consts_inner(f)?;
                t2.validate_consts_inner(f)?;
                t3.validate_consts_inner(f)
            }
            TermVariant::Match(t, _, ret, arms) => {
                t.validate_consts_inner(f)?;
                ret.body.validate_consts_inner(f)?;
                arms.iter()
                    .try_for_each(|arm| arm.body.validate_consts_inner(f))
            }
        }
    }

    pub fn strict_positivity(
        &self,
        global: &Global<M, B>,
        mut f: impl FnMut(&str) -> bool,
    ) -> bool {
        self.strict_positivity_inner(global, &mut f)
    }

    fn strict_positivity_inner(
        mut self: &Self,
        global: &Global<M, B>,
        f: &mut impl FnMut(&str) -> bool,
    ) -> bool {
        while let TermVariant::Product(binder, body) = &*self.variant {
            if binder
                .ttype
                .validate_consts(|name| (!f(name)).then_some(()).ok_or(()))
                .is_err()
            {
                return false;
            }
            self = body;
        }

        if self
            .validate_consts(|name| (!f(name)).then_some(()).ok_or(()))
            .is_ok()
        {
            return true;
        }

        let (hd, args) = self.strip_args_ref();
        let hd = if let TermVariant::Const(hd) = &*hd.variant {
            hd
        } else {
            return false;
        };

        if f(hd) {
            args.iter().all(|arg| {
                arg.validate_consts(|name| (!f(name)).then_some(()).ok_or(()))
                    .is_ok()
            })
        } else if let Some(GEntryRef::Inductive(_, _params, bodies)) = global.get_entry(hd) {
            if bodies.len() != 1 {
                return false;
            }
            let _body = &bodies[1];
            //TODO: [Nested Positivity](https://coq.inria.fr/distrib/current/refman/language/core/inductive.html#nested-positivity)
            unimplemented!();
        } else {
            false
        }
    }

    pub fn normalize(&mut self, global: &Global<M, B>, local: &mut Stack<Entry<M, B>>) {
        loop {
            match &mut *self.variant {
                TermVariant::Sort(_) => break,
                TermVariant::Var(n) => {
                    // δ reduction
                    if let Some(value) = &local.get(*n).unwrap().value {
                        // To move the value into scope, it must first be pushed passed it self, then passed the other `n`
                        *self = value.move_in(*n + 1);
                        continue;
                    }
                }
                TermVariant::Const(name) => {
                    // δ reduction
                    if let Some(value) = global.get(name).unwrap().value {
                        *self = value.move_in(local.len());
                        continue;
                    }
                }
                TermVariant::App(f, v) => {
                    f.normalize(global, local);
                    v.normalize(global, local);

                    // β reduction
                    if let TermVariant::Abstract(_, t) = &*f.variant {
                        *self = t.subst_single(0, v);
                        continue;
                    }
                }
                TermVariant::Product(binder, body) => {
                    binder.ttype.normalize(global, local);
                    let mut local = local.push(binder.clone().into());
                    body.normalize(global, &mut local);
                }
                TermVariant::Abstract(binder, body) => {
                    binder.ttype.normalize(global, local);
                    let mut local = local.push(binder.clone().into());
                    body.normalize(global, &mut local);
                }
                TermVariant::Bind(_name, _type, val, t) => {
                    val.normalize(global, local);
                    // ζ reduction (Remove let binding)
                    *self = t.subst_single(0, val);
                    continue;
                }
                TermVariant::Match(t, name, ret, arms) => {
                    t.normalize(global, local);

                    // ι reduction (Evaluate match expresions)
                    if let TermVariant::Const(constructor) = &*t.app_head().variant {
                        if let Some(arm) =
                            arms.iter_mut().find(|arm| arm.constructor == *constructor)
                        {
                            let body = Term {
                                meta: arm.body.meta.clone(),
                                variant: Box::new(TermVariant::Sort(Sort::Prop)),
                            };
                            let body = std::mem::replace(&mut arm.body, body);
                            let t_rep = Term {
                                meta: t.meta.clone(),
                                variant: Box::new(TermVariant::Sort(Sort::Prop)),
                            };
                            let t = std::mem::replace(t, t_rep);
                            let (_, args) = t.strip_args();
                            *self = body.subst_many(0, args.len(), |i| &args[i]);
                            continue;
                        }
                    }

                    let (i, params, bodies) = match global.get_entry(&ret.constructor) {
                        Some(GEntryRef::Inductive(i, params, bodies)) => (i, params, bodies),
                        Some(_) => panic!("{} is not an inductive type", ret.constructor),
                        None => panic!("{} is not defined", ret.constructor),
                    };
                    let body = &bodies[i];

                    let mut t_type = t.type_check(global, local).ok().unwrap();
                    t_type.normalize(global, local);
                    let (hd, mut args) = t_type.strip_args();
                    if !hd.is_const(&ret.constructor) {
                        panic!("{i} is not the inductive type {}", ret.constructor)
                    }
                    args.truncate(params.len());

                    {
                        let mut local = local.slot();
                        local.extend(ret.params.iter().zip(params).zip(&args).enumerate().map(
                            |(i, ((x, param), value))| {
                                Entry::with_value(x.clone(), value.move_in(i), param.ttype.clone())
                            },
                        ));
                        local.extend(
                            ret.params[params.len()..]
                                .iter()
                                .zip(&body.arity)
                                .map(|(x, param)| Entry::new(x.clone(), param.ttype.clone())),
                        );
                        let ttype = (0..ret.params.len())
                            .rev()
                            .map(|n| Term {
                                meta: self.meta.clone(),
                                variant: Box::new(TermVariant::Var(n)),
                            })
                            .fold(
                                Term {
                                    meta: self.meta.clone(),
                                    variant: Box::new(TermVariant::Const(ret.constructor.clone())),
                                },
                                |f, v| Term {
                                    meta: self.meta.clone(),
                                    variant: Box::new(TermVariant::App(f, v)),
                                },
                            );
                        local.push_onto(Entry::new(name.clone(), ttype));
                        ret.body.normalize(global, &mut local);
                    }

                    for arm in arms {
                        let constructor = if let Some(c) =
                            body.constructors.iter().find(|c| c.name == arm.constructor)
                        {
                            c
                        } else {
                            panic!("{} is not a constructor of {}", arm.constructor, body.name)
                        };

                        let mut local = local.slot();
                        local.extend(arm.params.iter().zip(params).zip(&args).enumerate().map(
                            |(i, ((x, param), value))| {
                                Entry::with_value(x.clone(), value.move_in(i), param.ttype.clone())
                            },
                        ));
                        local.extend(
                            arm.params[params.len()..]
                                .iter()
                                .zip(&constructor.arity)
                                .map(|(x, param)| Entry::new(x.clone(), param.ttype.clone())),
                        );
                        arm.body.normalize(global, &mut local);
                    }
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
            TermVariant::Product(binder, body) => {
                binder.ttype.eta();
                body.eta();
            }
            TermVariant::Abstract(binder, body) => {
                binder.ttype.eta();
                body.eta();
            }
            TermVariant::Bind(_, _, _, _) => unreachable!(),
            TermVariant::Match(t, _, ret, arms) => {
                t.eta();
                ret.body.eta();
                for arm in arms {
                    arm.body.eta();
                }
            }
        }

        if let TermVariant::Abstract(_, body) = &*self.variant {
            if let TermVariant::App(f, v) = &*body.variant {
                if let TermVariant::Var(0) = &*v.variant {
                    if let Some(f) = f.move_out(1) {
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
        local: &mut Stack<Entry<M, B>>,
    ) -> Result<(), TypeError<M, B>> {
        let mut this = self.clone();
        let mut other = other.clone();
        this.normalize(global, local);
        this.eta();
        other.normalize(global, local);
        other.eta();
        if this == other {
            Ok(())
        } else {
            Err(TypeError::new(
                local,
                TypeErrorVariant::IncompatibleTypes(other.clone(), self.clone()),
            ))
        }
    }

    fn subtype_inner(&self, other: &Self) -> bool {
        match (&*self.variant, &*other.variant) {
            (TermVariant::Sort(l), TermVariant::Sort(r)) => l <= r,
            (TermVariant::Product(l0, l1), TermVariant::Product(r0, r1)) => {
                l0 == r0 && l1.subtype_inner(r1)
            }
            (l, r) => l == r,
        }
    }

    pub fn expect_subtype(
        &self,
        other: &Self,
        global: &Global<M, B>,
        local: &mut Stack<Entry<M, B>>,
    ) -> Result<(), TypeError<M, B>> {
        let mut this = self.clone();
        let mut other0 = other.clone();
        this.normalize(global, local);
        this.eta();
        other0.normalize(global, local);
        other0.eta();
        if this.subtype_inner(&other0) {
            Ok(())
        } else {
            Err(TypeError::new(
                local,
                TypeErrorVariant::NotSubtypeType(other.clone(), self.clone()),
            ))
        }
    }

    /// Seperates terms of the form `forall (x1 : T1) .. (xn : Tn), t` into `([(x1 : T1), .. , (xn : Tn)], t)`.
    /// If the input is not a product, it is returned unchanged.
    pub fn strip_products(mut self) -> (Vec<Binder<M, B>>, Self) {
        let mut arity = Vec::new();
        while let TermVariant::Product(binder, body) = *self.variant {
            arity.push(binder);
            self = body
        }
        (arity, self)
    }

    /// Seperates terms of the form `f v1 .. vn` into `(f, [v1, .. , vn])`.
    /// If the input is not an application, it is returned unchanged.
    pub fn strip_args(mut self) -> (Self, Vec<Self>) {
        let mut args = Vec::new();
        while let TermVariant::App(fun, arg) = *self.variant {
            args.push(arg);
            self = fun
        }
        args.reverse();
        (self, args)
    }

    /// Seperates terms of the form `forall (x1 : T1) .. (xn : Tn), t` into `([(x1 : T1), .. , (xn : Tn)], t)`.
    /// If the input is not a product, it is returned unchanged.
    pub fn strip_args_ref(mut self: &Self) -> (&Self, Vec<&Self>) {
        let mut args = Vec::new();
        while let TermVariant::App(fun, arg) = &*self.variant {
            args.push(arg);
            self = fun
        }
        args.reverse();
        (self, args)
    }

    /// Finds the application head of a term.
    /// If the input is not an application, it is returned unchanged.
    pub fn app_head(mut self: &Self) -> &Self {
        while let TermVariant::App(fun, _) = &*self.variant {
            self = fun
        }
        self
    }

    pub fn expect_sort(
        &self,
        global: &Global<M, B>,
        local: &mut Stack<Entry<M, B>>,
    ) -> Result<Sort, TypeError<M, B>> {
        let mut t = self.clone();
        t.normalize(global, local);
        if let TermVariant::Sort(sort) = *t.variant {
            Ok(sort)
        } else {
            Err(TypeError::new(
                local,
                TypeErrorVariant::NotASort(self.clone()),
            ))
        }
    }

    pub fn expect_product(
        mut self,
        global: &Global<M, B>,
        local: &mut Stack<Entry<M, B>>,
    ) -> Result<(Self, Self), TypeError<M, B>> {
        self.normalize(global, local);
        if let TermVariant::Product(binder, body) = *self.variant {
            Ok((binder.ttype, body))
        } else {
            Err(TypeError::new(local, TypeErrorVariant::NotAProduct(self)))
        }
    }

    /// Return whether the term is a constant named `name`.
    pub fn is_const(&self, name: &str) -> bool {
        if let TermVariant::Const(c) = &*self.variant {
            c == name
        } else {
            false
        }
    }

    pub fn type_check(
        &self,
        global: &Global<M, B>,
        local: &mut Stack<Entry<M, B>>,
    ) -> Result<Self, (M, TypeError<M, B>)> {
        Ok(match &*self.variant {
            TermVariant::Sort(sort) => Term {
                meta: self.meta.clone(),
                variant: Box::new(TermVariant::Sort(sort.ttype())),
            },
            TermVariant::Var(n) => {
                // To move the type into scope, it must first be pushed passed it self, then passed the other `n`
                return local
                    .get(*n)
                    .map(|e| e.ttype.move_in(*n + 1))
                    .ok_or_else(|| {
                        (
                            self.meta.clone(),
                            TypeError::new(local, TypeErrorVariant::DebruijnOutOfScope(*n)),
                        )
                    });
            }
            TermVariant::Const(name) => {
                return global
                    .get(name)
                    .map(|EntryRef { ttype, .. }| ttype.move_in(local.len()))
                    .ok_or_else(|| {
                        (
                            self.meta.clone(),
                            TypeError::new(local, TypeErrorVariant::UndefinedConst(name.clone())),
                        )
                    })
            }
            TermVariant::App(f, v) => {
                let f_tp = f.type_check(global, local)?;
                let (input_type, output_type) = f_tp
                    .expect_product(global, local)
                    .map_err(|err| (f.meta.clone(), err))?;
                let v_tp = v.type_check(global, local)?;
                v_tp.expect_subtype(&input_type, global, local)
                    .map_err(|err| (self.meta.clone(), err))?;
                output_type.subst_single(0, v)
            }
            TermVariant::Product(binder, body) => {
                let x_sort = binder.ttype.type_check(global, local)?;
                let x_sort = x_sort
                    .expect_sort(global, local)
                    .map_err(|err| (binder.ttype.meta.clone(), err))?;
                let mut local = local.push(binder.clone().into());
                let body_tp = body.type_check(global, &mut local)?;
                let t_sort = body_tp
                    .expect_sort(global, &mut local)
                    .map_err(|err| (body.meta.clone(), err))?;
                Term {
                    meta: self.meta.clone(),
                    variant: Box::new(TermVariant::Sort(x_sort.product(t_sort))),
                }
            }
            TermVariant::Abstract(binder, body) => {
                let x_sort = binder.ttype.type_check(global, local)?;
                x_sort
                    .expect_sort(global, local)
                    .map_err(|err| (binder.ttype.meta.clone(), err))?;
                let mut local = local.push(binder.clone().into());
                let body = body.type_check(global, &mut local)?;
                Term {
                    meta: self.meta.clone(),
                    variant: Box::new(TermVariant::Product(binder.clone(), body)),
                }
            }
            TermVariant::Bind(x, x_tp, x_val, t) => {
                let x_sort = x_tp.type_check(global, local)?;
                x_sort
                    .expect_sort(global, local)
                    .map_err(|err| (x_tp.meta.clone(), err))?;
                let x_val_tp = x_val.type_check(global, local)?;
                x_val_tp
                    .expect_subtype(x_tp, global, local)
                    .map_err(|err| (x_val.meta.clone(), err))?;
                let t_subst = t.subst_single(0, x_val);
                let mut local = local.push(Entry::new(x.clone(), x_tp.clone()));
                t_subst.type_check(global, &mut local)?
            }
            TermVariant::Match(t, name, ret, arms) => {
                let (params, body) = match global.get_entry(&ret.constructor) {
                    Some(GEntryRef::Inductive(i, params, bodies)) => (params, &bodies[i]),
                    Some(_) => {
                        return Err((
                            ret.meta.clone(),
                            TypeError::new(
                                local,
                                TypeErrorVariant::NotAnInductiveType(ret.constructor.clone()),
                            ),
                        ))
                    }
                    None => {
                        return Err((
                            ret.meta.clone(),
                            TypeError::new(
                                local,
                                TypeErrorVariant::UndefinedConst(ret.constructor.clone()),
                            ),
                        ))
                    }
                };
                // Insure the parameter count on the return pattern is correct
                if ret.params.len() != params.len() + body.arity.len() {
                    return Err((
                        ret.meta.clone(),
                        TypeError::new(
                            local,
                            TypeErrorVariant::IncorrectParameterCount(
                                params.len() + body.arity.len(),
                                ret.params.len(),
                            ),
                        ),
                    ));
                }
                let t_type = t.type_check(global, local)?;
                let mut norm = t_type.clone();
                norm.normalize(global, local);
                let (hd, mut args) = norm.strip_args();
                // Ensure the type of `t` is of the same inductive type that we want to match on
                if !hd.is_const(&ret.constructor) {
                    return Err((
                        t.meta.clone(),
                        TypeError::new(
                            local,
                            TypeErrorVariant::NotOfExpectedInducitve(
                                ret.constructor.clone(),
                                t_type,
                            ),
                        ),
                    ));
                };
                let arity_args = args.drain(params.len()..).collect::<Vec<_>>();

                {
                    // To typecheck the return type, we first need to put the type parameters into scope as well as a opaque version of the match argument.
                    let mut local = local.slot();
                    local.extend(ret.params.iter().zip(params).zip(&args).enumerate().map(
                        |(i, ((x, param), value))| {
                            Entry::with_value(x.clone(), value.move_in(i), param.ttype.clone())
                        },
                    ));
                    local.extend(
                        ret.params[params.len()..]
                            .iter()
                            .zip(&body.arity)
                            .map(|(x, param)| Entry::new(x.clone(), param.ttype.clone())),
                    );
                    let ttype = (0..ret.params.len())
                        .rev()
                        .map(|n| Term {
                            meta: self.meta.clone(),
                            variant: Box::new(TermVariant::Var(n)),
                        })
                        .fold(
                            Term {
                                meta: self.meta.clone(),
                                variant: Box::new(TermVariant::Const(ret.constructor.clone())),
                            },
                            |f, v| Term {
                                meta: self.meta.clone(),
                                variant: Box::new(TermVariant::App(f, v)),
                            },
                        );
                    local.push_onto(Entry::new(name.clone(), ttype));
                    let ret_sort = ret.body.type_check(global, &mut local)?;
                    let sort = ret_sort
                        .expect_sort(global, &mut local)
                        .map_err(|err| (self.meta.clone(), err))?;

                    if body.sort == Sort::Prop
                        && sort != Sort::Prop
                        && !body.constructors.is_empty()
                    {
                        return Err((
                            ret.meta.clone(),
                            TypeError::new(
                                &local,
                                TypeErrorVariant::DisallowedEleminationSort(
                                    body.sort.clone(),
                                    sort,
                                ),
                            ),
                        ));
                    }
                };

                let mut constrs = vec![false; body.constructors.len()];
                for arm in arms {
                    let constructor = if let Some((i, constructor)) = body
                        .constructors
                        .iter()
                        .enumerate()
                        .find(|(_, c)| c.name == arm.constructor)
                    {
                        if constrs[i] {
                            return Err((
                                arm.meta.clone(),
                                TypeError::new(
                                    local,
                                    TypeErrorVariant::DupplicateConstructor(
                                        arm.constructor.clone(),
                                    ),
                                ),
                            ));
                        }
                        constrs[i] = true;
                        constructor
                    } else {
                        return Err((
                            arm.meta.clone(),
                            TypeError::new(
                                local,
                                TypeErrorVariant::NotAConstructor(
                                    body.name.clone(),
                                    arm.constructor.clone(),
                                    body.constructors
                                        .iter()
                                        .map(|constructor| constructor.name.clone())
                                        .collect(),
                                ),
                            ),
                        ));
                    };

                    if arm.params.len() != params.len() + constructor.arity.len() {
                        return Err((
                            arm.meta.clone(),
                            TypeError::new(
                                local,
                                TypeErrorVariant::IncorrectParameterCount(
                                    params.len() + constructor.arity.len(),
                                    arm.params.len(),
                                ),
                            ),
                        ));
                    }
                    let mut local = local.slot();
                    local.extend(arm.params.iter().zip(params).zip(&args).enumerate().map(
                        |(i, ((x, param), value))| {
                            Entry::with_value(x.clone(), value.move_in(i), param.ttype.clone())
                        },
                    ));
                    local.extend(
                        arm.params[params.len()..]
                            .iter()
                            .zip(&constructor.arity)
                            .map(|(x, param)| Entry::new(x.clone(), param.ttype.clone())),
                    );
                    let arm_type = arm.body.type_check(global, &mut local)?;
                    // `this` is the constructor applied to the pattern paramters
                    // It looks like `arm.constructor 'n '(n-1) ... '1 '0`
                    let this = (0..arm.params.len())
                        .rev()
                        .map(|n| Term {
                            meta: self.meta.clone(),
                            variant: Box::new(TermVariant::Var(n)),
                        })
                        .fold(
                            Term {
                                meta: self.meta.clone(),
                                variant: Box::new(TermVariant::Const(arm.constructor.clone())),
                            },
                            |f, v| Term {
                                meta: self.meta.clone(),
                                variant: Box::new(TermVariant::App(f, v)),
                            },
                        );
                    // The expected return type is moved into scope, then all the type arguments of the constructor are substituted into it.
                    let exp_type = ret.body.move_in(arm.params.len()).subst_many(
                        arm.params.len(),
                        constructor.args.len() + 1,
                        |i| {
                            if i < constructor.args.len() {
                                &constructor.args[i]
                            } else {
                                &this
                            }
                        },
                    );
                    arm_type
                        .expect_subtype(&exp_type, global, &mut local)
                        .map_err(|err| (arm.body.meta.clone(), err))?;
                }

                if !constrs.iter().all(|b| *b) {
                    return Err((
                        self.meta.clone(),
                        TypeError::new(
                            local,
                            TypeErrorVariant::MissingConstructors(
                                body.constructors
                                    .iter()
                                    .zip(constrs)
                                    .filter(|(_, b)| !b)
                                    .map(|(constructor, _)| constructor.name.clone())
                                    .collect(),
                            ),
                        ),
                    ));
                }

                ret.body
                    .subst_many(0, args.len() + arity_args.len() + 1, |i| {
                        if i < args.len() {
                            &args[i]
                        } else if i < args.len() + arity_args.len() {
                            &arity_args[i - args.len()]
                        } else {
                            t
                        }
                    })
            }
        })
    }
}
