use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use crate::{Binder, Command, CommandVariant, Expr, ExprVariant, Ident, SpanError};
use hane_kernel::{stack::StackSlot, Sort, Stack};

pub mod lowered {
    use crate::{Ident, Span};

    pub type Binder = hane_kernel::entry::Binder<Span, Ident>;
    pub type Command = hane_kernel::Command<Span, Ident>;
    pub type CommandVariant = hane_kernel::CommandVariant<Span, Ident>;
    pub type IndBody = hane_kernel::IndBody<Span, Ident>;
    pub type IndConstructor = hane_kernel::IndConstructor<Span, Ident>;
    pub type Term = hane_kernel::Term<Span, Ident>;
    pub type TermVariant = hane_kernel::TermVariant<Span, Ident>;
    pub type MatchArm = hane_kernel::term::MatchArm<Span, Ident>;
    pub type FixDecl = hane_kernel::term::FixDecl<Span, Ident>;
}

pub enum LoweringError {
    NameNotFree(String),
    UnknownVariable(String),
    ParamsMustMatch,
    NotAConstructorOf(String, String, Vec<String>),
    DupplicateConstructor(String),
    MissingConstructors(Vec<String>),
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::NameNotFree(x) => write!(f, "A constant named `{x}` already exists"),
            LoweringError::UnknownVariable(x) => write!(f, "Unknown variable `{x}`"),
            LoweringError::ParamsMustMatch => write!(
                f,
                "Parameters must be syntactically the same on all mutually defined types"
            ),
            LoweringError::NotAConstructorOf(ind, name, constructors) => {
                write!(f, "{name} is not a constructor for {ind}.")?;
                write!(f, " Constructors are: ")?;
                let mut sep = "";
                for constructor in constructors {
                    write!(f, "{sep}{constructor}")?;
                    sep = ", ";
                }
                Ok(())
            }
            LoweringError::DupplicateConstructor(name) => {
                write!(f, "The constructor {name} was previously covered")
            }
            LoweringError::MissingConstructors(constructors) => {
                write!(f, "Missing the constructors: ")?;
                let mut sep = "";
                for constructor in constructors {
                    write!(f, "{sep}{constructor}")?;
                    sep = ", ";
                }
                Ok(())
            }
        }
    }
}

pub enum LoweringEntry {
    Definition,
    Inductive(Vec<String>),
}

impl Command {
    pub fn lower(
        self,
        global: &mut HashMap<String, LoweringEntry>,
    ) -> Result<lowered::Command, SpanError<LoweringError>> {
        let mut names = Stack::new();
        let variant = match self.variant {
            CommandVariant::Definition(ident, ttype, value) => {
                if global.contains_key(&ident.name) {
                    return Err(SpanError {
                        span: ident.span,
                        err: LoweringError::NameNotFree(ident.name),
                    });
                }
                let ttype = ttype.lower(global, &mut names)?;
                let value = value.lower(global, &mut names)?;
                global.insert(ident.name.clone(), LoweringEntry::Definition);
                lowered::CommandVariant::Definition(ident.name, ttype, value)
            }
            CommandVariant::Axiom(ident, ttype) => {
                if global.contains_key(&ident.name) {
                    return Err(SpanError {
                        span: ident.span,
                        err: LoweringError::NameNotFree(ident.name),
                    });
                }
                let ttype = ttype.lower(global, &mut names)?;
                global.insert(ident.name.clone(), LoweringEntry::Definition);
                lowered::CommandVariant::Axiom(ident.name, ttype)
            }
            CommandVariant::Inductive(mut bodies) => {
                // The parameters must be syntactically the same on all the bodies.
                // To check this we steal the `params` from the first body and check that it matches all the others.
                // We can steal it because we won't use `IndBody.params` later, instead we'll use the stolen `param`
                let params = std::mem::take(&mut bodies[0].params);
                bodies
                    .iter()
                    .skip(1)
                    .map(|body| &body.params)
                    .try_for_each(|ps| {
                        if &params == ps {
                            Ok(())
                        } else {
                            Err(SpanError {
                                span: self.span.clone(),
                                err: LoweringError::ParamsMustMatch,
                            })
                        }
                    })?;

                // Next we lower all the parameters and push then into our local name scope
                let mut names = names.slot();
                let params = params
                    .into_iter()
                    .map(|binder| binder.lower(global, &mut names))
                    .collect::<Result<_, _>>()?;

                // Then we steal and lower all the arity sorts of the types
                let body_types = bodies
                    .iter_mut()
                    .map(|body| {
                        let span = body.ttype.span.clone();
                        std::mem::replace(
                            &mut body.ttype,
                            Expr {
                                span,
                                variant: Box::new(ExprVariant::Sort(crate::Sort::Prop)),
                            },
                        )
                    })
                    .map(|ttype| ttype.lower(global, &mut names))
                    .collect::<Result<Vec<_>, _>>()?;

                // With the types sorts lowered we can put the type names into the global name set as they are needed to handle the constructors
                for body in &bodies {
                    let constructors = body
                        .constructors
                        .iter()
                        .map(|constructor| constructor.name.name.clone())
                        .collect();
                    if global
                        .insert(
                            body.name.name.clone(),
                            LoweringEntry::Inductive(constructors),
                        )
                        .is_some()
                    {
                        return Err(SpanError {
                            span: body.name.span.clone(),
                            err: LoweringError::NameNotFree(body.name.name.clone()),
                        });
                    }
                }

                let mut cspans = Vec::new();

                // Then we lower the constructors and build the lowered bodies
                let lowered_bodies = bodies
                    .into_iter()
                    .zip(body_types)
                    .map(|(body, ttype)| {
                        let constructors = body
                            .constructors
                            .into_iter()
                            .map(|constructor| {
                                cspans.push(constructor.name.span);
                                Ok(lowered::IndConstructor {
                                    name: constructor.name.name,
                                    ttype: constructor.ttype.lower(global, &mut names)?,
                                })
                            })
                            .collect::<Result<_, SpanError<LoweringError>>>()?;
                        Ok(lowered::IndBody {
                            name: body.name.name,
                            ttype,
                            constructors,
                        })
                    })
                    .collect::<Result<Vec<_>, SpanError<LoweringError>>>()?;

                // Finally we put the constructors into the global name set
                lowered_bodies
                    .iter()
                    .flat_map(|body| &body.constructors)
                    .zip(cspans)
                    .try_for_each(|(constructor, span)| {
                        global
                            .insert(constructor.name.clone(), LoweringEntry::Definition)
                            .is_none()
                            .then_some(())
                            .ok_or_else(|| SpanError {
                                span,
                                err: LoweringError::NameNotFree(constructor.name.clone()),
                            })
                    })?;
                lowered::CommandVariant::Inductive(params, lowered_bodies)
            }
        };
        Ok(lowered::Command {
            meta: self.span,
            variant,
        })
    }
}

impl Binder {
    pub fn lower(
        self,
        global: &HashMap<String, LoweringEntry>,
        names: &mut StackSlot<Ident>,
    ) -> Result<lowered::Binder, SpanError<LoweringError>> {
        let ttype = self.ttype.lower(global, names)?;
        names.push_onto(self.ident.clone());
        Ok(lowered::Binder {
            x: self.ident,
            ttype,
        })
    }
}

impl Expr {
    pub fn lower(
        self,
        global: &HashMap<String, LoweringEntry>,
        names: &mut Stack<Ident>,
    ) -> Result<lowered::Term, SpanError<LoweringError>> {
        let variant = match *self.variant {
            ExprVariant::Sort(sort) => lowered::TermVariant::Sort(sort.lower()),
            ExprVariant::Var(x) => {
                if let Some((i, _)) = names.iter().enumerate().find(|(_, y)| x == y.name) {
                    lowered::TermVariant::Var(i)
                } else if global.contains_key(&x) {
                    lowered::TermVariant::Const(x)
                } else {
                    return Err(SpanError {
                        span: self.span.clone(),
                        err: LoweringError::UnknownVariable(x.to_owned()),
                    });
                }
            }
            ExprVariant::App(f, v) => {
                lowered::TermVariant::App(f.lower(global, names)?, v.lower(global, names)?)
            }
            ExprVariant::Product(binders, t) => {
                let mut type_stack = Vec::new();
                let mut names = names.slot();
                for Binder { ident, ttype } in binders {
                    let lowered_type = ttype.lower(global, &mut names)?;
                    type_stack.push(lowered_type);
                    names.push_onto(ident);
                }
                let t = t.lower(global, &mut names)?;
                let mut iter = names.pop().zip(type_stack.into_iter().rev());

                let make_term = |inner, (name, ttype)| lowered::Term {
                    meta: self.span.clone(),
                    variant: Box::new(lowered::TermVariant::Product(name, ttype, inner)),
                };
                let inner = iter.next().unwrap();
                return Ok(iter.fold(make_term(t, inner), make_term));
            }
            ExprVariant::Abstract(binders, t) => {
                let mut type_stack = Vec::new();
                let mut names = names.slot();
                for Binder { ident, ttype } in binders {
                    let lowered_type = ttype.lower(global, &mut names)?;
                    type_stack.push(lowered_type);
                    names.push_onto(ident);
                }
                let t = t.lower(global, &mut names)?;
                let mut iter = names.pop().zip(type_stack.into_iter().rev());

                let make_term = |inner, (name, ttype)| lowered::Term {
                    meta: self.span.clone(),
                    variant: Box::new(lowered::TermVariant::Abstract(name, ttype, inner)),
                };
                let inner = iter.next().unwrap();
                return Ok(iter.fold(make_term(t, inner), make_term));
            }
            ExprVariant::Bind(x, x_tp, x_val, t) => {
                let x_tp = x_tp.lower(global, names)?;
                let x_val = x_val.lower(global, names)?;
                let mut names = names.push(x);
                let t = t.lower(global, &mut names);
                let x = names.pop().next().unwrap();
                let t = t?;
                lowered::TermVariant::Bind(x, x_tp, x_val, t)
            }
            ExprVariant::Match(t, mut name, pat, ret, arms) => {
                let t = t.lower(global, names)?;
                let constructors = match global.get(&pat.constructor.name) {
                    Some(LoweringEntry::Inductive(constructors)) => constructors,
                    Some(_) => panic!(),
                    None => {
                        return Err(SpanError {
                            span: pat.constructor.span.clone(),
                            err: LoweringError::UnknownVariable(pat.constructor.name),
                        })
                    }
                };
                let ind = pat.constructor.name;
                let ret = {
                    let mut names = names.slot();
                    names.extend(pat.params);
                    let body = {
                        let mut names = names.push(name);
                        let ret = ret.lower(global, &mut names)?;
                        name = names.pop().next().unwrap();
                        ret
                    };
                    let params = names.pop().rev().collect();
                    lowered::MatchArm {
                        meta: pat.constructor.span,
                        params,
                        body,
                    }
                };
                let mut lowered_arms = vec![None; constructors.len()];
                for (pat, body) in arms {
                    if !global.contains_key(&pat.constructor.name) {
                        return Err(SpanError {
                            span: pat.constructor.span.clone(),
                            err: LoweringError::UnknownVariable(pat.constructor.name),
                        });
                    }

                    let lowered_arm = if let Some((lowered_arm, _)) = lowered_arms
                        .iter_mut()
                        .zip(constructors)
                        .find(|(_, name)| **name == pat.constructor.name)
                    {
                        if lowered_arm.is_some() {
                            return Err(SpanError {
                                span: pat.constructor.span,
                                err: LoweringError::DupplicateConstructor(
                                    pat.constructor.name.clone(),
                                ),
                            });
                        }
                        lowered_arm
                    } else {
                        return Err(SpanError {
                            span: pat.constructor.span.clone(),
                            err: LoweringError::NotAConstructorOf(
                                ind,
                                pat.constructor.name,
                                constructors.clone(),
                            ),
                        });
                    };

                    let mut names = names.slot();
                    names.extend(pat.params);
                    let body = body.lower(global, &mut names)?;
                    let params = names.pop().rev().collect();
                    let arm = lowered::MatchArm {
                        meta: pat.constructor.span,
                        params,
                        body,
                    };

                    *lowered_arm = Some(arm)
                }

                let mut missed = Vec::new();
                let arms = lowered_arms
                    .into_iter()
                    .zip(constructors)
                    .filter_map(|(arm, constructor)| {
                        if arm.is_none() {
                            missed.push(constructor.clone());
                        }
                        arm
                    })
                    .collect();

                if !missed.is_empty() {
                    return Err(SpanError {
                        span: self.span,
                        err: LoweringError::MissingConstructors(missed),
                    });
                }

                lowered::TermVariant::Match(t, name, ind, ret, arms)
            }
            ExprVariant::Fix(decls, sel) => {
                let sel = if let Some((sel, _)) = decls
                    .iter()
                    .enumerate()
                    .rev()
                    .find(|(_, decl)| decl.name == sel)
                {
                    sel
                } else {
                    panic!()
                };
                let mut names = names.slot();
                let mut fnames = decls.iter().map(|decl| decl.name.to_owned()).collect();
                let decls = decls
                    .into_iter()
                    .map(|decl| decl.lower(global, &mut names, &mut fnames))
                    .collect::<Result<_, _>>()?;
                lowered::TermVariant::Fix(decls, sel)
            }
        };
        Ok(lowered::Term {
            meta: self.span,
            variant: Box::new(variant),
        })
    }
}

impl crate::Sort {
    pub fn lower(self) -> Sort {
        match self {
            crate::Sort::Prop => Sort::Prop,
            crate::Sort::Set => Sort::Set,
            crate::Sort::Type(n) => Sort::Type(n),
        }
    }
}

impl crate::FixDecl {
    pub fn lower(
        self,
        global: &HashMap<String, LoweringEntry>,
        names: &mut Stack<Ident>,
        fnames: &mut Vec<Ident>,
    ) -> Result<lowered::FixDecl, SpanError<LoweringError>> {
        let name = self.name;
        let anot = if let Some((anot, _)) = self
            .params
            .iter()
            .enumerate()
            .rev()
            .find(|(_, binder)| binder.ident == self.anot)
        {
            anot
        } else {
            panic!()
        };
        let mut names = names.slot();
        let params = self
            .params
            .into_iter()
            .map(|binder| binder.lower(global, &mut names))
            .collect::<Result<_, _>>()?;
        let ttype = self.ttype.lower(global, &mut names)?;
        let mut names = names.slot();
        names.extend(fnames.drain(..));
        let body = self.body.lower(global, &mut names)?;
        fnames.extend(names.pop().rev());
        Ok(lowered::FixDecl { name, anot, params, ttype, body })
    }
}
