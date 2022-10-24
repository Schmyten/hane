use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};

use crate::{Binder, Command, CommandVariant, Expr, ExprVariant, SpanError, Ident};
use hane_kernel::{Sort, Stack};

pub mod lowered {
    use crate::{Ident, Span};

    pub type Binder = hane_kernel::entry::Binder<Span, Ident>;
    pub type Command = hane_kernel::Command<Span, Ident>;
    pub type CommandVariant = hane_kernel::CommandVariant<Span, Ident>;
    pub type IndBody = hane_kernel::IndBody<Span, Ident>;
    pub type IndConstructor = hane_kernel::IndConstructor<Span, Ident>;
    pub type Term = hane_kernel::Term<Span, Ident>;
    pub type TermVariant = hane_kernel::TermVariant<Span, Ident>;
}

pub enum LoweringError {
    NameNotFree(String),
    UnknownVariable(String),
    ParamsMustMatch,
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
        }
    }
}

impl Command {
    pub fn lower(
        self,
        global: &mut HashSet<String>,
    ) -> Result<lowered::Command, SpanError<LoweringError>> {
        let mut names = Stack::new();
        let variant = match self.variant {
            CommandVariant::Definition(ident, ttype, value) => {
                if global.contains(&ident.name) {
                    return Err(SpanError {
                        span: ident.span,
                        err: LoweringError::NameNotFree(ident.name),
                    });
                }
                let ttype = ttype.lower(global, &mut names)?;
                let value = value.lower(global, &mut names)?;
                global.insert(ident.name.clone());
                lowered::CommandVariant::Definition(ident.name, ttype, value)
            }
            CommandVariant::Axiom(ident, ttype) => {
                if global.contains(&ident.name) {
                    return Err(SpanError {
                        span: self.span,
                        err: LoweringError::NameNotFree(ident.name),
                    });
                }
                let ttype = ttype.lower(global, &mut names)?;
                global.insert(ident.name.clone());
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
                let mut lowered_params = Vec::with_capacity(params.len());
                let mut names = names.slot();
                for param in params {
                    let name = param.ident.clone();
                    lowered_params.push(param.lower(global, &mut names)?);
                    names.push_onto(name);
                }

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
                    if !global.insert(body.name.name.clone()) {
                        return Err(SpanError {
                            span: body.name.span.clone(),
                            err: LoweringError::NameNotFree(body.name.name.clone()),
                        });
                    }
                }

                // Then we lower the constructors and build the lowered bodies
                let lowered_bodies = bodies
                    .into_iter()
                    .zip(body_types)
                    .map(|(body, ttype)| {
                        let constructors = body
                            .constructors
                            .into_iter()
                            .map(|constructor| {
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
                for body in &lowered_bodies {
                    for constructor in &body.constructors {
                        if !global.insert(constructor.name.clone()) {
                            return Err(SpanError {
                                span: self.span.clone(),
                                err: LoweringError::NameNotFree(constructor.name.clone()),
                            });
                        }
                    }
                }
                lowered::CommandVariant::Inductive(lowered_params, lowered_bodies)
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
        global: &HashSet<String>,
        names: &mut Stack<Ident>,
    ) -> Result<lowered::Binder, SpanError<LoweringError>> {
        let ttype = self.ttype.lower(global, names)?;
        Ok(lowered::Binder {
            x: self.ident,
            ttype,
        })
    }
}

impl Expr {
    pub fn lower(
        self,
        global: &HashSet<String>,
        names: &mut Stack<Ident>,
    ) -> Result<lowered::Term, SpanError<LoweringError>> {
        let variant = match *self.variant {
            ExprVariant::Sort(sort) => lowered::TermVariant::Sort(sort.lower()),
            ExprVariant::Var(x) => {
                if let Some((i, _)) = names.iter().enumerate().find(|(_, y)| x == y.name) {
                    lowered::TermVariant::Var(i)
                } else if global.contains(&x) {
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
