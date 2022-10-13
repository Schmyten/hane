use std::collections::HashSet;
use std::fmt::{self, Display, Formatter};

use crate::entry::{Entry, EntryRef};
use crate::{CommandError, Sort, Stack, Term, TermVariant, TypeError, TypeErrorVariant};

#[derive(Default)]
pub struct Global<M, B> {
    env: Vec<(M, GEntry<M, B>)>,
}

enum GEntry<M, B> {
    Definition(String, Term<M, B>, Term<M, B>),
    Axiom(String, Term<M, B>),
    Inductive(Vec<(B, Term<M, B>)>, Vec<IndBody<M, B>>),
}

struct IndBody<M, B> {
    name: String,
    arity: Vec<(B, Term<M, B>)>,
    sort: Sort,
    /// Shorthand for `∀ arity.., sort`
    arity_type: Term<M, B>,
    /// Shorthand for `∀ param.. arity.., sort`
    full_type: Term<M, B>,
    constructors: Vec<IndConstructor<M, B>>,
}

struct IndConstructor<M, B> {
    name: String,
    arity: Vec<(B, Term<M, B>)>,
    ttype: Term<M, B>,
    /// Shorthand for `∀ arity.., ttype`
    arity_type: Term<M, B>,
    /// Shorthand for `∀ param.. arity.., ttype`
    full_type: Term<M, B>,
}

impl<M, B> Display for Global<M, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.env
            .iter()
            .try_for_each(|(_, entry)| writeln!(f, "{entry}"))
    }
}

impl<M, B> Display for GEntry<M, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GEntry::Definition(name, ttype, value) => {
                write!(f, "Definition {name} : {ttype} := {value}.")
            }
            GEntry::Axiom(name, ttype) => write!(f, "Axiom {name} : {ttype}."),
            GEntry::Inductive(params, bodies) => {
                let mut pre = "Inductive";
                for body in bodies {
                    write!(f, "{pre} {}", body.name)?;
                    pre = "\n    with";
                    for (_, param) in params {
                        write!(f, " ({param})")?;
                    }
                    write!(f, " : {} :=", body.arity_type)?;
                    for constructor in &body.constructors {
                        write!(f, "\n    | {} : {}", constructor.name, constructor.arity_type)?;
                    }
                }
                write!(f, ".")
            }
        }
    }
}

impl<M: Clone, B: Clone> Global<M, B> {
    pub fn new() -> Self {
        Global { env: Vec::new() }
    }

    /// Checks whether `name` is alrady used, returning an error if is.
    pub fn expect_fresh(&self, name: &str) -> Result<(), CommandError<M, B>> {
        let free = self.env.iter().all(|(_, entry)| match entry {
            GEntry::Definition(x, _, _) => x != name,
            GEntry::Axiom(x, _) => x != name,
            GEntry::Inductive(_, bodies) => bodies
                .iter()
                .all(|body| body.name != name && body.constructors.iter().all(|c| c.name != name)),
        });
        if free {
            Ok(())
        } else {
            Err(CommandError::NameAlreadyExists(name.to_owned()))
        }
    }

    pub fn get(&self, name: &str) -> Option<EntryRef<M, B>> {
        self.env.iter().find_map(|(_, entry)| match entry {
            GEntry::Definition(x, ttype, value) => {
                (x == name).then_some(EntryRef::with_value(value, ttype))
            }
            GEntry::Axiom(x, ttype) => (x == name).then_some(EntryRef::new(ttype)),
            GEntry::Inductive(_, bodies) => bodies
                .iter()
                .find_map(|body| {
                    if body.name == name {
                        Some(&body.full_type)
                    } else {
                        body.constructors.iter().find_map(|constructor| {
                            (constructor.name == name).then_some(&constructor.full_type)
                        })
                    }
                })
                .map(|ttype| EntryRef::new(ttype)),
        })
    }

    pub fn definition(
        &mut self,
        meta: M,
        name: String,
        ttype: Term<M, B>,
        value: Term<M, B>,
    ) -> Result<(), (M, CommandError<M, B>)> {
        self.expect_fresh(&name).map_err(|err| (meta.clone(), err))?;
        let mut lenv = Stack::new();
        let sort = ttype
            .type_check(self, &mut lenv)
            .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
        sort.expect_sort(self, &mut lenv)
            .map_err(|err| (ttype.meta.clone(), CommandError::TypeError(err)))?;
        let value_type = value
            .type_check(self, &mut lenv)
            .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
        value_type
            .expect_subtype(&ttype, self, &mut lenv)
            .map_err(|err| (value.meta.clone(), CommandError::TypeError(err)))?;
        self.env
            .push((meta, GEntry::Definition(name, ttype, value)));
        Ok(())
    }

    pub fn axiom(
        &mut self,
        meta: M,
        name: String,
        ttype: Term<M, B>,
    ) -> Result<(), (M, CommandError<M, B>)> {
        self.expect_fresh(&name).map_err(|err| (meta.clone(), err))?;
        let mut lenv = Stack::new();
        let sort = ttype
            .type_check(self, &mut lenv)
            .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
        sort.expect_sort(self, &mut lenv)
            .map_err(|err| (ttype.meta.clone(), CommandError::TypeError(err)))?;
        self.env.push((meta, GEntry::Axiom(name, ttype)));
        Ok(())
    }

    pub fn inductive(
        &mut self,
        meta: M,
        params: Vec<(B, Term<M, B>)>,
        bodies: Vec<(String, Term<M, B>, Vec<(String, Term<M, B>)>)>,
    ) -> Result<(), (M, CommandError<M, B>)> {
        let mut names = HashSet::new();
        for (name, _, constructors) in &bodies {
            self.expect_fresh(name).map_err(|err| (meta.clone(), err))?;
            if !names.insert(&**name) {
                return Err((meta.clone(), CommandError::NameAlreadyExists(name.clone())));
            }

            for (name, _) in constructors {
                self.expect_fresh(name).map_err(|err| (meta.clone(), err))?;
                if !names.insert(&**name) {
                    return Err((meta.clone(), CommandError::NameAlreadyExists(name.clone())));
                }
            }
        }

        let mut ind_bodies = Vec::with_capacity(bodies.len());
        let mut constructors = Vec::with_capacity(bodies.len());
        let mut lenv = Stack::new();

        for (x, param) in &params {
            let ttype = param
                .type_check(self, &mut lenv)
                .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
            ttype
                .expect_sort(self, &mut lenv)
                .map_err(|err|(param.meta.clone(), CommandError::TypeError(err)))?;
            lenv.push(Entry::new(x.clone(), param.clone()));
        }

        for (name, arity_type, cs) in bodies {
            constructors.push(cs);

            arity_type
                .type_check(self, &mut lenv)
                .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;

            let mut norm = arity_type.clone();
            norm.normalize(self, &mut lenv);
            let (arity, norm) = norm.strip_products();

            let sort = if let TermVariant::Sort(sort) = *norm.variant {
                sort
            } else {
                return Err((
                    arity_type.meta.clone(),
                    CommandError::TypeError(TypeError::new(
                        &mut lenv,
                        TypeErrorVariant::NotASort(norm),
                    )),
                ));
            };

            let full_type =
                params
                    .iter()
                    .cloned()
                    .rev()
                    .fold(arity_type.clone(), |body, (x, ttype)| Term {
                        meta: body.meta.clone(),
                        variant: Box::new(TermVariant::Product(x, ttype, body)),
                    });

            ind_bodies.push(IndBody {
                name,
                arity,
                sort,
                arity_type,
                full_type,
                constructors: Vec::new(),
            })
        }

        self.env.extend(ind_bodies.iter().map(|body| {
            (
                meta.clone(),
                GEntry::Axiom(body.name.clone(), body.full_type.clone()),
            )
        }));

        for (body, constructors) in ind_bodies.iter_mut().zip(constructors) {
            body.constructors = constructors
                .into_iter()
                .map(|(name, arity_type)| {
                    let sort = arity_type
                        .type_check(self, &mut lenv)
                        .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
                    let _ = sort
                        .expect_sort(self, &mut lenv)
                        .map_err(|err| (arity_type.meta.clone(), CommandError::TypeError(err)))?;

                    let mut norm = arity_type.clone();
                    norm.normalize(self, &mut lenv);
                    let (arity, ttype) = norm.strip_products();
                    let full_type = params.iter().cloned().rev().fold(
                        arity_type.clone(),
                        |body, (x, ttype)| Term {
                            meta: body.meta.clone(),
                            variant: Box::new(TermVariant::Product(x, ttype, body)),
                        },
                    );

                    Ok(IndConstructor {
                        name,
                        arity,
                        ttype,
                        arity_type,
                        full_type,
                    })
                })
                .collect::<Result<_, (M, CommandError<M, B>)>>()?;
        }

        self.env.truncate(self.env.len() - ind_bodies.len());

        //Todo: Positivity of constructors

        self.env.push((meta, GEntry::Inductive(params, ind_bodies)));
        Ok(())
    }
}
