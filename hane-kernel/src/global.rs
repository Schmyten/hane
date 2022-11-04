use std::collections::HashSet;
use std::fmt::{self, Display, Formatter};

use crate::entry::{Binder, EntryRef};
use crate::{CommandError, Sort, Stack, Term, TermVariant, TypeError, TypeErrorVariant};

#[derive(Default)]
pub struct Global<M, B> {
    env: Vec<(M, GEntry<M, B>)>,
}

/// A reference to a name in the global environment.
pub(crate) enum GEntryRef<'a, M, B> {
    Definition(&'a str, &'a Term<M, B>, &'a Term<M, B>),
    Axiom(&'a str, &'a Term<M, B>),
    Inductive(usize, &'a [Binder<M, B>], &'a [GIndBody<M, B>]),
    InductiveConstructor(usize, usize, &'a [Binder<M, B>], &'a [GIndBody<M, B>]),
}

enum GEntry<M, B> {
    Definition(String, Term<M, B>, Term<M, B>),
    Axiom(String, Term<M, B>),
    Inductive(Vec<Binder<M, B>>, Vec<GIndBody<M, B>>),
}

/// A single inductive type in a mutually defined set in the global environment.
pub(crate) struct GIndBody<M, B> {
    pub(crate) name: String,
    pub(crate) arity: Vec<Binder<M, B>>,
    pub(crate) sort: Sort,
    /// Shorthand for `∀ arity.., sort`
    pub(crate) arity_type: Term<M, B>,
    /// Shorthand for `∀ param.. arity.., sort`
    pub(crate) full_type: Term<M, B>,
    pub(crate) constructors: Vec<GIndConstructor<M, B>>,
}

/// A Constructor of an inductive type.
pub(crate) struct GIndConstructor<M, B> {
    pub(crate) name: String,
    pub(crate) arity: Vec<Binder<M, B>>,
    pub(crate) args: Vec<Term<M, B>>,
    /// Shorthand for `∀ arity.., ttype`
    pub(crate) arity_type: Term<M, B>,
    /// Shorthand for `∀ param.. arity.., ttype`
    pub(crate) full_type: Term<M, B>,
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
                    for param in params {
                        write!(f, " ({})", param.ttype)?;
                    }
                    write!(f, " : {} :=", body.arity_type)?;
                    for constructor in &body.constructors {
                        write!(
                            f,
                            "\n    | {} : {}",
                            constructor.name, constructor.arity_type
                        )?;
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

    /// Returns the type and value of the constant `name`.
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

    /// Returns a reference to the entry containing the constant `name` along with where inside the entry `name` was found.
    pub(crate) fn get_entry(&self, name: &str) -> Option<GEntryRef<M, B>> {
        self.env.iter().find_map(|(_, entry)| match entry {
            GEntry::Definition(x, ttype, val) => {
                (x == name).then_some(GEntryRef::Definition(x, ttype, val))
            }
            GEntry::Axiom(x, ttype) => (x == name).then_some(GEntryRef::Axiom(x, ttype)),
            GEntry::Inductive(params, bodies) => bodies.iter().enumerate().find_map(|(i, body)| {
                if body.name == name {
                    Some(GEntryRef::Inductive(i, params, bodies))
                } else {
                    body.constructors
                        .iter()
                        .enumerate()
                        .find_map(|(j, constructor)| {
                            (constructor.name == name)
                                .then_some(GEntryRef::InductiveConstructor(i, j, params, bodies))
                        })
                }
            }),
        })
    }
}

pub struct Command<M, B> {
    pub meta: M,
    pub variant: CommandVariant<M, B>,
}

pub enum CommandVariant<M, B> {
    /// Defines a new constant in the global environment.
    Definition(String, Term<M, B>, Term<M, B>),
    /// Creates a constant with the given type. This could make the logic inconsistent.
    Axiom(String, Term<M, B>),
    /// Defines a set of mutually inductive types.
    Inductive(Vec<Binder<M, B>>, Vec<IndBody<M, B>>),
}

/// A single type in a mutually defined inductive type set
pub struct IndBody<M, B> {
    pub name: String,
    pub ttype: Term<M, B>,
    pub constructors: Vec<IndConstructor<M, B>>,
}

/// A single constructor of an inductive type
pub struct IndConstructor<M, B> {
    pub name: String,
    pub ttype: Term<M, B>,
}

impl<M, B> Display for Command<M, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.variant {
            CommandVariant::Definition(name, ttype, value) => {
                write!(f, "Definition {name} : {ttype} := {value}.")
            }
            CommandVariant::Axiom(name, ttype) => write!(f, "Axiom {name} : {ttype}."),
            CommandVariant::Inductive(params, bodies) => {
                let mut sep = "Inductive";
                for body in bodies {
                    write!(f, "{sep} {}", body.name)?;
                    sep = "\n    with";
                    for param in params {
                        write!(f, " ({})", param.ttype)?;
                    }
                    write!(f, " : {} :=", body.ttype)?;
                    for constructor in &body.constructors {
                        write!(f, "\n    | {} : {}", constructor.name, constructor.ttype)?;
                    }
                }
                write!(f, ".")
            }
        }
    }
}

impl<M: Clone, B: Clone> Command<M, B> {
    /// Evaluates the command, mutating the global environment acordingly.
    pub fn eval(self, global: &mut Global<M, B>) -> Result<(), (M, CommandError<M, B>)> {
        match self.variant {
            CommandVariant::Definition(name, ttype, value) => {
                global
                    .expect_fresh(&name)
                    .map_err(|err| (self.meta.clone(), err))?;
                let mut local = Stack::new();
                let sort = ttype
                    .type_check(global, &mut local)
                    .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
                sort.expect_sort(global, &mut local)
                    .map_err(|err| (ttype.meta.clone(), CommandError::TypeError(err)))?;
                let value_type = value
                    .type_check(global, &mut local)
                    .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
                value_type
                    .expect_subtype(&ttype, global, &mut local)
                    .map_err(|err| (value.meta.clone(), CommandError::TypeError(err)))?;
                global
                    .env
                    .push((self.meta, GEntry::Definition(name, ttype, value)));
            }
            CommandVariant::Axiom(name, ttype) => {
                global
                    .expect_fresh(&name)
                    .map_err(|err| (self.meta.clone(), err))?;
                let mut local = Stack::new();
                let sort = ttype
                    .type_check(global, &mut local)
                    .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
                sort.expect_sort(global, &mut local)
                    .map_err(|err| (ttype.meta.clone(), CommandError::TypeError(err)))?;
                global.env.push((self.meta, GEntry::Axiom(name, ttype)));
            }
            CommandVariant::Inductive(params, bodies) => {
                // Ensure all names are fresh
                let mut names = HashSet::new();
                for body in &bodies {
                    global
                        .expect_fresh(&body.name)
                        .map_err(|err| (self.meta.clone(), err))?;
                    if !names.insert(&*body.name) {
                        return Err((
                            self.meta.clone(),
                            CommandError::NameAlreadyExists(body.name.clone()),
                        ));
                    }

                    for constructor in &body.constructors {
                        global
                            .expect_fresh(&constructor.name)
                            .map_err(|err| (self.meta.clone(), err))?;
                        if !names.insert(&*constructor.name) {
                            return Err((
                                self.meta.clone(),
                                CommandError::NameAlreadyExists(constructor.name.clone()),
                            ));
                        }
                    }
                }

                let mut ind_bodies = Vec::with_capacity(bodies.len());
                let mut constructors = Vec::with_capacity(bodies.len());
                let mut local = Stack::new();
                let mut local = local.slot();

                // We start of by adding the parameters to the local environment
                for param in &params {
                    let ttype = param
                        .ttype
                        .type_check(global, &mut local)
                        .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
                    ttype
                        .expect_sort(global, &mut local)
                        .map_err(|err| (param.ttype.meta.clone(), CommandError::TypeError(err)))?;
                    local.push_onto(param.clone().into());
                }

                // Next we typecheck the new types' sorts, ignoring all constructors
                for body in bodies {
                    constructors.push(body.constructors);

                    body.ttype
                        .type_check(global, &mut local)
                        .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;

                    let mut norm = body.ttype.clone();
                    norm.normalize(global, &mut local);
                    let (arity, norm) = norm.strip_products();

                    let sort = if let TermVariant::Sort(sort) = *norm.variant {
                        sort
                    } else {
                        return Err((
                            body.ttype.meta,
                            CommandError::TypeError(TypeError::new(
                                &local,
                                TypeErrorVariant::NotASort(norm),
                            )),
                        ));
                    };

                    let full_type =
                        params
                            .iter()
                            .cloned()
                            .rev()
                            .fold(body.ttype.clone(), |body, param| Term {
                                meta: body.meta.clone(),
                                variant: Box::new(TermVariant::Product(param.x, param.ttype, body)),
                            });

                    ind_bodies.push(GIndBody {
                        name: body.name,
                        arity,
                        sort,
                        arity_type: body.ttype,
                        full_type,
                        constructors: Vec::new(),
                    })
                }

                // Then we temporarily put all the new types into the global environment, as they should be in scope for the types of the constructors
                global.env.extend(ind_bodies.iter().map(|body| {
                    (
                        self.meta.clone(),
                        GEntry::Axiom(body.name.clone(), body.full_type.clone()),
                    )
                }));

                // Finally we typecheck the constructors and add them to the bodies of each type
                for (body, constructors) in ind_bodies.iter_mut().zip(constructors) {
                    body.constructors = constructors
                        .into_iter()
                        .map(|constructor| {
                            let sort = constructor
                                .ttype
                                .type_check(global, &mut local)
                                .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
                            let _ = sort.expect_sort(global, &mut local).map_err(|err| {
                                (constructor.ttype.meta.clone(), CommandError::TypeError(err))
                            })?;

                            // Ensure the constructor produces the correct type
                            let mut norm = constructor.ttype.clone();
                            norm.normalize(global, &mut local);
                            let (arity, ttype) = norm.strip_products();
                            let (hd, args) = ttype.strip_args();
                            if !hd.is_const(&body.name) {
                                return Err((
                                    constructor.ttype.meta.clone(),
                                    CommandError::TypeError(TypeError::new(
                                        &local,
                                        TypeErrorVariant::NotOfExpectedInducitve(
                                            body.name.clone(),
                                            constructor.ttype,
                                        ),
                                    )),
                                ));
                            }

                            let full_type = params.iter().cloned().rev().fold(
                                constructor.ttype.clone(),
                                |body, binder| Term {
                                    meta: body.meta.clone(),
                                    variant: Box::new(TermVariant::Product(
                                        binder.x,
                                        binder.ttype,
                                        body,
                                    )),
                                },
                            );

                            Ok(GIndConstructor {
                                name: constructor.name,
                                arity,
                                args,
                                arity_type: constructor.ttype,
                                full_type,
                            })
                        })
                        .collect::<Result<_, (M, CommandError<M, B>)>>()?;
                }

                for body in &ind_bodies {
                    for constructor in &body.constructors {
                        for arg in &constructor.args {
                            arg.validate_consts(|name| {
                                ind_bodies
                                    .iter()
                                    .any(|body| body.name != name)
                                    .then_some(())
                                    .ok_or(CommandError::ConstructorArgsContainsType)
                            })?;
                        }
                    }
                }

                // With the constructors typechecked, we can now remove the new types, so that they can be properly instantiated as inductive types
                global.env.truncate(global.env.len() - ind_bodies.len());

                //TODO: [Positivity Condition](https://coq.inria.fr/distrib/current/refman/language/core/inductive.html#positivity-condition)

                global
                    .env
                    .push((self.meta, GEntry::Inductive(params, ind_bodies)));
            }
        }
        Ok(())
    }
}
