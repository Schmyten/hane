use std::fmt::{self, Display, Formatter};

use crate::{entry::EntryRef, CommandError, Stack, Term};

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
    sort: Term<M, B>,
    /// Shorthand for `∀ arity.., sort`
    arity_ttype: Term<M, B>,
    /// Shorthand for `∀ param.. arity.., sort`
    full_ttype: Term<M, B>,
    constructors: Vec<IndConstructor<M, B>>,
}

struct IndConstructor<M, B> {
    name: String,
    arity: Vec<(B, Term<M, B>)>,
    ttype: Term<M, B>,
    /// Shorthand for `∀ arity.., ttype`
    arity_ttype: Term<M, B>,
    /// Shorthand for `∀ param.. arity.., ttype`
    full_ttype: Term<M, B>,
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
                    pre = "    with";
                    for (_, param) in params {
                        write!(f, " ({param})")?;
                    }
                    write!(f, " : {} :=", body.arity_ttype)?;
                    for constructor in &body.constructors {
                        writeln!(f, "    | {} : {}", constructor.name, constructor.arity_ttype)?;
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

    pub fn free(&self, name: &str) -> Result<(), CommandError<M, B>> {
        let free = self.env.iter().all(|(_, entry)| match entry {
            GEntry::Definition(x, _, _) => x != name,
            GEntry::Axiom(x, _) => x != name,
            GEntry::Inductive(_, bodies) => bodies.iter().all(|body|
                body.name != name && body.constructors.iter().all(|c|c.name != name)
            )
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
            GEntry::Inductive(_, bodies) => {
                bodies.iter().find_map(|body| {
                    if body.name == name {
                        Some(&body.full_ttype)
                    } else {
                        body.constructors.iter().find_map(|constructor|(constructor.name == name).then_some(&constructor.full_ttype))
                    }
                }).map(|ttype| EntryRef::new(ttype))
            }
        })
    }

    pub fn definition(
        &mut self,
        meta: M,
        name: String,
        ttype: Term<M, B>,
        value: Term<M, B>,
    ) -> Result<(), (M, CommandError<M, B>)> {
        self.free(&name).map_err(|err| (meta.clone(), err))?;
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
        self.free(&name).map_err(|err| (meta.clone(), err))?;
        let mut lenv = Stack::new();
        let sort = ttype
            .type_check(self, &mut lenv)
            .map_err(|(meta, err)| (meta, CommandError::TypeError(err)))?;
        sort.expect_sort(self, &mut lenv)
            .map_err(|err| (ttype.meta.clone(), CommandError::TypeError(err)))?;
        self.env.push((meta, GEntry::Axiom(name, ttype)));
        Ok(())
    }
}
