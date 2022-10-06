use std::fmt::{self, Display, Formatter};

use crate::{term::Term, stack::Stack};

#[derive(Default)]
pub struct Global<B> {
    env: Vec<GEntry<B>>,
}

impl<B> Display for Global<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for entry in &self.env {
            match entry {
                GEntry::Definition(name, ttype, value) =>
                    writeln!(f, "Definition {name} : {ttype} := {value}.")?,
                GEntry::Axiom(name, ttype) =>
                    writeln!(f, "Axiom {name} : {ttype}.")?,
            }
        }
        Ok(())
    }
}

enum GEntry<B> {
    Definition(String, Term<B>, Term<B>),
    Axiom(String, Term<B>),
}

pub enum CommandError<B> {
    NameAlreadyExists(String),
    TypeError,
    IncompatibleType(Term<B>, Term<B>),
    ExpectedSort(Term<B>, Term<B>),
}

impl<B: Clone> Global<B> {
    pub fn new() -> Self {
        Global { env: Vec::new() }
    }

    pub fn free(&self, name: &str) -> Result<(), CommandError<B>> {
        let free = self.env.iter().all(|entry|
            match entry {
                GEntry::Definition(x, _, _) => x != name,
                GEntry::Axiom(x, _) => x != name,
            }
        );
        if free { Ok(()) } else { Err(CommandError::NameAlreadyExists(name.to_owned())) }
    }

    pub fn get(&self, name: &str) -> Option<(Option<&Term<B>>, &Term<B>)> {
        self.env.iter().find_map(|entry|
            match entry {
                GEntry::Definition(x, ttype, value) => (x == name).then(||(Some(value), ttype)),
                GEntry::Axiom(x, ttype) => (x == name).then(||(None, ttype)),
            }
        )
    }

    pub fn definition(&mut self, name: String, ttype: Term<B>, value: Term<B>) -> Result<(), CommandError<B>> {
        self.free(&name)?;
        let mut lenv = Stack::new();
        let value_type = value.type_check(self, &mut lenv).ok_or(CommandError::TypeError)?;
        if !value_type.convertable(&ttype, self, &mut lenv) {
            return Err(CommandError::IncompatibleType(ttype, value_type));
        }
        self.env.push(GEntry::Definition(name, ttype, value));
        Ok(())
    }

    pub fn axiom(&mut self, name: String, ttype: Term<B>) -> Result<(), CommandError<B>> {
        self.free(&name)?;
        let mut lenv = Stack::new();
        let sort = ttype.type_check(self, &mut lenv).ok_or(CommandError::TypeError)?;
        if !sort.is_sort(self, &mut lenv) {
            return Err(CommandError::ExpectedSort(ttype, sort));
        }
        self.env.push(GEntry::Axiom(name, ttype));
        Ok(())
    }
}
