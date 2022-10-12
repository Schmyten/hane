use std::collections::HashMap;

use hane_kernel::Global;

pub type Meta = String;

#[derive(Clone)]
pub struct Universe {
    name: Option<String>,
    offset: usize,
}

#[derive(Clone)]
pub struct Sort(Vec<Universe>);

#[derive(Clone)]
pub struct Term<M, B> {
    pub meta: M,
    pub variant: Box<TermVariant<M, B>>,
}

#[derive(Clone)]
pub enum TermVariant<M, B> {
    Var(usize),
    Const(String),
    IndType(String, Vec<Universe>),
    IndConstr(String, Vec<Universe>),
    Meta(Meta, Vec<Term<M, B>>),
    Sort(Sort),
    Product(B, Term<M, B>, Term<M, B>),
    Lamda(B, Term<M, B>, Term<M, B>),
    App(Term<M, B>, Term<M, B>),
    LetBind(B, Term<M, B>, Term<M, B>, Term<M, B>),
    Match(Term<M, B>, Term<M, B>, Vec<Term<M, B>>),
    Fix(usize, Vec<(usize, Term<M, B>, Term<M, B>)>),
}

pub enum UniverseConstraint {
    Le(Universe, Universe),
    Eq(Universe, Universe),
    //TODO
}

pub struct UniverseCtx {
    constraints: Vec<UniverseConstraint>,
}

impl UniverseCtx {
    pub fn satisfiable(&self) -> bool {
        todo!()
    }
}

#[derive(Clone)]
pub struct LocalEntry<M, B> {
    value: Option<Term<M, B>>,
    ttype: Term<M, B>,
}

#[derive(Clone)]
pub struct LocalContext<M, B>(Vec<LocalEntry<M, B>>);

pub struct MetaContext<M, B>(HashMap<String, LocalContext<M, B>>);

#[derive(Clone, Copy)]
pub enum UnifyRelation {
    Le,
    Eq,
}

pub struct Unification<M, B> {
    relation: UnifyRelation,
    local: LocalContext<M, B>,
    lhs: Term<M, B>,
    rhs: Term<M, B>,
}

impl<M: Clone, B: Clone> Unification<M, B> {
    pub fn process(
        self,
        global: &Global<M, B>,
        universes: &mut UniverseCtx,
        metas: &mut MetaContext<M, B>,
    ) -> Vec<Unification<M, B>> {
        match (*self.lhs.variant, *self.rhs.variant) {
            (TermVariant::Sort(lhs), TermVariant::Sort(mut rhs)) => {
                if rhs.0.len() != 1 {
                    panic!()
                }
                let rhs = rhs.0.pop().unwrap();

                match self.relation {
                    UnifyRelation::Le => {
                        for lhs in lhs.0 {
                            universes
                                .constraints
                                .push(UniverseConstraint::Le(lhs, rhs.clone()));
                        }
                    }
                    UnifyRelation::Eq => {
                        for lhs in lhs.0 {
                            universes
                                .constraints
                                .push(UniverseConstraint::Eq(lhs, rhs.clone()));
                        }
                    }
                }

                Vec::new()
            }
            (
                TermVariant::Product(_, x_input, x_output),
                TermVariant::Product(_, y_input, y_output),
            )
            | (
                TermVariant::Lamda(_, x_input, x_output),
                TermVariant::Lamda(_, y_input, y_output),
            ) => {
                let mut local_out = self.local.clone();
                local_out.0.push(LocalEntry {
                    value: None,
                    ttype: x_input.clone(),
                });
                vec![
                    Unification {
                        relation: UnifyRelation::Eq,
                        local: self.local,
                        lhs: x_input,
                        rhs: y_input,
                    },
                    Unification {
                        relation: self.relation,
                        local: local_out,
                        lhs: x_output,
                        rhs: y_output,
                    },
                ]
            }
            (
                TermVariant::LetBind(_, x_type, x_value, lhs),
                TermVariant::LetBind(_, y_type, y_value, rhs),
            ) => {
                let mut local_out = self.local.clone();
                local_out.0.push(LocalEntry {
                    value: None,
                    ttype: x_type.clone(),
                });
                vec![
                    Unification {
                        relation: UnifyRelation::Eq,
                        local: self.local.clone(),
                        lhs: x_type,
                        rhs: y_type,
                    },
                    Unification {
                        relation: UnifyRelation::Eq,
                        local: self.local,
                        lhs: x_value,
                        rhs: y_value,
                    },
                    Unification {
                        relation: self.relation,
                        local: local_out,
                        lhs: lhs,
                        rhs: rhs,
                    },
                ]
            }
            _ => todo!(),
        }
    }
}
