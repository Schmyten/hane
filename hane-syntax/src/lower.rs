use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};

use crate::{
    Binder, Command, CommandVariant, Expr, ExprVariant, LoweredCommand, LoweredCommandVariant,
    Span, SpanError,
};
use hane_kernel::{Sort, Stack, Term, TermVariant};

pub enum LoweringError {
    NameNotFree(String),
    UnknownVariable(String),
}

impl Display for LoweringError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::NameNotFree(x) => write!(f, "A constant named `{x}` already exists"),
            LoweringError::UnknownVariable(x) => write!(f, "Unknown variable `{x}`"),
        }
    }
}

impl Display for LoweredCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.variant {
            LoweredCommandVariant::Definition(name, ttype, value) => {
                write!(f, "Definition {name} : {ttype} := {value}.")
            }
            LoweredCommandVariant::Axiom(name, ttype) => write!(f, "Axiom {name} : {ttype}."),
        }
    }
}

impl Command {
    pub fn lower(
        self,
        global: &mut HashSet<String>,
    ) -> Result<LoweredCommand, SpanError<LoweringError>> {
        let mut names = Stack::new();
        let variant = match self.variant {
            CommandVariant::Definition(name, ttype, value) => {
                if global.contains(&name) {
                    return Err(SpanError {
                        span: self.span,
                        err: LoweringError::NameNotFree(name),
                    });
                }
                let ttype = ttype.lower(global, &mut names)?;
                let value = value.lower(global, &mut names)?;
                global.insert(name.clone());
                LoweredCommandVariant::Definition(name, ttype, value)
            }
            CommandVariant::Axiom(name, ttype) => {
                if global.contains(&name) {
                    return Err(SpanError {
                        span: self.span,
                        err: LoweringError::NameNotFree(name),
                    });
                }
                let ttype = ttype.lower(global, &mut names)?;
                global.insert(name.clone());
                LoweredCommandVariant::Axiom(name, ttype)
            }
        };
        Ok(LoweredCommand {
            span: self.span,
            variant,
        })
    }
}

impl Expr {
    pub fn lower(
        self,
        global: &HashSet<String>,
        names: &mut Stack<String>,
    ) -> Result<Term<Span, String>, SpanError<LoweringError>> {
        let variant = match *self.variant {
            ExprVariant::Sort(sort) => TermVariant::Sort(sort.lower()),
            ExprVariant::Var(x) => {
                if let Some((i, _)) = names.iter().enumerate().find(|(_, y)| x == **y) {
                    TermVariant::Var(i)
                } else if global.contains(&x) {
                    TermVariant::Const(x)
                } else {
                    return Err(SpanError {
                        span: self.span.clone(),
                        err: LoweringError::UnknownVariable(x.to_owned()),
                    });
                }
            }
            ExprVariant::App(f, v) => {
                TermVariant::App(f.lower(global, names)?, v.lower(global, names)?)
            }
            // TODO: Possibly implement something more RAII like for cleanup of names
            ExprVariant::Product(binders, t) => {
                let mut type_stack = Vec::new();
                for Binder { name, ttype } in binders {
                    let lowered_type = match ttype.lower(global, names) {
                        Ok(lt) => lt,
                        Err(e) => {
                            drop(names.pop_n(type_stack.len()));
                            return Err(e);
                        }
                    };
                    type_stack.push(lowered_type);
                    names.push(name);
                }
                match t.lower(global, names) {
                    Err(e) => {
                        // Lowering failed, but we still have to clean up the names stack
                        drop(names.pop_n(type_stack.len()));
                        return Err(e);
                    }
                    Ok(t) => {
                        let mut iter = names
                            .pop_n(type_stack.len())
                            .zip(type_stack.into_iter().rev());

                        let make_term = |inner, (name, ttype)| Term {
                            meta: self.span.clone(),
                            variant: Box::new(TermVariant::Product(name, ttype, inner)),
                        };
                        let inner = iter.next().unwrap();
                        return Ok(iter.fold(make_term(t, inner), make_term));
                    }
                }
            }
            // TODO: Possibly implement something more RAII like for cleanup of names
            ExprVariant::Abstract(binders, t) => {
                let mut type_stack = Vec::new();
                for Binder { name, ttype } in binders {
                    let lowered_type = match ttype.lower(global, names) {
                        Ok(lt) => lt,
                        Err(e) => {
                            drop(names.pop_n(type_stack.len()));
                            return Err(e);
                        }
                    };
                    type_stack.push(lowered_type);
                    names.push(name);
                }
                match t.lower(global, names) {
                    Err(e) => {
                        // Lowering failed, but we still have to clean up the names stack
                        drop(names.pop_n(type_stack.len()));
                        return Err(e);
                    }
                    Ok(t) => {
                        let mut iter = names
                            .pop_n(type_stack.len())
                            .zip(type_stack.into_iter().rev());

                        let make_term = |inner, (name, ttype)| Term {
                            meta: self.span.clone(),
                            variant: Box::new(TermVariant::Abstract(name, ttype, inner)),
                        };
                        let inner = iter.next().unwrap();
                        return Ok(iter.fold(make_term(t, inner), make_term));
                    }
                }
            }
            ExprVariant::Bind(x, x_tp, x_val, t) => {
                let x_tp = x_tp.lower(global, names)?;
                let x_val = x_val.lower(global, names)?;
                names.push(x);
                let t = t.lower(global, names);
                let x = names.pop().unwrap();
                let t = t?;
                TermVariant::Bind(x, x_tp, x_val, t)
            }
        };
        Ok(Term {
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
