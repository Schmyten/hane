use std::fmt::{self, Display, Write};

use hane_kernel::{
    entry::Entry,
    global::{CommandOut, GEntryRef},
    stack::StackSlot,
    term::TermVariant,
    Stack,
};

use crate::Ident;

type Term<M> = hane_kernel::term::Term<M, Ident>;

pub struct Print<T>(pub T);

impl<'a, M> Display for Print<CommandOut<'a, M, Ident>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut names = Stack::new();
        match &self.0 {
            CommandOut::Entry(entry) => match *entry {
                GEntryRef::Definition(name, ttype, val) => {
                    write!(f, "{name} = ")?;
                    write_term(f, val, &mut names, 200)?;
                    write!(f, "\n\t : ")?;
                    write_term(f, ttype, &mut names, 200)?;
                    writeln!(f)
                }
                GEntryRef::Axiom(name, ttype) => {
                    write!(f, "*** [ {name} : ")?;
                    write_term(f, ttype, &mut names, 200)?;
                    writeln!(f, " ]")
                }
                GEntryRef::Inductive(_, params, bodies)
                | GEntryRef::InductiveConstructor(_, _, params, bodies) => {
                    let mut sep = "Inductive";
                    for body in bodies {
                        write!(f, "{sep} {}", body.name)?;
                        sep = "\n  with";
                        let mut names = names.slot();
                        for param in params {
                            write!(f, " ({} : ", param.x.name)?;
                            write_term(f, &param.ttype, &mut names, 200)?;
                            write!(f, ")")?;
                            names.push_onto(param.x.clone());
                        }

                        write!(f, " : ")?;
                        write_term(f, &body.arity_type, &mut names, 200)?;
                        write!(f, " :=")?;

                        let mut sep = " ";
                        for constructor in &body.constructors {
                            write!(f, "\n  {sep} {} : ", constructor.name)?;
                            sep = "|";
                            write_term(f, &constructor.arity_type, &mut names, 200)?;
                        }
                    }
                    writeln!(f, ".")
                }
            },
            CommandOut::Term(term) => {
                write_term(f, term, &mut names, 200)?;
                writeln!(f)
            }
        }
    }
}

fn fresh(x: &Ident, names: &Stack<Ident>) -> Ident {
    if !names.contains(x) {
        return x.clone();
    }

    let span = x.span.clone();
    let x = x
        .name
        .trim_end_matches(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
    for n in 0usize.. {
        let fresh = format!("{x}{n}");
        if !names.iter().any(|ident| ident.name == fresh) {
            return Ident { span, name: fresh };
        }
    }
    unreachable!()
}

pub fn write_term<M>(
    buf: &mut impl Write,
    term: &Term<M>,
    names: &mut Stack<Ident>,
    level: usize,
) -> fmt::Result {
    match &*term.variant {
        TermVariant::Sort(sort) => write!(buf, "{sort}"),
        TermVariant::Var(n) => {
            if let Some(x) = names.get(*n) {
                write!(buf, "{}", x.name)
            } else {
                write!(buf, "?:{}", n - names.len())
            }
        }
        TermVariant::Const(name) => write!(buf, "{name}"),
        TermVariant::App(f, v) => {
            if level < 10 {
                write!(buf, "(")?;
            }
            write_term(buf, f, names, 10)?;
            write!(buf, " ")?;
            write_term(buf, v, names, 9)?;
            if level < 10 {
                write!(buf, ")")?;
            }
            Ok(())
        }
        TermVariant::Product(x, x_tp, t) => {
            let x = fresh(x, names);
            if level < 200 {
                write!(buf, "(")?;
            }
            write!(buf, "forall {} : ", x.name)?;
            write_term(buf, x_tp, names, 200)?;
            write!(buf, ", ")?;
            {
                let mut names = names.push(x);
                write_term(buf, t, &mut names, 200)?
            }
            if level < 200 {
                write!(buf, ")")?;
            }
            Ok(())
        }
        TermVariant::Abstract(x, x_tp, t) => {
            let x = fresh(x, names);
            if level < 200 {
                write!(buf, "(")?;
            }
            write!(buf, "fun {} : ", x.name)?;
            write_term(buf, x_tp, names, 200)?;
            write!(buf, " => ")?;
            {
                let mut names = names.push(x);
                write_term(buf, t, &mut names, 200)?
            }
            if level < 200 {
                write!(buf, ")")?;
            }
            Ok(())
        }
        TermVariant::Bind(x, x_tp, x_val, t) => {
            let x = fresh(x, names);
            if level < 200 {
                write!(buf, "(")?;
            }
            write!(buf, "let {} : ", x.name)?;
            write_term(buf, x_tp, names, 200)?;
            write!(buf, " := ")?;
            write_term(buf, x_val, names, 200)?;
            write!(buf, " in ")?;
            {
                let mut names = names.push(x);
                write_term(buf, t, &mut names, 200)?
            }
            if level < 200 {
                write!(buf, ")")?;
            }
            Ok(())
        }
        TermVariant::Match(t, name, ret, arms) => {
            write!(buf, "match ")?;
            write_term(buf, t, names, 200)?;
            let mut name = fresh(name, names);
            write!(buf, " as {} in {}", name.name, ret.constructor)?;
            {
                let mut names = names.slot();
                for x in &ret.params {
                    let as_names = names.push(name);
                    let x = fresh(x, &as_names);
                    name = as_names.pop().next().unwrap();
                    write!(buf, " {}", x.name)?;
                    names.push_onto(x);
                }
                write!(buf, " return ")?;
                let mut names = names.push(name);
                write_term(buf, term, &mut names, 200)?;
                name = names.pop().next().unwrap();
            }
            write!(buf, " with")?;
            let mut sep = "";
            for arm in arms {
                write!(buf, "{sep} {}", arm.constructor)?;
                sep = " |";
                let mut names = names.slot();
                for x in &arm.params {
                    let as_names = names.push(name);
                    let x = fresh(x, &as_names);
                    name = as_names.pop().next().unwrap();
                    write!(buf, " {}", x.name)?;
                    names.push_onto(x);
                }
                write!(buf, " => ")?;
                let mut names = names.push(name);
                write_term(buf, term, &mut names, 200)?;
                name = names.pop().next().unwrap();
            }
            write!(buf, " end")
        }
    }
}

pub fn print_term<M>(term: &Term<M>, names: &mut Stack<Ident>, level: usize) -> String {
    let mut buf = String::new();
    write_term(&mut buf, term, names, level).unwrap();
    buf
}

pub fn write_local<'a, M>(
    buf: &mut impl Write,
    local: &Stack<Entry<M, Ident>>,
    names: &'a mut Stack<Ident>,
) -> Result<StackSlot<'a, Ident>, fmt::Error> {
    let mut names = names.slot();
    for entry in local.iter().rev() {
        let x = fresh(&entry.x, &names);
        write!(buf, "{}: ", x.name)?;
        write_term(buf, &entry.ttype, &mut names, 200)?;
        if let Some(value) = &entry.value {
            write!(buf, " := ")?;
            write_term(buf, value, &mut names, 200)?;
        }
        writeln!(buf)?;
        names.push_onto(x);
    }
    Ok(names)
}
