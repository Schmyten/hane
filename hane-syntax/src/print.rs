use std::fmt::{self, Write};

use hane_kernel::{entry::Entry, stack::StackSlot, term::TermVariant, Stack};

type Term<M> = hane_kernel::term::Term<M, String>;

fn fresh(x: &String, names: &Stack<String>) -> String {
    if !names.contains(x) {
        return x.to_owned();
    }

    let x = x.trim_end_matches(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
    for n in 0usize.. {
        let fresh = format!("{x}{n}");
        if !names.contains(&fresh) {
            return fresh;
        }
    }
    unreachable!()
}

pub fn write_term<M>(
    buf: &mut impl Write,
    term: &Term<M>,
    names: &mut Stack<String>,
    level: usize,
) -> fmt::Result {
    match &*term.variant {
        TermVariant::Sort(sort) => write!(buf, "{sort}"),
        TermVariant::Var(n) => {
            if let Some(x) = names.get(*n) {
                write!(buf, "{}", x)
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
            write!(buf, "forall {x} : ")?;
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
            write!(buf, "fun {x} : ")?;
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
            write!(buf, "let {x} : ")?;
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
            let name = fresh(name, names);
            write!(buf, " as {name} in {}", ret.constructor)?;
            for x in &ret.params {
                write!(buf, " {x}")?;
            }
            write!(buf, " return ")?;
            write_term(buf, &ret.body, names, 200)?;
            write!(buf, " with")?;
            let mut sep = "";
            for arm in arms {
                write!(buf, "{sep} {}", arm.constructor)?;
                sep = " |";
                for x in &arm.params {
                    write!(buf, " {x}")?;
                }
                write!(buf, " => {}", arm.body)?;
            }
            write!(buf, " end")
        }
    }
}

pub fn print_term<M>(term: &Term<M>, names: &mut Stack<String>, level: usize) -> String {
    let mut buf = String::new();
    write_term(&mut buf, term, names, level).unwrap();
    buf
}

pub fn write_local<'a, M>(
    buf: &mut impl Write,
    local: &Stack<Entry<M, String>>,
    names: &'a mut Stack<String>,
) -> Result<StackSlot<'a, String>, fmt::Error> {
    let mut names = names.slot();
    for entry in local.iter().rev() {
        let x = fresh(&entry.x, &names);
        write!(buf, "{x}: ")?;
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
