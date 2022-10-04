use std::fmt::{self, Write};

use hane_kernel::stack::Stack;

type Term = hane_kernel::term::Term<String>;

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

pub fn write_term(buf: &mut impl Write, term: &Term, names: &mut Stack<String>, level: usize) -> fmt::Result {
    match term {
        Term::Prop => write!(buf, "Prop"),
        Term::Var(n) =>
            if let Some(x) = names.get(*n) { write!(buf, "{}", x) }
            else { write!(buf, "?:{}", n - names.len()) },
        Term::App(f, v) => {
            if level < 10 { write!(buf, "(")?; }
            write_term(buf, f, names, 10)?;
            write!(buf, " ")?;
            write_term(buf, v, names, 9)?;
            if level < 10 { write!(buf, ")")?; }
            Ok(())
        },
        Term::Product(x, x_tp, t) => {
            let x = fresh(x, names);
            if level < 200 { write!(buf, "(")?; }
            write!(buf, "forall {x} : ")?;
            write_term(buf, x_tp, names, 200)?;
            write!(buf, ", ")?;
            names.push(x);
            let r = write_term(buf, t, names, 200);
            names.pop();
            r?;
            if level < 200 { write!(buf, ")")?; }
            Ok(())
        },
        Term::Abstract(x, x_tp, t) => {
            let x = fresh(x, names);
            if level < 200 { write!(buf, "(")?; }
            write!(buf, "fun {x} : ")?;
            write_term(buf, x_tp, names, 200)?;
            write!(buf, " => ")?;
            names.push(x);
            let r = write_term(buf, t, names, 200);
            names.pop();
            r?;
            if level < 200 { write!(buf, ")")?; }
            Ok(())
        },
        Term::Bind(x, x_tp, x_val, t) => {
            let x = fresh(x, names);
            if level < 200 { write!(buf, "(")?; }
            write!(buf, "let {x} : ")?;
            write_term(buf, x_tp, names, 200)?;
            write!(buf, " := ")?;
            write_term(buf, x_val, names, 200)?;
            write!(buf, " in ")?;
            names.push(x);
            let r = write_term(buf, t, names, 200);
            names.pop();
            r?;
            if level < 200 { write!(buf, ")")?; }
            Ok(())
        },
    }
}

pub fn print_term(term: &Term, names: &mut Stack<String>, level: usize) -> String {
    let mut buf = String::new();
    write_term(&mut buf, term, names, level).unwrap();
    buf
}
