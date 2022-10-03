use hane_kernel::term::Term;
use hane_syntax::print::print_term;

use Term::Prop;
use Term::Var;

fn app<B>(f: Term<B>, args: impl IntoIterator<Item = Term<B>>) -> Term<B> {
    args.into_iter().fold(f, |f, v| Term::App(Box::new(f), Box::new(v)))
}

fn product<B, I: IntoIterator<Item = (B, Term<B>)>>(xs: I, t: Term<B>) -> Term<B> where I::IntoIter: DoubleEndedIterator {
    xs.into_iter().rev().fold(t, |t, (x, x_tp)| Term::Product(x, Box::new(x_tp), Box::new(t)))
}

fn lamda<B, I: IntoIterator<Item = (B, Term<B>)>>(xs: I, t: Term<B>) -> Term<B> where I::IntoIter: DoubleEndedIterator {
    xs.into_iter().rev().fold(t, |t, (x, x_tp)| Term::Abstract(x, Box::new(x_tp), Box::new(t)))
}

fn bind<B>(x: B, x_tp: Term<B>, x_val: Term<B>, t: Term<B>) -> Term<B> {
    Term::Bind(x, Box::new(x_tp), Box::new(x_val), Box::new(t))
}

fn main() {
    let conn = product([("P".to_owned(), Prop), ("Q".to_owned(), Prop)], Prop);
    let imply = lamda([("P".to_owned(), Prop), ("Q".to_owned(), Prop)],
        product([("p".to_owned(), Var(1))], Var(1))
    );
    println!("let imply := {}", print_term(&imply, &mut Vec::new(), 200));
    let mut names = vec!["imply".to_owned()];

    let and = lamda([("P".to_owned(), Prop), ("Q".to_owned(), Prop)],
        product([("R".to_owned(), Prop)],
            app(Var(3), [app(Var(3), [Var(2), app(Var(3), [Var(1), Var(0)])]), Var(0)])
        )
    );
    println!("let and := {}", print_term(&and, &mut names, 200));
    let mut and = bind("imply".to_owned(), conn.clone(), imply.clone(), and);
    and.normalize();
    println!("{}", print_term(&and, &mut Vec::new(), 200));
    let or = lamda([("P".to_owned(), Prop), ("Q".to_owned(), Prop)],
        product([("R".to_owned(), Prop)],
            app(Var(3), [app(Var(3), [app(Var(3), [Var(2), Var(0)]), app(Var(3), [app(Var(3), [Var(1), Var(0)]), Var(0)])]), Var(0)])
        )
    );
    println!("let or := {}", print_term(&or, &mut names, 200));
    let mut or = bind("imply".to_owned(), conn.clone(), imply.clone(), or);
    or.normalize();
    println!("{}", print_term(&or, &mut Vec::new(), 200));
}
