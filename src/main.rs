use hane_kernel::term::Term;
use hane_syntax::print::print_term;

fn main() {
    let imply = Term::Abstract("P".to_owned(), Box::new(Term::Prop), Box::new(
        Term::Abstract("Q".to_owned(), Box::new(Term::Prop), Box::new(
            Term::Product("p".to_owned(), Box::new(Term::Var(1)), Box::new(Term::Var(1)))
        ))
    ));
    println!("let imply := {}", print_term(&imply, &mut Vec::new(), 200));
    let mut names = vec!["imply".to_owned()];
    let and = Term::Abstract("P".to_owned(), Box::new(Term::Prop), Box::new(
        Term::Abstract("Q".to_owned(), Box::new(Term::Prop), Box::new(
            Term::Product("R".to_owned(), Box::new(Term::Prop), Box::new(
                Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(
                    Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(Term::Var(2)))), Box::new(
                        Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(Term::Var(1)))), Box::new(
                            Term::Var(0)
                        ))
                    ))
                ))), Box::new(Term::Var(0)))
            ))
        ))
    ));
    println!("let and := {}", print_term(&and, &mut names, 200));
    let and = Term::Abstract("P".to_owned(), Box::new(Term::Prop), Box::new(
        Term::Abstract("Q".to_owned(), Box::new(Term::Prop), Box::new(
            Term::Product("R".to_owned(), Box::new(Term::Prop), Box::new(
                Term::Product("f".to_owned(), Box::new(
                    Term::Product("p".to_owned(), Box::new(Term::Var(2)), Box::new(
                        Term::Product("q".to_owned(), Box::new(Term::Var(2)), Box::new(Term::Var(2)))
                    ))
                ), Box::new(Term::Var(1)))
            ))
        ))
    ));
    println!("and := {}", print_term(&and, &mut Vec::new(), 200));
    let or = Term::Abstract("P".to_owned(), Box::new(Term::Prop), Box::new(
        Term::Abstract("Q".to_owned(), Box::new(Term::Prop), Box::new(
            Term::Product("R".to_owned(), Box::new(Term::Prop), Box::new(
                Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(
                    Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(Term::Var(2)))), Box::new(Term::Var(0)))
                ))), Box::new(
                    Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(
                        Term::App(Box::new(Term::App(Box::new(Term::Var(3)), Box::new(Term::Var(1)))), Box::new(Term::Var(0)))
                    ))), Box::new(
                        Term::Var(0)
                    ))
                ))
            ))
        ))
    ));
    println!("let or := {}", print_term(&or, &mut names, 200));
    let or = Term::Abstract("P".to_owned(), Box::new(Term::Prop), Box::new(
        Term::Abstract("Q".to_owned(), Box::new(Term::Prop), Box::new(
            Term::Product("R".to_owned(), Box::new(Term::Prop), Box::new(
                Term::Product("p".to_owned(), Box::new(
                    Term::Product("p".to_owned(), Box::new(Term::Var(2)), Box::new(Term::Var(1)))
                ), Box::new(
                    Term::Product("q".to_owned(), Box::new(
                        Term::Product("q".to_owned(), Box::new(Term::Var(2)), Box::new(Term::Var(2)))
                    ), Box::new(Term::Var(2)))
                ))
            ))
        ))
    ));
    println!("or := {}", print_term(&or, &mut Vec::new(), 200));
}
