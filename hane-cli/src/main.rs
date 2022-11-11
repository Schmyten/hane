use std::{io::stdin, collections::HashSet};
use hane_kernel::Global;
use hane_syntax::{parser::parse, SpanError, eval::EvalError};

fn main() {
    let mut names = HashSet::new();
    let mut global = Global::new();

    for line in stdin().lines() {
        let line = line.unwrap();

        let mut commands = match parse(&line) {
            Ok(commands) => commands,
            Err(err) => {
                let err = err.print(None, &line);
                eprintln!("{err}");
                continue
            },
        };

        if commands.len() != 1 {
            eprintln!("The cli only accepts a single command at a time");
            continue
        }

        let command = commands.pop().unwrap();
        let command = match command.lower(&mut names) {
            Ok(command) => command,
            Err(err) => {
                let err = err.print(None, &line);
                eprintln!("{err}");
                continue
            }
        };

        match command.eval(&mut global) {
            Ok(command) => command,
            Err((span, err)) => {
                let err = SpanError {
                    span,
                    err: EvalError(err),
                }
                .print(None, &line);
                eprintln!("{err}");
                continue
            }
        };
    }
}
