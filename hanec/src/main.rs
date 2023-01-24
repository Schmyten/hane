use clap::Parser;
use hane_kernel::global::Global;
use hane_syntax::eval::EvalError;
use hane_syntax::parser::parse;
use hane_syntax::print::Print;
use hane_syntax::SpanError;
use std::collections::HashSet;
use std::fmt::Write;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{env, os, process};

#[derive(Parser, Debug)]
#[command(version)]
struct Cli {
    files: Vec<String>,
}

fn main() -> process::ExitCode {
    let cli = Cli::parse();
    let mut paths = Vec::new();
    for f in cli.files.iter() {
        let path = PathBuf::from_str(f).expect(&format!("Could not create path from \"{f}\""));
        let path = path
            .canonicalize()
            .expect(&format!("Could not find \"{}\"", path.to_string_lossy()));
        paths.push(path);
    }
    let mut global = HashSet::new();
    for path in paths {
        let content = read_to_string(&path).expect(&format!(
            "Found invalid utf-8 in {}",
            path.to_string_lossy()
        ));
        let commands = match parse(&content) {
            Ok(p) => p,
            Err(parse_err) => {
                eprintln!("{}", parse_err.print(path.to_str(), &content));
                return process::ExitCode::FAILURE;
            }
        };

        let lower = commands
            .into_iter()
            .map(|c| c.lower(&mut global))
            .collect::<Result<Vec<_>, _>>();
        let lowered_commands = match lower {
            Ok(c) => c,
            Err(e) => {
                eprintln!("{}", e.print(path.to_str(), &content));
                return process::ExitCode::FAILURE;
            }
        };

        let mut global = Global::new();
        let mut out_buf = String::new();
        let result = lowered_commands.into_iter().try_for_each(|command| {
            command.eval(&mut global, |out| {
                write!(out_buf, "{}", Print(out)).unwrap()
            })
        });
        if let Err(e) = result {
            eprintln!(
                "{}",
                SpanError {
                    span: e.0,
                    err: EvalError(e.1),
                }
                .print(path.to_str(), &content)
            );
            return process::ExitCode::FAILURE;
        }
    }
    process::ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    //Fails if main panics
    #[test]
    fn test_main() {
        main();
    }
}
