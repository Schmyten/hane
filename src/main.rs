use hane_kernel::global::Global;
use hane_syntax::eval::EvalError;
use hane_syntax::parser::parse;
use hane_syntax::SpanError;
use std::collections::HashSet;
use std::fs::read_to_string;

fn main() {
    let mut tests = 0;
    let mut failed = 0;

    for (name, file) in walkdir::WalkDir::new("tests")
        .into_iter()
        .filter_map(|f| f.ok())
        .filter(|f| f.file_type().is_file())
        .map(|f| (f.file_name().to_string_lossy().into_owned(), f))
        .filter(|(name, _)| name.ends_with(".v"))
    {
        tests += 1;

        let name = &name[..name.len() - 2];

        let path = file.path();

        let parse_err_path = {
            let mut path = path.to_path_buf();
            assert!(path.pop());
            path.push(format!("{name}.parse.err"));
            path
        };

        let content = read_to_string(path)
            .unwrap_or_else(|_| panic!("valid utf-8 in {}", path.to_string_lossy()));

        let parse_result = parse(&content);

        if parse_err_path.exists() {
            let parse_err = read_to_string(&parse_err_path)
                .unwrap_or_else(|_| panic!("valid utf-8 in {}", parse_err_path.to_string_lossy()));
            match parse_result {
                Ok(_commands) => {
                    eprintln!("{name}: Expected a parsing error, but expresion passed succesfully");
                    failed += 1;
                    continue;
                }
                Err(err) => {
                    let err = err.print(path.to_string_lossy().as_ref(), &content);
                    if err != parse_err {
                        eprintln!("{name}: Parsing error does not match expected error");
                        eprintln!("expected:");
                        eprintln!("```\n{parse_err}\n```");
                        eprintln!("actual:");
                        eprintln!("```\n{err}\n```");
                        failed += 1;
                        continue;
                    }
                }
            }

            continue;
        }

        let commands = match parse_result {
            Ok(commands) => commands,
            Err(err) => {
                let err = err.print(path.to_string_lossy().as_ref(), &content);
                eprintln!("{name}: Failed to parse expresion");
                eprintln!("```\n{err}\n```");
                failed += 1;
                continue;
            }
        };

        let mut global = HashSet::new();
        let lower = commands
            .into_iter()
            .map(|command| command.lower(&mut global))
            .collect::<Result<Vec<_>, _>>();

        let lower_err_path = {
            let mut path = path.to_path_buf();
            assert!(path.pop());
            path.push(format!("{name}.lower.err"));
            path
        };
        if lower_err_path.exists() {
            let lower_err = read_to_string(&lower_err_path).unwrap();
            match lower {
                Ok(commands) => {
                    eprintln!("{name}: Expected a lowering error, but test ran successfully");
                    eprintln!("```");
                    commands.iter().for_each(|command| eprintln!("{command}"));
                    eprintln!("```");
                    failed += 1;
                    continue;
                }
                Err(err) => {
                    let err = err.print(path.to_string_lossy().as_ref(), &content);
                    if err != lower_err {
                        eprintln!("{name}: Lowering error does not match expected error");
                        eprintln!("expected:");
                        eprintln!("```\n{lower_err}\n```");
                        eprintln!("actual:");
                        eprintln!("```\n{err}\n```");
                        failed += 1;
                        continue;
                    }
                }
            }

            continue;
        }

        let lower_out = {
            let mut path = path.to_path_buf();
            assert!(path.pop());
            path.push(format!("{name}.lower"));
            path
        };
        if !lower_out.exists() {
            continue;
        }

        let commands = match lower {
            Ok(commands) => commands,
            Err(err) => {
                let err = err.print(path.to_string_lossy().as_ref(), &content);
                eprintln!("{name}: Lowering failed with error:");
                eprintln!("```\n{err}\n```");
                failed += 1;
                continue;
            }
        };

        let lower_out = read_to_string(&lower_out).unwrap();
        let lower_print = {
            use std::fmt::Write;
            let mut print = String::new();
            commands
                .iter()
                .for_each(|command| writeln!(print, "{command}").unwrap());
            print
        };

        if lower_print != lower_out {
            eprintln!("{name}: Lowering result does not match expected output");
            eprintln!("expected:");
            eprintln!("```\n{lower_out}\n```");
            eprintln!("actual:");
            eprintln!("```\n{lower_print}\n```");
            failed += 1;
            continue;
        }

        let mut global = Global::new();
        let result = commands
            .into_iter()
            .try_for_each(|command| command.eval(&mut global));

        let result_err_path = {
            let mut path = path.to_path_buf();
            assert!(path.pop());
            path.push(format!("{name}.err"));
            path
        };
        if result_err_path.exists() {
            let result_err = read_to_string(&result_err_path).unwrap();
            match result {
                Ok(_) => {
                    eprintln!("{name}: Expected an error, but test ran successfully");
                    eprintln!("```\n{global}\n```");
                    failed += 1;
                    continue;
                }
                Err((span, err)) => {
                    let err = SpanError {
                        span,
                        err: EvalError(err),
                    }
                    .print(path.to_string_lossy().as_ref(), &content);
                    if err != result_err {
                        eprintln!("{name}: Error does not match expected error");
                        eprintln!("expected:");
                        eprintln!("```\n{result_err}\n```");
                        eprintln!("actual:");
                        eprintln!("```\n{err}\n```");
                        failed += 1;
                        continue;
                    }
                }
            }

            continue;
        }

        let result_out = {
            let mut path = path.to_path_buf();
            assert!(path.pop());
            path.push(format!("{name}.out"));
            path
        };
        if !result_out.exists() {
            continue;
        }

        if let Err((span, err)) = result {
            let err = SpanError {
                span,
                err: EvalError(err),
            }
            .print(path.to_string_lossy().as_ref(), &content);
            eprintln!("{name}: Failed with error:");
            eprintln!("```\n{err}\n```");
            failed += 1;
            continue;
        }

        let result_out = read_to_string(&result_out).unwrap();
        let result_print = format!("{global}");
        if result_print != result_out {
            eprintln!("{name}: Result does not match expected output");
            eprintln!("expected:");
            eprintln!("```\n{result_out}\n```");
            eprintln!("actual:");
            eprintln!("```\n{result_print}\n```");
            failed += 1;
            continue;
        }
    }

    if failed != 0 {
        panic!("Failed {failed}/{tests}");
    } else {
        println!("All tests succeded: {tests}/{tests}");
    }
}
