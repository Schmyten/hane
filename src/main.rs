use std::fs::{read_dir, read_to_string};
use std::path::Path;
use hane_kernel::stack::Stack;
use hane_syntax::parser::parse;

fn main() {
    let mut tests = 0;
    let mut failed = 0;

    for test in read_dir("tests").unwrap() {
        let test = test.unwrap();
        if !test.file_type().unwrap().is_file() { continue; }
        let fullname = test.file_name();
        let fullname = fullname.to_str().unwrap();
        if !fullname.ends_with(".v") { continue; }
        let name = &fullname[..fullname.len()-2];
        let path = test.path();
        let path = path.to_str().unwrap();

        tests += 1;

        let content = read_to_string(path).unwrap();
        let parse_result = parse(&content);

        let parse_err = format!("tests/{}.parse.err", name);
        if Path::new(&parse_err).exists() {
            let parse_err = read_to_string(parse_err).unwrap();
            match parse_result {
                Ok(expr) => {
                    eprintln!("{name}: Expected a parsing error, but expresion passed succesfully");
                    failed += 1;
                    continue;
                },
                Err(err) => {
                    let err = err.print(path, &content);
                    if err != parse_err {
                        eprintln!("{name}: Parsing error does not match expected error");
                        eprintln!("expected:");
                        eprintln!("```\n{parse_err}\n```");
                        eprintln!("actual:");
                        eprintln!("```\n{err}\n```");
                        failed += 1;
                        continue;
                    }
                },
            }

            continue;
        }

        let expr = match parse_result {
            Ok(expr) => expr,
            Err(err) => {
                let err = err.print(path, &content);
                eprintln!("{name}: Failed to parse expresion");
                eprintln!("```\n{err}\n```");
                failed += 1;
                continue;
            },
        };

        let mut names = Stack::new();
        let lower_result = expr.lower(&mut names);

        let lower_err = &format!("tests/{name}.lower.err");
        if Path::new(&lower_err).exists() {
            let lower_err = read_to_string(lower_err).unwrap();
            match lower_result {
                Ok(expr) => {
                    eprintln!("{name}: Expected a lowering error, but expresion lowered succesfully");
                    failed += 1;
                    continue;
                },
                Err(err) => {
                    let err = err.print(path, &content);
                    if err != lower_err {
                        eprintln!("{name}: Lowering error does not match expected error");
                        eprintln!("expected:");
                        eprintln!("```\n{lower_err}\n```");
                        eprintln!("actual:");
                        eprintln!("```\n{err}\n```");
                        failed += 1;
                        continue;
                    }
                },
            }

            continue;
        }

        let lower_out = format!("tests/{name}.lower");
        if !Path::new(&lower_out).exists() { continue; }

        let term = match lower_result {
            Ok(term) => term,
            Err(err) => {
                let err = err.print(path, &content);
                eprintln!("{name}: Failed to lower expresion");
                eprintln!("```\n{err}\n```");
                failed += 1;
                continue;
            },
        };

        let lower_out = read_to_string(&lower_out).unwrap();
        let lower_print = format!("{term}");
        if lower_print != lower_out {
            eprintln!("{name}: Lowered expresion does not match expected output");
            eprintln!("expected:");
            eprintln!("```\n{lower_out}\n```");
            eprintln!("actual:");
            eprintln!("```\n{lower_print}\n```");
            failed += 1;
            continue;
        }

        let mut env = Stack::new();
        let type_result = term.type_check(&mut env);

        let type_out = format!("tests/{name}.type");
        if !Path::new(&type_out).exists() { continue; }

        let term = match type_result {
            Some(term) => term,
            None => {
                eprintln!("{name}: Failed to type check expresion");
                failed += 1;
                continue;
            },
        };

        let type_out = read_to_string(&type_out).unwrap();
        let type_print = format!("{term}");
        if type_print != type_out {
            eprintln!("{name}: The expresions type does not match expected output");
            eprintln!("expected:");
            eprintln!("```\n{type_out}\n```");
            eprintln!("actual:");
            eprintln!("```\n{type_print}\n```");
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
