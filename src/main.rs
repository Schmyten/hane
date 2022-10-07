use hane_kernel::global::Global;
use hane_syntax::parser::parse;
use std::fs::{read_dir, read_to_string};
use std::path::Path;

fn main() {
    let mut tests = 0;
    let mut failed = 0;

    for test in read_dir("tests").unwrap() {
        let test = test.unwrap();
        if !test.file_type().unwrap().is_file() {
            continue;
        }
        let fullname = test.file_name();
        let fullname = fullname.to_str().unwrap();
        if !fullname.ends_with(".v") {
            continue;
        }
        let name = &fullname[..fullname.len() - 2];
        let path = test.path();
        let path = path.to_str().unwrap();

        tests += 1;

        let content = read_to_string(path).unwrap();
        let parse_result = parse(&content);

        let parse_err = format!("tests/{}.parse.err", name);
        if Path::new(&parse_err).exists() {
            let parse_err = read_to_string(parse_err).unwrap();
            match parse_result {
                Ok(_commands) => {
                    eprintln!("{name}: Expected a parsing error, but expresion passed succesfully");
                    failed += 1;
                    continue;
                }
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
                }
            }

            continue;
        }

        let commands = match parse_result {
            Ok(commands) => commands,
            Err(err) => {
                let err = err.print(path, &content);
                eprintln!("{name}: Failed to parse expresion");
                eprintln!("```\n{err}\n```");
                failed += 1;
                continue;
            }
        };

        let mut global = Global::new();
        let result = commands
            .into_iter()
            .try_for_each(|command| command.lower(&mut global));

        let result_err = &format!("tests/{name}.err");
        if Path::new(&result_err).exists() {
            let result_err = read_to_string(result_err).unwrap();
            match result {
                Ok(_) => {
                    eprintln!("{name}: Expected an error, but test ran successfully");
                    eprintln!("```\n{global}\n```");
                    failed += 1;
                    continue;
                }
                Err(err) => {
                    let err = err.print(path, &content);
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

        let result_out = format!("tests/{name}.out");
        if !Path::new(&result_out).exists() {
            continue;
        }

        if let Err(err) = result {
            let err = err.print(path, &content);
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
