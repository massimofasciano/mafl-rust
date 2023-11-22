use std::io::{stdin, BufRead, stdout, Write};
use mafl::{Interpreter, expression::Value};
use anyhow::{Result, anyhow};

fn main() -> Result<()> {
    env_logger::init();
    let mut args = std::env::args();
    if args.next().is_none() {
        return Err(anyhow!("no arg 0"));
    }
    let mut interpreter = Interpreter::new()?;
    let mut opt_file : Option<String> = None;
    let mut show_result = false;
    let mut print_result = false;
    match args.next().as_deref() {
        Some("-r") => { show_result = true; }
        Some("-p") => { print_result = true; }
        Some(name) => { opt_file = Some(name.to_owned()); }
        None => {}
    }
    if opt_file.is_none() { opt_file = args.next(); }
    if let Some(file) = opt_file {
        // run the program
        let source = if file == "-" {
            std::io::read_to_string(stdin())?          
        } else {
            std::fs::read_to_string(file)?
        };
        interpreter.set_args(args.map(|x|x.to_string()).collect());
        let result = interpreter.run(&source)?;
        if print_result {
            println!();
            println!("*** Program result via @println:");
            interpreter.println(result.to_owned())?;
        }
        if show_result {
            println!();
            println!("*** Program result as a value:");
            println!("{}", Value::try_from(result)?);
        }
        let (pass_count, fail_count) = interpreter.test_report();
        if pass_count > 0 || fail_count > 0 {
            println!();
            println!("*** UNIT TEST SUMMARY: ");
            println!("{pass_count} passed. {fail_count} failed.");
        }
    } else {
        // REPL
        let stdin = stdin();
        loop {
            print!("MAFL> "); stdout().flush()?;
            let next_line = stdin.lock().lines().next();
            if let Some(line_result) = next_line {
                match interpreter.run(&line_result?) {
                    Ok(result) => {
                        interpreter.println(result)?;
                    }
                    Err(error) => println!("Error: {error}"),
                }
            } else {
                break;
            }
        }
    }
    Ok(())
}
