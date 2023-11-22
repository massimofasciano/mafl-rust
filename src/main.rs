use std::{io::{stdin, BufRead, stdout, Write}, env::args};
use mafl::{Interpreter, expression::Value};
use anyhow::{Result, anyhow};

fn main() -> Result<()> {
    env_logger::init();
    let mut env = args();
    if env.next().is_none() {
        return Err(anyhow!("no arg 0"));
    }
    let mut interpreter = Interpreter::new()?;
    if let Some(file) = env.next() {
        // run the program
        let source = std::fs::read_to_string(file)?;
        interpreter.set_env(env.map(|x|x.to_string()).collect());
        let result = interpreter.run(&source)?;
        // println!();
        // println!("*** Program result via @println:");
        // interpreter.println(result.to_owned())?;
        println!();
        println!("*** Program result as a value:");
        let value = Value::try_from(result)?;
        println!("{value}");
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
