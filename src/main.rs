use std::{io::{stdin, BufRead, stdout, Write}, env::args};
use mfel::Interpreter;
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
        println!();
        println!("*** DEBUG INFO: full program evaluates to:");
        interpreter.println(result)?;
    } else {
        // REPL
        let stdin = stdin();
        loop {
            print!("MFEL> "); stdout().flush()?;
            let next_line = stdin.lock().lines().next();
            if let Some(line_result) = next_line {
                let result = interpreter.run(&line_result?)?;
                interpreter.println(result)?;
            } else {
                break;
            }
        }
    }
    Ok(())
}
