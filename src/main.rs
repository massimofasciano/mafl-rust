use std::{io::stdin, env::args};
use mfel::Interpreter;
use anyhow::{Result, anyhow};

fn main() -> Result<()> {
    env_logger::init();
    let mut env = args();
    if env.next().is_none() {
        return Err(anyhow!("no arg 0"));
    }
    let source = if let Some(file) = env.next() {
        std::fs::read_to_string(file)?
    } else {
        std::io::read_to_string(stdin())?
    };
    let interpreter = Interpreter::new(env)?;
    let result = interpreter.run(&source)?;
    // println!("{result:#?}");
    println!("{result}");
    Ok(())
}
