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
    let mut interpreter = Interpreter::new()?;
    interpreter.set_env(env.map(|x|x.to_string()).collect());
    let result = interpreter.run(&source)?;
    println!();
    println!("*** DEBUG INFO: full program evaluates to:");
    interpreter.println(result)?;
    Ok(())
}
