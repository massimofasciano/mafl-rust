use mafl::Interpreter;
use anyhow::{Result, anyhow};

fn main() -> Result<()> {
    let mut args = std::env::args();
    if args.next().is_none() {
        return Err(anyhow!("no arg 0"));
    }
    let interpreter = Interpreter::default();
    if let Some(file) = args.next() {
        let source = std::fs::read_to_string(file)?;
        let syntax = interpreter.parse_source_syntax(&source)?;
        println!("{}",syntax.try_to_json()?);
    }
    Ok(())
}
