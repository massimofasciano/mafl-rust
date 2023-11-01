use std::{io::stdin, env::args};
use mfel::{parse_source, eval::eval, context::Context};
use anyhow::Result;

fn main() -> Result<()> {
    env_logger::init();
    let source = if let Some(file) = args().nth(1) {
        std::fs::read_to_string(file)?
    } else {
        std::io::read_to_string(stdin())?
    };
    let expr = parse_source(&source)?;
    // println!("{ast:#?}");
    let ctx = Context::new();
    let result = eval(&ctx, &expr)?;
    // println!("{result:#?}");
    println!("{result}");
    Ok(())
}
