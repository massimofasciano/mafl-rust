use std::{io::stdin, env::args};
use mfel::{parse_source, eval::eval, context::Context, expression::{Expression, self}};
use anyhow::{Result, anyhow};

fn main() -> Result<()> {
    env_logger::init();
    let mut env_iter = args();
    if env_iter.next().is_none() {
        return Err(anyhow!("no arg 0"));
    }
    let source = if let Some(file) = env_iter.next() {
        std::fs::read_to_string(file)?
    } else {
        std::io::read_to_string(stdin())?
    };
    let expr = parse_source(&source)?;
    // println!("{ast:#?}");
    let ctx = Context::new();
    let env_exprs = env_iter.map(expression::string).collect();
    let env_array = expression::array(env_exprs);
    ctx.add_binding("@env".to_owned(), env_array);
    let result : Expression = eval(&ctx, expr)?;
    // println!("{result:#?}");
    println!("{result}");
    Ok(())
}
