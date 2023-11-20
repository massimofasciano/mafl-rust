use std::{io::{stdin, BufRead, stdout, Write}, env::args};
use mfel::{Interpreter, expression::Value};
use anyhow::{Result, anyhow};

#[cfg(feature = "test_custom")]
use mfel::{expression::{Expr,self}, context::Context, Ptr};

// must match the signature of Builtin
#[cfg(feature = "test_custom")]
fn custom_fn(_: &Interpreter, ctx: &Context, args: &[Ptr<Expr>]) -> Result<Ptr<Expr>> {
    // a custom function that takes an array of strings and returns an array of values.
    // each string is taken as the name of an identifier to fetch from the current context.
    // defaults to "x" if no arguments (can be used as a builtin variable or function).
    let mut values = vec![];
    if args.is_empty() {
        let id = "x";
        values.push(ctx.get_binding(id).ok_or(anyhow!("binding not found: {id}"))?)
    } else {
        for arg in args {
            match arg.as_ref() {
                Expr::String(id) => {
                    values.push(ctx.get_binding(id).ok_or(anyhow!("binding not found: {id}"))?)
                }
                _ => Err(anyhow!("custom called with non-string"))?
            }
        }
    }
    Ok(expression::array(values))
}

fn main() -> Result<()> {
    env_logger::init();
    let mut env = args();
    if env.next().is_none() {
        return Err(anyhow!("no arg 0"));
    }
    let mut interpreter = Interpreter::new()?;
    #[cfg(feature = "test_custom")]
    interpreter.add_builtin_var("my_custom_var".to_owned(), custom_fn);
    #[cfg(feature = "test_custom")]
    interpreter.add_builtin_fn("my_custom_fn".to_owned(), custom_fn);
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
        println!("{}", Value::try_from(result)?);        
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
            print!("MFEL> "); stdout().flush()?;
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
