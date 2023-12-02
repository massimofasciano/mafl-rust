use std::{io::{stdin, BufRead, stdout, Write}, path::PathBuf};
use mafl::{Interpreter, expression::Value};
use anyhow::Result;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// File name of the MAFL program to run (- for stdin).
    /// if absent, fall into a REPL
    pub program: Option<PathBuf>,
    /// Command line arguments that are passed to the MAFL program via @args
    pub args: Vec<String>,
    /// Show the result of the program as a Value
    #[arg(short, long, conflicts_with="println")]
    result: bool,
    /// Show the result of the program using @println
    #[arg(short, long, conflicts_with="result")]
    println: bool,
    /// Print summary of unit tests if any were performed
    #[arg(short, long)]
    tests: bool,
    /// Don't include default prelude in initial environment
    #[arg(long)]
    no_prelude: bool,
    /// Initialize @std from this external file instead of embedded version
    #[arg(short, long, value_name = "FILE")]
    std: Option<PathBuf>,
}

fn main() -> Result<()> {
    env_logger::init();
    let args = Args::parse();
    let mut interpreter = Interpreter::new()?;
    if let Some(ext_std) = args.std {
        interpreter.init_ext_std(ext_std)?;
    } else {
        #[cfg(not(feature = "std_internal"))]
        Err(anyhow::anyhow!("must provide external std library when internal std is disabled"))?;
    }
    if !args.no_prelude {
        interpreter.prelude()?;
    }
    if let Some(file) = args.program {
        // run the program
        let source = if file == PathBuf::from("-") {
            std::io::read_to_string(stdin())?          
        } else {
            std::fs::read_to_string(file)?
        };
        interpreter.set_args(args.args);
        let result = interpreter.run(&source)?;
        if args.println {
            println!();
            println!("*** Program result via @println:");
            interpreter.println(result.to_owned())?;
        }
        if args.result {
            println!();
            println!("*** Program result as a value:");
            println!("{}", Value::try_from(result)?);
        }
        let (pass_count, fail_count) = interpreter.test_report();
        if args.tests && (pass_count > 0 || fail_count > 0) {
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
