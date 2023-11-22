use std::collections::HashMap;

use mafl::{Interpreter, expression::Value};
use anyhow::{Result, anyhow, Ok};

use mafl::{expression::{Expr,self}, context::Context, Ptr};

const SOURCE_FILE : &str = "examples/embed.mafl";

// must match the signature of Builtin
fn validate_subnet(_: &Interpreter, _: &Context, args: &[Ptr<Expr>]) -> Result<Ptr<Expr>> {
    // a custom function that takes an array of integers and validates that the 2 first are 192 and 168
    if args.len() != 1 {
        return Ok(expression::boolean(false));
    }
    let valid = if let Expr::Array(ptr) = args[0].as_ref() {
        let arr = ptr.borrow();
        arr[0].as_ref() == &Expr::Integer(192) && 
        arr[1].as_ref() == &Expr::Integer(168)
    } else {
        false
    };
    Ok(expression::boolean(valid))
}

fn main() -> Result<()> {
    let mut interpreter = Interpreter::new()?;
    let subnet = vec![192,168,1,0];
    let port_min = 8100;
    let port_max = 8199;
    let bindings = HashMap::from([
        ("subnet".to_owned(), subnet.into()),
        ("port_min".to_owned(), port_min.into()),
        ("port_max".to_owned(), port_max.into()),
    ]);
    interpreter.set_bindings(bindings);
    interpreter.add_builtin_fn("is_valid_subnet".to_owned(), validate_subnet);
    // the MAFL program is read from a file
    let source = std::fs::read_to_string(SOURCE_FILE)?;
    // the Rust side passes 3 bindings into MAFL
    // plus a custom builtin function @is_valid_subnet
    // and expects a dict in return with subnet and port members
    let value : Value = interpreter.run(&source)?.try_into()?; 
    // extracting the value into Rust variables (should make this easier with Serde) 
    // we control what is returned from MAFL (can be a single atomic value or in this case an object)
    let dict : HashMap<String,Value> = value.clone().try_into()?;
    let ip : Vec<i64> = dict.get("subnet").ok_or(anyhow!("no subnet"))?.clone().try_into()?;
    let port : i64 = dict.get("port").ok_or(anyhow!("no port"))?.clone().try_into()?;
    let name : String = dict.get("name").ok_or(anyhow!("no name"))?.clone().try_into()?;
    println!("{name}.local/{}:{port}",ip.iter().map(|i|i.to_string()).collect::<Vec<_>>().join("."));
    // we could also grab the full context from the interpreter as a HashMap<String,Value>
    // this grabs every variable that is in scope at the top level of the MAFL program (forget can clean this up)
    // let bindings = interpreter.get_bindings()?;
    // println!("{:?}", bindings);
    Ok(())
}
