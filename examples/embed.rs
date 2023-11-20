use std::collections::HashMap;

use mafl::{Interpreter, expression::Value};
use anyhow::{Result, anyhow, Ok};

use mafl::{expression::{Expr,self}, context::Context, Ptr};

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
    let mut subnet = [192,168,1,0];
    let port_min_max = [8100,8199];
    let bindings = HashMap::from([
        ("subnet".to_owned(), Value::Array(subnet.map(Value::Integer).to_vec())),
        ("port_min".to_owned(), Value::Integer(port_min_max[0])),
        ("port_max".to_owned(), Value::Integer(port_min_max[1])),
    ]);
    interpreter.set_bindings(bindings);
    interpreter.add_builtin_fn("is_valid_subnet".to_owned(), validate_subnet);
    // ideally, this should be in a text file that the user can modify
    // the Rust side passes 3 bindings into MAFL
    // plus a custom builtin function @is_valid_subnet
    // and expects a dict in return with subnet and port members
    let source = r#"
        if not(@is_valid_subnet(subnet)) {
            @error("invalid subnet");
        }
        subnet[3] = @randint(100,199);
        let port = @randint(port_min, port_max);
        forget port_min port_max;
        module {
            use subnet port;
        }
    "#;
    let value : Value = interpreter.run(source)?.try_into()?; 
    // extracting the value into Rust variables (should make this easier with Serde) 
    let dict : HashMap<String,Value> = value.try_into()?;
    let vals : Vec<Value> = dict.get("subnet").ok_or(anyhow!("no subnet"))?.clone().try_into()?;
    for (i, val) in vals.iter().cloned().enumerate() {
        subnet[i] = val.try_into()?;
    }
    let port : i64 = dict.get("port").ok_or(anyhow!("no port"))?.clone().try_into()?;
    println!("{}:{port}",subnet.map(|i|i.to_string()).join("."));
    Ok(())
}
