use std::cell::RefCell;
use std::io::{self, BufRead, stdout, Write};
use std::fmt::Write as _;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH, Duration};
use anyhow::{Result,anyhow};
use log::debug;
use regex::Regex;

use crate::context::{MemCell, Bindings};
use crate::{expression::{Expression, self, nil, closure, ExpressionType}, context::Context, Interpreter};

pub fn pow(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a.powf(*b)),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a.powf(*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64).powf(*b)),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a.pow(*b as u32)),
        _ => Err(anyhow!("pow {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn exp(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Float(a) => ExpressionType::Float(a.exp()),
        ExpressionType::Integer(a) => ExpressionType::Float((*a as f64).exp()),
        _ => Err(anyhow!("exp {val:?}"))?,
    }.into())
}

pub fn log(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Float(a) => ExpressionType::Float(a.ln()),
        ExpressionType::Integer(a) => ExpressionType::Float((*a as f64).ln()),
        _ => Err(anyhow!("log {val:?}"))?,
    }.into())
}

pub fn add(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a+b),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a + (*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64) + b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a+b),
        (ExpressionType::String(a), ExpressionType::String(b)) => ExpressionType::String(format!("{a}{b}")),
        (ExpressionType::String(a), ExpressionType::Integer(b)) => ExpressionType::String(format!("{a}{b}")),
        (ExpressionType::Integer(a), ExpressionType::String(b)) => ExpressionType::String(format!("{a}{b}")),
        (ExpressionType::String(a), ExpressionType::Float(b)) => ExpressionType::String(format!("{a}{b}")),
        (ExpressionType::Float(a), ExpressionType::String(b)) => ExpressionType::String(format!("{a}{b}")),
        (ExpressionType::Array(a), ExpressionType::Array(b)) => 
                ExpressionType::Array(RefCell::new(
                    [a.borrow().to_owned(), b.borrow().to_owned()].concat()
                )),
        _ => Err(anyhow!("add {lhs:?} {rhs:?}"))?,
        }.into())
}

pub fn sub(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a-b),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a - (*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64) - b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a-b),
        _ => Err(anyhow!("sub {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn mul(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a*b),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a * (*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64) * b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a*b),
        (ExpressionType::String(a), ExpressionType::Integer(b)) => ExpressionType::String(a.repeat(*b as usize)),
        (ExpressionType::Integer(a), ExpressionType::String(b)) => ExpressionType::String(b.repeat(*a as usize)),
        _ => Err(anyhow!("mul {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn div(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let error_div0 = Err(anyhow!("division by zero"));
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (_, ExpressionType::Float(b)) if b == &0.0 => error_div0?,
        (_, ExpressionType::Integer(0)) => error_div0?,
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a/b).into(),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a / (*b as f64)).into(),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64) / b).into(),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Float((*a as f64) / (*b as f64)).into(),
        _ => Err(anyhow!("div requires 2 integers or floats"))?,
    })
}

pub fn intdiv(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let error_div0 = Err(anyhow!("division by zero"));
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (_, ExpressionType::Integer(0)) => error_div0?,
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a/b).into(),
        _ => Err(anyhow!("intdiv requires 2 integers"))?,
    })
}

pub fn modulo(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a % b),
        _ => Err(anyhow!("mod {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn and(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Boolean(a), ExpressionType::Boolean(b)) => ExpressionType::Boolean(*a && *b),
        _ => Err(anyhow!("and {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn or(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Boolean(a), ExpressionType::Boolean(b)) => ExpressionType::Boolean(*a || *b),
        _ => Err(anyhow!("or {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn and_lazy(interpreter: &Interpreter, ctx: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let lhs = interpreter.eval(ctx,lhs)?;
    match lhs.as_ref() {
        // propagate exception
        ExpressionType::Throw(val) => {
            return Ok(ExpressionType::Throw(val.to_owned()).into());
        }
        ExpressionType::Boolean(b) => if !b { return Ok(ExpressionType::Boolean(false).into()) },
        _ => Err(anyhow!("and_lazy lhs not boolean: {lhs:?}"))?,
    };
    let rhs = interpreter.eval(ctx,rhs)?;
    match rhs.as_ref() {
        // propagate exception
        ExpressionType::Throw(val) => {
            return Ok(ExpressionType::Throw(val.to_owned()).into());
        }
        ExpressionType::Boolean(b) => if !b { return Ok(ExpressionType::Boolean(false).into()) },
        _ => Err(anyhow!("and_lazy rhs not boolean: {rhs:?}"))?,
    };
    Ok(ExpressionType::Boolean(true).into())
}

pub fn or_lazy(interpreter: &Interpreter, ctx: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let lhs = interpreter.eval(ctx,lhs)?;
    match lhs.as_ref() {
        // propagate exception
        ExpressionType::Throw(val) => {
            return Ok(ExpressionType::Throw(val.to_owned()).into());
        }
        ExpressionType::Boolean(b) => if *b { return Ok(ExpressionType::Boolean(true).into()) },
        _ => Err(anyhow!("or_lazy lhs not boolean: {lhs:?}"))?,
    };
    let rhs = interpreter.eval(ctx,rhs)?;
    match rhs.as_ref() {
        // propagate exception
        ExpressionType::Throw(val) => {
            return Ok(ExpressionType::Throw(val.to_owned()).into());
        }
        ExpressionType::Boolean(b) => if *b { return Ok(ExpressionType::Boolean(true).into()) },
        _ => Err(anyhow!("or_lazy rhs not boolean: {rhs:?}"))?,
    };
    Ok(ExpressionType::Boolean(false).into())
}

pub fn gt(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Boolean(a>b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Boolean(a>b),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Boolean((*a) > (*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Boolean((*a as f64) > (*b)),
        (ExpressionType::String(a), ExpressionType::String(b)) => ExpressionType::Boolean(a>b),
        (ExpressionType::Boolean(a), ExpressionType::Boolean(b)) => ExpressionType::Boolean(a>b),
        (ExpressionType::Nil, ExpressionType::Nil) => ExpressionType::Boolean(false),
        _ => Err(anyhow!("gt {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn lt(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Boolean(a<b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Boolean(a<b),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Boolean((*a) < (*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Boolean((*a as f64) < (*b)),
        (ExpressionType::String(a), ExpressionType::String(b)) => ExpressionType::Boolean(a<b),
        (ExpressionType::Boolean(a), ExpressionType::Boolean(b)) => ExpressionType::Boolean(a<b),
        (ExpressionType::Nil, ExpressionType::Nil) => ExpressionType::Boolean(false),
        _ => Err(anyhow!("lt {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn eq(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(ExpressionType::Boolean(lhs == rhs).into())
}

pub fn ne(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(ExpressionType::Boolean(lhs != rhs).into())
}

pub fn ge(ctx: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let eq = eq(ctx,lhs,rhs)?;
    let gt = gt(ctx,lhs,rhs)?;
    or(ctx,&eq,&gt)
}

pub fn le(ctx: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let eq = eq(ctx,lhs,rhs)?;
    let lt = lt(ctx,lhs,rhs)?;
    or(ctx,&eq,&lt)
}

pub fn neg(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Float(a) => ExpressionType::Float(-a),
        ExpressionType::Integer(a) => ExpressionType::Integer(-a),
        ExpressionType::String(a) => ExpressionType::String(a.chars().rev().collect::<String>()),
        _ => Err(anyhow!("neg {val:?}"))?,
    }.into())
}

pub fn not(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Boolean(b) => ExpressionType::Boolean(! *b),
        _ => Err(anyhow!("not {val:?}"))?,
    }.into())
}

#[allow(clippy::only_used_in_recursion)]
pub fn print(interpreter: &Interpreter, ctx: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { 
        match arg.as_ref() {
            ExpressionType::Closure(cctx, _, _) if cctx.get_binding(&interpreter.ident("__str__")).is_some() => {
                // if the closure/object has a binding for __str__, we call it
                let str = cctx.get_binding(&interpreter.ident("__str__")).unwrap();
                let string = interpreter.eval(cctx, 
                    &ExpressionType::FunctionCall(str, vec![]).into()
                )?;
                print(interpreter, ctx, &[string])?;
            }
            _ => print!("{arg}"),
        } 
    }
    stdout().flush()?;
    Ok(nil())
}

pub fn command(_: &Interpreter, _: &Context, cmd: &Expression, args: &[Expression]) -> Result<Expression> {
    if let ExpressionType::String(cmd) = cmd.as_ref() {
        let mut cmd = Command::new(cmd);
        for arg in args { 
            cmd.arg(format!("{arg}"));
        }
        Ok(expression::string(String::from_utf8_lossy(&cmd.output()?.stdout).to_string()))
    } else {
        Err(anyhow!("command must be a string"))?
    }
}

pub fn println(interpreter: &Interpreter, ctx: &Context, args: &[Expression]) -> Result<Expression> {
    print(interpreter, ctx, args)?;
    println!();
    stdout().flush()?;
    Ok(nil())
}

pub fn debug(_: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg:?}"); }
    stdout().flush()?;
    Ok(nil())
}

pub fn debugln(_: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg:#?}"); }
    println!();
    stdout().flush()?;
    Ok(nil())
}

pub fn eval_string_as_source(interpreter: &Interpreter, ctx: &Context, arg: &Expression) -> Result<Expression> {
    match arg.as_ref() {
        ExpressionType::String(s) => {
            debug!("evaluating string: {s}");
            interpreter.eval(ctx, &interpreter.parse_source(s)?)
        }
        _ => arg.to_error()
    }
}

pub fn array(interpreter: &Interpreter, ctx: &Context, size: &Expression, init: &Expression) -> Result<Expression> {
    let mut arr : Vec<Expression> = vec![];
    if let ExpressionType::Integer(size) = size.as_ref() {
        let size = *size;
        let init = &interpreter.eval(ctx,init)?;
        for i in 0..size {
            let init = match init.as_ref() {
                ExpressionType::Closure(_, _, _) => {
                    interpreter.eval(ctx, &ExpressionType::FunctionCall(init.to_owned(), vec![ExpressionType::Integer(i).into()]).into())?
                }
                _ => init.to_owned(),
            };
            arr.push(init);
        }
        Ok(expression::array(arr))
    } else {
        Err(anyhow!("array {size:?} {init:?}"))?
    }
}

pub fn to_array(ctx: &Context, init: &Expression) -> Result<Expression> {
    let mut arr = vec![];
    match init.as_ref() {
        ExpressionType::String(s) => {
            for c in s.chars() {
                arr.push(ExpressionType::Character(c).into());
            }
            Ok(expression::array(arr))
        } 
        ExpressionType::Closure(cctx, _args, _body) => {
            for (s, cell) in cctx.bindings_ref() {
                let pair = vec![expression::string(Interpreter::ident_to_string(&s)),cell.get()];
                arr.push(expression::array(pair));
            }
            Ok(expression::array(arr))
        } 
        ExpressionType::Array(_) => {
            deep_copy(ctx, init)
        } 
        _ => Ok(Err(anyhow!("to array {init:?}"))?)
    }
}

pub fn slice(_ctx: &Context, container: &Expression, start: &Expression, end: &Expression) -> Result<Expression> {
    Ok(match (container.as_ref(),start.as_ref(),end.as_ref()) {
        (ExpressionType::Array(a0),
            ExpressionType::Integer(start), 
            ExpressionType::Integer(end)) => {
                let a1 = &a0.borrow();
                let a2 = a1.as_slice();
                let arr = a2[*start as usize .. *end as usize].to_vec();
                expression::array(arr)
        }
        (ExpressionType::String(s),
            ExpressionType::Integer(start), 
            ExpressionType::Integer(end)) => {
                ExpressionType::String(s[*start as usize .. *end as usize].to_owned()).into()
        }
        _ => Err(anyhow!("slice {container:?} {start:?} {end:?}"))?,
    })
}

pub fn to_array_lines(_ctx: &Context, init: &Expression) -> Result<Expression> {
    let mut arr : Vec<Expression> = vec![];
    if let ExpressionType::String(s) = init.as_ref() {
        for line in s.lines() {
            arr.push(ExpressionType::String(line.to_owned()).into());
        }
        Ok(expression::array(arr))
    } else {
        Err(anyhow!("to array lines {init:?}"))?
    }
}

pub fn len(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Array(a) => ExpressionType::Integer(a.borrow().len() as i64),
        ExpressionType::String(a) => ExpressionType::Integer(a.len() as i64),
        _ => Err(anyhow!("len {val:?}"))?,
    }.into())
}

pub fn append(interpreter: &Interpreter, ctx: &Context, target: &Expression, new: &Expression) -> Result<Expression> {
    Ok(match target.as_ref() {
        ExpressionType::Array(a) => {
            let new = interpreter.eval(ctx,new)?;
            a.borrow_mut().push(new);
            target.to_owned()
        }
        _ => Err(anyhow!("append {target:?} {new:?}"))?,
    })
}

pub fn include(interpreter: &Interpreter, ctx: &Context, file_expr: &Expression) -> Result<Expression> {
    debug!("include {file_expr:?}");
    Ok(match file_expr.as_ref() {
        ExpressionType::String(file) => {
            let source = std::fs::read_to_string(file)?;
            eval_string_as_source(interpreter, ctx, &ExpressionType::String(source).into())?
        }
        _ => Err(anyhow!("include {file_expr:?}"))?,
    })
}

pub fn include_str(interpreter: &Interpreter, ctx: &Context, s: &str) -> Result<Expression> {
    debug!("include_str");
    interpreter.eval(ctx, &interpreter.parse_source(s)?)
}

pub fn read_file(_ctx: &Context, file_expr: &Expression) -> Result<Expression> {
    debug!("read_file {file_expr:?}");
    Ok(match file_expr.as_ref() {
        ExpressionType::String(file) => {
            let source = std::fs::read_to_string(file)?;
            ExpressionType::String(source).into()
        }
        _ => Err(anyhow!("read_file {file_expr:?}"))?,
    })
}

pub fn capture_context(ctx: &Context) -> Result<Expression> {
    Ok(expression::context(ctx.capture()))
}

pub fn is_error(_: &Interpreter, _: &Context, expr: &Expression) -> Result<Expression> {
    if let ExpressionType::Error(_) = expr.as_ref() {
        return Ok(expression::boolean(true));
    }
    Ok(expression::boolean(false))
}

pub fn is_ref(_: &Interpreter, _: &Context, expr: &Expression) -> Result<Expression> {
    if let ExpressionType::Ref(_) = expr.as_ref() {
        return Ok(expression::boolean(true));
    }
    Ok(expression::boolean(false))
}

pub fn type_of(_: &Context, expr: &Expression) -> Result<Expression> {
    debug!("type of");
    Ok(ExpressionType::String(match expr.as_ref() {
        ExpressionType::Nil => "()",
        ExpressionType::Float(_) => "Float",
        ExpressionType::Integer(_) => "Integer",
        ExpressionType::String(_) => "String",
        ExpressionType::Character(_) => "Character",
        ExpressionType::Boolean(_) => "Boolean",
        ExpressionType::Array(_) => "Array",
        ExpressionType::Error(_) => "Error",
        ExpressionType::Closure(_,_,_) => "Closure",
        ExpressionType::Ref(_) => "Ref",
        ExpressionType::Break(_) |
        ExpressionType::Continue |
        ExpressionType::Return(_) |
        ExpressionType::Throw(_) => "Exception",
        _ => Err(anyhow!("type of {expr:?}"))?,
    }.to_owned()).into())
}

pub fn read_line(_ctx: &Context) -> Result<Expression> {
    let stdin = io::stdin();
    let next_line = stdin.lock().lines().next();
    if let Some(line_result) = next_line {
        Ok(ExpressionType::String(line_result?).into())
    } else {
        Ok(nil())
    }
}

pub fn get(interpreter: &Interpreter, _ctx: &Context, container: &Expression, key: &Expression) -> Result<Expression> {
    Ok(match container.as_ref() {
        ExpressionType::Array(a) => {
            match key.as_ref() {
                ExpressionType::Integer(i) => 
                    a.borrow().get(*i as usize).ok_or(anyhow!("index {i} out of bounds"))?.to_owned(),
                _ => Err(anyhow!("get on array with non-integer key {key:?}"))?,
            }
        }
        ExpressionType::Closure(c,_,_) => {
            match key.as_ref() {
                ExpressionType::String(s) => 
                    c.get_binding(&interpreter.ident(s)).ok_or(anyhow!("binding not found: {s}"))?,
                _ => Err(anyhow!("get on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("get {key:?}"))?,
    })
}

pub fn set(interpreter: &Interpreter, _ctx: &Context, container: &Expression, key: &Expression, value: &Expression) -> Result<Expression> {
    Ok(match container.as_ref() {
        ExpressionType::Array(a) => {
            match key.as_ref() {
                ExpressionType::Integer(i) => 
                    if let Some(result) = a.borrow_mut().get_mut(*i as usize) {
                        *result = value.to_owned();
                        result.to_owned()
                    } else {
                        Err(anyhow!("index {i} out of bounds"))?
                    }
                _ => Err(anyhow!("set on array with non-integer key {key:?}"))?,
            }
        }
        ExpressionType::Closure(c,_,_) => {
            match key.as_ref() {
                ExpressionType::String(s) => 
                    c.set_binding(interpreter.ident(s), value.to_owned())
                        .ok_or(anyhow!("binding not found: {s}"))?,
                _ => Err(anyhow!("set on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("set {key:?}"))?,
    })
}

pub fn insert(interpreter: &Interpreter, _ctx: &Context, container: &Expression, key: &Expression, value: &Expression) -> Result<Expression> {
    Ok(match container.as_ref() {
        ExpressionType::Closure(c,_,_) => {
            match key.as_ref() {
                ExpressionType::String(s) => {
                    c.add_binding(interpreter.ident(s), value.to_owned());
                    value.to_owned()
                }
                _ => Err(anyhow!("insert on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("insert {key:?}"))?,
    })
}

pub fn dict(_ctx: &Context) -> Result<Expression> {
    debug!("new dict");
    Ok(expression::context(Context::new()))
}

pub fn dict_extend(_ctx: &Context, parent: &Expression) -> Result<Expression> {
    debug!("extend dict");
    Ok(match parent.as_ref() {
        ExpressionType::Closure(c,a,b) => {
            closure(c.with_new_context(),a.to_owned(),b.to_owned())
        }
        _ => Err(anyhow!("dict_extend"))?,
    })
}

pub fn error_from_strings(_: &Context, args: &[Expression]) -> Result<Expression> {
    let mut output = String::new();
    for arg in args { write!(output,"{arg}")?; }
    Err(anyhow!(output))
}

pub fn make_error(msg: &Expression) -> Result<Expression> {
    Ok(match msg.as_ref() {
        ExpressionType::String(s) => ExpressionType::Error(s.to_owned()).into(),
        _ => Err(anyhow!("make-error with non-string message"))?,
    })
}

pub fn get_var(interpreter: &Interpreter, ctx: &Context, key: &Expression) -> Result<Expression> {
    debug!("get_var {key:?}");
    Ok(match key.as_ref() {
        ExpressionType::String(s) => 
            ctx.get_binding(&interpreter.ident(s)).ok_or(anyhow!("binding not found: {s}"))?,
        _ => Err(anyhow!("get on closure with non-string key {key:?}"))?,
    })
}

pub fn assign_var(interpreter: &Interpreter, ctx: &Context, key: &Expression, value: &Expression) -> Result<Expression> {
    debug!("set_var {key:?} {value:?}");
    Ok(match key.as_ref() {
        ExpressionType::String(s) => 
            ctx.set_binding(interpreter.ident(s), value.to_owned())
                .ok_or(anyhow!("binding not found: {s}"))?,
        _ => Err(anyhow!("set on closure with non-string key {key:?}"))?,
    })
}

pub fn let_var(interpreter: &Interpreter, ctx: &Context, key: &Expression, value: &Expression) -> Result<Expression> {
    debug!("insert_var {key:?} {value:?}");
    Ok(match key.as_ref() {
        ExpressionType::String(s) => {
            ctx.add_binding(interpreter.ident(s), value.to_owned());
            value.to_owned()
        }
        _ => Err(anyhow!("insert on closure with non-string key {key:?}"))?,
    })
}

#[allow(clippy::only_used_in_recursion)]
pub fn shallow_copy(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Array(a) => {
            let ac = a.borrow().iter().cloned().collect::<Vec<Expression>>();
            expression::array(ac)
        }
        ExpressionType::Closure(cctx,args,body) => {
            let copy = cctx.bindings_cloned();
            let new_ctx = cctx.with_bindings(copy);
            expression::closure(new_ctx.to_owned(), args.to_owned(), body.to_owned())
        }
        _ => val.to_owned(),
    })
}

#[allow(clippy::only_used_in_recursion)]
pub fn deep_copy(ctx: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Array(a) => {
            let ac = a.borrow().iter()
                    .map(|e| { deep_copy(ctx,e) })
                    .collect::<Result<Vec<Expression>>>()?;
            expression::array(ac)
        }
        ExpressionType::Closure(cctx,args,body) => {
            let copyrec = cctx.bindings_cloned().into_iter().map(|(k,v)| {
                (k, MemCell::new_ref(deep_copy(ctx, &v.get()).unwrap()))
            }).collect::<Bindings>();
            let new_ctx = cctx.with_bindings(copyrec);
            expression::closure(new_ctx.to_owned(), args.to_owned(), body.to_owned())
        }
        _ => val.to_owned(),
    })
}

pub fn ref_var(ctx: &Context, var: &Expression) -> Result<Expression> {
    Ok(match var.as_ref() {
        ExpressionType::Variable(s) => {
            if let Some(rc) = ctx.get_binding_ref(s) {
                ExpressionType::Ref(rc).into()
            } else {
                Err(anyhow!("binding not found: {s}"))?
            }
        }
        _ => Err(anyhow!("ref on non-variable {var}"))?,
    })
}

pub fn test(interpreter: &Interpreter, ctx: &Context, source: &Expression, expected: &Expression) -> Result<Expression> {
    let result = eval_string_as_source(interpreter, ctx, source)?;
    if &result == expected {
        println!("# test success: {source} == {expected}");
    } else {
        println!("# test failure: {source} == {result} != {expected}");
    }
    stdout().flush()?;
    Ok(nil())
}

pub fn now(_: &Context) -> Result<Expression> {
    debug!("now");
    let now = SystemTime::now().duration_since(UNIX_EPOCH)?;
    Ok(expression::float(now.as_secs_f64()))
}

pub fn sleep(_: &Context, seconds: &Expression) -> Result<Expression> {
    debug!("sleep {seconds}");
    match seconds.as_ref() {
        ExpressionType::Float(secs) if secs > &0.0 => std::thread::sleep(Duration::from_secs_f64(*secs)),
        ExpressionType::Integer(secs) if secs > &0 => std::thread::sleep(Duration::from_secs(*secs as u64)),
        _ => Err(anyhow!("sleep argument must be non-negative number {seconds}"))?,
    };
    Ok(seconds.to_owned())
}

pub fn call(interpreter: &Interpreter, ctx: &Context, callable: &Expression, args: &Expression) -> Result<Expression> {
    match args.as_ref() {
        ExpressionType::Array(args_vec) => {
            let args_copy = args_vec.clone().into_inner();
            interpreter.eval(ctx, &ExpressionType::FunctionCall(callable.to_owned(), args_copy).into())
        }
        _ => Err(anyhow!("call {callable:?} {args:?}"))?
    }
}

pub fn split(_: &Interpreter, _: &Context, string: &Expression, pattern: &Expression) -> Result<Expression> {
    Ok(match (string.as_ref(),pattern.as_ref()) {
        (ExpressionType::String(s),ExpressionType::String(reg)) => {
            let re = Regex::new(reg)?;
            let parts = re.split(s)
                .map(|p| expression::string(p.to_owned())).collect();
            // let parts = s.split(pat).map(|p| expression::string(p.to_owned())).collect();
            expression::array(parts)
        }
        _ => Err(anyhow!("split with non-string arguments"))?,
    })
}

pub fn trim(_: &Interpreter, _: &Context, string: &Expression) -> Result<Expression> {
    Ok(match string.as_ref() {
        ExpressionType::String(s) => {
            expression::string(s.trim().to_owned())
        }
        _ => Err(anyhow!("trim on non-string argument"))?,
    })
}

pub fn matches(_: &Interpreter, _: &Context, string: &Expression, regex: &Expression) -> Result<Expression> {
    Ok(match (string.as_ref(),regex.as_ref()) {
        (ExpressionType::String(s),ExpressionType::String(reg)) => {
            let re = Regex::new(reg)?;
            let parts = re.find_iter(s)
                .map(|p| expression::string(p.as_str().to_owned())).collect();
            expression::array(parts)
        }
        _ => Err(anyhow!("match with non-string arguments"))?,
    })
}

pub fn sort(interpreter: &Interpreter, ctx: &Context, target: &Expression, compare: Option<&Expression>) -> Result<Expression> {
    Ok(match target.as_ref() {
        ExpressionType::Array(a) => {
            a.borrow_mut().sort_by(|lhs,rhs| {
                let res = if let Some(compare) = compare {
                    interpreter.eval(ctx, 
                        &ExpressionType::FunctionCall(compare.to_owned(), vec![lhs.to_owned(), rhs.to_owned()]).into())
                } else {
                    lt(ctx, lhs, rhs)
                };
                match res.as_deref() {
                    Ok(ExpressionType::Boolean(true)) => std::cmp::Ordering::Less,
                    Ok(ExpressionType::Boolean(false)) => std::cmp::Ordering::Greater,
                    _ => std::cmp::Ordering::Equal,
                }
            });
            expression::nil()
        }
        _ => Err(anyhow!("sort on non-array"))?,
    })
}

pub fn integer(interpreter: &Interpreter, ctx: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Float(a) => ExpressionType::Integer(*a as i64).into(),
        ExpressionType::Integer(a) => ExpressionType::Integer(*a).into(),
        ExpressionType::String(_) => {
            let r = eval_string_as_source(interpreter, ctx, val)?;
            if let ExpressionType::Integer(_) = r.as_ref() {
                r
            } else if let ExpressionType::Float(f) = r.as_ref() {
                expression::integer(*f as i64)
            } else {
                Err(anyhow!("can't parse as an integer"))?
            }
        }
        _ => Err(anyhow!("integer {val:?}"))?,
    })
}

pub fn float(interpreter: &Interpreter, ctx: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Float(a) => ExpressionType::Float(*a).into(),
        ExpressionType::Integer(a) => ExpressionType::Float(*a as f64).into(),
        ExpressionType::String(_) => {
            let r = eval_string_as_source(interpreter, ctx, val)?;
            if let ExpressionType::Float(_) = r.as_ref() {
                r
            } else if let ExpressionType::Integer(i) = r.as_ref() {
                expression::float(*i as f64)
            } else {
                Err(anyhow!("can't parse as a float"))?
            }
        }
        _ => Err(anyhow!("float {val:?}"))?,
    })
}

pub fn string(_: &Interpreter, _: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Error(e) => expression::string(e.to_owned()),
        ExpressionType::Array(arr) => {
            let all_chars = arr.borrow().iter().all(|e| {
                matches!(e.as_ref(), ExpressionType::Character(_))
            });
            if all_chars { 
                expression::string(arr.borrow().iter().filter_map(|e| {
                    if let ExpressionType::Character(c) = e.as_ref() { Some(*c) } 
                    else { None }
                }).collect())
            } else {
                expression::string(format!("{val}"))   
            }
        }
        _ => expression::string(format!("{val}")),
    })
}

