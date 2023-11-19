use std::io::{self, BufRead, stdout, Write};
use std::fmt::Write as _;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH, Duration};
use anyhow::{Result,anyhow};
use log::debug;
use regex::Regex;
use rand::Rng;

use crate::{RefC, R, __STR__, PragmaLevel, expression};
use crate::context::{MemCell, Bindings};
use crate::{expression::{nil, closure, Expr}, context::Context, Interpreter};

pub fn pow(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Float(a), Expr::Float(b)) => Expr::Float(a.powf(*b)),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Float(a.powf(*b as f64)),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Float((*a as f64).powf(*b)),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Integer(a.pow(*b as u32)),
        _ => Err(anyhow!("pow {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn exp(_: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Float(a) => Expr::Float(a.exp()),
        Expr::Integer(a) => Expr::Float((*a as f64).exp()),
        _ => Err(anyhow!("exp {val:?}"))?,
    }.into())
}

pub fn log(_: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Float(a) => Expr::Float(a.ln()),
        Expr::Integer(a) => Expr::Float((*a as f64).ln()),
        _ => Err(anyhow!("log {val:?}"))?,
    }.into())
}

pub fn add(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Float(a), Expr::Float(b)) => Expr::Float(a+b),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Float(a + (*b as f64)),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Float((*a as f64) + b),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Integer(a+b),
        (Expr::String(a), Expr::String(b)) => Expr::String(format!("{a}{b}")),
        (Expr::String(a), Expr::Integer(b)) => Expr::String(format!("{a}{b}")),
        (Expr::Integer(a), Expr::String(b)) => Expr::String(format!("{a}{b}")),
        (Expr::String(a), Expr::Float(b)) => Expr::String(format!("{a}{b}")),
        (Expr::Float(a), Expr::String(b)) => Expr::String(format!("{a}{b}")),
        (Expr::Array(a), Expr::Array(b)) => 
                Expr::Array(RefC::new(
                    [a.borrow().to_owned(), b.borrow().to_owned()].concat()
                )),
        _ => Err(anyhow!("add {lhs:?} {rhs:?}"))?,
        }.into())
}

pub fn sub(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Float(a), Expr::Float(b)) => Expr::Float(a-b),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Float(a - (*b as f64)),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Float((*a as f64) - b),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Integer(a-b),
        _ => Err(anyhow!("sub {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn mul(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Float(a), Expr::Float(b)) => Expr::Float(a*b),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Float(a * (*b as f64)),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Float((*a as f64) * b),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Integer(a*b),
        (Expr::String(a), Expr::Integer(b)) => Expr::String(a.repeat(*b as usize)),
        (Expr::Integer(a), Expr::String(b)) => Expr::String(b.repeat(*a as usize)),
        _ => Err(anyhow!("mul {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn div(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    let error_div0 = Err(anyhow!("division by zero"));
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (_, Expr::Float(b)) if b == &0.0 => error_div0?,
        (_, Expr::Integer(0)) => error_div0?,
        (Expr::Float(a), Expr::Float(b)) => Expr::Float(a/b).into(),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Float(a / (*b as f64)).into(),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Float((*a as f64) / b).into(),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Float((*a as f64) / (*b as f64)).into(),
        _ => Err(anyhow!("div requires 2 integers or floats"))?,
    })
}

pub fn intdiv(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    let error_div0 = Err(anyhow!("division by zero"));
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (_, Expr::Integer(0)) => error_div0?,
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Integer(a/b).into(),
        _ => Err(anyhow!("intdiv requires 2 integers"))?,
    })
}

pub fn modulo(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Integer(a % b),
        _ => Err(anyhow!("mod {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn and(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Boolean(a), Expr::Boolean(b)) => Expr::Boolean(*a && *b),
        _ => Err(anyhow!("and {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn or(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Boolean(a), Expr::Boolean(b)) => Expr::Boolean(*a || *b),
        _ => Err(anyhow!("or {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn and_lazy(interpreter: &Interpreter, ctx: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    let lhs = interpreter.eval(ctx,lhs)?;
    match lhs.as_ref() {
        // propagate exception
        Expr::Throw(val) => {
            return Ok(Expr::Throw(val.to_owned()).into());
        }
        Expr::Boolean(b) => if !b { return Ok(Expr::Boolean(false).into()) },
        _ => Err(anyhow!("and_lazy lhs not boolean: {lhs:?}"))?,
    };
    let rhs = interpreter.eval(ctx,rhs)?;
    match rhs.as_ref() {
        // propagate exception
        Expr::Throw(val) => {
            return Ok(Expr::Throw(val.to_owned()).into());
        }
        Expr::Boolean(b) => if !b { return Ok(Expr::Boolean(false).into()) },
        _ => Err(anyhow!("and_lazy rhs not boolean: {rhs:?}"))?,
    };
    Ok(Expr::Boolean(true).into())
}

pub fn or_lazy(interpreter: &Interpreter, ctx: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    let lhs = interpreter.eval(ctx,lhs)?;
    match lhs.as_ref() {
        // propagate exception
        Expr::Throw(val) => {
            return Ok(Expr::Throw(val.to_owned()).into());
        }
        Expr::Boolean(b) => if *b { return Ok(Expr::Boolean(true).into()) },
        _ => Err(anyhow!("or_lazy lhs not boolean: {lhs:?}"))?,
    };
    let rhs = interpreter.eval(ctx,rhs)?;
    match rhs.as_ref() {
        // propagate exception
        Expr::Throw(val) => {
            return Ok(Expr::Throw(val.to_owned()).into());
        }
        Expr::Boolean(b) => if *b { return Ok(Expr::Boolean(true).into()) },
        _ => Err(anyhow!("or_lazy rhs not boolean: {rhs:?}"))?,
    };
    Ok(Expr::Boolean(false).into())
}

pub fn gt(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Float(a), Expr::Float(b)) => Expr::Boolean(a>b),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Boolean(a>b),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Boolean((*a) > (*b as f64)),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Boolean((*a as f64) > (*b)),
        (Expr::String(a), Expr::String(b)) => Expr::Boolean(a>b),
        (Expr::Boolean(a), Expr::Boolean(b)) => Expr::Boolean(a>b),
        (Expr::Nil, Expr::Nil) => Expr::Boolean(false),
        _ => Err(anyhow!("gt {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn lt(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (Expr::Float(a), Expr::Float(b)) => Expr::Boolean(a<b),
        (Expr::Integer(a), Expr::Integer(b)) => Expr::Boolean(a<b),
        (Expr::Float(a), Expr::Integer(b)) => Expr::Boolean((*a) < (*b as f64)),
        (Expr::Integer(a), Expr::Float(b)) => Expr::Boolean((*a as f64) < (*b)),
        (Expr::String(a), Expr::String(b)) => Expr::Boolean(a<b),
        (Expr::Boolean(a), Expr::Boolean(b)) => Expr::Boolean(a<b),
        (Expr::Nil, Expr::Nil) => Expr::Boolean(false),
        _ => Err(anyhow!("lt {lhs:?} {rhs:?}"))?,
    }.into())
}

pub fn eq(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(Expr::Boolean(lhs == rhs).into())
}

pub fn ne(_: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    Ok(Expr::Boolean(lhs != rhs).into())
}

pub fn ge(ctx: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    let eq = eq(ctx,lhs,rhs)?;
    let gt = gt(ctx,lhs,rhs)?;
    or(ctx,&eq,&gt)
}

pub fn le(ctx: &Context, lhs: &R<Expr>, rhs: &R<Expr>) -> Result<R<Expr>> {
    let eq = eq(ctx,lhs,rhs)?;
    let lt = lt(ctx,lhs,rhs)?;
    or(ctx,&eq,&lt)
}

pub fn neg(_: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Float(a) => Expr::Float(-a),
        Expr::Integer(a) => Expr::Integer(-a),
        Expr::String(a) => Expr::String(a.chars().rev().collect::<String>()),
        _ => Err(anyhow!("neg {val:?}"))?,
    }.into())
}

pub fn not(_: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Boolean(b) => Expr::Boolean(! *b),
        _ => Err(anyhow!("not {val:?}"))?,
    }.into())
}

#[allow(clippy::only_used_in_recursion)]
pub fn print(interpreter: &Interpreter, ctx: &Context, args: &[R<Expr>]) -> Result<R<Expr>> {
    for arg in args { 
        match arg.as_ref() {
            Expr::Closure(cctx, _, _) if cctx.get_binding(__STR__).is_some() => {
                // if the closure/object has a binding for __STR__, we call it
                let str = cctx.get_binding(__STR__).unwrap();
                let string = interpreter.eval(cctx, 
                    &Expr::FunctionCall(str, vec![]).into()
                )?;
                print(interpreter, ctx, &[string])?;
            }
            _ => print!("{arg}"),
        } 
    }
    stdout().flush()?;
    Ok(nil())
}

pub fn command(_: &Interpreter, _: &Context, cmd: &R<Expr>, args: &[R<Expr>]) -> Result<R<Expr>> {
    if let Expr::String(cmd) = cmd.as_ref() {
        let mut cmd = Command::new(cmd);
        for arg in args { 
            cmd.arg(format!("{arg}"));
        }
        Ok(expression::string(String::from_utf8_lossy(&cmd.output()?.stdout).to_string()))
    } else {
        Err(anyhow!("command must be a string"))?
    }
}

pub fn println(interpreter: &Interpreter, ctx: &Context, args: &[R<Expr>]) -> Result<R<Expr>> {
    print(interpreter, ctx, args)?;
    println!();
    stdout().flush()?;
    Ok(nil())
}

pub fn debug(_: &Context, args: &[R<Expr>]) -> Result<R<Expr>> {
    for arg in args { print!("{arg:?}"); }
    stdout().flush()?;
    Ok(nil())
}

pub fn debugln(_: &Context, args: &[R<Expr>]) -> Result<R<Expr>> {
    for arg in args { print!("{arg:#?}"); }
    println!();
    stdout().flush()?;
    Ok(nil())
}

pub fn eval_string_as_source(interpreter: &Interpreter, ctx: &Context, arg: &R<Expr>) -> Result<R<Expr>> {
    match arg.as_ref() {
        Expr::String(s) => {
            debug!("evaluating string: {s}");
            interpreter.eval(ctx, &interpreter.parse_source(s)?)
        }
        _ => arg.to_error()
    }
}

pub fn make_array(interpreter: &Interpreter, ctx: &Context, size: &R<Expr>, init: &R<Expr>) -> Result<R<Expr>> {
    let mut arr : Vec<R<Expr>> = vec![];
    if let Expr::Integer(size) = size.as_ref() {
        let size = *size;
        let init = &interpreter.eval(ctx,init)?;
        for i in 0..size {
            let init = match init.as_ref() {
                Expr::Closure(_, _, _) => {
                    interpreter.eval(ctx, &Expr::FunctionCall(init.to_owned(), vec![Expr::Integer(i).into()]).into())?
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

pub fn to_array(_interpreter: &Interpreter, ctx: &Context, init: &R<Expr>) -> Result<R<Expr>> {
    let mut arr = vec![];
    match init.as_ref() {
        Expr::String(s) => {
            for c in s.chars() {
                arr.push(Expr::Character(c).into());
            }
            Ok(expression::array(arr))
        } 
        Expr::Closure(cctx, _args, _body) => {
            for (s, cell) in cctx.bindings_ref() {
                let pair = vec![expression::string(s),cell.get()];
                arr.push(expression::array(pair));
            }
            Ok(expression::array(arr))
        } 
        Expr::Array(_) => {
            deep_copy(ctx, init)
        } 
        _ => Ok(Err(anyhow!("to array {init:?}"))?)
    }
}

pub fn slice(_ctx: &Context, container: &R<Expr>, start: &R<Expr>, end: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (container.as_ref(),start.as_ref(),end.as_ref()) {
        (Expr::Array(a0),
            Expr::Integer(start), 
            Expr::Integer(end)) => {
                let a1 = &a0.borrow();
                let a2 = a1.as_slice();
                let arr = a2[*start as usize .. *end as usize].to_vec();
                expression::array(arr)
        }
        (Expr::String(s),
            Expr::Integer(start), 
            Expr::Integer(end)) => {
                Expr::String(s[*start as usize .. *end as usize].to_owned()).into()
        }
        _ => Err(anyhow!("slice {container:?} {start:?} {end:?}"))?,
    })
}

pub fn to_array_lines(_ctx: &Context, init: &R<Expr>) -> Result<R<Expr>> {
    let mut arr : Vec<R<Expr>> = vec![];
    if let Expr::String(s) = init.as_ref() {
        for line in s.lines() {
            arr.push(Expr::String(line.to_owned()).into());
        }
        Ok(expression::array(arr))
    } else {
        Err(anyhow!("to array lines {init:?}"))?
    }
}

pub fn len(_: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Array(a) => Expr::Integer(a.borrow().len() as i64),
        Expr::String(a) => Expr::Integer(a.len() as i64),
        _ => Err(anyhow!("len {val:?}"))?,
    }.into())
}

pub fn append(interpreter: &Interpreter, ctx: &Context, target: &R<Expr>, new: &R<Expr>) -> Result<R<Expr>> {
    Ok(match target.as_ref() {
        Expr::Array(a) => {
            let new = interpreter.eval(ctx,new)?;
            a.borrow_mut().push(new);
            target.to_owned()
        }
        _ => Err(anyhow!("append {target:?} {new:?}"))?,
    })
}

pub fn include(interpreter: &Interpreter, ctx: &Context, file_expr: &R<Expr>) -> Result<R<Expr>> {
    debug!("include {file_expr:?}");
    Ok(match file_expr.as_ref() {
        Expr::String(file) => {
            let source = std::fs::read_to_string(file)?;
            eval_string_as_source(interpreter, ctx, &Expr::String(source).into())?
        }
        _ => Err(anyhow!("include {file_expr:?}"))?,
    })
}

pub fn include_str(interpreter: &Interpreter, ctx: &Context, s: &str) -> Result<R<Expr>> {
    debug!("include_str");
    let ast = interpreter.parse_source(s)?;
    interpreter.eval(ctx, &ast)
}

pub fn read_file(_ctx: &Context, file_expr: &R<Expr>) -> Result<R<Expr>> {
    debug!("read_file {file_expr:?}");
    Ok(match file_expr.as_ref() {
        Expr::String(file) => {
            let source = std::fs::read_to_string(file)?;
            Expr::String(source).into()
        }
        _ => Err(anyhow!("read_file {file_expr:?}"))?,
    })
}

pub fn is_error(_: &Interpreter, _: &Context, expr: &R<Expr>) -> Result<R<Expr>> {
    if let Expr::Error(_) = expr.as_ref() {
        return Ok(expression::boolean(true));
    }
    Ok(expression::boolean(false))
}

pub fn is_ref(_: &Interpreter, _: &Context, expr: &R<Expr>) -> Result<R<Expr>> {
    if let Expr::Ref(_) = expr.as_ref() {
        return Ok(expression::boolean(true));
    }
    Ok(expression::boolean(false))
}

pub fn type_of(_: &Context, expr: &R<Expr>) -> Result<R<Expr>> {
    debug!("type of");
    Ok(Expr::String(match expr.as_ref() {
        Expr::Nil => "()",
        Expr::Float(_) => "Float",
        Expr::Integer(_) => "Integer",
        Expr::String(_) => "String",
        Expr::Character(_) => "Character",
        Expr::Boolean(_) => "Boolean",
        Expr::Array(_) => "Array",
        Expr::Error(_) => "Error",
        Expr::Closure(_,_,_) => "Closure",
        Expr::Ref(_) => "Ref",
        Expr::Break(_) |
        Expr::Continue |
        Expr::Return(_) |
        Expr::Throw(_) => "Exception",
        _ => Err(anyhow!("type of {expr:?}"))?,
    }.to_owned()).into())
}

pub fn read_line(_ctx: &Context) -> Result<R<Expr>> {
    let stdin = io::stdin();
    let next_line = stdin.lock().lines().next();
    if let Some(line_result) = next_line {
        Ok(Expr::String(line_result?).into())
    } else {
        Ok(nil())
    }
}

pub fn get(_interpreter: &Interpreter, _ctx: &Context, container: &R<Expr>, key: &R<Expr>) -> Result<R<Expr>> {
    Ok(match container.as_ref() {
        Expr::Array(a) => {
            match key.as_ref() {
                Expr::Integer(i) => 
                    a.borrow().get(*i as usize).ok_or(anyhow!("index {i} out of bounds"))?.to_owned(),
                _ => Err(anyhow!("get on array with non-integer key {key:?}"))?,
            }
        }
        Expr::Closure(c,_,_) => {
            match key.as_ref() {
                Expr::String(s) => 
                    c.get_binding(s).ok_or(anyhow!("binding not found: {s}"))?,
                _ => Err(anyhow!("get on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("get {key:?}"))?,
    })
}

pub fn set(_interpreter: &Interpreter, _ctx: &Context, container: &R<Expr>, key: &R<Expr>, value: &R<Expr>) -> Result<R<Expr>> {
    Ok(match container.as_ref() {
        Expr::Array(a) => {
            match key.as_ref() {
                Expr::Integer(i) => 
                    if let Some(result) = a.borrow_mut().get_mut(*i as usize) {
                        *result = value.to_owned();
                        result.to_owned()
                    } else {
                        Err(anyhow!("index {i} out of bounds"))?
                    }
                _ => Err(anyhow!("set on array with non-integer key {key:?}"))?,
            }
        }
        Expr::Closure(c,_,_) => {
            match key.as_ref() {
                Expr::String(s) => 
                    c.set_binding(s.to_owned(), value.to_owned())
                        .ok_or(anyhow!("binding not found: {s}"))?,
                _ => Err(anyhow!("set on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("set {key:?}"))?,
    })
}

pub fn insert(_interpreter: &Interpreter, _ctx: &Context, container: &R<Expr>, key: &R<Expr>, value: &R<Expr>) -> Result<R<Expr>> {
    Ok(match container.as_ref() {
        Expr::Closure(c,_,_) => {
            match key.as_ref() {
                Expr::String(s) => {
                    c.add_binding(s.to_owned(), value.to_owned());
                    value.to_owned()
                }
                _ => Err(anyhow!("insert on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("insert {key:?}"))?,
    })
}

pub fn remove(_interpreter: &Interpreter, _ctx: &Context, container: &R<Expr>, key: &R<Expr>) -> Result<R<Expr>> {
    Ok(match container.as_ref() {
        Expr::Closure(c,_,_) => {
            match key.as_ref() {
                Expr::String(s) => {
                    c.remove_binding(s)
                        .ok_or(anyhow!("binding not found: {s}"))?
                }
                _ => Err(anyhow!("remove on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("remove {key:?}"))?,
    })
}

pub fn make_dict(_ctx: &Context) -> Result<R<Expr>> {
    debug!("make dict");
    Ok(expression::context(Context::new()))
}

pub fn extend_dict(_ctx: &Context, parent: &R<Expr>) -> Result<R<Expr>> {
    debug!("extend dict");
    Ok(match parent.as_ref() {
        Expr::Closure(c,a,b) => {
            closure(c.with_new_context(),a.to_owned(),b.to_owned())
        }
        _ => Err(anyhow!("extend_dict"))?,
    })
}

pub fn flatten_dict(_ctx: &Context, dict: &R<Expr>) -> Result<R<Expr>> {
    debug!("flatten dict");
    Ok(match dict.as_ref() {
        Expr::Closure(c,a,b) => {
            closure(c.flatten_ref(),a.to_owned(),b.to_owned())
        }
        _ => Err(anyhow!("flatten_dict"))?,
    })
}

pub fn error_from_strings(_: &Context, args: &[R<Expr>]) -> Result<R<Expr>> {
    let mut output = String::new();
    for arg in args { write!(output,"{arg}")?; }
    Err(anyhow!(output))
}

pub fn make_error(msg: &R<Expr>) -> Result<R<Expr>> {
    Ok(match msg.as_ref() {
        Expr::String(s) => Expr::Error(s.to_owned()).into(),
        _ => Err(anyhow!("make-error with non-string message"))?,
    })
}

#[allow(clippy::only_used_in_recursion)]
pub fn shallow_copy(_: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Array(a) => {
            let ac = a.borrow().iter().cloned().collect::<Vec<R<Expr>>>();
            expression::array(ac)
        }
        Expr::Closure(cctx,args,body) => {
            let copy = cctx.bindings_cloned();
            let new_ctx = Context::from_bindings(copy);
            expression::closure(new_ctx.to_owned(), args.to_owned(), body.to_owned())
        }
        _ => val.to_owned(),
    })
}

#[allow(clippy::only_used_in_recursion)]
pub fn deep_copy(ctx: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Array(a) => {
            let ac = a.borrow().iter()
                    .map(|e| { deep_copy(ctx,e) })
                    .collect::<Result<Vec<R<Expr>>>>()?;
            expression::array(ac)
        }
        Expr::Closure(cctx,args,body) => {
            let copyrec = cctx.bindings_cloned().into_iter().map(|(k,v)| {
                (k, MemCell::new_ref(deep_copy(ctx, &v.get()).unwrap()))
            }).collect::<Bindings>();
            let new_ctx = Context::from_bindings(copyrec);
            expression::closure(new_ctx.to_owned(), args.to_owned(), body.to_owned())
        }
        _ => val.to_owned(),
    })
}

pub fn get_ref(interpreter: &Interpreter, ctx: &Context, ref_target: &R<Expr>) -> Result<R<Expr>> {
    Ok(match ref_target.as_ref() {
        Expr::Variable(s) => {
            if let Some(rc) = ctx.get_binding_ref(s) {
                Expr::Ref(rc).into()
            } else {
                Err(anyhow!("binding not found: {s}"))?
            }
        }
        Expr::Field(field_target, field) => {
            let field_target = interpreter.eval(ctx, field_target)?;
            match field_target.as_ref() {
                Expr::Closure(closure_ctx, _arg_names, _body) => {
                    if let Some(rc) = closure_ctx.get_binding_ref(field) {
                        Expr::Ref(rc).into()
                    } else {
                        Err(anyhow!("field binding not found: {field}"))?
                    }
                }
                _ => Err(anyhow!("field lookup on non-object/dict: {field}"))?,
            }
        }
        _ => Err(anyhow!("taking ref of incompatible value"))?,
    })
}

pub fn now(_: &Context) -> Result<R<Expr>> {
    debug!("now");
    let now = SystemTime::now().duration_since(UNIX_EPOCH)?;
    Ok(expression::float(now.as_secs_f64()))
}

pub fn sleep(_: &Context, seconds: &R<Expr>) -> Result<R<Expr>> {
    debug!("sleep {seconds}");
    match seconds.as_ref() {
        Expr::Float(secs) if secs > &0.0 => std::thread::sleep(Duration::from_secs_f64(*secs)),
        Expr::Integer(secs) if secs > &0 => std::thread::sleep(Duration::from_secs(*secs as u64)),
        _ => Err(anyhow!("sleep argument must be non-negative number {seconds}"))?,
    };
    Ok(seconds.to_owned())
}

pub fn call(interpreter: &Interpreter, ctx: &Context, callable: &R<Expr>, args: &R<Expr>) -> Result<R<Expr>> {
    match args.as_ref() {
        Expr::Array(args_vec) => {
            let args_copy = args_vec.clone().into_inner();
            interpreter.eval(ctx, &Expr::FunctionCall(callable.to_owned(), args_copy).into())
        }
        _ => Err(anyhow!("call {callable:?} {args:?}"))?
    }
}

pub fn split(_: &Interpreter, _: &Context, string: &R<Expr>, pattern: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (string.as_ref(),pattern.as_ref()) {
        (Expr::String(s),Expr::String(reg)) => {
            let re = Regex::new(reg)?;
            let parts = re.split(s)
                .map(|p| expression::string(p.to_owned())).collect();
            // let parts = s.split(pat).map(|p| expression::string(p.to_owned())).collect();
            expression::array(parts)
        }
        _ => Err(anyhow!("split with non-string arguments"))?,
    })
}

pub fn trim(_: &Interpreter, _: &Context, string: &R<Expr>) -> Result<R<Expr>> {
    Ok(match string.as_ref() {
        Expr::String(s) => {
            expression::string(s.trim().to_owned())
        }
        _ => Err(anyhow!("trim on non-string argument"))?,
    })
}

pub fn matches(_: &Interpreter, _: &Context, string: &R<Expr>, regex: &R<Expr>) -> Result<R<Expr>> {
    Ok(match (string.as_ref(),regex.as_ref()) {
        (Expr::String(s),Expr::String(reg)) => {
            let re = Regex::new(reg)?;
            let parts = re.find_iter(s)
                .map(|p| expression::string(p.as_str().to_owned())).collect();
            expression::array(parts)
        }
        _ => Err(anyhow!("match with non-string arguments"))?,
    })
}

pub fn sort(interpreter: &Interpreter, ctx: &Context, target: &R<Expr>, compare: Option<&R<Expr>>) -> Result<R<Expr>> {
    Ok(match target.as_ref() {
        Expr::Array(a) => {
            a.borrow_mut().sort_by(|lhs,rhs| {
                let res = if let Some(compare) = compare {
                    interpreter.eval(ctx, 
                        &Expr::FunctionCall(compare.to_owned(), vec![lhs.to_owned(), rhs.to_owned()]).into())
                } else {
                    lt(ctx, lhs, rhs)
                };
                match res.as_deref() {
                    Ok(Expr::Boolean(true)) => std::cmp::Ordering::Less,
                    Ok(Expr::Boolean(false)) => std::cmp::Ordering::Greater,
                    _ => std::cmp::Ordering::Equal,
                }
            });
            expression::nil()
        }
        _ => Err(anyhow!("sort on non-array"))?,
    })
}

pub fn integer(interpreter: &Interpreter, ctx: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Float(a) => Expr::Integer(*a as i64).into(),
        Expr::Integer(a) => Expr::Integer(*a).into(),
        Expr::String(_) => {
            let r = eval_string_as_source(interpreter, ctx, val)?;
            if let Expr::Integer(_) = r.as_ref() {
                r
            } else if let Expr::Float(f) = r.as_ref() {
                expression::integer(*f as i64)
            } else {
                Err(anyhow!("can't parse as an integer"))?
            }
        }
        _ => Err(anyhow!("integer {val:?}"))?,
    })
}

pub fn float(interpreter: &Interpreter, ctx: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Float(a) => Expr::Float(*a).into(),
        Expr::Integer(a) => Expr::Float(*a as f64).into(),
        Expr::String(_) => {
            let r = eval_string_as_source(interpreter, ctx, val)?;
            if let Expr::Float(_) = r.as_ref() {
                r
            } else if let Expr::Integer(i) = r.as_ref() {
                expression::float(*i as f64)
            } else {
                Err(anyhow!("can't parse as a float"))?
            }
        }
        _ => Err(anyhow!("float {val:?}"))?,
    })
}

pub fn string(interpreter: &Interpreter, _: &Context, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match val.as_ref() {
        Expr::Error(e) => expression::string(e.to_owned()),
        Expr::Array(arr) => {
            let all_chars = arr.borrow().iter().all(|e| {
                matches!(e.as_ref(), Expr::Character(_))
            });
            if all_chars { 
                expression::string(arr.borrow().iter().filter_map(|e| {
                    if let Expr::Character(c) = e.as_ref() { Some(*c) } 
                    else { None }
                }).collect())
            } else {
                expression::string(format!("{val}"))   
            }
        }
        Expr::Closure(cctx, _, _) if cctx.get_binding(__STR__).is_some() => {
            // if the closure/object has a binding for __STR__, we call it
            let str = cctx.get_binding(__STR__).unwrap();
            interpreter.eval(cctx, 
                &Expr::FunctionCall(str, vec![]).into()
            )?
        }
        _ => expression::string(format!("{val}")),
    })
}

pub fn randint(_: &Interpreter, _: &Context, min: &R<Expr>, max: &R<Expr>) -> Result<R<Expr>> {
    let mut rng = rand::thread_rng();
    Ok(match (min.as_ref(),max.as_ref()) {
        (Expr::Integer(min), Expr::Integer(max)) => 
            expression::integer(rng.gen_range(*min..=*max)),
        _ => Err(anyhow!("randint requires 2 integers"))?
    })
}

pub fn randfloat(_: &Interpreter, _: &Context) -> Result<R<Expr>> {
    let mut rng = rand::thread_rng();
    Ok(expression::float(rng.gen()))
}

pub fn pragma(interpreter: &Interpreter, _ctx: &Context, id: &R<Expr>, val: &R<Expr>) -> Result<R<Expr>> {
    Ok(match id.as_ref() {
        Expr::String(id) => {
            match id.as_str() {
                "shadow_local" => {
                    match val.as_ref() {
                        Expr::String(val) => {
                            let pragma_level = match val.as_str() {
                                "warn" => { PragmaLevel::Warn },
                                "allow" => { PragmaLevel::Allow },
                                "error" => { PragmaLevel::Error },
                                _ => Err(anyhow!("unknown pragma level for {id}: {val}"))?,
                            };
                            *interpreter.pragma_shadow_local.borrow_mut() = pragma_level;
                        }
                        _ => Err(anyhow!("pragma value is not a string"))?,
                    }
                }
                _ => Err(anyhow!("unknown pragma id: {id}"))?,
            }
            val.to_owned()
        }
        _ => Err(anyhow!("pragma id not a string"))?,
    })
}

