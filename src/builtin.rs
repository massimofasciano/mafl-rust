use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, BufRead, stdout, Write};
use std::fmt::Write as _;
use std::rc::Rc;

use crate::context::MemCell;
use crate::{expression::{Expression, self, nil, closure, ExpressionType}, parse_source, context::Context, Interpreter};
use anyhow::{Result,anyhow};
use log::debug;

pub fn pow(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a.powf(*b)),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a.powf(*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64).powf(*b)),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a.pow(*b as u32)),
        _ => Err(anyhow!("pow {lhs:?} {rhs:?}"))?,
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
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Float(a/b),
        (ExpressionType::Float(a), ExpressionType::Integer(b)) => ExpressionType::Float(a / (*b as f64)),
        (ExpressionType::Integer(a), ExpressionType::Float(b)) => ExpressionType::Float((*a as f64) / b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Integer(a/b),
        _ => Err(anyhow!("div {lhs:?} {rhs:?}"))?,
    }.into())
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
    let lhs = interpreter.eval(ctx,lhs.to_owned())?;
    match lhs.as_ref() {
        ExpressionType::Boolean(b) => if !b { return Ok(ExpressionType::Boolean(false).into()) },
        _ => Err(anyhow!("and_lazy lhs not boolean: {lhs:?}"))?,
    };
    let rhs = interpreter.eval(ctx,rhs.to_owned())?;
    match rhs.as_ref() {
        ExpressionType::Boolean(b) => if !b { return Ok(ExpressionType::Boolean(false).into()) },
        _ => Err(anyhow!("and_lazy rhs not boolean: {rhs:?}"))?,
    };
    Ok(ExpressionType::Boolean(true).into())
}

pub fn or_lazy(interpreter: &Interpreter, ctx: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    let lhs = interpreter.eval(ctx,lhs.to_owned())?;
    match lhs.as_ref() {
        ExpressionType::Boolean(b) => if *b { return Ok(ExpressionType::Boolean(true).into()) },
        _ => Err(anyhow!("or_lazy lhs not boolean: {lhs:?}"))?,
    };
    let rhs = interpreter.eval(ctx,rhs.to_owned())?;
    match rhs.as_ref() {
        ExpressionType::Boolean(b) => if *b { return Ok(ExpressionType::Boolean(true).into()) },
        _ => Err(anyhow!("or_lazy rhs not boolean: {rhs:?}"))?,
    };
    Ok(ExpressionType::Boolean(false).into())
}

pub fn gt(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs.as_ref(), rhs.as_ref()) {
        (ExpressionType::Float(a), ExpressionType::Float(b)) => ExpressionType::Boolean(a>b),
        (ExpressionType::Integer(a), ExpressionType::Integer(b)) => ExpressionType::Boolean(a>b),
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

pub fn print(_: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg}"); }
    stdout().flush()?;
    Ok(nil())
}

pub fn println(_: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg}"); }
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
            interpreter.eval(ctx, parse_source(s)?)
        }
        _ => arg.to_error()
    }
}

pub fn array(interpreter: &Interpreter, ctx: &Context, size: &Expression, init: &Expression) -> Result<Expression> {
    let mut arr : Vec<Expression> = vec![];
    if let ExpressionType::Integer(size) = size.as_ref() {
        let size = *size;
        let init = &interpreter.eval(ctx,init.to_owned())?;
        for i in 0..size {
            let init = match init.as_ref() {
                ExpressionType::Closure(_, _, _) => {
                    interpreter.eval(ctx, ExpressionType::FunctionCall(init.to_owned(), vec![ExpressionType::Integer(i).into()]).into())?
                }
                ExpressionType::DynFn(_, _) => {
                    interpreter.eval(ctx, ExpressionType::FunctionCall(init.to_owned(), vec![ExpressionType::Integer(i).into()]).into())?
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
                let pair = vec![expression::string(s),cell.get()];
                arr.push(expression::array(pair));
            }
            Ok(expression::array(arr))
        } 
        ExpressionType::Array(_) => {
            copy(ctx, init)
        } 
        _ => Ok(expression::error(format!("to array {init:?}")))
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
            let new = interpreter.eval(ctx,new.to_owned())?;
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
    interpreter.eval(ctx, parse_source(s)?)
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
    Ok(expression::closure(ctx.capture(),vec![],nil()))
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

pub fn get(_ctx: &Context, container: &Expression, key: &Expression) -> Result<Expression> {
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
                    c.get_binding(s).unwrap_or(expression::error(format!("binding not found {s}"))),
                _ => Err(anyhow!("get on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("get {key:?}"))?,
    })
}

pub fn set(_ctx: &Context, container: &Expression, key: &Expression, value: &Expression) -> Result<Expression> {
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
                    c.set_binding(s.to_owned(), value.to_owned())
                        .unwrap_or(expression::error(format!("binding not found {s}"))),
                _ => Err(anyhow!("set on closure with non-string key {key:?}"))?,
            }
        }
        _ => Err(anyhow!("set {key:?}"))?,
    })
}

pub fn insert(_ctx: &Context, container: &Expression, key: &Expression, value: &Expression) -> Result<Expression> {
    Ok(match container.as_ref() {
        ExpressionType::Closure(c,_,_) => {
            match key.as_ref() {
                ExpressionType::String(s) => {
                    c.add_binding(s.to_owned(), value.to_owned());
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
    Ok(closure(Context::new(),vec![],nil()))
}

pub fn dict_extend(_ctx: &Context, parent: &Expression) -> Result<Expression> {
    debug!("extend dict");
    Ok(match parent.as_ref() {
        ExpressionType::Closure(c,a,b) => {
            closure(c.with_new_scope(),a.to_owned(),b.to_owned())
        }
        _ => Err(anyhow!("dict_extend"))?,
    })
}

pub fn error_from_strings(_: &Context, args: &[Expression]) -> Result<Expression> {
    debug!("error");
    let mut output = String::new();
    for arg in args { write!(output,"{arg}")?; }
    Ok(expression::error(output))
}

pub fn get_var(ctx: &Context, key: &Expression) -> Result<Expression> {
    debug!("get_var {key:?}");
    Ok(match key.as_ref() {
        ExpressionType::String(s) => 
            ctx.get_binding(s).unwrap_or(expression::error(format!("binding not found {s}"))),
        _ => Err(anyhow!("get on closure with non-string key {key:?}"))?,
    })
}

pub fn assign_var(ctx: &Context, key: &Expression, value: &Expression) -> Result<Expression> {
    debug!("set_var {key:?} {value:?}");
    Ok(match key.as_ref() {
        ExpressionType::String(s) => 
            ctx.set_binding(s.to_owned(), value.to_owned())
                .unwrap_or(expression::error(format!("binding not found {s}"))),
        _ => Err(anyhow!("set on closure with non-string key {key:?}"))?,
    })
}

pub fn let_var(ctx: &Context, key: &Expression, value: &Expression) -> Result<Expression> {
    debug!("insert_var {key:?} {value:?}");
    Ok(match key.as_ref() {
        ExpressionType::String(s) => {
            ctx.add_binding(s.to_owned(), value.to_owned());
            value.to_owned()
        }
        _ => Err(anyhow!("insert on closure with non-string key {key:?}"))?,
    })
}

#[allow(clippy::only_used_in_recursion)]
pub fn copy(ctx: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val.as_ref() {
        ExpressionType::Array(a) => {

            let ac = a.borrow().iter()
                    .map(|e| { copy(ctx,e) })
                    .collect::<Result<Vec<Expression>>>()?;
            expression::array(ac)
        }
        ExpressionType::Closure(cctx,args,body) => {
            // single level copy
            // expression::closure(cctx.flatten_clone(), args.to_owned(), body.to_owned())   
            // recursive copy
            let copyrec = cctx.bindings_cloned().into_iter().map(|(k,v)| {
                (k, MemCell::new_ref(copy(ctx, &v.get()).unwrap()))
            }).collect::<HashMap<String,Rc<MemCell>>>();
            expression::closure(cctx.with_bindings(copyrec), args.to_owned(), body.to_owned())   
        }
        _ => val.to_owned(),
    })
}

pub fn ref_var(ctx: &Context, var: &Expression) -> Result<Expression> {
    debug!("ref_var {var}");
    Ok(match var.as_ref() {
        ExpressionType::Variable(s) => {
            if let Some(rc) = ctx.get_binding_ref(s) {
                ExpressionType::Ref(rc).into()
            } else {
                expression::error(format!("binding not found {s}"))
            }
        }
        _ => Err(anyhow!("ref on non-variable {var}"))?,
    })
}

