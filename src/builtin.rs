use std::{cell::RefCell, rc::Rc};

use crate::{expression::Expression, parse_source, eval, context::Context};
use anyhow::{Result,anyhow};
use log::debug;

pub fn pow(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a.powf(*b)),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a.powf(*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64).powf(*b)),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a.pow(*b as u32)),
        _ => Err(anyhow!("pow {lhs:?} {rhs:?}"))?,
    })
}

pub fn add(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a+b),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a + (*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64) + b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a+b),
        (Expression::String(a), Expression::String(b)) => Expression::String(format!("{a}{b}")),
        (Expression::String(a), Expression::Integer(b)) => Expression::String(format!("{a}{b}")),
        (Expression::Integer(a), Expression::String(b)) => Expression::String(format!("{a}{b}")),
        (Expression::String(a), Expression::Float(b)) => Expression::String(format!("{a}{b}")),
        (Expression::Float(a), Expression::String(b)) => Expression::String(format!("{a}{b}")),
        (Expression::Array(a), Expression::Array(b)) => 
                Expression::Array(Rc::new(RefCell::new(
                    [a.borrow().to_owned(), b.borrow().to_owned()].concat()
                ))),
        _ => Err(anyhow!("add {lhs:?} {rhs:?}"))?,
        })
}


pub fn sub(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a-b),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a - (*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64) - b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a-b),
        _ => Err(anyhow!("sub {lhs:?} {rhs:?}"))?,
    })
}

pub fn mul(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a*b),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a * (*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64) * b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a*b),
        (Expression::String(a), Expression::Integer(b)) => Expression::String(a.repeat(*b as usize)),
        (Expression::Integer(a), Expression::String(b)) => Expression::String(b.repeat(*a as usize)),
        _ => Err(anyhow!("mul {lhs:?} {rhs:?}"))?,
    })
}

pub fn div(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a/b),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a / (*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64) / b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a/b),
        _ => Err(anyhow!("div {lhs:?} {rhs:?}"))?,
    })
}

pub fn modulo(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a % b),
        _ => Err(anyhow!("mod {lhs:?} {rhs:?}"))?,
    })
}

pub fn and(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Boolean(a), Expression::Boolean(b)) => Expression::Boolean(*a && *b),
        _ => Err(anyhow!("and {lhs:?} {rhs:?}"))?,
    })
}

pub fn or(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Boolean(a), Expression::Boolean(b)) => Expression::Boolean(*a || *b),
        _ => Err(anyhow!("or {lhs:?} {rhs:?}"))?,
    })
}

pub fn gt(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Boolean(a>b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Boolean(a>b),
        (Expression::String(a), Expression::String(b)) => Expression::Boolean(a>b),
        (Expression::Boolean(a), Expression::Boolean(b)) => Expression::Boolean(a>b),
        (Expression::Unit, Expression::Unit) => Expression::Boolean(false),
        _ => Err(anyhow!("gt {lhs:?} {rhs:?}"))?,
    })
}

pub fn lt(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Boolean(a<b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Boolean(a<b),
        (Expression::String(a), Expression::String(b)) => Expression::Boolean(a<b),
        (Expression::Boolean(a), Expression::Boolean(b)) => Expression::Boolean(a<b),
        (Expression::Unit, Expression::Unit) => Expression::Boolean(false),
        _ => Err(anyhow!("lt {lhs:?} {rhs:?}"))?,
    })
}

pub fn eq(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(Expression::Boolean(lhs == rhs))
}

pub fn ne(_: &Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(Expression::Boolean(lhs != rhs))
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
    Ok(match val {
        Expression::Float(a) => Expression::Float(-a),
        Expression::Integer(a) => Expression::Integer(-a),
        Expression::String(a) => Expression::String(a.chars().rev().collect::<String>()),
        _ => Err(anyhow!("neg {val:?}"))?,
    })
}

pub fn not(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val {
        Expression::Boolean(b) => Expression::Boolean(! *b),
        _ => Err(anyhow!("not {val:?}"))?,
    })
}

pub fn print(_: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg}"); }
    Ok(Expression::Unit)
}

pub fn println(_: &Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg}"); }
    println!();
    Ok(Expression::Unit)
}

pub fn eval_string_as_source(ctx: &Context, arg: &Expression) -> Result<Expression> {
    match arg {
        Expression::String(s) => {
            debug!("evaluating string: {s}");
            eval::eval(ctx, &parse_source(s).unwrap())
        }
        _ => arg.to_error()
    }
}

pub fn array(ctx: &Context, size: &Expression, init: &Expression) -> Result<Expression> {
    let mut arr : Vec<Expression> = vec![];
    if let Expression::Integer(size) = size {
        let size = *size;
        let init = &eval::eval(ctx,init)?;
        for i in 0..size {
            let init = match init {
                Expression::Closure(_, _, _) => {
                    eval::eval(ctx, &Expression::FunctionCall(Box::new(init.to_owned()), vec![Expression::Integer(i)]))?
                }
                _ => init.to_owned(),
            };
            arr.push(init);
        }
        Ok(Expression::Array(Rc::new(RefCell::new(arr))))
    } else {
        Err(anyhow!("array {size:?} {init:?}"))?
    }
}

pub fn len(_: &Context, val: &Expression) -> Result<Expression> {
    Ok(match val {
        Expression::Array(a) => Expression::Integer(a.borrow().len() as i64),
        Expression::String(a) => Expression::Integer(a.len() as i64),
        _ => Err(anyhow!("len {val:?}"))?,
    })
}

pub fn include(ctx: &Context, file_expr: &Expression) -> Result<Expression> {
    debug!("include {file_expr:?}");
    Ok(match file_expr {
        Expression::String(file) => {
            let source = std::fs::read_to_string(file)?;
            eval_string_as_source(ctx, &Expression::String(source))?
        }
        _ => Err(anyhow!("include {file_expr:?}"))?,
    })
}

pub fn struct_from_context(ctx: &Context) -> Result<Expression> {
    Ok(Expression::Struct(ctx.capture()))
}
