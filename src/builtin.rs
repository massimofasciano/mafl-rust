use crate::{expression::Expression, parse_source, eval, context::Context};
use anyhow::{Result,anyhow};
use log::debug;

pub fn pow(_: &mut Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a.powf(*b)),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a.powf(*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64).powf(*b)),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a.pow(*b as u32)),
        _ => Err(anyhow!("pow {lhs},{rhs}"))?,
    })
}

pub fn add(_: &mut Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
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
        _ => Err(anyhow!("add {lhs},{rhs}"))?,
        })
}


pub fn sub(_: &mut Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a-b),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a - (*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64) - b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a-b),
        _ => Err(anyhow!("sub {lhs},{rhs}"))?,
    })
}

pub fn mul(_: &mut Context, lhs: &Expression, rhs: &Expression) -> Result<Expression> {
    Ok(match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a*b),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a * (*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64) * b),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a*b),
        (Expression::String(a), Expression::Integer(b)) => Expression::String(a.repeat(*b as usize)),
        (Expression::Integer(a), Expression::String(b)) => Expression::String(b.repeat(*a as usize)),
        _ => Err(anyhow!("mul {lhs},{rhs}"))?,
    })
}

pub fn neg(_: &mut Context, val: &Expression) -> Result<Expression> {
    Ok(match val {
        Expression::Float(a) => Expression::Float(-a),
        Expression::Integer(a) => Expression::Integer(-a),
        Expression::String(a) => Expression::String(a.chars().rev().collect::<String>()),
        _ => Err(anyhow!("mul {val}"))?,
    })
}

pub fn print(_: &mut Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg}"); }
    Ok(Expression::Unit)
}

pub fn println(_: &mut Context, args: &[Expression]) -> Result<Expression> {
    for arg in args { print!("{arg}"); }
    println!();
    Ok(Expression::Unit)
}

pub fn eval_string_as_source(ctx: &mut Context, arg: &Expression) -> Result<Expression> {
    match arg {
        Expression::String(s) => {
            debug!("evaluating string: {s}");
            eval::eval(ctx, &parse_source(s).unwrap())
        }
        _ => arg.to_error()
    }
}
