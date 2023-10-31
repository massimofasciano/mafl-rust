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
