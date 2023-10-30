use crate::{types::{Context, Expression}, parse_source};

pub fn pow(_: &mut Context, lhs: &Expression, rhs: &Expression) -> Expression {
    match (lhs, rhs) {
        (Expression::Float(a), Expression::Float(b)) => Expression::Float(a.powf(*b)),
        (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a.powf(*b as f64)),
        (Expression::Integer(a), Expression::Float(b)) => Expression::Float((*a as f64).powf(*b)),
        (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a.pow(*b as u32)),
        _ => Expression::Error("pow".to_owned()),
    }
}

pub fn print(_: &mut Context, args: &[Expression]) -> Expression {
    for arg in args { 
        match arg {
            Expression::Float(a) => print!("{a}"),
            Expression::Integer(a) => print!("{a}"),
            Expression::String(a) => print!("{a}"),
            _ => print!("{:?}",arg),
        }
    }
    Expression::Unit
}

pub fn println(ctx: &mut Context, args: &[Expression]) -> Expression {
    print(ctx, args); println!();
    Expression::Unit
}

pub fn eval_string(ctx: &mut Context, arg: &Expression) -> Expression {
    match arg {
        Expression::String(s) => {
            ctx.eval(&parse_source(s))
        }
        _ => Expression::from(arg)
    }
}
