use crate::{expression::Expression, parse_source, eval, context::Context};

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
    for arg in args { print!("{arg}"); }
    Expression::Unit
}

pub fn println(_: &mut Context, args: &[Expression]) -> Expression {
    for arg in args { print!("{arg}"); }
    println!();
    Expression::Unit
}

pub fn eval_string_as_source(ctx: &mut Context, arg: &Expression) -> Expression {
    match arg {
        Expression::String(s) => {
            eval::eval(ctx, &parse_source(s))
        }
        _ => Expression::from(arg)
    }
}
