use std::{collections::HashMap, ops::{Add, Mul, Neg}};
use crate::{ast::Ast, parse_string_to_ast, unescape_string};

pub type Context = HashMap<String,Value>;

#[derive(Debug,Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Unit,
}

pub fn eval(mut ctx: Context, ast: &Ast) -> (Context,Value) {
    match ast {
        Ast::Integer(i) => (ctx, Value::Integer(*i)),
        Ast::Float(f) => (ctx, Value::Float(*f)),
        Ast::String(s) => (ctx, Value::String(unescape_string(s.to_string()))),
        Ast::Block(exprs) => {
            exprs.iter().fold((ctx, Value::Unit), |(ctx, _),expr| {
                eval(ctx,expr)
            })
        }
        Ast::Let(var, val) => {
            let (_, val) = eval(ctx.clone(),val);
            let var = var.as_ref();
            match var {
                &Ast::Identifier(id) => {
                    ctx.insert(id.to_owned(), val.clone());
                    (ctx, val)
                }
                _ => (ctx, Value::Unit)
            }
        }
        Ast::Identifier(s) => {
            let val = ctx.get(*s).unwrap_or(&Value::Unit).to_owned();
            (ctx, val)
        }
        Ast::BinOpCall(op, left, right) => {
            let op = op.as_ref();
            let (_, left) = eval(ctx.clone(),left.as_ref());
            let (_, right) = eval(ctx.clone(),right.as_ref());
            (ctx, match op {
                Ast::AddOp => left+right,
                Ast::MultOp => left*right,
                Ast::ExpOp => left.exp(right),
                _ => Value::Unit,
            })
        }
        Ast::UnaryOpCall(op, expr) => {
            let op = op.as_ref();
            let (_, expr) = eval(ctx.clone(),expr.as_ref());
            match op {
                Ast::NegOp => (ctx, -expr),
                Ast::DollarOp => {
                    match expr {
                        Value::String(s) => {
                            eval(ctx, &parse_string_to_ast(&s))
                        }
                        _ => (ctx, expr)
                    }
                }
                _ => (ctx, Value::Unit),
            }
        }
        _ => (ctx, Value::Unit),
    }
}

impl Value {
    fn exp(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(b)),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a.powf(b as f64)),
            (Value::Integer(a), Value::Float(b)) => Value::Float((a as f64).powf(b)),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a.pow(b as u32)),
            _ => Self::Unit,
        }
    }
}

impl Add for Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a+b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a+b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as f64+b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a+b),
            (Value::String(a), Value::String(b)) => Value::String(format!("{a}{b}")),
            (Value::String(a), Value::Integer(b)) => Value::String(format!("{a}{b}")),
            (Value::Integer(a), Value::String(b)) => Value::String(format!("{a}{b}")),
            (Value::String(a), Value::Float(b)) => Value::String(format!("{a}{b}")),
            (Value::Float(a), Value::String(b)) => Value::String(format!("{a}{b}")),
            _ => Self::Unit,
        }
    }
}

impl Mul for Value {
    type Output = Value;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a*b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a*b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as f64*b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a*b),
            (Value::String(a), Value::Integer(b)) => Value::String(a.repeat(b as usize)),
            (Value::Integer(a), Value::String(b)) => Value::String(b.repeat(a as usize)),
            _ => Self::Unit,
        }
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> Self::Output {
        match self {
            Value::Float(a) => Value::Float(-a),
            Value::Integer(a) => Value::Integer(-a),
            Value::String(a) => Value::String(a.chars().rev().collect::<String>()),
            _ => Self::Unit,
        }
    }
}
