use std::ops::{Add, Mul, Neg, Sub};
use crate::{parse_string_to_ast, unescape_string, types::{Value, Context, Ast}};

pub fn eval(mut ctx: Context, ast: &Ast) -> (Context,Value) {
    match ast {
        Ast::Integer(i) => (ctx, Value::Integer(*i)),
        Ast::Float(f) => (ctx, Value::Float(*f)),
        Ast::Boolean(b) => { (ctx, Value::Boolean(*b)) }
        Ast::String(s) => (ctx, Value::String(unescape_string(s.to_string()))),
        Ast::Block(exprs) => {
            exprs.iter().fold((ctx, Value::Unit), |(ctx, _),expr| {
                eval(ctx,expr)
            })
        }
        Ast::Let(id, val) => {
            let (_, val) = eval(ctx.clone(),val);
            ctx.insert(id.to_owned(), val.to_owned());
            (ctx, val)
        }
        Ast::Assign(id, val) => {
            let (_, val) = eval(ctx.clone(),val);
            ctx.insert(id.to_owned(), val.to_owned());
            (ctx, val)
        }
        Ast::Variable(s) => {
            let val = ctx.get(s).unwrap_or(&Value::Ast(ast.to_owned())).to_owned();
            (ctx, val)
        }
        Ast::If(cond, then, r#else) => {
            let (_, cond) = eval(ctx.clone(),cond.as_ref());
            match cond {
                Value::Boolean(b) =>
                    if b {
                        let (_, then) = eval(ctx.clone(),then.as_ref());
                        (ctx, then)
                    } else {
                        let (_, r#else) = eval(ctx.clone(),r#else.as_ref());
                        (ctx, r#else)
                    }
                _ => (ctx, Value::Ast(ast.to_owned())),
            }
        }
        Ast::While(cond, body) => {
            // we have no side-effects for now so we put a println in there
            // we should use a stack of contexts and decl vs assign
            let (_, cond) = eval(ctx.clone(),cond.as_ref());
            match cond {
                Value::Boolean(b) =>
                    if b {
                        let (ctx, _body) = eval(ctx.clone(),body.as_ref());
                        println!("WHILE {ctx:?} {_body:?}");
                        eval(ctx, ast)
                    } else {
                        (ctx, Value::Unit)
                    }
                _ => (ctx, Value::Ast(ast.to_owned())),
            }
        }
        Ast::FunctionCall(lambda, arg_values) => {
            let lambda = eval(ctx.clone(), lambda).1;
            match lambda {
                Value::Ast(Ast::Function(arg_names, body)) => {
                    let fctx = arg_names.iter().zip(arg_values).fold(ctx.clone(), |mut nctx,(name,value)| {
                        nctx.insert(name.to_owned(), eval(ctx.clone(),value).1);
                        nctx
                    });
                    (ctx,eval(fctx,&body).1)
                },
                _ => unreachable!()
            }
        }
        Ast::BinOpCall(op, left, right) => {
            let op = op.as_ref();
            let (_, left) = eval(ctx.clone(),left.as_ref());
            let (_, right) = eval(ctx.clone(),right.as_ref());
            (ctx, match op {
                Ast::AddOp => left+right,
                Ast::SubOp => left-right,
                Ast::MultOp => left*right,
                Ast::ExpOp => left.exp(right),
                Ast::GtOp => Value::Boolean(left>right),
                Ast::GeOp => Value::Boolean(left>=right),
                Ast::LtOp => Value::Boolean(left<right),
                Ast::LeOp => Value::Boolean(left<=right),
                Ast::EqOp => Value::Boolean(left==right),
                Ast::NeOp => Value::Boolean(left!=right),
                _ => Value::Ast(ast.to_owned()),
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
                _ => (ctx, Value::Ast(ast.to_owned())),
            }
        }
        _ => (ctx, Value::Ast(ast.to_owned())),
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

impl Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a-b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a-b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as f64-b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a-b),
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
