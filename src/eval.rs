use std::{cell::RefCell, rc::Rc};

use crate::{unescape_string, expression::Expression, builtin, context::Context};
use anyhow::{Result,anyhow};
use log::debug;

pub fn eval(ctx: &mut Context, ast: &Expression) -> Result<Expression> {
    Ok(match ast {
        Expression::Unit => Expression::Unit,
        Expression::Integer(i) => Expression::Integer(*i),
        Expression::Float(f) => Expression::Float(*f),
        Expression::Boolean(b) => { Expression::Boolean(*b) }
        Expression::String(s) => Expression::String(unescape_string(s.to_string())),
        Expression::Block(exprs) => {
            ctx.start_scope();
            let mut block_value = Expression::Unit;
            for expr in exprs {
                block_value = eval(ctx,expr)?;
            }
            ctx.end_scope();
            block_value
        }
        Expression::Let(id, val, expr) => {
            let val = eval(ctx,val)?;
            ctx.start_scope();
            ctx.add_binding(id.to_owned(), val.to_owned());
            let result = eval(ctx,expr)?;
            ctx.end_scope();
            result
        }
        Expression::Var(id, val) => {
            let val = eval(ctx,val)?;
            ctx.add_binding(id.to_owned(), val.to_owned());
            val
        }
        Expression::Assign(id, val) => {
            let val = eval(ctx,val)?;
            if let Some(binding) = ctx.get_mut_binding(id) {
                *binding = val.to_owned();
            } else {
                Err(anyhow!("binding not found {id}"))?
            }
            val
        }
        Expression::AssignToExpression(target, val) => {
            let val = eval(ctx,val)?;
            match target.as_ref() {
                Expression::Variable(id) => {
                    if let Some(binding) = ctx.get_mut_binding(id) {
                        *binding = val.to_owned();
                    } else {
                        Err(anyhow!("binding not found {id}"))?
                    }
                    val
                },
                Expression::Field(target, field) => {
                    let target = eval(ctx, target)?;
                    match target {
                        Expression::Closure(closure_ctx, _arg_names, _body) => {
                            let mut closure_ctx = closure_ctx.borrow_mut();
                            if let Some(target_value) = closure_ctx.get_mut_binding(field) {
                                *target_value = val.to_owned();
                                val
                            } else {
                                Err(anyhow!("field binding not found {field}"))?
                            }
                        }
                        _ => ast.to_error()?,
                    }
                },
                _ => ast.to_error()?,
            }
        }
        Expression::Variable(s) => {
            if s.starts_with('@') {
                Expression::Builtin(s.strip_prefix('@').unwrap().to_owned())
            } else if let Some(value) = ctx.get_binding(s) {
                value.to_owned()
            } else {
                Err(anyhow!("binding not found {s}"))?
            }
        }
        Expression::If(cond, then, r#else) => {
            let cond = eval(ctx,cond.as_ref())?;
            match cond {
                Expression::Boolean(b) =>
                    if b { eval(ctx,then.as_ref())? } 
                    else { eval(ctx,r#else.as_ref())? }
                _ => ast.to_error()?,
            }
        }
        Expression::While(cond, body) => {
            let mut body_value = Expression::Unit;
            #[allow(clippy::while_let_loop)]
            loop {
                match eval(ctx,cond.as_ref())? {
                    Expression::Boolean(b) => if b {
                        body_value = eval(ctx,body.as_ref())?;
                    } else {
                        break;
                    },
                    _ => break,
                };
            }
            body_value
        }
        Expression::Field(target, field) => {
            let target = eval(ctx, target)?;
            match target {
                Expression::Closure(closure_ctx, _arg_names, _body) => {
                    let closure_ctx = closure_ctx.borrow();
                    if let Some(value) = closure_ctx.get_binding(field) {
                        value.to_owned()
                    } else {
                        Err(anyhow!("field binding not found {field}"))?
                    }
                }
                _ => ast.to_error()?,
            }
        }
        Expression::Function(arg_names, body) => {
            Expression::Closure(Rc::new(RefCell::new(ctx.to_owned())), arg_names.to_owned(), body.to_owned())
        }
        Expression::FunctionCall(lambda, arg_values) => {
            match eval(ctx,lambda)? {
                Expression::Builtin(name) => {
                    let eval_args = arg_values.iter().map(|e|eval(ctx,e)).collect::<Result<Vec<_>>>()?;
                    builtin(ctx,&name, &eval_args)?
                }
                Expression::Closure(closure_ctx, arg_names, body) => {
                    let mut function_ctx = ctx.clone();
                    let closure_ctx = closure_ctx.borrow();
                    function_ctx.add_context(&closure_ctx);
                    for (name,value) in arg_names.iter().zip(arg_values) {
                        function_ctx.add_binding(name.to_owned(), eval(ctx,value)?);
                    }
                    #[allow(clippy::comparison_chain)]
                    if arg_names.len() > arg_values.len() {
                        debug!("performing currying: {arg_names:?} {arg_values:?}");
                        Expression::Closure(Rc::new(RefCell::new(function_ctx)), arg_names[arg_values.len()..].to_vec(), body)
                    } else if arg_names.len() < arg_values.len() {
                        debug!("extra args supplied: {arg_names:?} {arg_values:?}");
                        let uncurried = eval(&mut function_ctx,&body)?;
                        eval(&mut function_ctx,&Expression::FunctionCall(Box::new(uncurried),arg_values[arg_names.len()..].to_vec()))?
                    } else {
                        eval(&mut function_ctx,&body)?
                    }
                },
                _ => ast.to_error()?
            }
        }
        Expression::Context(arg_names, body) => {
            let mut local_ctx = Context::new();
            for name in arg_names {
                if let Some(val) = ctx.get_binding(name) { 
                    local_ctx.add_binding(name.to_owned(), val.to_owned()) 
                }
            }
            eval(&mut local_ctx, body)?
        }
        Expression::BinOpCall(op, left, right) => {
            let op = op.as_ref();
            let left = eval(ctx,left.as_ref())?;
            let right = eval(ctx,right.as_ref())?;
            match op {
                Expression::InfixOp(fname) => {
                    if fname.starts_with('$') {
                        let fvar = Box::new(Expression::Variable(fname.strip_prefix('$').unwrap().to_owned()));
                        eval(ctx,&Expression::FunctionCall(fvar, vec![left,right]))?
                    } else {
                        Err(anyhow!("infix op bad prefix: {fname}"))?
                    }
                },
                Expression::PipeOp => {
                    let fvar = Box::new(right);
                    eval(ctx,&Expression::FunctionCall(fvar, vec![left]))?
                },
                Expression::AddOp => builtin::add(ctx,&left,&right)?,
                Expression::SubOp => builtin::sub(ctx,&left,&right)?,
                Expression::MultOp => builtin::mul(ctx,&left,&right)?,
                Expression::DivOp => builtin::div(ctx,&left,&right)?,
                Expression::ModOp => builtin::modulo(ctx,&left,&right)?,
                Expression::ExpOp => builtin::pow(ctx, &left, &right)?,
                Expression::GtOp => builtin::gt(ctx, &left, &right)?,
                Expression::GeOp => builtin::ge(ctx, &left, &right)?,
                Expression::LtOp => builtin::lt(ctx, &left, &right)?,
                Expression::LeOp => builtin::le(ctx, &left, &right)?,
                Expression::EqOp => builtin::eq(ctx, &left, &right)?,
                Expression::NeOp => builtin::ne(ctx, &left, &right)?,
                _ => ast.to_error()?,
            }
        }
        Expression::UnaryOpCall(op, expr) => {
            let op = op.as_ref();
            let expr = eval(ctx,expr.as_ref())?;
            match op {
                Expression::NegOp => builtin::neg(ctx,&expr)?,
                Expression::NotOp => builtin::not(ctx,&expr)?,
                _ => ast.to_error()?,
            }
        }
        Expression::Closure(_, _, _) => ast.to_owned(), 
        Expression::Array(vals) => {
            Expression::Array(vals.iter().map(|v|eval(ctx,v)).collect::<Result<Vec<_>>>()?)
        }
        _ => ast.to_error()?,
    })
}

pub fn builtin(ctx: &mut Context, name: &str, args: &[Expression]) -> Result<Expression> {
    match (name, args) {
        ("println", args) => { builtin::println(ctx, args) },
        ("print", args) => { builtin::print(ctx, args) },
        ("eval", [arg]) => { builtin::eval_string_as_source(ctx, arg) },
        ("pow", [lhs, rhs]) => builtin::pow(ctx, lhs, rhs),
        ("add", [lhs, rhs]) => builtin::add(ctx, lhs, rhs),
        ("sub", [lhs, rhs]) => builtin::sub(ctx, lhs, rhs),
        ("mul", [lhs, rhs]) => builtin::mul(ctx, lhs, rhs),
        ("div", [lhs, rhs]) => builtin::div(ctx, lhs, rhs),
        ("mod", [lhs, rhs]) => builtin::modulo(ctx, lhs, rhs),
        ("neg", [val]) => builtin::neg(ctx, val),
        ("not", [val]) => builtin::not(ctx, val),
        ("and", [lhs, rhs]) => builtin::and(ctx, lhs, rhs),
        ("or", [lhs, rhs]) => builtin::or(ctx, lhs, rhs),
        ("gt", [lhs, rhs]) => builtin::gt(ctx, lhs, rhs),
        ("lt", [lhs, rhs]) => builtin::lt(ctx, lhs, rhs),
        ("eq", [lhs, rhs]) => builtin::eq(ctx, lhs, rhs),
        ("ne", [lhs, rhs]) => builtin::ne(ctx, lhs, rhs),
        ("ge", [lhs, rhs]) => builtin::ge(ctx, lhs, rhs),
        ("le", [lhs, rhs]) => builtin::le(ctx, lhs, rhs),
        _ => Err(anyhow!("builtin {name}")),
    }
}

