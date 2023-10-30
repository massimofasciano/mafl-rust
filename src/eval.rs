use crate::{unescape_string, expression::{Expression, AtomicExpression}, builtin, context::Context};
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
            ctx.set_binding(id.to_owned(), val.to_owned());
            val
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
        Expression::Function(arg_names, body) => {
            Expression::Closure(ctx.to_owned(), arg_names.to_owned(), body.to_owned())
        }
        Expression::FunctionCall(lambda, arg_values) => {
            match eval(ctx,lambda)? {
                Expression::Builtin(name) => {
                    let eval_args = arg_values.iter().map(|e|eval(ctx,e)).collect::<Result<Vec<_>>>()?;
                    builtin(ctx,&name, &eval_args)?
                }
                Expression::Closure(closure_ctx, arg_names, body) => {
                    let mut function_ctx = ctx.clone();
                    function_ctx.add_context(closure_ctx);
                    for (name,value) in arg_names.iter().zip(arg_values) {
                        function_ctx.add_binding(name.to_owned(), eval(ctx,value)?);
                    }
                    #[allow(clippy::comparison_chain)]
                    if arg_names.len() > arg_values.len() {
                        debug!("performing currying: {arg_names:?} {arg_values:?}");
                        Expression::Closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body)
                    } else if arg_names.len() < arg_values.len() {
                        debug!("extra args supplied: {arg_names:?} {arg_values:?}");
                        let uncurried = eval(&mut function_ctx,&body)?;
                        eval(&mut function_ctx,&Expression::FunctionCall(Box::new(uncurried),arg_values[arg_names.len()..].to_vec()))?
                    } else {
                        eval(&mut function_ctx,&body)?
                    }
                },
                _ => unreachable!()
            }
        }
        Expression::BinOpCall(op, left, right) => {
            let op = op.as_ref();
            let left = eval(ctx,left.as_ref())?;
            let right = eval(ctx,right.as_ref())?;
            let aleft = AtomicExpression::from(left.to_owned());
            let aright = AtomicExpression::from(right.to_owned());
            match op {
                Expression::InfixOp(fname) => {
                    if fname.starts_with('$') {
                        let fvar = Box::new(Expression::Variable(fname.strip_prefix('$').unwrap().to_owned()));
                        eval(ctx,&Expression::FunctionCall(fvar, vec![left,right]))?
                    } else {
                        Expression::Error("infix op bad prefix".to_owned())
                    }
                },
                Expression::PipeOp => {
                    let fvar = Box::new(right);
                    eval(ctx,&Expression::FunctionCall(fvar, vec![left]))?
                },
                Expression::AddOp => left+right,
                Expression::SubOp => left-right,
                Expression::MultOp => left*right,
                Expression::ExpOp => builtin::pow(ctx, &left, &right)?,
                Expression::GtOp => Expression::Boolean(aleft>aright),
                Expression::GeOp => Expression::Boolean(aleft>=aright),
                Expression::LtOp => Expression::Boolean(aleft<aright),
                Expression::LeOp => Expression::Boolean(aleft<=aright),
                Expression::EqOp => Expression::Boolean(aleft==aright),
                Expression::NeOp => Expression::Boolean(aleft!=aright),
                _ => ast.to_error()?,
            }
        }
        Expression::UnaryOpCall(op, expr) => {
            let op = op.as_ref();
            let expr = eval(ctx,expr.as_ref())?;
            match op {
                Expression::NegOp => -expr,
                _ => ast.to_error()?,
            }
        }
        Expression::Closure(_, _, _) => ast.to_owned(),
        _ => ast.to_error()?,
    })
}

pub fn builtin(ctx: &mut Context, name: &str, args: &[Expression]) -> Result<Expression> {
    match (name, args) {
        ("println", args) => { builtin::println(ctx, args) },
        ("print", args) => { builtin::print(ctx, args) },
        ("eval", [arg]) => { builtin::eval_string_as_source(ctx, arg) },
        ("pow", [lhs, rhs]) => builtin::pow(ctx, lhs, rhs),
        _ => Err(anyhow!("builtin {name}")),
    }
}

