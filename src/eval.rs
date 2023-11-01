use std::{cell::RefCell, rc::Rc};

use crate::{unescape_string, expression::Expression, builtin, context::Context};
use anyhow::{Result,anyhow};
use log::{debug, error};

pub fn eval(ctx: &Context, ast: &Expression) -> Result<Expression> {
    Ok(match ast {
        Expression::Unit => Expression::Unit,
        Expression::Integer(i) => Expression::Integer(*i),
        Expression::Float(f) => Expression::Float(*f),
        Expression::Boolean(b) => { Expression::Boolean(*b) }
        Expression::String(s) => Expression::String(unescape_string(s.to_string())),
        Expression::Block(exprs) => {
            debug!("eval block");
            let ctx = &ctx.with_new_scope();
            let mut block_value = Expression::Unit;
            for expr in exprs {
                block_value = eval(ctx,expr)?;
            }
            block_value
        }
        Expression::Sequence(exprs) => {
            debug!("eval sequence");
            let mut seq_value = Expression::Unit;
            for expr in exprs {
                seq_value = eval(ctx,expr)?;
            }
            seq_value
        }
        Expression::LetIn(id, val, expr) => {
            debug!("eval let {id} in");
            let val = eval(ctx,val)?;
            let ctx = &ctx.with_new_scope();
            ctx.add_binding(id.to_owned(), val.to_owned());
            eval(ctx,expr)?
        }
        Expression::Let(id, val) => {
            debug!("eval var declaration: {id}");
            let val = eval(ctx,val)?;
            ctx.add_binding(id.to_owned(), val.to_owned());
            val
        }
        Expression::Assign(id, val) => {
            debug!("eval assign to identifier: {id}");
            let val = eval(ctx,val)?;
            if ctx.set_binding(id.to_owned(), val.to_owned()).is_none() {
                Err(anyhow!("binding not found {id}"))?
            } else {
                val
            }
        }
        // Expression::AssignToExpression(target, val) => {
        //     debug!("eval assign to expression");
        //     let val = eval(ctx,val)?;
        //     match target.as_ref() {
        //         Expression::Variable(id) => {
        //             if ctx.set_binding(id.to_owned(), val.to_owned()).is_none() {
        //                 Err(anyhow!("binding not found {id}"))?
        //             } else {
        //                 val
        //             }
        //         },
        //         Expression::Field(target, field) => {
        //             let target = eval(ctx, target)?;
        //             match target {
        //                 Expression::Closure(closure_ctx, _arg_names, _body) => {
        //                     if closure_ctx.set_binding(field.to_owned(), val.to_owned()).is_none() {
        //                         Err(anyhow!("field binding not found {field}"))?
        //                     } else {
        //                         val
        //                     }
        //                 }
        //                 _ => ast.to_error()?,
        //             }
        //         },
        //         _ => ast.to_error()?,
        //     }
        // }
        Expression::Variable(s) => {
            debug!("eval variable: {s}");
            if s.starts_with('@') {
                Expression::Builtin(s.strip_prefix('@').unwrap().to_owned())
            } else if let Some(value) = ctx.get_binding(s) {
                value.to_owned()
            } else {
                Err(anyhow!("binding not found {s}"))?
            }
        }
        Expression::If(cond, then, r#else) => {
            debug!("eval if");
            let cond = eval(ctx,cond.as_ref())?;
            match cond {
                Expression::Boolean(b) =>
                    if b { eval(ctx,then.as_ref())? } 
                    else { eval(ctx,r#else.as_ref())? }
                _ => ast.to_error()?,
            }
        }
        Expression::While(cond, body) => {
            debug!("eval while");
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
            debug!("eval field: .{field}");
            let target = eval(ctx, target)?;
            match target {
                Expression::Closure(closure_ctx, _arg_names, _body) => {
                    if let Some(value) = closure_ctx.get_binding(field) {
                        value.to_owned()
                    } else {
                        Err(anyhow!("field binding not found {field}"))?
                    }
                }
                _ => ast.to_error()?,
            }
        }
        Expression::ClosureSyntax(arg_names, body) => {
            debug!("eval function");
            Expression::Closure(ctx.capture(), arg_names.to_owned(), body.to_owned())
        }
        Expression::Function(arg_names, body) => {
            debug!("eval function");
            Expression::Closure(Context::new(), arg_names.to_owned(), body.to_owned())
        }
        Expression::FunctionDynamic(_arg_names, _body) => {
            debug!("eval dynamic function");
            ast.to_owned()
        }
        Expression::FunctionCall(lambda, arg_values) => {
            debug!("eval function call");
            match eval(ctx,lambda)? {
                Expression::Builtin(name) => {
                    let eval_args = arg_values.iter().map(|e|eval(ctx,e)).collect::<Result<Vec<_>>>()?;
                    builtin(ctx,&name, &eval_args)?
                }
                Expression::Array(vals) => {
                    // indexing
                    if arg_values.len() == 1 {
                        if let Expression::Integer(index) = eval(ctx, &arg_values[0])? {
                            if let Some(result) = vals.borrow().get(index as usize) {
                                result.to_owned()
                            } else {
                                Err(anyhow!("index {index} out of bounds"))?
                            }
                        } else {
                            Err(anyhow!("index by non-integer"))?
                        }
                    // mutate at index
                    } else if arg_values.len() == 2 {
                        if let Expression::Integer(index) = eval(ctx, &arg_values[0])? {
                            debug!("array set index {index}");
                            if let Some(result) = vals.borrow_mut().get_mut(index as usize) {
                                *result = eval(ctx, &arg_values[1])?;
                                error!("array set index {index} to {result:?}");
                                result.to_owned()
                            } else {
                                Err(anyhow!("index {index} out of bounds"))?
                            }
                        } else {
                            Err(anyhow!("index by non-integer"))?
                        }
                    } else {
                        Err(anyhow!("array get or set index: too many arguments"))?
                    }
                }
                Expression::Closure(closure_ctx, arg_names, body) => {
                    let function_ctx = closure_ctx.with_new_scope();
                    for (name,value) in arg_names.iter().zip(arg_values) {
                        function_ctx.add_binding(name.to_owned(), eval(ctx,value)?);
                    }
                    #[allow(clippy::comparison_chain)]
                    if arg_names.len() > arg_values.len() {
                        debug!("performing currying: {arg_names:?} {arg_values:?}");
                        Expression::Closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body)
                    } else if arg_names.len() < arg_values.len() {
                        debug!("extra args supplied: {arg_names:?} {arg_values:?}");
                        let uncurried = eval(&function_ctx,&body)?;
                        eval(&function_ctx,&Expression::FunctionCall(Box::new(uncurried),arg_values[arg_names.len()..].to_vec()))?
                    } else {
                        eval(&function_ctx,&body)?
                    }
                },
                Expression::FunctionDynamic(arg_names, body) => {
                    // a dynamic function is applied in the global context
                    // so we create a closure at application time and apply it
                    let closure = Box::new(Expression::Closure(ctx.to_owned(), arg_names, body));
                    let apply = Expression::FunctionCall(closure, arg_values.to_owned());
                    eval(ctx, &apply)?
                },
                _ => ast.to_error()?
            }
        }
        Expression::ContextSyntax(arg_names, body) => {
            debug!("eval context: {arg_names:?}");
            let local_ctx = Context::new();
            for name in arg_names {
                if let Some(val) = ctx.get_binding(name) { 
                    local_ctx.add_binding(name.to_owned(), val.to_owned()); 
                }
            }
            eval(&local_ctx, body)?
        }
        Expression::BinOpCall(op, left, right) => {
            debug!("eval bin op call");
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
                Expression::AndOp => builtin::and(ctx,&left,&right)?,
                Expression::OrOp => builtin::or(ctx,&left,&right)?,
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
            debug!("eval unary op call");
            let op = op.as_ref();
            let expr = eval(ctx,expr.as_ref())?;
            match op {
                Expression::NegOp => builtin::neg(ctx,&expr)?,
                Expression::NotOp => builtin::not(ctx,&expr)?,
                _ => ast.to_error()?,
            }
        }
        Expression::Closure(cctx, args, body) => {
            debug!("eval closure");
            Expression::Closure(cctx.capture(), args.to_owned(), body.to_owned())
        } 
        Expression::Array(vals) => {
            debug!("eval array");
            Expression::Array(Rc::new(RefCell::new(
                vals.borrow().iter().map(|v|eval(ctx,v)).collect::<Result<Vec<_>>>()?
            )))
        }
        _ => ast.to_error()?,
    })
}

pub fn builtin(ctx: &Context, name: &str, args: &[Expression]) -> Result<Expression> {
    match (name, args) {
        ("println", args) => { builtin::println(ctx, args) },
        ("print", args) => { builtin::print(ctx, args) },
        ("eval", [arg]) => { builtin::eval_string_as_source(ctx, arg) },
        ("include", [file_expr]) => builtin::include(ctx, file_expr),
        ("readfile", [file_expr]) => builtin::read_file(ctx, file_expr),
        ("pow", [lhs, rhs]) => builtin::pow(ctx, lhs, rhs),
        ("add", [lhs, rhs]) => builtin::add(ctx, lhs, rhs),
        ("sub", [lhs, rhs]) => builtin::sub(ctx, lhs, rhs),
        ("mul", [lhs, rhs]) => builtin::mul(ctx, lhs, rhs),
        ("div", [lhs, rhs]) => builtin::div(ctx, lhs, rhs),
        ("mod", [lhs, rhs]) => builtin::modulo(ctx, lhs, rhs),
        ("neg", [val]) => builtin::neg(ctx, val),
        ("not", [val]) => builtin::not(ctx, val),
        ("len", [val]) => builtin::len(ctx, val),
        ("type", [val]) => builtin::type_of(ctx, val),
        ("and", [lhs, rhs]) => builtin::and(ctx, lhs, rhs),
        ("or", [lhs, rhs]) => builtin::or(ctx, lhs, rhs),
        ("gt", [lhs, rhs]) => builtin::gt(ctx, lhs, rhs),
        ("lt", [lhs, rhs]) => builtin::lt(ctx, lhs, rhs),
        ("eq", [lhs, rhs]) => builtin::eq(ctx, lhs, rhs),
        ("ne", [lhs, rhs]) => builtin::ne(ctx, lhs, rhs),
        ("ge", [lhs, rhs]) => builtin::ge(ctx, lhs, rhs),
        ("le", [lhs, rhs]) => builtin::le(ctx, lhs, rhs),
        ("array", [size, init]) => builtin::array(ctx, size, init),
        ("append", [target, new]) => builtin::append(ctx, target, new),
        ("capture", []) => builtin::capture_context(ctx),
        ("readline", []) => builtin::read_line(ctx),
        ("env", []) => ctx.get_binding("@env").ok_or(anyhow!("special @env not in context")),
        _ => Err(anyhow!("builtin {name}")),
    }
}

