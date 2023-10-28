use crate::{parse_string_to_ast, unescape_string, types::{Value, Context, Ast, AstAtom}};

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
            let val = ctx.get(s).unwrap_or(&Value::from(ast)).to_owned();
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
                _ => (ctx, Value::from(ast)),
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
                _ => (ctx, Value::from(ast)),
            }
        }
        Ast::Function(arg_names, body) => {
            (ctx.clone(), Value::Closure(ctx, arg_names.to_owned(), body.to_owned()))
        }
        Ast::FunctionCall(lambda, arg_values) => {
            let lambda = eval(ctx.clone(), lambda).1;
            match lambda {
                Value::Function(arg_names, body) => {
                    let fctx = arg_names.iter().zip(arg_values).fold(ctx.clone(), |mut nctx,(name,value)| {
                        nctx.insert(name.to_owned(), eval(ctx.clone(),value).1);
                        nctx
                    });
                    (ctx,eval(fctx,&body).1)
                },
                Value::Closure(cctx, arg_names, body) => {
                    let mut clctx = ctx.clone();
                    clctx.extend(cctx);
                    let fctx = arg_names.iter().zip(arg_values).fold(clctx, |mut nctx,(name,value)| {
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
            let aleft = AstAtom::from(left.to_owned());
            let aright = AstAtom::from(right.to_owned());
            (ctx, match op {
                Ast::AddOp => left+right,
                Ast::SubOp => left-right,
                Ast::MultOp => left*right,
                Ast::ExpOp => left.pow(right),
                Ast::GtOp => Value::Boolean(aleft>aright),
                Ast::GeOp => Value::Boolean(aleft>=aright),
                Ast::LtOp => Value::Boolean(aleft<aright),
                Ast::LeOp => Value::Boolean(aleft<=aright),
                Ast::EqOp => Value::Boolean(aleft==aright),
                Ast::NeOp => Value::Boolean(aleft!=aright),
                _ => Value::from(ast),
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
                _ => (ctx, Value::from(ast)),
            }
        }
        _ => (ctx, Value::from(ast)),
    }
}

