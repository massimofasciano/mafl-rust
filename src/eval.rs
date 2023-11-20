use std::io::{stdout, Write};

use crate::{Ptr, expression::{self, Expr, Operator, BlockType}, builtin::{self}, context::Context, Interpreter, PragmaLevel};
use anyhow::{Result,anyhow};
use log::debug;

impl Interpreter {

    pub fn eval(&self, ctx: &Context, ast: &Ptr<Expr>) -> Result<Ptr<Expr>> {
        Ok(match ast.as_ref() {
            Expr::Ref(_) |
            Expr::Dyn(_,_,_) |
            Expr::Nil |
            Expr::Integer(_) |
            Expr::Float(_) |
            Expr::Character(_) |
            Expr::Boolean(_) |
            Expr::String(_)
                => ast.to_owned(),

            Expr::Break(br) =>
                Expr::Break(self.eval(ctx, br)?).into(),
            Expr::Exit(br) =>
                Expr::Exit(self.eval(ctx, br)?).into(),
            Expr::Return(br) =>
                Expr::Return(self.eval(ctx, br)?).into(),
            Expr::Throw(e) => {
                let exception = self.eval(ctx, e)?;
                if let Expr::Error(msg) = exception.as_ref() {
                    Err(anyhow!(msg.to_owned()))?
                } else {
                    Expr::Throw(e.to_owned()).into()
                }
            }
            Expr::Continue => Expr::Continue.into(),

            Expr::Block{r#type: block_type, body: exprs} => {
                let block_ctx = match block_type {
                    BlockType::Sequence | BlockType::Function => ctx.to_owned(),
                    BlockType::Block | BlockType::If =>
                        ctx.with_new_context(),
                };
                let mut last_value = expression::nil();
                for expr in exprs {
                    last_value = self.eval(&block_ctx,expr)?;
                    match block_type {
                        BlockType::Sequence | BlockType::Block => {
                            match last_value.as_ref() {
                                Expr::Return(_) |
                                Expr::Break(_) |
                                Expr::Throw(_) |
                                Expr::Continue => break,
                                Expr::Exit(val) => {
                                    last_value = val.to_owned();
                                    break
                                }
                                _ => {},
                            }
                        }
                        BlockType::If => {
                            match last_value.as_ref() {
                                Expr::Exit(_) |
                                Expr::Return(_) |
                                Expr::Break(_) |
                                Expr::Throw(_) |
                                Expr::Continue => break,
                                _ => {},
                            }
                        }
                        BlockType::Function => {
                            match last_value.as_ref() {
                                Expr::Throw(_) => break,
                                Expr::Break(val) |
                                Expr::Exit(val) |
                                Expr::Return(val) => {
                                    last_value = val.to_owned();
                                    break
                                }
                                Expr::Continue => {
                                    last_value = expression::nil();
                                    break
                                }
                                _ => {},
                            }
                        }
                    }
                }
                last_value
            }

            Expr::Forget(ids) => {
                for id in ids {
                    ctx.remove_binding(id).ok_or(anyhow!("binding not found: {id}"))?;
                }
                expression::nil()
            }

            Expr::LetRef(id, expr) => {
                let expr_ref = builtin::get_ref(self, ctx,&expr.to_owned())?;
                match expr_ref.as_ref() {
                    Expr::Ref(rc) => {
                        ctx.add_binding_ref(id.to_owned(), rc.to_owned());
                        rc.get()
                    }
                    _ => Err(anyhow!("invalid alias expression"))?
                }
            }

            Expr::Let(id, val) => {
                let val = self.eval(ctx,val)?;
                let replaced = ctx.add_binding(id.to_owned(), val.to_owned());
                if replaced.is_some() {
                    let msg = format!("shadowed {id} in local context");
                    match *self.pragma_shadow_local.borrow() {
                        PragmaLevel::Allow => {},
                        PragmaLevel::Warn => { eprintln!("# warning: {msg}.") },
                        PragmaLevel::Error => { Err(anyhow!(msg))?},
                    }
                }
                val
            }

            Expr::LetArray(ids, val) => {
                let val = self.eval(ctx,val)?;
                match val.as_ref() {
                    Expr::Array(rc) => {
                        let vals = rc.borrow().to_owned();
                        for (var, val) in ids.iter().zip(vals) {
                            self.eval(ctx, &Expr::Let(var.to_owned(), val).into())?;
                        }
                        val.to_owned()
                    }
                    _ => {
                        Err(anyhow!("let array (destructure) on non-array"))?
                    }
                }
            }
            Expr::ArrayAccess(target, index) => {
            let target = self.eval(ctx, target)?;
                match target.as_ref() {
                    Expr::Array(arr) => {
                        if let Expr::Integer(index) = self.eval(ctx, index)?.as_ref() {
                            let index = (if *index >= 0 { *index } else { arr.borrow().len() as i64 + *index}) as usize;
                            if let Some(result) = arr.borrow().get(index) {
                                result.to_owned()
                            } else {
                                Err(anyhow!("index out of bounds: {index}"))?
                            }
                        } else {
                            Err(anyhow!("index by non-integer"))?
                        }
                    }
                    Expr::String(s) => {
                        if let Expr::Integer(index) = self.eval(ctx, index)?.as_ref() {
                            let index = (if *index >= 0 { *index } else { s.len() as i64 + *index}) as usize;
                            if let Some(result) = s.chars().nth(index) {
                                expression::character(result)
                            } else {
                                Err(anyhow!("index out of bounds: {index}"))?
                            }
                        } else {
                            Err(anyhow!("index by non-integer"))?
                        }
                    }
                    _ => Err(anyhow!("index on non-array/string"))?
                }
            }
            Expr::AssignToDeRefExpression(target, val) => {
                let deref = self.eval(ctx,target)?;
                let val = self.eval(ctx,val)?;
                match deref.as_ref() {
                    Expr::Ref(mc) => {
                        mc.set(val.to_owned());
                        val
                    }
                    _ => Err(anyhow!("deref on non-ref"))?
                }
            }
            Expr::OpAssignToExpression(op, target, val) => {
                self.eval(ctx,&Expr::AssignToExpression(target.to_owned(), 
                    Expr::BinOpCall(op.to_owned(), target.to_owned(), val.to_owned()).into()
                ).into())?
            }
            Expr::AssignToExpression(target, val) => {
                let val = self.eval(ctx,val)?;
                match target.as_ref() {
                    Expr::Variable(id) => {
                        let val = self.eval(ctx,&val)?;
                        if ctx.set_binding(id.to_owned(), val.to_owned()).is_none() {
                            Err(anyhow!("binding not found {id}"))?
                        } else {
                            val
                        }
                    },
                    Expr::Field(target, field) => {
                        let target = self.eval(ctx, target)?;
                        match target.as_ref() {
                            Expr::Closure(closure_ctx, _arg_names, _body) => {
                                if let Some(value) = closure_ctx.get_binding_ref(field) {
                                    let mut cell_mut = value.get_refmut();
                                    *cell_mut = val;
                                    cell_mut.to_owned()
                                } else {
                                    Err(anyhow!("field binding not found {field}"))?
                                }
                            }
                            _ => ast.to_error()?,
                        }
                    },
                    Expr::ArrayAccess(target, index) => {
                        let target = self.eval(ctx, target)?;
                        match target.as_ref() {
                            Expr::Array(arr) => {
                                if let Expr::Integer(index) = self.eval(ctx, index)?.as_ref() {
                                    let index = (if *index >= 0 { *index } else { arr.borrow().len() as i64 + *index}) as usize;
                                    if let Some(result) = arr.borrow_mut().get_mut(index) {
                                        *result = val;
                                        result.to_owned()
                                    } else {
                                        Err(anyhow!("index out of bounds: {index}"))?
                                    }
                                } else {
                                    Err(anyhow!("index assign by non-integer"))?
                                }
                            }
                        _ => Err(anyhow!("index assign on non-array"))?
                        }
                    }
                    _ => ast.to_error()?,
                }
            }
            Expr::BuiltinVariable(name) => {
                if let Some(result) = self.builtin_var(ctx, name) {
                    result?
                } else {
                    Expr::BuiltinFunction(name.to_owned()).into()
                }
            }
            Expr::Variable(s) => {
                if let Some(value) = ctx.get_binding(s) {
                    value.to_owned()
                } else {
                    Err(anyhow!("binding not found: {s}"))?
                }
            }
            Expr::If(cond, then, r#else) => {
                let cond = self.eval(ctx,cond)?;
                match cond.as_ref() {
                    Expr::Boolean(b) =>
                        if *b { self.eval(ctx,then)? } 
                        else  { self.eval(ctx,r#else)? }
                    _ => ast.to_error()?,
                }
            }

            Expr::Loop(body) => {
                let mut body_value;
                let ctx = &ctx.with_new_context();
                #[allow(clippy::while_let_loop)]
                loop {
                    body_value = self.eval(ctx,body)?;
                    match body_value.as_ref() {
                        Expr::Exit(val) |
                        Expr::Break(val) => {
                            body_value = val.to_owned();
                            break
                        }
                        Expr::Continue => continue,
                        Expr::Return(_) | 
                        Expr::Throw(_) => break,
                        _ => {},
                    }
                }
                body_value
            }

            Expr::Iterate(var, iterator, body) => {
                let iterator = self.eval(ctx, iterator)?;
                let mut body_value = expression::nil();
                let ctx = &ctx.with_new_context();
                ctx.add_binding(var.to_owned(), expression::nil());
                match self.eval(ctx,&iterator)?.as_ref() {
                    Expr::Array(arr) => {
                        for v in arr.borrow().iter() {
                            ctx.set_binding(var.to_owned(), v.to_owned());
                            body_value = self.eval(ctx,body)?;
                            match body_value.as_ref() {
                                Expr::Exit(val) |
                                Expr::Break(val) => {
                                    body_value = val.to_owned();
                                    break
                                }
                                Expr::Continue => continue,
                                Expr::Return(_) | 
                                Expr::Throw(_) => break,
                                _ => {},
                            }
                        }
                    },
                    Expr::Closure(_,_,_) => {
                        loop {
                            let apply = Expr::FunctionCall(iterator.to_owned(), vec![]).into();
                            let next = self.eval(ctx, &apply)?;
                            if next == expression::nil() { break; }
                            ctx.set_binding(var.to_owned(), next.to_owned());
                            body_value = self.eval(ctx,body)?;
                            match body_value.as_ref() {
                                Expr::Exit(val) |
                                Expr::Break(val) => {
                                    body_value = val.to_owned();
                                    break
                                }
                                Expr::Continue => continue,
                                Expr::Return(_) | 
                                Expr::Throw(_) => break,
                                _ => {},
                            }
                        }
                    },
                    _ => return ast.to_error(),
                }
                body_value
            }
            Expr::TryCatch(expr, var, body) => {
                let result = self.eval(ctx, expr);
                match result {
                    Err(err) => {
                        let ctx = &ctx.with_new_context();
                        ctx.add_binding(var.to_owned(), Expr::Error(err.to_string()).into());
                        self.eval(ctx,body)?
                    }
                    Ok(expr) => {
                        match expr.as_ref() {
                            Expr::Throw(val) => {
                                let ctx = &ctx.with_new_context();
                                ctx.add_binding(var.to_owned(), val.to_owned());
                                self.eval(ctx,body)?
                            }
                            _ => expr,     
                        }
                    }
                }
            }
            Expr::Field(target, field) => {
                let target = self.eval(ctx, target)?;
                let target_type = if let Expr::String(t) = builtin::type_of(self, ctx, &target)?.as_ref() {
                    t.to_owned()
                } else {
                    Err(anyhow!("type_of always returns a string"))?
                };
                match target.as_ref() {
                    // if a closure object, get the field member from it
                    Expr::Closure(closure_ctx, _arg_names, _body) => {
                        if let Some(value) = closure_ctx.get_binding(field) {
                            value.to_owned()
                        } else {
                            Err(anyhow!("field binding not found: {field}"))?
                        }
                    }
                    // otherwise, if the target is of type T, call @std.methods.T.field(target)
                    // ex: [1,2,3].map(\x{x+1}) => @std.methods.Array.map([1,2,3])(\x{x+1})
                    // the functions in the methods modules must have their arguments in the proper order 
                    _ => {
                        Expr::Closure(Context::new(), vec![],
                            Expr::FunctionCall(
                                Expr::Field(
                                    Expr::Field(
                                        Expr::Field(
                                            Expr::BuiltinVariable("std".to_owned()).into(), 
                                            "methods".to_owned(),
                                        ).into(),
                                        target_type,
                                    ).into(),
                                    field.to_owned(),
                                ).into(),
                                vec![target],
                            ).into()
                        ).into()
                    }
                }
            }
            Expr::Fun(arg_names, body) => {
                let open_vars = self.open(&ctx.capture(), ast)?;
                let captured = Context::new();
                for open_var in open_vars {
                    debug!("*** fun capturing {}",open_var);
                    if let Some(mc) = ctx.get_binding_ref(&open_var) { 
                        captured.add_binding_ref(open_var.to_owned(), mc); 
                    } else {
                        Err(anyhow!("binding not found: {open_var}"))?;
                    };
                }
                let closure : Ptr<Expr> = Expr::Closure(captured.to_owned(), arg_names.to_owned(), body.to_owned()).into();
                closure
            }

            Expr::FunctionCall(fnct, arg_values) => {
                match self.eval(ctx,fnct)?.as_ref() {
                    Expr::BuiltinFunction(name) => {
                        let mut eval_args = vec![];
                        for v in arg_values.iter() {
                            let value = self.eval(ctx,v)?;
                            // propagate exception
                            if let Expr::Throw(val) = value.as_ref() {
                                return Ok(Expr::Throw(val.to_owned()).into());
                            }
                            eval_args.push(value);
                        }
                        self.builtin_fn(ctx, name, &eval_args)?
                    }
                    Expr::Closure(closure_ctx, arg_names, body) => {
                        // lexical binding (using closure context)
                        let function_ctx = closure_ctx.flatten_ref();
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            let value = self.eval(ctx,value)?;
                            // propagate exception
                            if let Expr::Throw(val) = value.as_ref() {
                                return Ok(Expr::Throw(val.to_owned()).into());
                            }
                            function_ctx.add_binding(name.to_owned(), value.to_owned());
                        }
                        #[allow(clippy::comparison_chain)]
                        if arg_names.len() > arg_values.len() {
                            debug!("performing currying: {arg_names:?} {arg_values:?}");
                            expression::closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body.to_owned())
                        } else if arg_names.len() < arg_values.len() {
                            debug!("extra args supplied: {arg_names:?} {arg_values:?}");
                            let uncurried = self.eval(&function_ctx,body)?;
                            self.eval(&function_ctx,&Expr::FunctionCall(uncurried,arg_values[arg_names.len()..].to_vec()).into())?
                        } else {
                            self.eval(&function_ctx,body)?
                        }
                    },
                    Expr::Dyn(mutates_ctx, arg_names, body) => {
                        // dynamic binding (using global context)
                        let function_ctx = if *mutates_ctx {
                            ctx.to_owned()
                        } else {
                            ctx.with_new_context()
                        };
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            let value = self.eval(ctx,value)?;
                            // propagate exception
                            if let Expr::Throw(val) = value.as_ref() {
                                return Ok(Expr::Throw(val.to_owned()).into());
                            }
                            function_ctx.add_binding(name.to_owned(), value.to_owned());
                        }
                        #[allow(clippy::comparison_chain)]
                        if arg_names.len() > arg_values.len() {
                            debug!("performing currying: {arg_names:?} {arg_values:?}");
                            expression::closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body.to_owned())
                        } else if arg_names.len() < arg_values.len() {
                            debug!("extra args supplied: {arg_names:?} {arg_values:?}");
                            let uncurried = self.eval(&function_ctx,body)?;
                            self.eval(&function_ctx,&Expr::FunctionCall(uncurried,arg_values[arg_names.len()..].to_vec()).into())?
                        } else {
                            self.eval(&function_ctx,body)?
                        }
                    },
                    _ => ast.to_error()?
                }
            }

            Expr::Use(opt_source, members) => {
                if let Some(source) = opt_source {
                    let source = self.eval(ctx, source)?;
                    match source.as_ref() {
                        Expr::Closure(cctx,_,_) => {
                            for var in members {
                                if let Some(rc) = cctx.get_binding_ref(var) {
                                    ctx.add_binding_ref(var.to_owned(), rc.to_owned());
                                } else {
                                    Err(anyhow!("use member not found: {var}"))?
                                }
                            }
                        }
                        _ => Err(anyhow!("use source is not a closure"))?
                    }                        
                } else {
                    for var in members {
                        if let Some(rc) = ctx.get_binding_ref(var) {
                            ctx.add_binding_ref(var.to_owned(), rc.to_owned());
                        } else {
                            Err(anyhow!("use member not found: {var}"))?
                        }
                    }
                }
                expression::nil()
            }

            Expr::Test(source, expr, expected) => {
                let expr = self.eval(ctx, expr)?;
                let expected = self.eval(ctx, expected)?;
                let source = source.trim();
                let test = expr == expected;
                if test {
                    *self.test_pass_count.borrow_mut() += 1;
                } else {
                    *self.test_fail_count.borrow_mut() += 1;
                }
                print!("# test {} {source}", if test {"passed"} else {"failed"});
                if !matches!(expected.as_ref(),Expr::Boolean(true)) {
                    print!(": result ");
                    if test { print!("{expected}"); } 
                    else { print!("{expr} expected {expected}"); }
                }
                println!();
                stdout().flush()?;
                expression::boolean(test)
            }

            Expr::Closed(vars, body) => {
                let closed_ctx = Context::new();
                for open_var in vars {
                    debug!("*** closed capturing {}",open_var);
                    if let Some(mc) = ctx.get_binding_ref(open_var) { 
                        closed_ctx.add_binding_ref(open_var.to_owned(), mc); 
                    } else {
                        Err(anyhow!("binding not found: {open_var}"))?;
                    };
                }
                self.eval(&closed_ctx,body)?
            }

            Expr::BinOpCall(op, left, right) => {
                if let Operator::And = op {
                    return builtin::and_lazy(self,ctx,left,right);
                }
                if let Operator::Or = op {
                    return builtin::or_lazy(self,ctx,left,right);
                }
                let left = self.eval(ctx,left)?;
                let right = self.eval(ctx,right)?;
                // propagate exception
                if let Expr::Throw(val) = left.as_ref() {
                    return Ok(Expr::Throw(val.to_owned()).into());
                }
                // propagate exception
                if let Expr::Throw(val) = right.as_ref() {
                    return Ok(Expr::Throw(val.to_owned()).into());
                }
                match op {
                    Operator::Identifier(fname) => {
                        let fvar = Expr::Variable(fname.to_owned()).into();
                        self.eval(ctx,&Expr::FunctionCall(fvar, vec![left,right]).into())?
                    },
                    Operator::Pipe => self.eval(ctx,&Expr::FunctionCall(right, vec![left]).into())?,
                    Operator::Add => builtin::add(ctx,&left,&right)?,
                    Operator::Sub => builtin::sub(ctx,&left,&right)?,
                    Operator::Mul => builtin::mul(ctx,&left,&right)?,
                    Operator::Div => builtin::div(ctx,&left,&right)?,
                    Operator::IntDiv => builtin::intdiv(ctx,&left,&right)?,
                    Operator::Mod => builtin::modulo(ctx,&left,&right)?,
                    Operator::Exp => builtin::pow(ctx, &left, &right)?,
                    Operator::Gt => builtin::gt(ctx, &left, &right)?,
                    Operator::Ge => builtin::ge(ctx, &left, &right)?,
                    Operator::Lt => builtin::lt(ctx, &left, &right)?,
                    Operator::Le => builtin::le(ctx, &left, &right)?,
                    Operator::Eq => builtin::eq(ctx, &left, &right)?,
                    Operator::Ne => builtin::ne(ctx, &left, &right)?,
                    _ => ast.to_error()?,
                }
            }
            
            Expr::UnaryOpCall(op, expr) => {
                if let Operator::Ref = op {
                    return builtin::get_ref(self, ctx,&expr.to_owned());
                }
                if let Operator::Exclam = op {
                    // unwraps a thrown value
                    let result = self.eval(ctx, expr);
                    return Ok(match result {
                        Err(err) => {
                            Expr::Error(err.to_string()).into()
                        }
                        Ok(expr) => {
                            match expr.as_ref() {
                                Expr::Throw(val) => {
                                    val.to_owned()
                                }
                                _ => expr,     
                            }
                        }
                    })
                }
                let expr = self.eval(ctx,expr)?;
                if let Expr::Throw(val) = expr.as_ref() {
                    return Ok(val.to_owned());
                }
                match op {
                    Operator::Neg => builtin::neg(ctx,&expr)?,
                    Operator::Not => builtin::not(ctx,&expr)?,
                    Operator::DeRef => {
                        match expr.as_ref() {
                            Expr::Ref(rc) => {
                                rc.get()
                            }
                            _ => Err(anyhow!("deref on non-ref"))?
                        }
                    }
                    _ => ast.to_error()?,
                }
            }
            Expr::Closure(cctx, args, body) => {
                Expr::Closure(cctx.capture(), args.to_owned(), body.to_owned()).into()
            } 
            Expr::Array(vals) => {
                expression::array({
                    let mut new = vec![];
                    for v in vals.borrow().iter() {
                        let value = self.eval(ctx,v)?;
                        // propagate exception
                        if let Expr::Throw(val) = value.as_ref() {
                            return Ok(Expr::Throw(val.to_owned()).into());
                        }
                        new.push(value);
                    }
                    new
                })
            }
            _ => ast.to_error()?,
        })
    }

    pub fn builtin_var(&self, ctx: &Context, name: &str) -> Option<Result<Ptr<Expr>>> {
        match name {
            "args" => Some(Ok(self.args.to_owned())),
            "std" => { Some(Ok(self.std.to_owned())) }
            "self" => Some(Ok(expression::context(ctx.to_owned()))),
            "version" => option_env!("CARGO_PKG_VERSION").map(|s| { Ok(expression::string(s.to_owned())) }),
            "os" => Some(Ok(expression::string(std::env::consts::OS.to_owned()))),
            "test_pass_count" => Some(Ok(expression::integer(*self.test_pass_count.borrow() as i64))),
            "test_fail_count" => Some(Ok(expression::integer(*self.test_fail_count.borrow() as i64))),
            _ => {
                self.builtin_vars.get(name)
                    .map(|f| Ok({
                        f(self, ctx, &[])?
                    }))
            }
        }
    }
    
    pub fn builtin_fn(&self, ctx: &Context, name: &str, args: &[Ptr<Expr>]) -> Result<Ptr<Expr>> {
        match (name, args) {
            ("pragma", [id, val]) => { builtin::pragma(self, ctx, id, val) },
            ("call", [callable, args]) => { builtin::call(self, ctx, callable, args) },
            ("println", args) => { builtin::println(self, ctx, args) },
            ("print", args) => { builtin::print(self, ctx, args) },
            ("debugln", args) => { builtin::debugln(self, ctx, args) },
            ("debug", args) => { builtin::debug(self, ctx, args) },
            ("error", args) => { builtin::error_from_strings(ctx, args) },
            ("make_error", [msg]) => { builtin::make_error(msg) },
            ("eval", [arg]) => { builtin::eval_string_as_source(self, ctx, arg) },
            ("include", [file_expr]) => builtin::include(self, ctx, file_expr),
            ("readfile", [file_expr]) => builtin::read_file(ctx, file_expr),
            ("readline", []) => builtin::read_line(ctx),
            ("pow", [lhs, rhs]) => builtin::pow(ctx, lhs, rhs),
            ("exp", [val]) => builtin::exp(self, ctx, val),
            ("log", [val]) => builtin::log(ctx, val),
            ("len", [val]) => builtin::len(ctx, val),
            ("to_integer", [val]) => builtin::to_integer(self, ctx, val),
            ("to_float", [val]) => builtin::to_float(self, ctx, val),
            ("to_string", [val]) => builtin::to_string(self, ctx, val),
            ("type_of", [val]) => builtin::type_of(self, ctx, val),
            ("make_array", [size, init]) => builtin::make_array(self, ctx, size, init),
            ("to_array", [init]) => builtin::to_array(self, ctx, init),
            ("slice", [container, start, end]) => builtin::slice(ctx, container, start, end),
            ("copy", [container]) => builtin::shallow_copy(ctx, container),
            ("clone", [container]) => builtin::deep_copy(ctx, container),
            ("append", [target, new]) => builtin::append(self, ctx, target, new),
            ("make_dict", []) => builtin::make_dict(ctx),
            ("extend_dict", [parent]) => builtin::extend_dict(ctx, parent),
            ("flatten_dict", [parent]) => builtin::flatten_dict(ctx, parent),
            ("get", [container, key]) => builtin::get(self, ctx, container, key),
            ("set", [container, key, value]) => builtin::set(self, ctx, container, key, value),
            ("insert", [container, key, value]) => builtin::insert(self, ctx, container, key, value),
            ("remove", [container, key]) => builtin::remove(self, ctx, container, key),
            ("now", []) => builtin::now(ctx),
            ("sleep", [seconds]) => builtin::sleep(ctx, seconds),
            ("flatten_self", []) => Ok(expression::context(ctx.flatten_ref())), // returns a flattened version of @self
            ("lines", [init]) => builtin::to_array_lines(ctx, init),
            ("split", [source, pattern]) => builtin::split(self, ctx, source, pattern),
            ("trim", [source]) => builtin::trim(self, ctx, source),
            ("matches", [source, regex]) => builtin::matches(self, ctx, source, regex),
            ("command", cmd_args) if cmd_args.len() > 1 => { 
                builtin::command(self, ctx, &cmd_args[0], &cmd_args[1..]) 
            },
            ("sort", [target]) => builtin::sort(self, ctx, target, None),
            ("sort_by", [compare, target]) => builtin::sort(self, ctx, target, Some(compare)),
            ("randint", [min, max]) => builtin::randint(self, ctx, min, max),
            ("random", []) => builtin::randfloat(self, ctx),
            _ => {
                self.builtin_fns.get(name)
                    .map(|f| Ok({
                        f(self, ctx, args)?
                    }))
                    .ok_or(anyhow!("can't process builtin: {name}"))?
            }
        }
    }

}

