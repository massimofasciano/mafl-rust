use crate::{expression::{ExpressionType, Expression, self, Operator, BlockType}, builtin::{self}, context::Context, Interpreter};
use anyhow::{Result,anyhow};
use log::debug;

impl Interpreter {

    pub fn eval(&self, ctx: &Context, ast: &Expression) -> Result<Expression> {
        Ok(match ast.as_ref() {
            ExpressionType::Nil |
            ExpressionType::Integer(_) |
            ExpressionType::Float(_) |
            ExpressionType::Character(_) |
            ExpressionType::Boolean(_) |
            ExpressionType::String(_)
                => ast.to_owned(),

            ExpressionType::Break(br) =>
                ExpressionType::Break(self.eval(ctx, br)?).into(),
            ExpressionType::EndBlock(br) =>
                ExpressionType::EndBlock(self.eval(ctx, br)?).into(),
            ExpressionType::Return(br) =>
                ExpressionType::Return(self.eval(ctx, br)?).into(),
            ExpressionType::Throw(br) =>
                ExpressionType::Throw(self.eval(ctx, br)?).into(),
            ExpressionType::Continue => ExpressionType::Continue.into(),

            ExpressionType::Block{r#type: block_type, body: exprs} => {
                let block_ctx = match block_type {
                    BlockType::Sequence => ctx.to_owned(),
                    BlockType::Block | BlockType::If | BlockType::Function =>
                        ctx.with_new_context(),
                };
                let mut last_value = expression::nil();
                for expr in exprs {
                    last_value = self.eval(&block_ctx,expr)?;
                    match block_type {
                        BlockType::Sequence | BlockType::Block => {
                            match last_value.as_ref() {
                                ExpressionType::Return(_) |
                                ExpressionType::Break(_) |
                                ExpressionType::Throw(_) |
                                ExpressionType::Continue => break,
                                ExpressionType::EndBlock(val) => {
                                    last_value = val.to_owned();
                                    break
                                }
                                _ => {},
                            }
                        }
                        BlockType::If => {
                            match last_value.as_ref() {
                                ExpressionType::EndBlock(_) |
                                ExpressionType::Return(_) |
                                ExpressionType::Break(_) |
                                ExpressionType::Throw(_) |
                                ExpressionType::Continue => break,
                                _ => {},
                            }
                        }
                        BlockType::Function => {
                            match last_value.as_ref() {
                                ExpressionType::Throw(_) => break,
                                ExpressionType::Break(val) |
                                ExpressionType::EndBlock(val) |
                                ExpressionType::Return(val) => {
                                    last_value = val.to_owned();
                                    break
                                }
                                ExpressionType::Continue => {
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

            ExpressionType::BindIn(id, value, container) => {
                let container = self.eval(ctx,container)?;
                let value = self.eval(ctx,value)?;
                match container.as_ref() {
                    ExpressionType::Closure(cctx,_,_) => {
                        cctx.add_binding(id.to_owned(), value.to_owned());
                        value.to_owned()
                    }
                    _ => Err(anyhow!("bind {id}"))?,
                }
            }
            ExpressionType::Let(id, val) => {
                let val = self.eval(ctx,val)?;
                ctx.add_binding(id.to_owned(), val.to_owned());
                val
            }
            ExpressionType::LetArray(ids, val) => {
                let val = self.eval(ctx,val)?;
                match val.as_ref() {
                    ExpressionType::Array(rc) => {
                        let vals = rc.borrow().to_owned();
                        for (var, val) in ids.iter().zip(vals) {
                            self.eval(ctx, &ExpressionType::Let(var.to_owned(), val).into())?;
                        }
                        val.to_owned()
                    }
                    _ => {
                        Err(anyhow!("let array (destructure) on non-array"))?
                    }
                }
            }
            ExpressionType::ArrayAccess(target, index) => {
            let target = self.eval(ctx, target)?;
                match target.as_ref() {
                    ExpressionType::Array(arr) => {
                        if let ExpressionType::Integer(index) = self.eval(ctx, index)?.as_ref() {
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
                    ExpressionType::String(s) => {
                        if let ExpressionType::Integer(index) = self.eval(ctx, index)?.as_ref() {
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
            ExpressionType::AssignToDeRefExpression(target, val) => {
                let deref = self.eval(ctx,target)?;
                let val = self.eval(ctx,val)?;
                match deref.as_ref() {
                    ExpressionType::Ref(mc) => {
                        mc.set(val.to_owned());
                        val
                    }
                    _ => Err(anyhow!("deref on non-ref"))?
                }
            }
            ExpressionType::OpAssignToExpression(op, target, val) => {
                self.eval(ctx,&ExpressionType::AssignToExpression(target.to_owned(), 
                    ExpressionType::BinOpCall(op.to_owned(), target.to_owned(), val.to_owned()).into()
                ).into())?
            }
            ExpressionType::AssignToExpression(target, val) => {
                let val = self.eval(ctx,val)?;
                match target.as_ref() {
                    ExpressionType::Variable(id) => {
                        debug!("eval assign to identifier: {id}");
                        let val = self.eval(ctx,&val)?;
                        if ctx.set_binding(id.to_owned(), val.to_owned()).is_none() {
                            Err(anyhow!("binding not found {id}"))?
                        } else {
                            val
                        }
                    },
                    ExpressionType::Field(target, field) => {
                        debug!("eval assign to dict/object with key/field {field}");
                        let target = self.eval(ctx, target)?;
                        match target.as_ref() {
                            ExpressionType::Closure(closure_ctx, _arg_names, _body) => {
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
                    ExpressionType::ArrayAccess(target, index) => {
                        debug!("eval assign to array with index {index}");
                        let target = self.eval(ctx, target)?;
                        match target.as_ref() {
                            ExpressionType::Array(arr) => {
                                if let ExpressionType::Integer(index) = self.eval(ctx, index)?.as_ref() {
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
            ExpressionType::BuiltinVariable(name) => {
                if let Some(result) = self.builtin_var(ctx, name) {
                    result?
                } else {
                    ExpressionType::BuiltinFunction(name.to_owned()).into()
                }
            }
            ExpressionType::Variable(s) => {
                if let Some(value) = ctx.get_binding(s) {
                    value.to_owned()
                } else {
                    Err(anyhow!("binding not found: {s}"))?
                }
            }
            ExpressionType::If(cond, then, r#else) => {
                let cond = self.eval(ctx,cond)?;
                match cond.as_ref() {
                    ExpressionType::Boolean(b) =>
                        if *b { self.eval(ctx,then)? } 
                        else  { self.eval(ctx,r#else)? }
                    _ => ast.to_error()?,
                }
            }

            ExpressionType::Loop(body) => {
                let mut body_value;
                let ctx = &ctx.with_new_context();
                #[allow(clippy::while_let_loop)]
                loop {
                    body_value = self.eval(ctx,body)?;
                    match body_value.as_ref() {
                        ExpressionType::EndBlock(val) |
                        ExpressionType::Break(val) => {
                            body_value = val.to_owned();
                            break
                        }
                        ExpressionType::Continue => continue,
                        ExpressionType::Return(_) | 
                        ExpressionType::Throw(_) => break,
                        _ => {},
                    }
                }
                body_value
            }

            ExpressionType::Iterate(var, iterator, body) => {
                let iterator = self.eval(ctx, iterator)?;
                let mut body_value = expression::nil();
                let ctx = &ctx.with_new_context();
                ctx.add_binding(var.to_owned(), expression::nil());
                match self.eval(ctx,&iterator)?.as_ref() {
                    ExpressionType::Array(arr) => {
                        for v in arr.borrow().iter() {
                            ctx.set_binding(var.to_owned(), v.to_owned());
                            body_value = self.eval(ctx,body)?;
                            match body_value.as_ref() {
                                ExpressionType::EndBlock(val) |
                                ExpressionType::Break(val) => {
                                    body_value = val.to_owned();
                                    break
                                }
                                ExpressionType::Continue => continue,
                                ExpressionType::Return(_) | 
                                ExpressionType::Throw(_) => break,
                                _ => {},
                            }
                        }
                    },
                    ExpressionType::Closure(_,_,_) => {
                        loop {
                            let apply = ExpressionType::FunctionCall(iterator.to_owned(), vec![]).into();
                            let next = self.eval(ctx, &apply)?;
                            if next == expression::nil() { break; }
                            ctx.set_binding(var.to_owned(), next.to_owned());
                            body_value = self.eval(ctx,body)?;
                            match body_value.as_ref() {
                                ExpressionType::EndBlock(val) |
                                ExpressionType::Break(val) => {
                                    body_value = val.to_owned();
                                    break
                                }
                                ExpressionType::Continue => continue,
                                ExpressionType::Return(_) | 
                                ExpressionType::Throw(_) => break,
                                _ => {},
                            }
                        }
                    },
                    _ => return ast.to_error(),
                }
                body_value
            }
            ExpressionType::TryCatch(expr, var, body) => {
                let result = self.eval(ctx, expr);
                match result {
                    Err(err) => {
                        let ctx = &ctx.with_new_context();
                        ctx.add_binding(var.to_owned(), ExpressionType::Error(err.to_string()).into());
                        self.eval(ctx,body)?
                    }
                    Ok(expr) => {
                        match expr.as_ref() {
                            ExpressionType::Throw(val) => {
                                let ctx = &ctx.with_new_context();
                                ctx.add_binding(var.to_owned(), val.to_owned());
                                self.eval(ctx,body)?
                            }
                            _ => expr,     
                        }
                    }
                }
            }
            ExpressionType::Field(target, field) => {
                let target = self.eval(ctx, target)?;
                match target.as_ref() {
                    ExpressionType::Closure(closure_ctx, _arg_names, _body) => {
                        if let Some(value) = closure_ctx.get_binding(field) {
                            value.to_owned()
                        } else {
                            Err(anyhow!("field binding not found: {field}"))?
                        }
                    }
                    _ => Err(anyhow!("field lookup on non-object/dict: {field}"))?,
                }
            }
            ExpressionType::Fun(arg_names, capture_pairs, 
                    self_names, persist, 
                    body) => {
                let open_vars = self.open(&ctx.capture(), ast)?;
                let captured = Context::new();
                for open_var in open_vars {
                    println!("*** capturing {}",open_var);
                    if let Some(mc) = ctx.get_binding_ref(&open_var) { 
                        captured.add_binding_ref(open_var.to_owned(), mc); 
                    } else {
                        Err(anyhow!("binding not found: {open_var}"))?;
                    };
                }
                for (alias, var) in capture_pairs {
                    if let Some(mc) = ctx.get_binding_ref(var) { 
                        captured.add_binding_ref(alias.to_owned(), mc); 
                    } else {
                        Err(anyhow!("binding not found: {var}"))?;
                    };
                }
                for (alias, value) in persist {
                    let value = self.eval(ctx, value)?;
                    captured.add_binding(alias.to_owned(), value); 
                }
                let closure : Expression = ExpressionType::Closure(captured.to_owned(), arg_names.to_owned(), body.to_owned()).into();
                for name in self_names {
                   captured.add_binding(name.to_owned(), closure.to_owned()); 
                }
                closure
            }
            ExpressionType::FunctionCall(lambda, arg_values) => {
                match self.eval(ctx,lambda)?.as_ref() {
                    ExpressionType::BuiltinFunction(name) => {
                        let mut eval_args = vec![];
                        for v in arg_values.iter() {
                            let value = self.eval(ctx,v)?;
                            // propagate exception
                            if let ExpressionType::Throw(val) = value.as_ref() {
                                return Ok(ExpressionType::Throw(val.to_owned()).into());
                            }
                            eval_args.push(value);
                        }
                        self.builtin_fn(ctx, name, &eval_args)?
                    }
                    ExpressionType::Closure(closure_ctx, arg_names, body) => {
                        // dynamic binding (default to global)
                        // let function_ctx = ctx.with_context(closure_ctx.to_owned());
                        // lexical binding (default to empty)
                        let function_ctx = Context::new().with_context(closure_ctx.to_owned());
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            let value = self.eval(ctx,value)?;
                            // propagate exception
                            if let ExpressionType::Throw(val) = value.as_ref() {
                                return Ok(ExpressionType::Throw(val.to_owned()).into());
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
                            self.eval(&function_ctx,&ExpressionType::FunctionCall(uncurried,arg_values[arg_names.len()..].to_vec()).into())?
                        } else {
                            self.eval(&function_ctx,body)?
                        }
                    },
                    _ => ast.to_error()?
                }
            }
            ExpressionType::Object(body) => {
                let ctx = &ctx.with_new_context();
                self.eval(ctx, body)?;
                // we capture the context from the body in a closure
                let cap_ctx = ctx.capture();
                expression::context(cap_ctx.to_owned())
            }
            ExpressionType::Context(arg_names, body) => {
                let local_ctx = Context::new();
                for name in arg_names {
                    if let Some(val) = ctx.get_binding(name) { 
                        local_ctx.add_binding(name.to_owned(), val.to_owned()); 
                    }
                }
                self.eval(&local_ctx, body)?
            }
            ExpressionType::Module(modname, arg_names, body) => {
                let local_ctx = Context::new();
                for name in arg_names {
                    if let Some(val) = ctx.get_binding(name) { 
                        local_ctx.add_binding(name.to_owned(), val.to_owned()); 
                    }
                }
                let val = self.eval(&local_ctx, body)?;
                let captured = builtin::capture_context(&local_ctx)?;
                ctx.add_binding(modname.to_owned(), captured);
                val
            }
            ExpressionType::BinOpCall(op, left, right) => {
                if let Operator::And = op {
                    return builtin::and_lazy(self,ctx,left,right);
                }
                if let Operator::Or = op {
                    return builtin::or_lazy(self,ctx,left,right);
                }
                let left = self.eval(ctx,left)?;
                let right = self.eval(ctx,right)?;
                // propagate exception
                if let ExpressionType::Throw(val) = left.as_ref() {
                    return Ok(ExpressionType::Throw(val.to_owned()).into());
                }
                // propagate exception
                if let ExpressionType::Throw(val) = right.as_ref() {
                    return Ok(ExpressionType::Throw(val.to_owned()).into());
                }
                match op {
                    Operator::Identifier(fname) => {
                        let fvar = ExpressionType::Variable(fname.to_owned()).into();
                        self.eval(ctx,&ExpressionType::FunctionCall(fvar, vec![left,right]).into())?
                    },
                    Operator::Pipe => self.eval(ctx,&ExpressionType::FunctionCall(right, vec![left]).into())?,
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
            ExpressionType::UnaryOpCall(op, expr) => {
                if let Operator::Ref = op {
                    return builtin::get_ref(self, ctx,&expr.to_owned());
                }
                if let Operator::Exclam = op {
                    // unwraps a thrown value
                    let result = self.eval(ctx, expr);
                    return Ok(match result {
                        Err(err) => {
                            ExpressionType::Error(err.to_string()).into()
                        }
                        Ok(expr) => {
                            match expr.as_ref() {
                                ExpressionType::Throw(val) => {
                                    val.to_owned()
                                }
                                _ => expr,     
                            }
                        }
                    })
                }
                let expr = self.eval(ctx,expr)?;
                if let ExpressionType::Throw(val) = expr.as_ref() {
                    return Ok(val.to_owned());
                }
                match op {
                    Operator::Neg => builtin::neg(ctx,&expr)?,
                    Operator::Not => builtin::not(ctx,&expr)?,
                    Operator::DeRef => {
                        match expr.as_ref() {
                            ExpressionType::Ref(rc) => {
                                rc.get()
                            }
                            _ => Err(anyhow!("deref on non-ref"))?
                        }
                    }
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::Closure(cctx, args, body) => {
                ExpressionType::Closure(cctx.capture(), args.to_owned(), body.to_owned()).into()
            } 
            ExpressionType::Array(vals) => {
                expression::array({
                    let mut new = vec![];
                    for v in vals.borrow().iter() {
                        let value = self.eval(ctx,v)?;
                        // propagate exception
                        if let ExpressionType::Throw(val) = value.as_ref() {
                            return Ok(ExpressionType::Throw(val.to_owned()).into());
                        }
                        new.push(value);
                    }
                    new
                })
            }
            _ => ast.to_error()?,
        })
    }

    pub fn builtin_var(&self, ctx: &Context, name: &str) -> Option<Result<Expression>> {
        match name {
            "env" => Some(Ok(self.env.to_owned())),
            "std" => { Some(Ok(self.std.to_owned())) }
            "self" => Some(Ok(expression::context(ctx.to_owned()))),
            _ => None,
        }
    }

    pub fn builtin_fn(&self, ctx: &Context, name: &str, args: &[Expression]) -> Result<Expression> {
        match (name, args) {
            ("call", [callable, args]) => { builtin::call(self, ctx, callable, args) },
            ("println", args) => { builtin::println(self, ctx, args) },
            ("print", args) => { builtin::print(self, ctx, args) },
            ("debugln", args) => { builtin::debugln(ctx, args) },
            ("debug", args) => { builtin::debug(ctx, args) },
            ("error", args) => { builtin::error_from_strings(ctx, args) },
            ("make_error", [msg]) => { builtin::make_error(msg) },
            ("eval", [arg]) => { builtin::eval_string_as_source(self, ctx, arg) },
            ("include", [file_expr]) => builtin::include(self, ctx, file_expr),
            ("readfile", [file_expr]) => builtin::read_file(ctx, file_expr),
            ("pow", [lhs, rhs]) => builtin::pow(ctx, lhs, rhs),
            ("exp", [val]) => builtin::exp(ctx, val),
            ("log", [val]) => builtin::log(ctx, val),
            ("add", [lhs, rhs]) => builtin::add(ctx, lhs, rhs),
            ("sub", [lhs, rhs]) => builtin::sub(ctx, lhs, rhs),
            ("mul", [lhs, rhs]) => builtin::mul(ctx, lhs, rhs),
            ("div", [lhs, rhs]) => builtin::div(ctx, lhs, rhs),
            ("intdiv", [lhs, rhs]) => builtin::intdiv(ctx, lhs, rhs),
            ("mod", [lhs, rhs]) => builtin::modulo(ctx, lhs, rhs),
            ("neg", [val]) => builtin::neg(ctx, val),
            ("not", [val]) => builtin::not(ctx, val),
            ("len", [val]) => builtin::len(ctx, val),
            ("is_error", [expr]) => builtin::is_error(self, ctx, expr),
            ("is_ref", [expr]) => builtin::is_ref(self, ctx, expr),
            ("integer", [val]) => builtin::integer(self, ctx, val),
            ("float", [val]) => builtin::float(self, ctx, val),
            ("string", [val]) => builtin::string(self, ctx, val),
            ("type", [val]) => builtin::type_of(ctx, val),
            ("and_eager", [lhs, rhs]) => builtin::and(ctx, lhs, rhs),
            ("or_eager", [lhs, rhs]) => builtin::or(ctx, lhs, rhs),
            ("gt", [lhs, rhs]) => builtin::gt(ctx, lhs, rhs),
            ("lt", [lhs, rhs]) => builtin::lt(ctx, lhs, rhs),
            ("eq", [lhs, rhs]) => builtin::eq(ctx, lhs, rhs),
            ("ne", [lhs, rhs]) => builtin::ne(ctx, lhs, rhs),
            ("ge", [lhs, rhs]) => builtin::ge(ctx, lhs, rhs),
            ("le", [lhs, rhs]) => builtin::le(ctx, lhs, rhs),
            ("array", [size, init]) => builtin::array(self, ctx, size, init),
            ("array", [init]) => builtin::to_array(ctx, init),
            ("slice", [container, start, end]) => builtin::slice(ctx, container, start, end),
            ("copy", [container]) => builtin::shallow_copy(ctx, container),
            ("clone", [container]) => builtin::deep_copy(ctx, container),
            ("lines", [init]) => builtin::to_array_lines(ctx, init),
            ("append", [target, new]) => builtin::append(self, ctx, target, new),
            ("readline", []) => builtin::read_line(ctx),
            ("get", [container, key]) => builtin::get(self, ctx, container, key),
            ("set", [container, key, value]) => builtin::set(self, ctx, container, key, value),
            ("insert", [container, key, value]) => builtin::insert(self, ctx, container, key, value),
            ("bind", [container, key, value]) => builtin::insert(self, ctx, container, key, value),
            ("dict", [parent]) => builtin::dict_extend(ctx, parent),
            ("dict", []) => builtin::dict(ctx),
            ("var", [key]) => builtin::get_var(self, ctx, key),
            ("assign", [key, value]) => builtin::assign_var(self, ctx, key, value),
            ("let", [key, value]) => builtin::let_var(self, ctx, key, value),
            ("test", [source, expected]) => builtin::test(self, ctx, source, expected),
            ("now", []) => builtin::now(ctx),
            ("sleep", [seconds]) => builtin::sleep(ctx, seconds),
            ("context", []) => builtin::capture_context(ctx),
            ("split", [source, pattern]) => builtin::split(self, ctx, source, pattern),
            ("trim", [source]) => builtin::trim(self, ctx, source),
            ("matches", [source, regex]) => builtin::matches(self, ctx, source, regex),
            ("command", cmd_args) if cmd_args.len() > 1 => { 
                builtin::command(self, ctx, &cmd_args[0], &cmd_args[1..]) 
            },
            ("sort", [target]) => builtin::sort(self, ctx, target, None),
            ("sort", [target, compare]) => builtin::sort(self, ctx, target, Some(compare)),
            _ => Err(anyhow!("builtin {name}")),
        }
    }

}
