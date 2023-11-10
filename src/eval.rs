use crate::{expression::{ExpressionType, Expression, self}, builtin::{self}, context::Context, Interpreter};
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
            ExpressionType::Return(br) =>
                ExpressionType::Return(self.eval(ctx, br)?).into(),
            ExpressionType::Throw(br) =>
                ExpressionType::Throw(self.eval(ctx, br)?).into(),
            ExpressionType::Continue => ExpressionType::Continue.into(),
            ExpressionType::Block(exprs) => {
                debug!("eval block");
                let ctx = &ctx.with_new_context();
                let mut last_value = expression::nil();
                for expr in exprs {
                    last_value = self.eval(ctx,expr)?;
                    match last_value.as_ref() {
                        ExpressionType::Return(_) |
                        ExpressionType::Break(_) |
                        ExpressionType::Throw(_) |
                        ExpressionType::Continue => break,
                        _ => {},
                    }
                }
                last_value
            }
            ExpressionType::FunctionBlock(exprs) => {
                debug!("eval function block");
                let ctx = &ctx.with_new_context();
                let mut last_value = expression::nil();
                for expr in exprs {
                    last_value = self.eval(ctx,expr)?;
                    match last_value.as_ref() {
                        ExpressionType::Throw(_) => break,
                        ExpressionType::Break(val) |
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
                last_value
            }
            ExpressionType::Sequence(exprs) => {
                debug!("eval sequence");
                let mut last_value = expression::nil();
                for expr in exprs {
                    last_value = self.eval(ctx,expr)?;
                    match last_value.as_ref() {
                        ExpressionType::Return(_) |
                        ExpressionType::Break(_) |
                        ExpressionType::Throw(_) |
                        ExpressionType::Continue => break,
                        _ => {},
                    }
                }
                last_value
            }
            ExpressionType::Ref(rc) => {
                debug!("eval ref");
                rc.get()
            }
            ExpressionType::BindIn(id, value, container) => {
                debug!("bind in declaration: {id}");
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
                debug!("eval var declaration: {id}");
                let val = self.eval(ctx,val)?;
                match val.as_ref() {
                    ExpressionType::Ref(rc) => {
                        ctx.add_binding_ref(id.to_owned(), rc.to_owned());
                        val
                    }
                    _ => {
                        ctx.add_binding(id.to_owned(), val.to_owned());
                        val
                    }
                }
            }
            ExpressionType::LetArray(ids, val) => {
                debug!("eval var array declaration: {ids:?}");
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
            ExpressionType::Defun(fname, arg_names, body) => {
                debug!("eval function definition {fname} {arg_names:?}");
                let cctx = ctx.capture();
                let val : Expression = ExpressionType::Closure(cctx.to_owned(), arg_names.to_owned(), body.to_owned()).into();
                cctx.add_binding(fname.to_owned(), val.to_owned());
                ctx.add_binding(fname.to_owned(), val.to_owned());
                val
            }
            ExpressionType::ArrayAccess(target, index) => {
            debug!("array access with index {index}");
            let target = self.eval(ctx, target)?;
                match target.as_ref() {
                    ExpressionType::Array(arr) => {
                        if let ExpressionType::Integer(index) = self.eval(ctx, index)?.as_ref() {
                            let index = (if *index >= 0 { *index } else { arr.borrow().len() as i64 + *index}) as usize;
                            if let Some(result) = arr.borrow().get(index) {
                                result.to_owned()
                            } else {
                                expression::error(format!("index {index} out of bounds"))
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
                                expression::error(format!("index {index} out of bounds"))
                            }
                        } else {
                            Err(anyhow!("index by non-integer"))?
                        }
                    }
                    _ => Err(anyhow!("index on non-array/string"))?
                }
            }
            ExpressionType::AssignToExpression(target, val) => {
                let val = self.eval(ctx,val)?;
                match target.as_ref() {
                    ExpressionType::Variable(id) => {
                        debug!("eval assign to identifier: {id}");
                        let val = self.eval(ctx,&val)?;
                        match val.as_ref() {
                            ExpressionType::Ref(rc) => {
                                if ctx.set_binding_ref(id.to_owned(), rc.to_owned()).is_none() {
                                    Err(anyhow!("binding not found {id}"))?
                                } else {
                                    val
                                }
                            }
                            _ => {
                                if ctx.set_binding(id.to_owned(), val.to_owned()).is_none() {
                                    Err(anyhow!("binding not found {id}"))?
                                } else {
                                    val
                                }
                            }
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
                                        debug!("array set index {index} to {result:?}");
                                        result.to_owned()
                                    } else {
                                        expression::error(format!("index {index} out of bounds"))
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
                    expression::error(format!("binding not found {s}"))
                }
            }
            ExpressionType::If(cond, then, r#else) => {
                debug!("eval if");
                let cond = self.eval(ctx,cond)?;
                match cond.as_ref() {
                    ExpressionType::Boolean(b) =>
                        if *b { self.eval(ctx,then)? } 
                        else  { self.eval(ctx,r#else)? }
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::While(cond, body) => {
                debug!("eval while");
                let mut body_value = expression::nil();
                let ctx = &ctx.with_new_context();
                #[allow(clippy::while_let_loop)]
                loop {
                    match self.eval(ctx,cond)?.as_ref() {
                        ExpressionType::Boolean(b) => if *b {
                            body_value = self.eval(ctx,body)?;
                            match body_value.as_ref() {
                                ExpressionType::Break(val) => {
                                    body_value = val.to_owned();
                                    break
                                }
                                ExpressionType::Continue => {
                                    body_value = expression::nil();
                                    continue
                                }
                                ExpressionType::Return(_) | 
                                ExpressionType::Throw(_) => break,
                                _ => {},
                            }
                        } else {
                            break;
                        },
                        _ => break,
                    };
                }
                body_value
            }
            ExpressionType::DoWhile(cond, body) => {
                debug!("eval do while");
                let mut body_value;
                let ctx = &ctx.with_new_context();
                #[allow(clippy::while_let_loop)]
                loop {
                    body_value = self.eval(ctx,body)?;
                    match body_value.as_ref() {
                        ExpressionType::Break(val) => {
                            body_value = val.to_owned();
                            break
                        }
                        ExpressionType::Continue => continue,
                        ExpressionType::Return(_) | 
                        ExpressionType::Throw(_) => break,
                        _ => {},
                    }
                    match self.eval(ctx,cond)?.as_ref() {
                        ExpressionType::Boolean(b) => if !b { break; },
                        _ => break,
                    };
                }
                body_value
            }
            ExpressionType::Loop(body) => {
                debug!("eval loop");
                let mut body_value;
                let ctx = &ctx.with_new_context();
                #[allow(clippy::while_let_loop)]
                loop {
                    body_value = self.eval(ctx,body)?;
                    match body_value.as_ref() {
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
            ExpressionType::For(var, iterator, body) => {
                debug!("eval for");
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
            ExpressionType::Field(target, field) => {
                debug!("eval field: .{field}");
                let target = self.eval(ctx, target)?;
                match target.as_ref() {
                    ExpressionType::Closure(closure_ctx, _arg_names, _body) => {
                        if let Some(value) = closure_ctx.get_binding(field) {
                            value.to_owned()
                        } else {
                            expression::error(format!("field binding not found {field}"))
                        }
                    }
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::Lambda(arg_names, body) => {
                debug!("eval function closure");
                ExpressionType::Closure(ctx.capture(), arg_names.to_owned(), body.to_owned()).into()
            }
            ExpressionType::FunctionCall(lambda, arg_values) => {
                debug!("eval function call");
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
                        let function_ctx = ctx.with_context(closure_ctx.to_owned());
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            let value = self.eval(ctx,value)?;
                            // propagate exception
                            if let ExpressionType::Throw(val) = value.as_ref() {
                                return Ok(ExpressionType::Throw(val.to_owned()).into());
                            }
                            match value.as_ref() {
                                ExpressionType::Ref(rc) => {
                                    function_ctx.add_binding_ref(name.to_owned(), rc.to_owned());
                                }
                                _ => {
                                    function_ctx.add_binding(name.to_owned(), value.to_owned());
                                }
                            }
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
                debug!("eval object");
                let ctx = &ctx.with_new_context();
                self.eval(ctx, body)?;
                // we capture the context from the body in a closure
                let cap_ctx = ctx.capture();
                expression::context(cap_ctx.to_owned())
            }
            ExpressionType::Context(arg_names, body) => {
                debug!("eval context: {arg_names:?}");
                let local_ctx = Context::new();
                for name in arg_names {
                    if let Some(val) = ctx.get_binding(name) { 
                        local_ctx.add_binding(name.to_owned(), val.to_owned()); 
                    }
                }
                self.eval(&local_ctx, body)?
            }
            ExpressionType::Module(modname, arg_names, body) => {
                debug!("eval module: {arg_names:?}");
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
                debug!("eval bin op call");
                let op = op.as_ref();
                if let ExpressionType::AndOp = op {
                    return builtin::and_lazy(self,ctx,left,right);
                }
                if let ExpressionType::OrOp = op {
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
                    ExpressionType::InfixOp(fname) => {
                        let fvar = ExpressionType::Variable(fname.to_owned()).into();
                        self.eval(ctx,&ExpressionType::FunctionCall(fvar, vec![left,right]).into())?
                    },
                    ExpressionType::PipeOp => self.eval(ctx,&ExpressionType::FunctionCall(right, vec![left]).into())?,
                    ExpressionType::AddOp => builtin::add(ctx,&left,&right)?,
                    ExpressionType::SubOp => builtin::sub(ctx,&left,&right)?,
                    ExpressionType::MultOp => builtin::mul(ctx,&left,&right)?,
                    ExpressionType::DivOp => builtin::div(ctx,&left,&right)?,
                    ExpressionType::IntDivOp => builtin::intdiv(ctx,&left,&right)?,
                    ExpressionType::ModOp => builtin::modulo(ctx,&left,&right)?,
                    ExpressionType::ExpOp => builtin::pow(ctx, &left, &right)?,
                    ExpressionType::GtOp => builtin::gt(ctx, &left, &right)?,
                    ExpressionType::GeOp => builtin::ge(ctx, &left, &right)?,
                    ExpressionType::LtOp => builtin::lt(ctx, &left, &right)?,
                    ExpressionType::LeOp => builtin::le(ctx, &left, &right)?,
                    ExpressionType::EqOp => builtin::eq(ctx, &left, &right)?,
                    ExpressionType::NeOp => builtin::ne(ctx, &left, &right)?,
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::UnaryOpCall(op, expr) => {
                debug!("eval unary op call");
                if let ExpressionType::RefOp = op.as_ref() {
                    return builtin::ref_var(ctx,&expr.to_owned());
                }
                if let ExpressionType::QuestionOp = op.as_ref() {
                    return Ok(
                        match self.eval(ctx,expr) {
                            Ok(r) => r,
                            Err(e) => expression::error(e.to_string()),
                        }
                    );
                }
                let expr = self.eval(ctx,expr)?;
                if let ExpressionType::Throw(val) = expr.as_ref() {
                    return Ok(val.to_owned());
                }
                match op.as_ref() {
                    ExpressionType::NegOp => builtin::neg(ctx,&expr)?,
                    ExpressionType::NotOp => builtin::not(ctx,&expr)?,
                    ExpressionType::DeRefOp => self.eval(ctx,&expr)?,
                    ExpressionType::ExclamOp => {
                        // unwraps a thrown value
                        match expr.as_ref() {
                            ExpressionType::Throw(val) => {
                                val.to_owned()
                            }
                            _ => expr,
                        }
                    }
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::Closure(cctx, args, body) => {
                debug!("eval closure");
                ExpressionType::Closure(cctx.capture(), args.to_owned(), body.to_owned()).into()
            } 
            ExpressionType::Array(vals) => {
                debug!("eval array");
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
