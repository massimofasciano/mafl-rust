use crate::{expression::{ExpressionType, Expression, self}, builtin, context::Context, Interpreter};
use anyhow::{Result,anyhow};
use log::{debug, error};

impl Interpreter {

    pub fn eval(&self, ctx: &Context, ast: Expression) -> Result<Expression> {
        Ok(match ast.as_ref() {
            ExpressionType::Unit |
            ExpressionType::Integer(_) |
            ExpressionType::Float(_) |
            ExpressionType::Character(_) |
            ExpressionType::Boolean(_) |
            ExpressionType::String(_) |
            ExpressionType::Break
                => ast.to_owned(),
            ExpressionType::Block(exprs) => {
                debug!("eval block");
                let ctx = &ctx.with_new_scope();
                let mut block_value = expression::unit();
                for expr in exprs {
                    block_value = self.eval(ctx,expr.to_owned())?;
                }
                block_value
            }
            ExpressionType::Sequence(exprs) => {
                debug!("eval sequence");
                let mut seq_value = expression::unit();
                for expr in exprs {
                    seq_value = self.eval(ctx,expr.to_owned())?;
                }
                seq_value
            }
            ExpressionType::LetIn(id, val, expr) => {
                debug!("eval let {id} in");
                let val = self.eval(ctx,val.to_owned())?;
                let ctx = &ctx.with_new_scope();
                ctx.add_binding(id.to_owned(), val.to_owned());
                self.eval(ctx,expr.to_owned())?
            }
            ExpressionType::Let(id, val) => {
                debug!("eval var declaration: {id}");
                let val = self.eval(ctx,val.to_owned())?;
                ctx.add_binding(id.to_owned(), val.to_owned());
                val
            }
            ExpressionType::Def(fname, arg_names, body) => {
                debug!("eval function definition {fname} {arg_names:?}");
                let cctx = ctx.capture();
                let val : Expression = ExpressionType::Closure(cctx.to_owned(), arg_names.to_owned(), body.to_owned()).into();
                cctx.add_binding(fname.to_owned(), val.to_owned());
                ctx.add_binding(fname.to_owned(), val.to_owned());
                val
            }
            ExpressionType::Assign(id, val) => {
                debug!("eval assign to identifier: {id}");
                let val = self.eval(ctx,val.to_owned())?;
                if ctx.set_binding(id.to_owned(), val.to_owned()).is_none() {
                    Err(anyhow!("binding not found {id}"))?
                } else {
                    val
                }
            }
            ExpressionType::Variable(s) => {
                debug!("eval variable: {s}");
                if s.starts_with('@') {
                    let name = s.strip_prefix('@').unwrap().to_owned();
                    if let Some(result) = self.builtin_var(ctx, &name) {
                        result?
                    } else {
                        ExpressionType::BuiltinFunction(name).into()
                    }
                } else if let Some(value) = ctx.get_binding(s) {
                    value.to_owned()
                } else {
                    expression::error(format!("binding not found {s}"))
                }
            }
            ExpressionType::If(cond, then, r#else) => {
                debug!("eval if");
                let cond = self.eval(ctx,cond.to_owned())?;
                match cond.as_ref() {
                    ExpressionType::Boolean(b) =>
                        if *b { self.eval(ctx,then.to_owned())? } 
                        else  { self.eval(ctx,r#else.to_owned())? }
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::While(cond, body) => {
                debug!("eval while");
                let mut body_value = expression::unit();
                let ctx = &ctx.with_new_scope();
                #[allow(clippy::while_let_loop)]
                loop {
                    match self.eval(ctx,cond.to_owned())?.as_ref() {
                        ExpressionType::Boolean(b) => if *b {
                            body_value = self.eval(ctx,body.to_owned())?;
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
                let ctx = &ctx.with_new_scope();
                #[allow(clippy::while_let_loop)]
                loop {
                    body_value = self.eval(ctx,body.to_owned())?;
                    match self.eval(ctx,cond.to_owned())?.as_ref() {
                        ExpressionType::Boolean(b) => if !b { break; },
                        _ => break,
                    };
                }
                body_value
            }
            ExpressionType::Loop(body) => {
                debug!("eval loop");
                let mut body_value;
                let ctx = &ctx.with_new_scope();
                #[allow(clippy::while_let_loop)]
                loop {
                    body_value = self.eval(ctx,body.to_owned())?;
                    // pretty weak implementation (the whole body has to eval to break)
                    if let ExpressionType::Break = body_value.as_ref() {
                        break
                    }
                }
                body_value
            }
            ExpressionType::For(var, iterator, body) => {
                debug!("eval for");
                let iterator = self.eval(ctx, iterator.to_owned())?;
                let mut body_value = expression::unit();
                let ctx = &ctx.with_new_scope();
                ctx.add_binding(var.to_owned(), expression::unit());
                match self.eval(ctx,iterator.to_owned())?.as_ref() {
                    ExpressionType::Array(arr) => {
                        for v in arr.borrow().iter() {
                            ctx.set_binding(var.to_owned(), v.to_owned());
                            body_value = self.eval(ctx,body.to_owned())?;
                        }
                    },
                    ExpressionType::Closure(_,_,_) => {
                        loop {
                            let apply = ExpressionType::FunctionCall(iterator.to_owned(), vec![]).into();
                            let next = self.eval(ctx, apply)?;
                            if next == expression::unit() { break; }
                            ctx.set_binding(var.to_owned(), next.to_owned());
                            body_value = self.eval(ctx,body.to_owned())?;
                        }
                    },
                    _ => return ast.to_error(),
                }
                body_value
            }
            ExpressionType::Field(target, field) => {
                debug!("eval field: .{field}");
                let target = self.eval(ctx, target.to_owned())?;
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
            ExpressionType::FunctionClosure(arg_names, body) => {
                debug!("eval function closure");
                ExpressionType::Closure(ctx.capture(), arg_names.to_owned(), body.to_owned()).into()
            }
            ExpressionType::FunctionStatic(arg_names, body) => {
                debug!("eval function static");
                ExpressionType::Closure(Context::new(), arg_names.to_owned(), body.to_owned()).into()
            }
            ExpressionType::FunctionDynamic(_arg_names, _body) => {
                debug!("eval function dynamic");
                ast.to_owned()
            }
            ExpressionType::FunctionCall(lambda, arg_values) => {
                debug!("eval function call");
                match self.eval(ctx,lambda.to_owned())?.as_ref() {
                    ExpressionType::BuiltinFunction(name) => {
                        let eval_args = arg_values.iter()
                            .map(|e|self.eval(ctx,e.to_owned())).collect::<Result<Vec<_>>>()?;
                        self.builtin_fn(ctx, name, &eval_args)?
                    }
                    ExpressionType::Array(vals) => {
                        // indexing
                        if arg_values.len() == 1 {
                            if let ExpressionType::Integer(index) = self.eval(ctx, arg_values[0].to_owned())?.as_ref() {
                                let index = (if *index >= 0 { *index } else { vals.borrow().len() as i64 + *index}) as usize;
                                if let Some(result) = vals.borrow().get(index) {
                                    result.to_owned()
                                } else {
                                    expression::error(format!("index {index} out of bounds"))
                                }
                            } else {
                                Err(anyhow!("index by non-integer"))?
                            }
                        // mutate at index
                        } else if arg_values.len() == 2 {
                            if let ExpressionType::Integer(index) = self.eval(ctx, arg_values[0].to_owned())?.as_ref() {
                                debug!("array set index {index}");
                                let index = (if *index >= 0 { *index } else { vals.borrow().len() as i64 + *index}) as usize;
                                let value = self.eval(ctx, arg_values[1].to_owned())?;
                                if let Some(result) = vals.borrow_mut().get_mut(index) {
                                    *result = value;
                                    error!("array set index {index} to {result:?}");
                                    result.to_owned()
                                } else {
                                    expression::error(format!("index {index} out of bounds"))
                                }
                            } else {
                                Err(anyhow!("index by non-integer"))?
                            }
                        } else {
                            Err(anyhow!("array get or set index: need 1 or 2 arguments"))?
                        }
                    }
                    ExpressionType::String(s) => {
                        // indexing
                        if arg_values.len() == 1 {
                            if let ExpressionType::Integer(index) = self.eval(ctx, arg_values[0].to_owned())?.as_ref() {
                                let index = (if *index >= 0 { *index } else { s.len() as i64 + *index}) as usize;
                                if let Some(result) = s.chars().nth(index) {
                                    expression::character(result)
                                } else {
                                    expression::error(format!("index {index} out of bounds"))
                                }
                            } else {
                                Err(anyhow!("index by non-integer"))?
                            }
                        } else {
                            Err(anyhow!("string get index: need one argument"))?
                        }
                    }
                    ExpressionType::Closure(closure_ctx, arg_names, body) => {
                        let function_ctx = closure_ctx.with_new_scope();
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            function_ctx.add_binding(name.to_owned(), self.eval(ctx,value.to_owned())?);
                        }
                        #[allow(clippy::comparison_chain)]
                        if arg_names.len() > arg_values.len() {
                            debug!("performing currying: {arg_names:?} {arg_values:?}");
                            expression::closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body.to_owned())
                        } else if arg_names.len() < arg_values.len() {
                            debug!("extra args supplied: {arg_names:?} {arg_values:?}");
                            let uncurried = self.eval(&function_ctx,body.to_owned())?;
                            self.eval(&function_ctx,ExpressionType::FunctionCall(uncurried,arg_values[arg_names.len()..].to_vec()).into())?
                        } else {
                            self.eval(&function_ctx,body.to_owned())?
                        }
                    },
                    ExpressionType::FunctionDynamic(arg_names, body) => {
                        // a dynamic function is applied in the global context
                        // so we create a closure at application time and apply it
                        let closure = expression::closure(ctx.to_owned(), arg_names.to_owned(), body.to_owned());
                        self.eval(ctx, ExpressionType::FunctionCall(closure, arg_values.to_owned()).into())?
                    },
                    _ => ast.to_error()?
                }
            }
            ExpressionType::Context(arg_names, body) => {
                debug!("eval context: {arg_names:?}");
                let local_ctx = Context::new();
                for name in arg_names {
                    if let Some(val) = ctx.get_binding(name) { 
                        local_ctx.add_binding(name.to_owned(), val.to_owned()); 
                    }
                }
                self.eval(&local_ctx, body.to_owned())?
            }
            ExpressionType::Module(modname, arg_names, body) => {
                debug!("eval module: {arg_names:?}");
                let local_ctx = Context::new();
                for name in arg_names {
                    if let Some(val) = ctx.get_binding(name) { 
                        local_ctx.add_binding(name.to_owned(), val.to_owned()); 
                    }
                }
                let val = self.eval(&local_ctx, body.to_owned())?;
                let captured = builtin::capture_context(&local_ctx)?;
                ctx.add_binding(modname.to_owned(), captured);
                val
            }
            ExpressionType::BinOpCall(op, left, right) => {
                debug!("eval bin op call");
                let op = op.as_ref();
                if op == &ExpressionType::AndOp {
                    return builtin::and_lazy(self,ctx,left,right);
                }
                if op == &ExpressionType::OrOp {
                    return builtin::or_lazy(self,ctx,left,right);
                }
                let left = self.eval(ctx,left.to_owned())?;
                let right = self.eval(ctx,right.to_owned())?;
                match op {
                    ExpressionType::InfixOp(fname) => {
                        let fvar = ExpressionType::Variable(fname.to_owned()).into();
                        self.eval(ctx,ExpressionType::FunctionCall(fvar, vec![left,right]).into())?
                    },
                    ExpressionType::PipeOp => self.eval(ctx,ExpressionType::FunctionCall(right, vec![left]).into())?,
                    // Expression::AndOp => builtin::and(ctx,&left,&right)?,
                    // Expression::OrOp => builtin::or(ctx,&left,&right)?,
                    ExpressionType::AddOp => builtin::add(ctx,&left,&right)?,
                    ExpressionType::SubOp => builtin::sub(ctx,&left,&right)?,
                    ExpressionType::MultOp => builtin::mul(ctx,&left,&right)?,
                    ExpressionType::DivOp => builtin::div(ctx,&left,&right)?,
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
                // if op.as_ref() == &ExpressionType::RefOp {
                //     return builtin::ref_var(ctx,&expr.to_owned());
                // }
                let expr = self.eval(ctx,expr.to_owned())?;
                match op.as_ref() {
                    ExpressionType::NegOp => builtin::neg(ctx,&expr.to_owned())?,
                    ExpressionType::NotOp => builtin::not(ctx,&expr.to_owned())?,
                    _ => ast.to_error()?,
                }
            }
            ExpressionType::Closure(cctx, args, body) => {
                debug!("eval closure");
                ExpressionType::Closure(cctx.capture(), args.to_owned(), body.to_owned()).into()
            } 
            ExpressionType::Array(vals) => {
                debug!("eval array");
                expression::array(
                    vals.borrow().iter().map(|v|self.eval(ctx,v.to_owned())).collect::<Result<Vec<_>>>()?
                )
            }
            _ => ast.to_error()?,
        })
    }

    pub fn builtin_var(&self, ctx: &Context, name: &str) -> Option<Result<Expression>> {
        match name {
            "env" => Some(Ok(self.env.to_owned())),
            "context" => Some(builtin::capture_context(ctx)),
            "std" => { Some(Ok(self.std.to_owned())) }
            _ => None,
        }
    }

    pub fn builtin_fn(&self, ctx: &Context, name: &str, args: &[Expression]) -> Result<Expression> {
        match (name, args) {
            ("println", args) => { builtin::println(ctx, args) },
            ("print", args) => { builtin::print(ctx, args) },
            ("debugln", args) => { builtin::debugln(ctx, args) },
            ("debug", args) => { builtin::debug(ctx, args) },
            ("error", args) => { builtin::error_from_strings(ctx, args) },
            ("eval", [arg]) => { builtin::eval_string_as_source(self, ctx, arg) },
            ("include", [file_expr]) => builtin::include(self, ctx, file_expr),
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
            ("copy", [container]) => builtin::copy(ctx, container),
            ("lines", [init]) => builtin::to_array_lines(ctx, init),
            ("append", [target, new]) => builtin::append(self, ctx, target, new),
            ("ctx", []) => builtin::capture_context(ctx),
            ("readline", []) => builtin::read_line(ctx),
            ("get", [container, key]) => builtin::get(ctx, container, key),
            ("set", [container, key, value]) => builtin::set(ctx, container, key, value),
            ("insert", [container, key, value]) => builtin::insert(ctx, container, key, value),
            ("bind", [container, key, value]) => builtin::insert(ctx, container, key, value),
            ("dict", [parent]) => builtin::dict_extend(ctx, parent),
            ("dict", []) => builtin::dict(ctx),
            ("var", [key]) => builtin::get_var(ctx, key),
            ("assign", [key, value]) => builtin::assign_var(ctx, key, value),
            ("let", [key, value]) => builtin::let_var(ctx, key, value),
            _ => Err(anyhow!("builtin {name}")),
        }
    }

}
