use crate::{parse_string_to_ast, unescape_string, types::{Value, Context, Ast, AstAtom}};

impl Context {
    pub fn eval(&mut self, ast: &Ast) -> Value {
        match ast {
            Ast::Integer(i) => Value::Integer(*i),
            Ast::Float(f) => Value::Float(*f),
            Ast::Boolean(b) => { Value::Boolean(*b) }
            Ast::String(s) => Value::String(unescape_string(s.to_string())),
            Ast::Block(exprs) => {
                self.start_scope();
                let mut block_value = Value::Unit;
                for expr in exprs {
                    block_value = self.eval(expr);
                }
                self.end_scope();
                block_value
            }
            Ast::Let(id, val, expr) => {
                let val = self.eval(val);
                self.start_scope();
                self.add_binding(id.to_owned(), val.to_owned());
                let result = self.eval(expr);
                self.end_scope();
                result
            }
            Ast::Var(id, val) => {
                let val = self.eval(val);
                self.add_binding(id.to_owned(), val.to_owned());
                val
            }
            Ast::Assign(id, val) => {
                let val = self.eval(val);
                self.set_binding(id.to_owned(), val.to_owned());
                val
            }
            Ast::Variable(s) => {
                if s.starts_with('@') {
                    Value::Builtin(s.strip_prefix('@').unwrap().to_owned())
                } else {
                    self.get_binding(s).unwrap_or(&Value::from(ast)).to_owned()
                }
            }
            Ast::If(cond, then, r#else) => {
                let cond = self.eval(cond.as_ref());
                match cond {
                    Value::Boolean(b) =>
                        if b { self.eval(then.as_ref()) } 
                        else { self.eval(r#else.as_ref()) }
                    _ => Value::from(ast),
                }
            }
            Ast::While(cond, body) => {
                let mut body_value = Value::Unit;
                #[allow(clippy::while_let_loop)]
                loop {
                    match self.eval(cond.as_ref()) {
                        Value::Boolean(b) => if b {
                            body_value = self.eval(body.as_ref());
                        } else {
                            break;
                        },
                        _ => break,
                    };
                }
                body_value
            }
            Ast::Function(arg_names, body) => {
                Value::Closure(self.to_owned(), arg_names.to_owned(), body.to_owned())
            }
            Ast::FunctionCall(lambda, arg_values) => {
                match self.eval(lambda) {
                    Value::Builtin(name) => {
                        let eval_args : Vec<_> = arg_values.iter().map(|e|self.eval(e)).collect();
                        self.builtin(&name, &eval_args)
                    }
                    Value::Closure(closure_ctx, arg_names, body) => {
                        let mut function_ctx = self.clone();
                        function_ctx.add_context(closure_ctx);
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            function_ctx.add_binding(name.to_owned(), self.eval(value));
                        }
                        #[allow(clippy::comparison_chain)]
                        if arg_names.len() > arg_values.len() {
                            println!("performing currying: {arg_names:?} {arg_values:?}");
                            Value::Closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body)
                        } else if arg_names.len() < arg_values.len() {
                            println!("extra args supplied: {arg_names:?} {arg_values:?}");
                            let uncurried = function_ctx.eval(&body);
                            function_ctx.eval(&Value::FunctionCall(Box::new(uncurried),arg_values[arg_names.len()..].to_vec()))
                        } else {
                            function_ctx.eval(&body)
                        }
                    },
                    _ => unreachable!()
                }
            }
            Ast::BinOpCall(op, left, right) => {
                let op = op.as_ref();
                let left = self.eval(left.as_ref());
                let right = self.eval(right.as_ref());
                let aleft = AstAtom::from(left.to_owned());
                let aright = AstAtom::from(right.to_owned());
                match op {
                    Ast::InfixOp(fname) => {
                        if fname.starts_with('$') {
                            let fvar = Box::new(Ast::Variable(fname.strip_prefix('$').unwrap().to_owned()));
                            self.eval(&Ast::FunctionCall(fvar, vec![left,right]))
                        } else {
                            Value::Error("infix op bad prefix".to_owned())
                        }
                    },
                    Ast::PipeOp => {
                        let fvar = Box::new(right);
                        self.eval(&Ast::FunctionCall(fvar, vec![left]))
                    },
                    Ast::AddOp => left+right,
                    Ast::SubOp => left-right,
                    Ast::MultOp => left*right,
                    Ast::ExpOp => self.builtin_pow(&left, &right),
                    Ast::GtOp => Value::Boolean(aleft>aright),
                    Ast::GeOp => Value::Boolean(aleft>=aright),
                    Ast::LtOp => Value::Boolean(aleft<aright),
                    Ast::LeOp => Value::Boolean(aleft<=aright),
                    Ast::EqOp => Value::Boolean(aleft==aright),
                    Ast::NeOp => Value::Boolean(aleft!=aright),
                    _ => Value::from(ast),
                }
            }
            Ast::UnaryOpCall(op, expr) => {
                let op = op.as_ref();
                let expr = self.eval(expr.as_ref());
                match op {
                    Ast::NegOp => -expr,
                    _ => Value::from(ast),
                }
            }
            _ => Value::from(ast),
        }
    }

    pub fn builtin(&mut self, name: &str, args: &[Value]) -> Value {
        match (name, args) {
            ("println", args) => {
                self.builtin("print", args);
                println!();
                Value::Unit
            }
            ("print", args) => {
                for arg in args { 
                    match arg {
                        Value::Float(a) => print!("{a}"),
                        Value::Integer(a) => print!("{a}"),
                        Value::String(a) => print!("{a}"),
                        _ => print!("{:?}",arg),
                    }
                }
                Value::Unit
            }
            ("eval", [arg]) => {
                match arg {
                    Value::String(s) => {
                        println!("eval {s}");
                        self.eval(&parse_string_to_ast(s))
                    }
                    _ => Value::from(arg)
                }
            }
            ("pow", [lhs, rhs]) => self.builtin_pow(lhs, rhs),
            _ => Value::Error("builtin".to_owned()),
        }
    }
    pub fn builtin_pow(&mut self, lhs: &Value, rhs: &Value) -> Value {
        match (lhs, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(*b)),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a.powf(*b as f64)),
            (Value::Integer(a), Value::Float(b)) => Value::Float((*a as f64).powf(*b)),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a.pow(*b as u32)),
            _ => Value::Error("pow".to_owned()),
        }
    }
}

