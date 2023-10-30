use crate::{parse_string_to_ast, unescape_string, types::{Expression, Context, AtomicExpression}, builtin};

impl Context {
    pub fn eval(&mut self, ast: &Expression) -> Expression {
        match ast {
            Expression::Integer(i) => Expression::Integer(*i),
            Expression::Float(f) => Expression::Float(*f),
            Expression::Boolean(b) => { Expression::Boolean(*b) }
            Expression::String(s) => Expression::String(unescape_string(s.to_string())),
            Expression::Block(exprs) => {
                self.start_scope();
                let mut block_value = Expression::Unit;
                for expr in exprs {
                    block_value = self.eval(expr);
                }
                self.end_scope();
                block_value
            }
            Expression::Let(id, val, expr) => {
                let val = self.eval(val);
                self.start_scope();
                self.add_binding(id.to_owned(), val.to_owned());
                let result = self.eval(expr);
                self.end_scope();
                result
            }
            Expression::Var(id, val) => {
                let val = self.eval(val);
                self.add_binding(id.to_owned(), val.to_owned());
                val
            }
            Expression::Assign(id, val) => {
                let val = self.eval(val);
                self.set_binding(id.to_owned(), val.to_owned());
                val
            }
            Expression::Variable(s) => {
                if s.starts_with('@') {
                    Expression::Builtin(s.strip_prefix('@').unwrap().to_owned())
                } else {
                    self.get_binding(s).unwrap_or(&Expression::from(ast)).to_owned()
                }
            }
            Expression::If(cond, then, r#else) => {
                let cond = self.eval(cond.as_ref());
                match cond {
                    Expression::Boolean(b) =>
                        if b { self.eval(then.as_ref()) } 
                        else { self.eval(r#else.as_ref()) }
                    _ => Expression::from(ast),
                }
            }
            Expression::While(cond, body) => {
                let mut body_value = Expression::Unit;
                #[allow(clippy::while_let_loop)]
                loop {
                    match self.eval(cond.as_ref()) {
                        Expression::Boolean(b) => if b {
                            body_value = self.eval(body.as_ref());
                        } else {
                            break;
                        },
                        _ => break,
                    };
                }
                body_value
            }
            Expression::Function(arg_names, body) => {
                Expression::Closure(self.to_owned(), arg_names.to_owned(), body.to_owned())
            }
            Expression::FunctionCall(lambda, arg_values) => {
                match self.eval(lambda) {
                    Expression::Builtin(name) => {
                        let eval_args : Vec<_> = arg_values.iter().map(|e|self.eval(e)).collect();
                        self.builtin(&name, &eval_args)
                    }
                    Expression::Closure(closure_ctx, arg_names, body) => {
                        let mut function_ctx = self.clone();
                        function_ctx.add_context(closure_ctx);
                        for (name,value) in arg_names.iter().zip(arg_values) {
                            function_ctx.add_binding(name.to_owned(), self.eval(value));
                        }
                        #[allow(clippy::comparison_chain)]
                        if arg_names.len() > arg_values.len() {
                            println!("performing currying: {arg_names:?} {arg_values:?}");
                            Expression::Closure(function_ctx, arg_names[arg_values.len()..].to_vec(), body)
                        } else if arg_names.len() < arg_values.len() {
                            println!("extra args supplied: {arg_names:?} {arg_values:?}");
                            let uncurried = function_ctx.eval(&body);
                            function_ctx.eval(&Expression::FunctionCall(Box::new(uncurried),arg_values[arg_names.len()..].to_vec()))
                        } else {
                            function_ctx.eval(&body)
                        }
                    },
                    _ => unreachable!()
                }
            }
            Expression::BinOpCall(op, left, right) => {
                let op = op.as_ref();
                let left = self.eval(left.as_ref());
                let right = self.eval(right.as_ref());
                let aleft = AtomicExpression::from(left.to_owned());
                let aright = AtomicExpression::from(right.to_owned());
                match op {
                    Expression::InfixOp(fname) => {
                        if fname.starts_with('$') {
                            let fvar = Box::new(Expression::Variable(fname.strip_prefix('$').unwrap().to_owned()));
                            self.eval(&Expression::FunctionCall(fvar, vec![left,right]))
                        } else {
                            Expression::Error("infix op bad prefix".to_owned())
                        }
                    },
                    Expression::PipeOp => {
                        let fvar = Box::new(right);
                        self.eval(&Expression::FunctionCall(fvar, vec![left]))
                    },
                    Expression::AddOp => left+right,
                    Expression::SubOp => left-right,
                    Expression::MultOp => left*right,
                    Expression::ExpOp => builtin::pow(self, &left, &right),
                    Expression::GtOp => Expression::Boolean(aleft>aright),
                    Expression::GeOp => Expression::Boolean(aleft>=aright),
                    Expression::LtOp => Expression::Boolean(aleft<aright),
                    Expression::LeOp => Expression::Boolean(aleft<=aright),
                    Expression::EqOp => Expression::Boolean(aleft==aright),
                    Expression::NeOp => Expression::Boolean(aleft!=aright),
                    _ => Expression::from(ast),
                }
            }
            Expression::UnaryOpCall(op, expr) => {
                let op = op.as_ref();
                let expr = self.eval(expr.as_ref());
                match op {
                    Expression::NegOp => -expr,
                    _ => Expression::from(ast),
                }
            }
            _ => Expression::from(ast),
        }
    }

    pub fn builtin(&mut self, name: &str, args: &[Expression]) -> Expression {
        match (name, args) {
            ("println", args) => { builtin::println(self, args) },
            ("print", args) => { builtin::print(self, args) },
            ("eval", [arg]) => { builtin::eval_string(self, arg) },
            ("pow", [lhs, rhs]) => builtin::pow(self, lhs, rhs),
            _ => Expression::Error("builtin".to_owned()),
        }
    }
}

