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
            Ast::Let(id, val) => {
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
                let val = self.get_binding(s).unwrap_or(&Value::from(ast)).to_owned();
                val
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
                // we have no side-effects for now so we put a println in there
                let mut body_value = Value::Unit;
                #[allow(clippy::while_let_loop)]
                loop {
                    match self.eval(cond.as_ref()) {
                        Value::Boolean(b) => if b {
                            body_value = self.eval(body.as_ref());
                            println!("WHILE {self:?} {body:?}");
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
                }
            }
            Ast::UnaryOpCall(op, expr) => {
                let op = op.as_ref();
                let expr = self.eval(expr.as_ref());
                match op {
                    Ast::NegOp => -expr,
                    Ast::DollarOp => {
                        match expr {
                            Value::String(s) => {
                                self.eval(&parse_string_to_ast(&s))
                            }
                            _ => expr
                        }
                    }
                    _ => Value::from(ast),
                }
            }
            _ => Value::from(ast),
        }
    }

}
