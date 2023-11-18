use std::{cell::RefCell, collections::HashMap};
use context::Context;
use expression::{R, Ident, Value, Expr};
use pest::Parser;
use crate::expression::{MfelParser, Rule};
use anyhow::{anyhow, Result};

pub mod eval;
pub mod expression;
pub mod context;
pub mod builtin;
pub mod parse;
pub mod open;

#[derive(Debug,Clone)]
pub struct Interpreter {
    env: R<Expr>,
    std: R<Expr>,
    ctx: Context,
    vars: RefCell<HashMap<String,usize>>,
    last_var: RefCell<usize>,
    test_pass_count: RefCell<usize>,
    test_fail_count: RefCell<usize>,
    pragma_shadow_local: RefCell<PragmaLevel>,
}

#[derive(Debug,Clone)]
pub enum PragmaLevel {
    Allow,
    Warn,
    Error,
}

static _STD_STR : &str = include_str!("std.mfel");

const __STR__ : &str = "string";

impl Interpreter {
    pub fn new() -> Result<Self> {
        let mut interpreter = Self::default();
        interpreter.init_std()?;
        Ok(interpreter)
    }
    pub fn set_env(&mut self, env: Vec<String>) {
        self.env = expression::array(env.into_iter().map(expression::string).collect());
    }
    pub fn init_std(&mut self) -> Result<()> {
        let ctx = Context::new();
        builtin::include_str(self, &ctx, _STD_STR)?;
        self.std = expression::closure(ctx, vec![], expression::nil());
        Ok(())
    }
    pub fn run(&self, source: &str) -> Result<R<Expr>> {
        let expr = self.parse_source(source)?;
        // println!("{expr:#?}");
        self.eval(&self.ctx,&expr)
    }
    pub fn print(&self, e: R<Expr>) -> Result<R<Expr>> {
        self.builtin_fn(&self.ctx, "print", &[e])
    }
    pub fn println(&self, e: R<Expr>) -> Result<R<Expr>> {
        self.builtin_fn(&self.ctx, "println", &[e])
    }
    pub fn parse_source(&self, source: &str) -> Result<R<Expr>> {
        let parsed = MfelParser::parse(Rule::file, source)?
            .next().ok_or(anyhow!("parse error"))?; 
        // println!("{:#?}",parsed);
        self.parse_rule(parsed)
    }
    pub fn var_num(&self, id: &str) -> usize {
        let num = self.vars.borrow().get(id).map(|x|x.to_owned());
        if let Some(num) = num {
            num
        } else {
            let num = {
                let mut last_var = self.last_var.borrow_mut();
                let tmp = *last_var;
                *last_var += 1;
                tmp
            };
            self.vars.borrow_mut().insert(id.to_owned(), num);
            num
        }
    }
    pub fn ident(&self, id: &str) -> Ident {
        // format!("_{}",self.var_num(id))
        // format!("_{}_{id}",self.var_num(id))
        id.to_owned()
        // self.var_num(id)
    } 
    pub fn ident_to_string(&self, id: &Ident) -> String {
        // format!("_{}",id)
        id.to_owned()
    }
    pub fn test_report(&self) -> (usize, usize) {
        let pass_count = *self.test_pass_count.borrow();
        let fail_count = *self.test_fail_count.borrow();
        (pass_count, fail_count)
    }
    pub fn expr_to_value(&self, expr: R<Expr>) -> Result<Value> {
        Ok(match expr.as_ref() {
            Expr::Nil => Value::Nil,
            Expr::Integer(i) => Value::Integer(*i),
            Expr::Float(f) => Value::Float(*f),
            Expr::Character(c) => Value::Character(*c),
            Expr::Boolean(b) => Value::Boolean(*b),
            Expr::String(s) => Value::String(s.to_owned()),
            Expr::Array(rc) => {
                let v = rc.borrow().iter()
                    .map(|e| { self.expr_to_value(e.to_owned()) })
                    .collect::<Result<Vec<_>>>()?;
                Value::Array(v)
            }
            Expr::Closure(ctx,_,_) => {
                let d = ctx.bindings_cloned().iter()
                    .map(|(id, mc)| -> Result<(String,Value)> { 
                        Ok((self.ident_to_string(id), self.expr_to_value(mc.get())?)) 
                    })
                    .collect::<Result<HashMap<String,Value>>>()?;
                Value::Dict(d)
            }
            _ => Err(anyhow!("can't convert this expression to a value"))?
        })
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {  
            env: expression::array(vec![]),
            std: expression::nil(),
            ctx: Context::new(),
            vars: RefCell::new(HashMap::new()),
            last_var: RefCell::new(0),
            test_pass_count: RefCell::new(0),
            test_fail_count: RefCell::new(0),
            pragma_shadow_local: RefCell::new(PragmaLevel::Allow),
        }
    }
}

pub fn unescape_string(sr: &str) -> Result<String> {
    unescape::unescape(sr).ok_or(anyhow!("error unescaping string: {sr}"))
}

#[cfg(test)]
mod tests {
    use crate::{Interpreter, expression};
    use anyhow::Result;

    #[test]
    fn syntax_test() -> Result<()> {
        // need extra stack during test... (set it at 8MB like the main thread)
        // RUST_MIN_STACK=8388608 cargo test
        let interpreter = Interpreter::new()?;
        // this program returns true if no tests failed
        let file = "examples/syntax.mfel";
        // run the program
        let source = std::fs::read_to_string(file)?;
        let result = interpreter.run(&source)?;
        // result should be true
        assert_eq!(result, expression::boolean(true));
        Ok(())
    }
}
