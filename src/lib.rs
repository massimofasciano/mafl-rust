use std::{cell::RefCell, collections::HashMap};
use context::Context;
use expression::{Expression, Ident};
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
    env: Expression,
    std: Expression,
    ctx: Context,
    vars: RefCell<HashMap<String,usize>>,
    last_var: RefCell<usize>,
    test_pass_count: RefCell<usize>,
    test_fail_count: RefCell<usize>,
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
    pub fn run(&self, source: &str) -> Result<Expression> {
        let expr = self.parse_source(source)?;
        // println!("{expr:#?}");
        self.eval(&self.ctx,&expr)
    }
    pub fn print(&self, e: Expression) -> Result<Expression> {
        self.builtin_fn(&self.ctx, "print", &[e])
    }
    pub fn println(&self, e: Expression) -> Result<Expression> {
        self.builtin_fn(&self.ctx, "println", &[e])
    }
    pub fn parse_source(&self, source: &str) -> Result<Expression> {
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
    pub fn ident_to_string(id: &Ident) -> String {
        // format!("_{}",id)
        id.to_owned()
    }
    pub fn test_report(&self) -> (usize, usize) {
        let pass_count = *self.test_pass_count.borrow();
        let fail_count = *self.test_fail_count.borrow();
        (pass_count, fail_count)
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
        }
    }
}

pub fn unescape_string(sr: &str) -> Result<String> {
    unescape::unescape(sr).ok_or(anyhow!("error unescaping string: {sr}"))
}
