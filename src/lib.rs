use std::{cell::{RefCell, Cell}, collections::HashMap};
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
    pub env: Expression,
    pub std: Expression,
    ctx: Context,
    vars: RefCell<HashMap<String,usize>>,
    last_var: Cell<usize>,
}

static _STD_STR : &str = include_str!("std.mfel");

static __STR__ : &str = "string";

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
            let num = self.last_var.get();
            self.vars.borrow_mut().insert(id.to_owned(), num);
            self.last_var.set(num+1);
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
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {  
            env: expression::array(vec![]),
            std: expression::nil(),
            ctx: Context::new(),
            vars: RefCell::new(HashMap::new()),
            last_var: Cell::new(0),
        }
    }
}

pub fn unescape_string(sr: &str) -> Result<String> {
    unescape::unescape(sr).ok_or(anyhow!("error unescaping string: {sr}"))
}
