use std::env::Args;
use context::Context;
use expression::Expression;
use pest::Parser;
use crate::expression::{MfelParser, Rule};
use anyhow::{anyhow, Result};
pub mod eval;
pub mod expression;
pub mod context;
pub mod builtin;
pub mod parse;
pub mod context_vec;

#[derive(Debug,Clone)]
pub struct Interpreter {
    pub env: Expression,
    pub std: Expression,
    ctx: Context,
}

static _STD_STR : &str = include_str!("std.mfel");

impl Interpreter {
    pub fn new(env: Args) -> Result<Self> {
        let ctx = Context::new();
        let env = env.map(expression::string).collect();
        let env = expression::array(env);
        let mut interpreter = Self {
            env, ctx, 
            ..Default::default()
        };
        let ctx = Context::new();
        builtin::include_str(&interpreter, &ctx, _STD_STR)?;
        interpreter.std = expression::closure(ctx, vec![], expression::nil());
        Ok(interpreter)
    }
    pub fn run(&self, source: &str) -> Result<Expression> {
        let expr = parse_source(source)?;
        // println!("{ast:#?}");
        self.eval(&self.ctx,&expr)
    }
    pub fn print(&self, e: Expression) -> Result<Expression> {
        self.builtin_fn(&self.ctx, "print", &[e])
    }
    pub fn println(&self, e: Expression) -> Result<Expression> {
        self.builtin_fn(&self.ctx, "println", &[e])
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {  
            env: expression::array(vec![]),
            std: expression::nil(),
            ctx: Context::new(),
        }
    }
}

pub fn parse_source(source: &str) -> Result<Expression> {
    let parsed = MfelParser::parse(Rule::file, source)?
        .next().ok_or(anyhow!("parse error"))?; 
    // println!("{:#?}",parsed);
    parse::parse_rule(parsed)
}

pub fn unescape_string(sr: &str) -> String {
    // this is incomplete
    let s = str::replace(sr, "\\n", "\n");
    let s = str::replace(&s, "\\t", "\t");
    str::replace(&s, "\\\"", "\"")
}
