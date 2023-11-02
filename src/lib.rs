use expression::Expression;
use pest::Parser;
use crate::expression::{MfelParser, Rule};
use anyhow::{anyhow, Result};
pub mod eval;
pub mod expression;
pub mod context;
pub mod builtin;
pub mod parse;

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
