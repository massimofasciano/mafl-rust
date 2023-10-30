use pest::Parser;
use crate::expression::{MfelParser, Rule, Expression};
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

pub fn unescape_string(text: String) -> String {
    // this is incomplete
    let text = str::replace(&text, "\\n", "\n");
    let text = str::replace(&text, "\\t", "\t");
    str::replace(&text, "\\\"", "\"")
}
