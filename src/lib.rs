use pest::Parser;
use crate::expression::{MfelParser, Rule, Expression};
pub mod eval;
pub mod expression;
pub mod context;
pub mod builtin;
pub mod parse;

pub fn parse_source(source: &str) -> Expression {
    let parsed = MfelParser::parse(Rule::file, source)
        .expect("unsuccessful parse") 
        .next().unwrap(); 
    // println!("{:#?}",parsed);
    parse::parse_rule(parsed)
}

pub fn unescape_string(text: String) -> String {
    // this is incomplete
    let text = str::replace(&text, "\\n", "\n");
    let text = str::replace(&text, "\\t", "\t");
    str::replace(&text, "\\\"", "\"")
}
