use pest::Parser;
use crate::ast::{ExprParser, parse_to_ast, Rule, Ast};
pub mod ast;
pub mod eval;

pub fn parse_string_to_ast(source: &str) -> Ast {
    let parsed = ExprParser::parse(Rule::file, source)
        .expect("unsuccessful parse") 
        .next().unwrap(); 
    // println!("{:#?}",parsed);
    parse_to_ast(parsed)
}

pub fn unescape_string(text: String) -> String {
    // this is incomplete
    let text = str::replace(&text, "\\n", "\n");
    let text = str::replace(&text, "\\t", "\t");
    str::replace(&text, "\\\"", "\"")
}
