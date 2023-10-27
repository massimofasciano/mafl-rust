use pest::Parser;
use crate::ast::{ExprParser, parse_to_ast, Rule, Ast};
pub mod ast;

pub fn parse_string_to_ast(source: &str) -> Ast {
    let parsed = ExprParser::parse(Rule::file, source)
        .expect("unsuccessful parse") 
        .next().unwrap(); 
    // println!("{:#?}",parsed);
    parse_to_ast(parsed)
}

