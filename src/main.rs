use std::io::{read_to_string, stdin};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "expr.pest"]
pub struct ExprParser;

#[derive(Debug,Clone)]
enum Ast<'a> {
    Integer(i64),
    Float(f64),
    Unit,
    Block(Vec<Ast<'a>>),
    Identifier(&'a str),
    Todo(String),
    Apply(Box<Ast<'a>>,Vec<Ast<'a>>),
}

fn parse_block(parsed: Pair<Rule>) -> Ast {
    let sequence : Vec<Ast> = parsed.into_inner()
        .filter_map(|e| {
            if e.as_rule() == Rule::EOI {
                None
            } else {
                Some(parse(e))
            }
        })
        .collect();
    match sequence.len() {
        0 => Ast::Unit,
        1 => sequence[0].clone(), // optional optim
        _ => Ast::Block(sequence),
    }
}

fn parse(parsed: Pair<Rule>) -> Ast {
    println!("[{:?}] {}",parsed.as_rule(), parsed.as_str());
    match parsed.as_rule() {
        Rule::unit_literal => { Ast::Unit },
        Rule::unit_implicit => { Ast::Unit },
        Rule::integer => { Ast::Integer(parsed.as_str().parse().unwrap()) },
        Rule::float => { Ast::Float(parsed.as_str().parse().unwrap()) },
        // Rule::expr_apply => { 
        //     let mut inner = parsed.into_inner();
        //     let fn_expr = inner.next().unwrap();
        //     println!("{:#?}",fn_expr);

        //     let fn_args = inner.next().unwrap();
        //     Ast::Apply(Box::new(parse(fn_expr)), vec![]) 
        // },
        Rule::identifier => { Ast::Identifier(parsed.as_str()) },
        Rule::block | Rule::file  => { parse_block(parsed) },
        // _ => Ast::Block(parsed.into_inner().map(|e| (parse(e))).collect()),        
        _ => {
            let rule_name = format!("{:?}",parsed.as_rule());
            parsed.into_inner().next().map(parse).unwrap_or(Ast::Todo(rule_name))
        }
    }
}

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let parsed = ExprParser::parse(Rule::file, &source)
        .expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap(); // get and unwrap the `file` rule; never fails

    println!("{:#?}",parsed);

    let ast = parse(parsed);

    println!("{ast:#?}");
}