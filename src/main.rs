use std::io::{read_to_string, stdin};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "expr.pest"]
pub struct ExprParser;

#[derive(Debug,Clone)]
enum Ast<'a> {
    // Todo(String),
    Integer(i64),
    Float(f64),
    Unit,
    Block(Vec<Ast<'a>>),
    Identifier(&'a str),
    FunctionCall(Box<Ast<'a>>,Vec<Ast<'a>>),
    MethodCall(Box<Ast<'a>>,Box<Ast<'a>>,Vec<Ast<'a>>),
    BinOpCall(Box<Ast<'a>>,Box<Ast<'a>>,Box<Ast<'a>>),
    UnaryOpCall(Box<Ast<'a>>,Box<Ast<'a>>),
    AddOp, MultOp, SubOp, DivOp,
    NotOp, AndOp, OrOp, 
    GtOp, NegOp,
    DollarOp, QuestionOp,
    If(Box<Ast<'a>>,Box<Ast<'a>>,Box<Ast<'a>>),
    While(Box<Ast<'a>>,Box<Ast<'a>>),
    DoWhile(Box<Ast<'a>>,Box<Ast<'a>>),
    Let(Box<Ast<'a>>,Box<Ast<'a>>),
    Loop(Box<Ast<'a>>),
    True, False,
    Function(Vec<Ast<'a>>,Box<Ast<'a>>),
    String(&'a str),
    Return(Box<Ast<'a>>),
    Continue, Break,
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
        1 => sequence[0].clone(),
        _ => Ast::Block(sequence),
    }
}

fn parse_vec(rule: Rule, _string: String, inner: Vec<Pair<Rule>>) -> Ast {
    match rule {
        Rule::expr_infix_id | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
        Rule::expr_rel | Rule::expr_add | Rule::expr_mul  => {
            assert!(inner.len() > 2);
            assert!(inner.len() % 2 == 1);
            let left = parse(inner[0].clone());
            inner[1..].chunks_exact(2).fold(left, |ast, pair| {
                let op = parse(pair[0].clone());
                let right = parse(pair[1].clone());
                Ast::BinOpCall(Box::new(op), Box::new(ast), Box::new(right))
            })
        }
        Rule::expr_post => {
            assert!(!inner.is_empty());
            let expr = parse(inner[0].clone());
            inner[1..].iter().fold(expr, |ast, pair| {
                let op = parse(pair.clone());
                Ast::UnaryOpCall(Box::new(op), Box::new(ast))
            })
        }
        Rule::expr_prefix => {
            assert!(!inner.is_empty());
            let mut rinner = inner;
            rinner.reverse();
            let expr = parse(rinner[0].clone());
            rinner[1..].iter().fold(expr, |ast, pair| {
                let op = parse(pair.clone());
                Ast::UnaryOpCall(Box::new(op), Box::new(ast))
            })
        }
        Rule::expr_apply => {
            assert!(inner.len() == 2);
            let func = parse(inner[0].clone());
            assert!(inner[1].as_rule() == Rule::apply_args);
            let args : Vec<_> = inner[1].clone().into_inner().map(|e| (parse(e.clone()))).collect();
            Ast::FunctionCall(Box::new(func), args)
        } 
        Rule::expr_dotcall => {
            assert!(inner.len() > 2);
            assert!(inner.len() % 2 == 1);
            let target = parse(inner[0].clone());
            inner[1..].chunks_exact(2).fold(target, |ast, pair| {
                let method = parse(pair[0].clone());
                assert!(pair[1].as_rule() == Rule::apply_args);
                let args : Vec<_> = pair[1].clone().into_inner().map(|e| (parse(e.clone()))).collect();
                Ast::MethodCall(Box::new(ast), Box::new(method), args)
            })
        } 
        Rule::function => {
            assert!(inner.len() == 2);
            assert!(inner[0].as_rule() == Rule::function_args);
            assert!(inner[1].as_rule() == Rule::block);
            let args : Vec<_> = inner[0].clone().into_inner().map(|e| (parse(e.clone()))).collect();
            let body = parse(inner[1].clone());
            Ast::Function(args, Box::new(body))
        } 
        Rule::r#if | Rule::unless => {
            assert!(inner.len() == 2 || inner.len() == 3);
            let expr = parse(inner[0].clone());
            let cond = if rule == Rule::unless {
                Ast::UnaryOpCall(Box::new(Ast::NotOp),Box::new(expr))
            } else {
                expr
            };
            assert!(inner[1].as_rule() == Rule::block);
            let then = parse(inner[1].clone());
            let r#else = if inner.len() < 3 {
                Ast::Unit
            } else {
                parse(inner[2].clone())
            };
            Ast::If(Box::new(cond), Box::new(then), Box::new(r#else))
        } 
        Rule::r#let => {
            assert!(inner.len() == 2);
            assert!(inner[0].as_rule() == Rule::identifier);
            let var = parse(inner[0].clone());
            let val = parse(inner[1].clone());
            Ast::Let(Box::new(var), Box::new(val))
        } 
        Rule::r#while => {
            assert!(inner.len() == 2);
            let cond = parse(inner[0].clone());
            assert!(inner[1].as_rule() == Rule::block);
            let body = parse(inner[1].clone());
            Ast::While(Box::new(cond), Box::new(body))
        } 
        Rule::do_while => {
            assert!(inner.len() == 2);
            let cond = parse(inner[1].clone());
            assert!(inner[0].as_rule() == Rule::block);
            let body = parse(inner[0].clone());
            Ast::DoWhile(Box::new(cond), Box::new(body))
        } 
        Rule::r#loop => {
            assert!(inner.len() == 1);
            assert!(inner[0].as_rule() == Rule::block);
            let body = parse(inner[0].clone());
            Ast::Loop(Box::new(body))
        }
        Rule::r#return => { 
            let body = if inner.len() == 1 {
                parse(inner[0].clone())
            } else {
                Ast::Unit
            };
            Ast::Return(Box::new(body)) 
        },
        _ => unreachable!(),
    }
}

fn parse(parsed: Pair<Rule>) -> Ast {
    match parsed.as_rule() {
        Rule::integer => { Ast::Integer(parsed.as_str().parse().unwrap()) },
        Rule::float => { Ast::Float(parsed.as_str().parse().unwrap()) },
        Rule::identifier => { Ast::Identifier(parsed.as_str()) },
        Rule::string => { Ast::String(parsed.as_str()) },
        Rule::unit_literal => { Ast::Unit },
        Rule::unit_implicit => { Ast::Unit },
        Rule::r#true => { Ast::True },
        Rule::r#false => { Ast::False },
        Rule::dollar => { Ast::DollarOp },
        Rule::question => { Ast::QuestionOp },
        Rule::neg => { Ast::NegOp },
        Rule::add => { Ast::AddOp },
        Rule::mult => { Ast::MultOp },
        Rule::sub => { Ast::SubOp },
        Rule::div => { Ast::DivOp },
        Rule::or => { Ast::OrOp },
        Rule::and => { Ast::AndOp },
        Rule::not => { Ast::NotOp },
        Rule::gt => { Ast::GtOp },
        Rule::expr_infix_id | Rule::expr_or | Rule::expr_and | 
        Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
        Rule::expr_mul | Rule::expr_apply | Rule::expr_post | 
        Rule::expr_dotcall | Rule::expr_prefix => { 
            let rule = parsed.as_rule();
            let str = parsed.as_str().to_owned();
            let inner : Vec<Pair<Rule>> = parsed.into_inner().collect();
            if inner.len() < 2 {
                parse(inner[0].clone())
            } else {
                parse_vec(rule,str,inner)
            }
        },
        Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | 
        Rule::r#let | Rule::r#loop | Rule::function | Rule::r#return => {
            let rule = parsed.as_rule();
            let str = parsed.as_str().to_owned();
            let inner : Vec<Pair<Rule>> = parsed.into_inner().collect();
            parse_vec(rule,str,inner)
        }
        Rule::r#continue => { Ast::Continue },
        Rule::r#break => { Ast::Break },
        Rule::block | Rule::file  => { parse_block(parsed) },
        Rule::string_literal | Rule::boolean => {
            // these rules could be made silent in the grammar
            parsed.into_inner().next().map(parse).unwrap()
        }
        _ => {
            println!("TODO: [{:?}] {}",parsed.as_rule(), parsed.as_str());
            // let rule_name = format!("{:?}",parsed.as_rule());
            // parsed.into_inner().next().map(parse).unwrap_or(Ast::Todo(rule_name))
            unreachable!()
        }
    }
}

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let parsed = ExprParser::parse(Rule::file, &source)
        .expect("unsuccessful parse") 
        .next().unwrap(); 

    // println!("{:#?}",parsed);

    let ast = parse(parsed);

    println!("{ast:#?}");
}