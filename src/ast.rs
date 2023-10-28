use pest::iterators::Pair;

use crate::types::{Rule, Ast};

fn parse_block(parsed: Pair<Rule>) -> Ast {
    let sequence : Vec<Ast> = parsed.into_inner()
        .filter_map(|e| {
            if e.as_rule() == Rule::EOI {
                None
            } else {
                Some(parse_to_ast(e))
            }
        })
        .collect();
    match sequence.len() {
        0 => Ast::Unit,
        1 => sequence[0].clone(),
        _ => Ast::Block(sequence),
    }
}

fn parse_vec(rule: Rule, string: String, inner: Vec<Pair<Rule>>) -> Ast {
    match rule {
        Rule::variable => {
            assert!(inner.len() == 1);
            Ast::Variable(string)
        }
        Rule::expr_infix_id | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
        Rule::expr_rel | Rule::expr_add | Rule::expr_mul | Rule::expr_exp => {
            if inner.len() == 1 { return parse_to_ast(inner[0].clone()) }
            assert!(inner.len() > 2);
            assert!(inner.len() % 2 == 1);
            let left = parse_to_ast(inner[0].clone());
            inner[1..].chunks_exact(2).fold(left, |ast, pair| {
                let op = parse_to_ast(pair[0].clone());
                let right = parse_to_ast(pair[1].clone());
                Ast::BinOpCall(Box::new(op), Box::new(ast), Box::new(right))
            })
        }
        Rule::expr_post => {
            if inner.len() == 1 { return parse_to_ast(inner[0].clone()) }
            assert!(!inner.is_empty());
            let expr = parse_to_ast(inner[0].clone());
            inner[1..].iter().fold(expr, |ast, pair| {
                let op = parse_to_ast(pair.clone());
                Ast::UnaryOpCall(Box::new(op), Box::new(ast))
            })
        }
        Rule::expr_prefix => {
            if inner.len() == 1 { return parse_to_ast(inner[0].clone()) }
            assert!(!inner.is_empty());
            let mut rinner = inner;
            rinner.reverse();
            let expr = parse_to_ast(rinner[0].clone());
            rinner[1..].iter().fold(expr, |ast, pair| {
                let op = parse_to_ast(pair.clone());
                Ast::UnaryOpCall(Box::new(op), Box::new(ast))
            })
        }
        Rule::expr_apply_or_field => {
            if inner.len() == 1 { return parse_to_ast(inner[0].clone()) }
            assert!(inner.len() > 1);
            let target = parse_to_ast(inner[0].clone());
            inner[1..].iter().fold(target, |ast, pair| {
                match pair.as_rule() {
                    Rule::apply_args => {
                        let args : Vec<_> = pair.clone().into_inner().map(|e| (parse_to_ast(e.clone()))).collect();
                        Ast::FunctionCall(Box::new(ast), args)
                    }
                    Rule::field_access => {
                        let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                        assert!(inner.len() == 1);
                        let field = parse_to_ast(inner[0].clone());
                        Ast::Field(Box::new(ast), Box::new(field))
                    }
                    _ => unreachable!()
                }
            })
        } 
        Rule::function => {
            assert!(inner.len() == 2);
            assert!(inner[0].as_rule() == Rule::function_args);
            assert!(inner[1].as_rule() == Rule::block);
            let args : Vec<_> = inner[0].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
            let body = parse_to_ast(inner[1].clone());
            Ast::Function(args, Box::new(body))
        } 
        Rule::r#if | Rule::unless => {
            assert!(inner.len() == 2 || inner.len() == 3);
            let expr = parse_to_ast(inner[0].clone());
            let cond = if rule == Rule::unless {
                Ast::UnaryOpCall(Box::new(Ast::NotOp),Box::new(expr))
            } else {
                expr
            };
            assert!(inner[1].as_rule() == Rule::block);
            let then = parse_to_ast(inner[1].clone());
            let r#else = if inner.len() < 3 {
                Ast::Unit
            } else {
                parse_to_ast(inner[2].clone())
            };
            Ast::If(Box::new(cond), Box::new(then), Box::new(r#else))
        } 
        Rule::r#let => {
            assert!(inner.len() == 2);
            let var = inner[0].as_str().to_owned();
            let val = parse_to_ast(inner[1].clone());
            Ast::Let(var, Box::new(val))
        } 
        Rule::assign => {
            assert!(inner.len() == 2);
            let var = inner[0].as_str().to_owned();
            let val = parse_to_ast(inner[1].clone());
            Ast::Assign(var, Box::new(val))
        } 
        Rule::r#while => {
            assert!(inner.len() == 2);
            let cond = parse_to_ast(inner[0].clone());
            assert!(inner[1].as_rule() == Rule::block);
            let body = parse_to_ast(inner[1].clone());
            Ast::While(Box::new(cond), Box::new(body))
        } 
        Rule::do_while => {
            assert!(inner.len() == 2);
            let cond = parse_to_ast(inner[1].clone());
            assert!(inner[0].as_rule() == Rule::block);
            let body = parse_to_ast(inner[0].clone());
            Ast::DoWhile(Box::new(cond), Box::new(body))
        } 
        Rule::r#loop => {
            assert!(inner.len() == 1);
            assert!(inner[0].as_rule() == Rule::block);
            let body = parse_to_ast(inner[0].clone());
            Ast::Loop(Box::new(body))
        }
        Rule::r#return => { 
            let body = if inner.len() == 1 {
                parse_to_ast(inner[0].clone())
            } else {
                Ast::Unit
            };
            Ast::Return(Box::new(body)) 
        },
        _ => {
            println!("TODO: [{:?}] {}",rule,string);
            unreachable!()
        }
    }
}

pub fn parse_to_ast(parsed: Pair<Rule>) -> Ast {
    match parsed.as_rule() {
        Rule::integer => { Ast::Integer(parsed.as_str().parse().unwrap()) },
        Rule::float => { Ast::Float(parsed.as_str().parse().unwrap()) },
        Rule::identifier => { Ast::Identifier(parsed.as_str().to_owned()) },
        Rule::string => { Ast::String(parsed.as_str().to_owned()) },
        Rule::unit_literal => { Ast::Unit },
        Rule::unit_implicit => { Ast::Unit },
        Rule::r#true => { Ast::Boolean(true) },
        Rule::r#false => { Ast::Boolean(false) },
        Rule::dollar => { Ast::DollarOp },
        Rule::question => { Ast::QuestionOp },
        Rule::exclam => { Ast::ExclamOp },
        Rule::neg => { Ast::NegOp },
        Rule::add => { Ast::AddOp },
        Rule::mult => { Ast::MultOp },
        Rule::sub => { Ast::SubOp },
        Rule::div => { Ast::DivOp },
        Rule::exp => { Ast::ExpOp },
        Rule::or => { Ast::OrOp },
        Rule::and => { Ast::AndOp },
        Rule::not => { Ast::NotOp },
        Rule::gt => { Ast::GtOp },
        Rule::ge => { Ast::GeOp },
        Rule::lt => { Ast::LtOp },
        Rule::le => { Ast::LeOp },
        Rule::ne => { Ast::NeOp },
        Rule::eq => { Ast::EqOp },
        Rule::r#continue => { Ast::Continue },
        Rule::r#break => { Ast::Break },
        Rule::block | Rule::file  => { parse_block(parsed) },
        Rule::string_literal => {
            // these rules can't be made silent in the grammar (as of current version)
            parsed.into_inner().next().map(parse_to_ast).unwrap()
        }
        Rule::expr_infix_id | Rule::expr_or | Rule::expr_and | 
        Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
        Rule::expr_mul | Rule::expr_apply_or_field | Rule::expr_post | 
        Rule::expr_prefix | Rule::expr_exp | Rule::variable |
        Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | 
        Rule::assign | Rule::r#let | Rule::r#loop | Rule::function | Rule::r#return => {
            let rule = parsed.as_rule();
            let str = parsed.as_str().to_owned();
            let inner : Vec<Pair<Rule>> = parsed.into_inner().collect();
            parse_vec(rule,str,inner)
        }
        _ => {
            println!("TODO: [{:?}] {}",parsed.as_rule(), parsed.as_str());
            // let rule_name = format!("{:?}",parsed.as_rule());
            // parsed.into_inner().next().map(parse).unwrap_or(Ast::Error(rule_name))
            unreachable!()
        }
    }
}

