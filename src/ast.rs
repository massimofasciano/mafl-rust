use pest::iterators::Pair;

use crate::types::{Rule, Expression};

fn parse_block(parsed: Pair<Rule>) -> Expression {
    let sequence : Vec<Expression> = parsed.into_inner()
        .filter_map(|e| {
            if e.as_rule() == Rule::EOI {
                None
            } else {
                Some(parse_to_ast(e))
            }
        })
        .collect();
    match sequence.len() {
        0 => Expression::Unit,
        1 => sequence[0].clone(),
        _ => Expression::Block(sequence),
    }
}

fn parse_vec(rule: Rule, string: String, inner: Vec<Pair<Rule>>) -> Expression {
    match rule {
        Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
        Rule::expr_rel | Rule::expr_add | Rule::expr_mul | Rule::expr_exp => {
            if inner.len() == 1 { return parse_to_ast(inner[0].clone()) }
            assert!(inner.len() > 2);
            assert!(inner.len() % 2 == 1);
            let left = parse_to_ast(inner[0].clone());
            inner[1..].chunks_exact(2).fold(left, |ast, pair| {
                let op = parse_to_ast(pair[0].clone());
                let right = parse_to_ast(pair[1].clone());
                Expression::BinOpCall(Box::new(op), Box::new(ast), Box::new(right))
            })
        }
        Rule::expr_post => {
            if inner.len() == 1 { return parse_to_ast(inner[0].clone()) }
            assert!(!inner.is_empty());
            let expr = parse_to_ast(inner[0].clone());
            inner[1..].iter().fold(expr, |ast, pair| {
                let op = parse_to_ast(pair.clone());
                Expression::UnaryOpCall(Box::new(op), Box::new(ast))
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
                Expression::UnaryOpCall(Box::new(op), Box::new(ast))
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
                        Expression::FunctionCall(Box::new(ast), args)
                    }
                    Rule::field_access => {
                        let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                        assert!(inner.len() == 1);
                        let field = parse_to_ast(inner[0].clone());
                        Expression::Field(Box::new(ast), Box::new(field))
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
            Expression::Function(args, Box::new(body))
        } 
        Rule::r#if | Rule::unless => {
            assert!(inner.len() == 2 || inner.len() == 3);
            let expr = parse_to_ast(inner[0].clone());
            let cond = if rule == Rule::unless {
                Expression::UnaryOpCall(Box::new(Expression::NotOp),Box::new(expr))
            } else {
                expr
            };
            assert!(inner[1].as_rule() == Rule::block);
            let then = parse_to_ast(inner[1].clone());
            let r#else = if inner.len() < 3 {
                Expression::Unit
            } else {
                parse_to_ast(inner[2].clone())
            };
            Expression::If(Box::new(cond), Box::new(then), Box::new(r#else))
        } 
        Rule::let_in => {
            assert!(inner.len() == 3);
            let var = inner[0].as_str().to_owned();
            let val = parse_to_ast(inner[1].clone());
            let body = parse_to_ast(inner[2].clone());
            Expression::Let(var, Box::new(val), Box::new(body))
        } 
        Rule::var => {
            assert!(inner.len() == 2);
            let var = inner[0].as_str().to_owned();
            let val = parse_to_ast(inner[1].clone());
            Expression::Var(var, Box::new(val))
        } 
        Rule::assign => {
            assert!(inner.len() == 2);
            let var = inner[0].as_str().to_owned();
            let val = parse_to_ast(inner[1].clone());
            Expression::Assign(var, Box::new(val))
        } 
        Rule::r#while => {
            assert!(inner.len() == 2);
            let cond = parse_to_ast(inner[0].clone());
            assert!(inner[1].as_rule() == Rule::block);
            let body = parse_to_ast(inner[1].clone());
            Expression::While(Box::new(cond), Box::new(body))
        } 
        Rule::do_while => {
            assert!(inner.len() == 2);
            let cond = parse_to_ast(inner[1].clone());
            assert!(inner[0].as_rule() == Rule::block);
            let body = parse_to_ast(inner[0].clone());
            Expression::DoWhile(Box::new(cond), Box::new(body))
        } 
        Rule::r#loop => {
            assert!(inner.len() == 1);
            assert!(inner[0].as_rule() == Rule::block);
            let body = parse_to_ast(inner[0].clone());
            Expression::Loop(Box::new(body))
        }
        Rule::r#return => { 
            let body = if inner.len() == 1 {
                parse_to_ast(inner[0].clone())
            } else {
                Expression::Unit
            };
            Expression::Return(Box::new(body)) 
        },
        _ => {
            println!("TODO: [{:?}] {}",rule,string);
            unreachable!()
        }
    }
}

pub fn parse_to_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::integer => { Expression::Integer(parsed.as_str().parse().unwrap()) },
        Rule::float => { Expression::Float(parsed.as_str().parse().unwrap()) },
        Rule::identifier => { Expression::Identifier(parsed.as_str().to_owned()) },
        Rule::infix_identifier => { Expression::InfixOp(parsed.as_str().to_owned()) },
        Rule::string => { Expression::String(parsed.as_str().to_owned()) },
        Rule::variable => { Expression::Variable(parsed.as_str().to_owned()) },
        Rule::unit_literal => { Expression::Unit },
        Rule::unit_implicit => { Expression::Unit },
        Rule::r#true => { Expression::Boolean(true) },
        Rule::r#false => { Expression::Boolean(false) },
        Rule::r#ref => { Expression::RefOp },
        Rule::deref => { Expression::DeRefOp },
        Rule::question => { Expression::QuestionOp },
        Rule::exclam => { Expression::ExclamOp },
        Rule::pipe => { Expression::PipeOp },
        Rule::neg => { Expression::NegOp },
        Rule::add => { Expression::AddOp },
        Rule::mult => { Expression::MultOp },
        Rule::sub => { Expression::SubOp },
        Rule::div => { Expression::DivOp },
        Rule::exp => { Expression::ExpOp },
        Rule::or => { Expression::OrOp },
        Rule::and => { Expression::AndOp },
        Rule::not => { Expression::NotOp },
        Rule::gt => { Expression::GtOp },
        Rule::ge => { Expression::GeOp },
        Rule::lt => { Expression::LtOp },
        Rule::le => { Expression::LeOp },
        Rule::ne => { Expression::NeOp },
        Rule::eq => { Expression::EqOp },
        Rule::r#continue => { Expression::Continue },
        Rule::r#break => { Expression::Break },
        Rule::block | Rule::file  => { parse_block(parsed) },
        Rule::string_literal => {
            // these rules can't be made silent in the grammar (as of current version)
            parsed.into_inner().next().map(parse_to_ast).unwrap()
        }
        Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | 
        Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
        Rule::expr_mul | Rule::expr_apply_or_field | Rule::expr_post | 
        Rule::expr_prefix | Rule::expr_exp | Rule::let_in | 
        Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | 
        Rule::assign | Rule::var | Rule::r#loop | Rule::function | Rule::r#return => {
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

