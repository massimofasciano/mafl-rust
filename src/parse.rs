use std::{cell::RefCell, rc::Rc};

use pest::iterators::Pair;
use anyhow::{anyhow, Result};
use crate::expression::{Rule, Expression};

fn parse_block(parsed: Pair<Rule>) -> Result<Expression> {
    let is_block = parsed.as_rule() != Rule::file;
    let sequence = parsed.into_inner()
        .filter_map(|e| {
            if e.as_rule() == Rule::EOI { None } 
            else { Some(parse_rule(e)) }
        })
        .collect::<Result<Vec<Expression>>>()?;
    Ok(match sequence.len() {
        0 => Expression::Unit,
        1 => sequence[0].clone(),
        _ => if is_block { 
            Expression::Block(sequence) 
        } else { 
            Expression::Sequence(sequence) 
        },
    })
}

fn parse_vec(rule: Rule, string: String, inner: Vec<Pair<Rule>>) -> Result<Expression> {
    Ok(match rule {
            Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
            Rule::expr_rel | Rule::expr_add | Rule::expr_mul | Rule::expr_exp => {
                if inner.len() == 1 { return parse_rule(inner[0].clone()) }
                assert!(inner.len() > 2);
                assert!(inner.len() % 2 == 1);
                let left = parse_rule(inner[0].clone())?;
                inner[1..].chunks_exact(2).try_fold(left, |ast, pair| -> Result<Expression> {
                    let op = parse_rule(pair[0].clone())?;
                    let right = parse_rule(pair[1].clone())?;
                    Ok(Expression::BinOpCall(Box::new(op), Box::new(ast), Box::new(right)))
                })?
            }
            Rule::expr_post => {
                if inner.len() == 1 { return parse_rule(inner[0].clone()) }
                assert!(!inner.is_empty());
                let expr = parse_rule(inner[0].clone())?;
                inner[1..].iter().try_fold(expr, |ast, pair| -> Result<Expression> {
                    let op = parse_rule(pair.clone())?;
                    Ok(Expression::UnaryOpCall(Box::new(op), Box::new(ast)))
                })?
            }
            Rule::expr_prefix => {
                if inner.len() == 1 { return parse_rule(inner[0].clone()) }
                assert!(!inner.is_empty());
                let mut rinner = inner;
                rinner.reverse();
                let expr = parse_rule(rinner[0].clone())?;
                rinner[1..].iter().try_fold(expr, |ast, pair| -> Result<Expression> {
                    let op = parse_rule(pair.clone())?;
                    Ok(Expression::UnaryOpCall(Box::new(op), Box::new(ast)))
                })?
            }
            Rule::expr_apply_or_field => {
                if inner.len() == 1 { return parse_rule(inner[0].clone()) }
                assert!(inner.len() > 1);
                let target = parse_rule(inner[0].clone())?;
                inner[1..].iter().try_fold(target, |ast, pair| -> Result<Expression> {
                    match pair.as_rule() {
                        Rule::apply_args => {
                            let args = pair.clone().into_inner()
                                .map(|e| (parse_rule(e.clone()))).collect::<Result<Vec<_>>>()?;
                            Ok(Expression::FunctionCall(Box::new(ast), args))
                        }
                        Rule::field_access => {
                            let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                            assert!(inner.len() == 1);
                            let field = inner[0].as_str().to_owned();
                            Ok(Expression::Field(Box::new(ast), field))
                        }
                        _ => Err(anyhow!("parse error expr_apply_or_field"))
                    }
                })?
            } 
            Rule::function | Rule::closure | Rule::dynamic => {
                assert!(inner.len() == 2);
                assert!(inner[0].as_rule() == Rule::function_args);
                assert!(inner[1].as_rule() == Rule::block);
                let args : Vec<_> = inner[0].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                let body = parse_rule(inner[1].clone())?;
                match rule {
                    Rule::function => Expression::Function(args, Box::new(body)),
                    Rule::closure => Expression::ClosureSyntax(args, Box::new(body)),
                    Rule::dynamic => Expression::FunctionDynamic(args, Box::new(body)),
                    _ => Err(anyhow!("parse error function or closure"))?
                }
            }
            Rule::array => {
                Expression::Array(Rc::new(RefCell::new(
                    inner.iter().map(|e| parse_rule(e.to_owned())).collect::<Result<Vec<_>>>()?
                )))
            } 
            Rule::context => {
                assert!(inner.len() == 2);
                assert!(inner[0].as_rule() == Rule::function_args);
                let args : Vec<_> = inner[0].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                let body = parse_rule(inner[1].clone())?;
                Expression::ContextSyntax(args, Box::new(body))
            } 
            Rule::r#if | Rule::unless => {
                assert!(inner.len() == 2 || inner.len() == 3);
                let expr = parse_rule(inner[0].clone())?;
                let cond = if rule == Rule::unless {
                    Expression::UnaryOpCall(Box::new(Expression::NotOp),Box::new(expr))
                } else {
                    expr
                };
                assert!(inner[1].as_rule() == Rule::block);
                let then = parse_rule(inner[1].clone())?;
                let r#else = if inner.len() < 3 {
                    Expression::Unit
                } else {
                    parse_rule(inner[2].clone())?
                };
                Expression::If(Box::new(cond), Box::new(then), Box::new(r#else))
            } 
            Rule::let_in => {
                assert!(inner.len() == 3);
                let var = inner[0].as_str().to_owned();
                let val = parse_rule(inner[1].clone())?;
                let body = parse_rule(inner[2].clone())?;
                Expression::LetIn(var, Box::new(val), Box::new(body))
            } 
            Rule::r#let => {
                assert!(inner.len() == 2);
                let var = inner[0].as_str().to_owned();
                let val = parse_rule(inner[1].clone())?;
                Expression::Let(var, Box::new(val))
            } 
            Rule::assign => {
                assert!(inner.len() == 2);
                let var = inner[0].as_str().to_owned();
                let val = parse_rule(inner[1].clone())?;
                Expression::Assign(var, Box::new(val))
            } 
            // Rule::assign => {
            //     assert!(inner.len() >= 2);
            //     let var = inner[0].as_str().to_owned();
            //     if inner.len() == 2 {
            //         let val = parse_rule(inner[1].clone())?;
            //         Expression::AssignToExpression(Box::new(Expression::Variable(var)), Box::new(val))
            //     } else if inner.len() == 3 {
            //         let field_inner : Vec<Pair<Rule>> = inner[1].clone().into_inner().collect();
            //         assert!(field_inner.len() == 1);
            //         let field = field_inner[0].as_str().to_owned();
            //         let val = parse_rule(inner[2].clone())?;
            //         Expression::AssignToExpression(Box::new(Expression::Field(Box::new(Expression::Variable(var)),field)), Box::new(val))
            //     }
            //     else {
            //         Err(anyhow!("error parsing assign (TODO: treat more than one field in chain)"))?
            //     }
            // } 
            Rule::r#while => {
                assert!(inner.len() == 2);
                let cond = parse_rule(inner[0].clone())?;
                assert!(inner[1].as_rule() == Rule::block);
                let body = parse_rule(inner[1].clone())?;
                Expression::While(Box::new(cond), Box::new(body))
            } 
            Rule::do_while => {
                assert!(inner.len() == 2);
                let cond = parse_rule(inner[1].clone())?;
                assert!(inner[0].as_rule() == Rule::block);
                let body = parse_rule(inner[0].clone())?;
                Expression::DoWhile(Box::new(cond), Box::new(body))
            } 
            Rule::r#loop => {
                assert!(inner.len() == 1);
                assert!(inner[0].as_rule() == Rule::block);
                let body = parse_rule(inner[0].clone())?;
                Expression::Loop(Box::new(body))
            }
            Rule::r#return => { 
                let body = if inner.len() == 1 {
                    parse_rule(inner[0].clone())?
                } else {
                    Expression::Unit
                };
                Expression::Return(Box::new(body)) 
            },
            _ => {
                Err(anyhow!("TODO: [{:?}] {}",rule,string))?
            }
        })
}

pub fn parse_rule(parsed: Pair<Rule>) -> Result<Expression> {
    Ok(match parsed.as_rule() {
        Rule::integer => { Expression::Integer(parsed.as_str().parse()?) },
        Rule::float => { Expression::Float(parsed.as_str().parse()?) },
        // Rule::identifier => { Expression::Identifier(parsed.as_str().to_owned()) },
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
        Rule::r#mod => { Expression::ModOp },
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
        Rule::block | Rule::file  => { parse_block(parsed)? },
        Rule::string_literal => {
            // these rules can't be made silent in the grammar (as of current version)
            let inner = parsed.into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?;
            parse_rule(inner)?
        }
        Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | 
        Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
        Rule::expr_mul | Rule::expr_apply_or_field | Rule::expr_post | 
        Rule::expr_prefix | Rule::expr_exp | Rule::let_in | Rule::context |
        Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | Rule::array |
        Rule::assign | Rule::r#let | Rule::r#loop | 
        Rule::function | Rule::closure | Rule::dynamic |
        Rule::r#return => {
            let rule = parsed.as_rule();
            let str = parsed.as_str().to_owned();
            let inner : Vec<Pair<Rule>> = parsed.into_inner().collect();
            parse_vec(rule,str,inner)?
        }
        _ => {
            Err(anyhow!("TODO: [{:?}] {}",parsed.as_rule(), parsed.as_str()))?
        }
    })
}

