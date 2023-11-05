use std::cell::RefCell;

use pest::iterators::Pair;
use anyhow::{anyhow, Result};
use crate::{expression::{Rule, ExpressionType, Expression}, unescape_string};

fn parse_block(parsed: Pair<Rule>) -> Result<Expression> {
    let rule = parsed.as_rule().to_owned();
    let sequence = parsed.into_inner()
        .filter_map(|e| {
            if e.as_rule() == Rule::EOI { None } 
            else { Some(parse_rule(e)) }
        })
        .collect::<Result<Vec<Expression>>>()?;
    Ok(match sequence.len() {
        0 => ExpressionType::Nil.into(),
        1 => sequence[0].clone(),
        _ => match rule { 
            Rule::block => ExpressionType::Block(sequence).into(),
            Rule::block_syntax => ExpressionType::Sequence(sequence).into(),
            Rule::file => ExpressionType::Sequence(sequence).into(),
            _ => Err(anyhow!("parse error block type: {rule:?}"))?,
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
                    Ok(ExpressionType::BinOpCall(op, ast, right).into())
                })?
            }
            Rule::expr_post => {
                if inner.len() == 1 { return parse_rule(inner[0].clone()) }
                assert!(!inner.is_empty());
                let expr = parse_rule(inner[0].clone())?;
                inner[1..].iter().try_fold(expr, |ast, pair| -> Result<Expression> {
                    let op = parse_rule(pair.clone())?;
                    Ok(ExpressionType::UnaryOpCall(op, ast).into())
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
                    Ok(ExpressionType::UnaryOpCall(op, ast).into())
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
                            Ok(ExpressionType::FunctionCall(ast, args).into())
                        }
                        Rule::field_access => {
                            let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                            assert!(inner.len() == 1);
                            let field = inner[0].as_str().to_owned();
                            Ok(ExpressionType::Field(ast, field).into())
                        }
                        _ => Err(anyhow!("parse error expr_apply_or_field"))
                    }
                })?
            } 
            Rule::closure => {
                assert!(inner.len() == 2);
                assert!(inner[0].as_rule() == Rule::function_args);
                assert!(inner[1].as_rule() == Rule::block);
                let args : Vec<_> = inner[0].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                let body = parse_rule(inner[1].clone())?;
                match rule {
                    Rule::closure => ExpressionType::Lambda(args, body).into(),
                    _ => Err(anyhow!("parse error lambda"))?
                }
            }
            Rule::array => {
                ExpressionType::Array(RefCell::new(
                    inner.iter().map(|e| parse_rule(e.to_owned())).collect::<Result<Vec<_>>>()?
                )).into()
            } 
            Rule::context => {
                assert!(inner.len() == 2);
                assert!(inner[0].as_rule() == Rule::function_args);
                let args : Vec<_> = inner[0].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                let body = parse_rule(inner[1].clone())?;
                ExpressionType::Context(args, body).into()
            } 
            Rule::module => {
                assert!(inner.len() == 3);
                let var = inner[0].as_str().to_owned();
                assert!(inner[1].as_rule() == Rule::function_args);
                assert!(inner[2].as_rule() == Rule::block_syntax);
                let args : Vec<_> = inner[1].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                let body = parse_rule(inner[2].clone())?;
                ExpressionType::Module(var, args, body).into()
            } 
            Rule::r#if | Rule::unless => {
                assert!(inner.len() == 2 || inner.len() == 3);
                let expr = parse_rule(inner[0].clone())?;
                let cond = if rule == Rule::unless {
                    ExpressionType::UnaryOpCall(ExpressionType::NotOp.into(),expr).into()
                } else {
                    expr
                };
                assert!(inner[1].as_rule() == Rule::block);
                let then = parse_rule(inner[1].clone())?;
                let r#else = if inner.len() < 3 {
                    ExpressionType::Nil.into()
                } else {
                    parse_rule(inner[2].clone())?
                };
                ExpressionType::If(cond, then, r#else).into()
            } 
            Rule::let_in => {
                assert!(inner.len() == 3);
                let var = inner[0].as_str().to_owned();
                let val = parse_rule(inner[1].clone())?;
                let body = parse_rule(inner[2].clone())?;
                ExpressionType::LetIn(var, val, body).into()
            } 
            Rule::r#let => {
                assert!(inner.len() == 2);
                let var = inner[0].as_str().to_owned();
                let val = parse_rule(inner[1].clone())?;
                ExpressionType::Let(var, val).into()
            } 
            Rule::defun => {
                assert!(inner.len() == 3);
                let var = inner[0].as_str().to_owned();
                assert!(inner[1].as_rule() == Rule::function_args);
                assert!(inner[2].as_rule() == Rule::block);
                let args : Vec<_> = inner[1].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                let body = parse_rule(inner[2].clone())?;
                ExpressionType::Defun(var, args, body).into()
            } 
            Rule::assign => {
                assert!(inner.len() == 2);
                let var = inner[0].as_str().to_owned();
                let val = parse_rule(inner[1].clone())?;
                ExpressionType::Assign(var, val).into()
            } 
            Rule::r#while => {
                assert!(inner.len() == 2);
                let cond = parse_rule(inner[0].clone())?;
                assert!(inner[1].as_rule() == Rule::block_syntax);
                let body = parse_rule(inner[1].clone())?;
                ExpressionType::While(cond, body).into()
            } 
            Rule::do_while => {
                assert!(inner.len() == 2);
                let cond = parse_rule(inner[1].clone())?;
                assert!(inner[0].as_rule() == Rule::block_syntax);
                let body = parse_rule(inner[0].clone())?;
                ExpressionType::DoWhile(cond, body).into()
            } 
            Rule::r#loop => {
                assert!(inner.len() == 1);
                assert!(inner[0].as_rule() == Rule::block_syntax);
                let body = parse_rule(inner[0].clone())?;
                ExpressionType::Loop(body).into()
            }
            Rule::r#for => {
                assert!(inner.len() == 3);
                assert!(inner[0].as_rule() == Rule::variable);
                let var = inner[0].as_str().to_owned();
                let expr = parse_rule(inner[1].clone())?;
                assert!(inner[2].as_rule() == Rule::block_syntax);
                let body = parse_rule(inner[2].clone())?;
                ExpressionType::For(var, expr, body).into()
            } 
            Rule::r#return => { 
                let body = if inner.len() == 1 {
                    parse_rule(inner[0].clone())?
                } else {
                    ExpressionType::Nil.into()
                };
                ExpressionType::Return(body).into() 
            },
            Rule::infix_identifier => { 
                assert!(inner.len() == 1);
                let id = inner[0].as_str().to_owned();
                ExpressionType::InfixOp(id).into() 
            },
            _ => {
                Err(anyhow!("TODO: [{:?}] {}",rule,string))?
            }
        })
}

pub fn parse_rule(parsed: Pair<Rule>) -> Result<Expression> {
    Ok(match parsed.as_rule() {
        Rule::integer => { ExpressionType::Integer(parsed.as_str().parse()?).into() },
        Rule::float => { ExpressionType::Float(parsed.as_str().parse()?).into() },
        Rule::string => { ExpressionType::String(unescape_string(parsed.as_str())).into() },
        Rule::character => { 
            assert!(!parsed.as_str().is_empty());
            ExpressionType::Character(unescape_string(parsed.as_str()).chars().next().unwrap()).into() 
        },
        Rule::variable => { ExpressionType::Variable(parsed.as_str().to_owned()).into() },
        Rule::nil_literal => { ExpressionType::Nil.into() },
        Rule::nil_implicit => { ExpressionType::Nil.into() },
        Rule::r#true => { ExpressionType::Boolean(true).into() },
        Rule::r#false => { ExpressionType::Boolean(false).into() },
        Rule::r#ref => { ExpressionType::RefOp.into() },
        Rule::deref => { ExpressionType::DeRefOp.into() },
        Rule::question => { ExpressionType::QuestionOp.into() },
        Rule::exclam => { ExpressionType::ExclamOp.into() },
        Rule::pipe => { ExpressionType::PipeOp.into() },
        Rule::neg => { ExpressionType::NegOp.into() },
        Rule::add => { ExpressionType::AddOp.into() },
        Rule::mult => { ExpressionType::MultOp.into() },
        Rule::sub => { ExpressionType::SubOp.into() },
        Rule::div => { ExpressionType::DivOp.into() },
        Rule::r#mod => { ExpressionType::ModOp.into() },
        Rule::exp => { ExpressionType::ExpOp.into() },
        Rule::or => { ExpressionType::OrOp.into() },
        Rule::and => { ExpressionType::AndOp.into() },
        Rule::not => { ExpressionType::NotOp.into() },
        Rule::gt => { ExpressionType::GtOp.into() },
        Rule::ge => { ExpressionType::GeOp.into() },
        Rule::lt => { ExpressionType::LtOp.into() },
        Rule::le => { ExpressionType::LeOp.into() },
        Rule::ne => { ExpressionType::NeOp.into() },
        Rule::eq => { ExpressionType::EqOp.into() },
        Rule::r#continue => { ExpressionType::Continue.into() },
        Rule::r#break => { ExpressionType::Break.into() },
        Rule::block_syntax | Rule::block | Rule::file  => { parse_block(parsed)? },
        Rule::string_literal | Rule::char_literal => {
            // these rules can't be made silent in the grammar (as of current version)
            let inner = parsed.into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?;
            parse_rule(inner)?
        }
        Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | 
        Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
        Rule::expr_mul | Rule::expr_apply_or_field | Rule::expr_post | 
        Rule::expr_prefix | Rule::expr_exp | 
        Rule::let_in | Rule::context | Rule::module | Rule::defun | 
        Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | Rule::array |
        Rule::assign | Rule::r#let | Rule::r#loop | Rule::r#for |
        Rule::closure | 
        Rule::infix_identifier | Rule::r#return => {
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

