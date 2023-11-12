use std::cell::RefCell;
use pest::iterators::Pair;
use anyhow::{anyhow, Result};
use crate::{expression::{Rule, ExpressionType, Expression, self, Operator}, unescape_string, Interpreter};

impl Interpreter {

    fn parse_block(&self, parsed: Pair<Rule>) -> Result<Expression> {
        let rule = parsed.as_rule().to_owned();
        let sequence = parsed.into_inner()
            .filter_map(|e| {
                if e.as_rule() == Rule::EOI { None } 
                else { Some(self.parse_rule(e)) }
            })
            .collect::<Result<Vec<Expression>>>()?;
        Ok(match sequence.len() {
            0 => ExpressionType::Nil.into(),
            1 => sequence[0].clone(),
            _ => match rule { 
                Rule::block => ExpressionType::Block(sequence).into(),
                Rule::function_block => ExpressionType::FunctionBlock(sequence).into(),
                Rule::block_syntax => ExpressionType::Sequence(sequence).into(),
                Rule::file => ExpressionType::Sequence(sequence).into(),
                _ => Err(anyhow!("parse error block type: {rule:?}"))?,
            },
        })
    }

    fn parse_vec(&self, rule: Rule, string: String, inner: Vec<Pair<Rule>>) -> Result<Expression> {
        Ok(match rule {
                Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
                Rule::expr_rel | Rule::expr_add | Rule::expr_mul | Rule::expr_exp => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(inner.len() > 2);
                    assert!(inner.len() % 2 == 1);
                    let left = self.parse_rule(inner[0].clone())?;
                    inner[1..].chunks_exact(2).try_fold(left, |ast, pair| -> Result<Expression> {
                        let op = self.parse_rule(pair[0].clone())?;
                        let right = self.parse_rule(pair[1].clone())?;
                        if let ExpressionType::ParsedOperator(op) = op.as_ref() {
                            Ok(ExpressionType::BinOpCall(op.to_owned(), ast, right).into())
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_post => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(!inner.is_empty());
                    let expr = self.parse_rule(inner[0].clone())?;
                    inner[1..].iter().try_fold(expr, |ast, pair| -> Result<Expression> {
                        let op = self.parse_rule(pair.clone())?;
                        if let ExpressionType::ParsedOperator(op) = op.as_ref() {
                            Ok(ExpressionType::UnaryOpCall(op.to_owned(), ast).into())
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_prefix => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(!inner.is_empty());
                    let mut rinner = inner;
                    rinner.reverse();
                    let expr = self.parse_rule(rinner[0].clone())?;
                    rinner[1..].iter().try_fold(expr, |ast, pair| -> Result<Expression> {
                        let op = self.parse_rule(pair.clone())?;
                        if let ExpressionType::ParsedOperator(op) = op.as_ref() {
                            Ok(ExpressionType::UnaryOpCall(op.to_owned(), ast).into())
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_apply_or_access => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(inner.len() > 1);
                    let target = self.parse_rule(inner[0].clone())?;
                    inner[1..].iter().try_fold(target, |ast, pair| -> Result<Expression> {
                        match pair.as_rule() {
                            Rule::apply_args => {
                                let args = pair.clone().into_inner()
                                    .map(|e| (self.parse_rule(e.clone()))).collect::<Result<Vec<_>>>()?;
                                Ok(ExpressionType::FunctionCall(ast, args).into())
                            }
                            Rule::array_access => Ok(ExpressionType::ArrayAccess(ast, self.parse_rule(pair.clone())?).into()),
                            Rule::field_access => {
                                let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                assert!(inner.len() == 1);
                                // let field = inner[0].as_str().to_owned();
                                let field = self.ident(inner[0].as_str());
                                Ok(ExpressionType::Field(ast, field).into())
                            }
                            _ => Err(anyhow!("parse error expr_apply_or_access"))
                        }
                    })?
                } 
                Rule::lambda => {
                    assert!(inner.len() == 2 || inner.len() == 1);
                    if inner.len() == 2 {
                        if inner[0].as_rule() == Rule::function_args {
                            // lambda (x,...) { ... }
                            assert!(inner[1].as_rule() == Rule::function_block);
                            let args : Vec<_> = inner[0].clone().into_inner().map(|e| self.ident(e.as_str())).collect();
                            let body = self.parse_rule(inner[1].clone())?;
                            ExpressionType::Lambda(args, body).into()
                        } else {
                            // lambda x { ... }
                            assert!(inner[0].as_rule() == Rule::identifier);
                            assert!(inner[1].as_rule() == Rule::function_block);
                            let args : Vec<_> = vec![self.ident(inner[0].as_str())];
                            let body = self.parse_rule(inner[1].clone())?;
                            ExpressionType::Lambda(args, body).into()
                        }
                    } else {
                        // lambda { ... }
                        assert!(inner[0].as_rule() == Rule::function_block);
                        let body = self.parse_rule(inner[0].clone())?;
                        ExpressionType::Lambda(vec![], body).into()
                    }
                }
                Rule::array => {
                    ExpressionType::Array(RefCell::new(
                        inner.iter().map(|e| self.parse_rule(e.to_owned())).collect::<Result<Vec<_>>>()?
                    )).into()
                } 
                Rule::context => {
                    assert!(inner.len() == 2);
                    assert!(inner[0].as_rule() == Rule::function_args);
                    // let args : Vec<_> = inner[0].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                    let args : Vec<_> = inner[0].clone().into_inner().map(|e| self.ident(e.as_str())).collect();
                    let body = self.parse_rule(inner[1].clone())?;
                    ExpressionType::Context(args, body).into()
                } 
                Rule::object => {
                    assert!(inner.len() == 1);
                    let body = self.parse_rule(inner[0].clone())?;
                    ExpressionType::Object(body).into()
                } 
                Rule::module => {
                    assert!(inner.len() == 3);
                    // let var = inner[0].as_str().to_owned();
                    let var = self.ident(inner[0].as_str());
                    assert!(inner[1].as_rule() == Rule::function_args);
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    // let args : Vec<_> = inner[1].clone().into_inner().map(|e| e.as_str().to_owned()).collect();
                    let args : Vec<_> = inner[1].clone().into_inner().map(|e| self.ident(e.as_str())).collect();
                    let body = self.parse_rule(inner[2].clone())?;
                    ExpressionType::Module(var, args, body).into()
                } 
                Rule::r#if | Rule::unless => {
                    assert!(inner.len() == 2 || inner.len() == 3);
                    let expr = self.parse_rule(inner[0].clone())?;
                    let cond = if rule == Rule::unless {
                        ExpressionType::UnaryOpCall(Operator::Not,expr).into()
                    } else {
                        expr
                    };
                    assert!(inner[1].as_rule() == Rule::block);
                    let then = self.parse_rule(inner[1].clone())?;
                    let r#else = if inner.len() < 3 {
                        ExpressionType::Nil.into()
                    } else {
                        self.parse_rule(inner[2].clone())?
                    };
                    ExpressionType::If(cond, then, r#else).into()
                } 
                Rule::bind => {
                    assert!(!inner.is_empty() || inner.len() <= 3);
                    // let var = inner[0].as_str().to_owned();
                    let var = self.ident(inner[0].as_str());
                    if inner.len() == 1 {
                        // let value = ExpressionType::Variable(var.to_owned()).into();
                        // let body = ExpressionType::Variable(var.to_owned()).into();
                        let value = ExpressionType::Variable(var.to_owned()).into();
                        let body = ExpressionType::Variable(var.to_owned()).into();
                        ExpressionType::BindIn(var, value, body).into()
                    } else if inner.len() == 2 {
                        // let value = ExpressionType::Variable(var.to_owned()).into();
                        let value = ExpressionType::Variable(var.to_owned()).into();
                        let body = self.parse_rule(inner[1].clone())?;
                        ExpressionType::BindIn(var, value, body).into()
                    } else {
                        let value = self.parse_rule(inner[1].clone())?;
                        let body = self.parse_rule(inner[2].clone())?;
                        ExpressionType::BindIn(var, value, body).into()
                    }
                } 
                Rule::r#let => {
                    assert!(inner.len() == 1 || inner.len() == 2);
                    let val = if inner.len() == 2 {
                        self.parse_rule(inner[1].clone())?
                    } else {
                        expression::nil()
                    };
                    match inner[0].as_rule() {
                        Rule::identifier => {
                            // let var = inner[0].as_str().to_owned();
                            let var = self.ident(inner[0].as_str());
                            ExpressionType::Let(var, val).into()
                        }
                        Rule::identifier_array => {
                            let vars = inner[0].clone().into_inner().map(|pair| {
                                // pair.as_str().to_owned()
                                self.ident(pair.as_str())
                            }).collect();
                            ExpressionType::LetArray(vars, val).into()
                        }
                        _ => Err(anyhow!("bad let syntax {:?}", inner[1].as_rule()))?,
                    }
                } 
                Rule::defun => {
                    assert!(inner.len() == 3);
                    let var = self.ident(inner[0].as_str());
                    assert!(inner[1].as_rule() == Rule::function_args);
                    assert!(inner[2].as_rule() == Rule::function_block);
                    let args : Vec<_> = inner[1].clone().into_inner().map(|e| self.ident(e.as_str())).collect();
                    let body = self.parse_rule(inner[2].clone())?;
                    ExpressionType::Defun(var, args, body).into()
                } 
                Rule::assign | Rule::deref_assign => {
                    assert!(inner.len() >= 2);
                    let var_str = inner[0].as_str().to_owned();
                    let var = ExpressionType::Variable(self.ident(&var_str)).into();
                    if inner.len() == 2 {
                        let val = self.parse_rule(inner[1].clone())?;
                        if rule == Rule::assign {
                            ExpressionType::AssignToExpression(var, val).into()
                        } else {
                            ExpressionType::AssignToDeRefExpression(var, val).into()
                        }
                    } else {
                        let chain = inner[1..inner.len()-1].iter().try_fold(var, |acc, pair| {
                            match pair.as_rule() {
                                Rule::array_access => Ok(ExpressionType::ArrayAccess(acc, self.parse_rule(pair.clone())?).into()),
                                Rule::field_access => {
                                    let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                    assert!(inner.len() == 1);
                                    // let field = inner[0].as_str().to_owned();
                                    let field = self.ident(inner[0].as_str());
                                    Ok(ExpressionType::Field(acc, field).into())
                                }
                                _ => Err(anyhow!("bad assign chain")),
                            }
                        })?;
                        let val = self.parse_rule(inner[inner.len()-1].clone())?;
                        if rule == Rule::assign {
                            ExpressionType::AssignToExpression(chain, val).into()
                        } else {
                            ExpressionType::AssignToDeRefExpression(chain, val).into()
                        }
                    }
                }
                Rule::r#while => {
                    assert!(inner.len() == 2);
                    let cond = self.parse_rule(inner[0].clone())?;
                    assert!(inner[1].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[1].clone())?;
                    ExpressionType::While(cond, body).into()
                } 
                Rule::do_while => {
                    assert!(inner.len() == 2);
                    let cond = self.parse_rule(inner[1].clone())?;
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    ExpressionType::DoWhile(cond, body).into()
                } 
                Rule::r#loop => {
                    assert!(inner.len() == 1);
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    ExpressionType::Loop(body).into()
                }
                Rule::r#for => {
                    assert!(inner.len() == 3);
                    assert!(inner[0].as_rule() == Rule::variable);
                    // let var = inner[0].as_str().to_owned();
                    let var = self.ident(inner[0].as_str());
                    let expr = self.parse_rule(inner[1].clone())?;
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    ExpressionType::For(var, expr, body).into()
                } 
                Rule::try_catch => {
                    assert!(inner.len() == 3);
                    let expr = self.parse_rule(inner[0].clone())?;
                    assert!(inner[1].as_rule() == Rule::variable);
                    let var = self.ident(inner[1].as_str());
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    ExpressionType::TryCatch(expr, var, body).into()
                } 
                Rule::r#return => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        ExpressionType::Nil.into()
                    };
                    ExpressionType::Return(body).into() 
                },
                Rule::r#break => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        ExpressionType::Nil.into()
                    };
                    ExpressionType::Break(body).into() 
                },
                Rule::throw => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        ExpressionType::Nil.into()
                    };
                    ExpressionType::Throw(body).into() 
                },
                Rule::infix_identifier => { 
                    assert!(inner.len() == 1);
                    let id = self.ident(inner[0].as_str());
                    ExpressionType::ParsedOperator(Operator::Identifier(id)).into() 
                },
                _ => {
                    Err(anyhow!("TODO: [{:?}] {}",rule,string))?
                }
            })
    }

    pub fn parse_rule(&self, parsed: Pair<Rule>) -> Result<Expression> {
        Ok(match parsed.as_rule() {
            Rule::integer => { ExpressionType::Integer(parsed.as_str().parse()?).into() },
            Rule::float => { ExpressionType::Float(parsed.as_str().parse()?).into() },
            Rule::string => { ExpressionType::String(unescape_string(parsed.as_str())).into() },
            Rule::identifier => { 
                // ExpressionType::Identifier(parsed.as_str().to_owned()).into() 
                ExpressionType::ParsedIdentifier(self.ident(parsed.as_str())).into() 
            },
            Rule::character => { 
                assert!(!parsed.as_str().is_empty());
                ExpressionType::Character(unescape_string(parsed.as_str()).chars().next().unwrap()).into() 
            },
            Rule::variable => { 
                if parsed.as_str().starts_with('@') {
                    ExpressionType::BuiltinVariable(parsed.as_str().strip_prefix('@').unwrap().to_owned()).into() 
                } else {
                    ExpressionType::Variable(self.ident(parsed.as_str())).into() 
                }
            },
            Rule::nil_literal => { ExpressionType::Nil.into() },
            Rule::nil_implicit => { ExpressionType::Nil.into() },
            Rule::r#true => { ExpressionType::Boolean(true).into() },
            Rule::r#false => { ExpressionType::Boolean(false).into() },
            Rule::r#ref => { ExpressionType::ParsedOperator(Operator::Ref).into() },
            Rule::deref => { ExpressionType::ParsedOperator(Operator::DeRef).into() },
            Rule::question => { ExpressionType::ParsedOperator(Operator::Question).into() },
            Rule::exclam => { ExpressionType::ParsedOperator(Operator::Exclam).into() },
            Rule::pipe => { ExpressionType::ParsedOperator(Operator::Pipe).into() },
            Rule::neg => { ExpressionType::ParsedOperator(Operator::Neg).into() },
            Rule::add => { ExpressionType::ParsedOperator(Operator::Add).into() },
            Rule::mult => { ExpressionType::ParsedOperator(Operator::Mul).into() },
            Rule::sub => { ExpressionType::ParsedOperator(Operator::Sub).into() },
            Rule::div => { ExpressionType::ParsedOperator(Operator::Div).into() },
            Rule::intdiv => { ExpressionType::ParsedOperator(Operator::IntDiv).into() },
            Rule::r#mod => { ExpressionType::ParsedOperator(Operator::Mod).into() },
            Rule::exp => { ExpressionType::ParsedOperator(Operator::Exp).into() },
            Rule::or => { ExpressionType::ParsedOperator(Operator::Or).into() },
            Rule::and => { ExpressionType::ParsedOperator(Operator::And).into() },
            Rule::not => { ExpressionType::ParsedOperator(Operator::Not).into() },
            Rule::gt => { ExpressionType::ParsedOperator(Operator::Gt).into() },
            Rule::ge => { ExpressionType::ParsedOperator(Operator::Ge).into() },
            Rule::lt => { ExpressionType::ParsedOperator(Operator::Lt).into() },
            Rule::le => { ExpressionType::ParsedOperator(Operator::Le).into() },
            Rule::ne => { ExpressionType::ParsedOperator(Operator::Ne).into() },
            Rule::eq => { ExpressionType::ParsedOperator(Operator::Eq).into() },
            Rule::r#continue => { ExpressionType::Continue.into() },
            Rule::block_syntax | Rule::block | Rule::file | 
            Rule::function_block  => { self.parse_block(parsed)? },
            Rule::string_literal | Rule::char_literal  | Rule::array_access => {
                let inner = parsed.into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?;
                self.parse_rule(inner)?
            }
            Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | 
            Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
            Rule::expr_mul | Rule::expr_apply_or_access | Rule::expr_post | 
            Rule::expr_prefix | Rule::expr_exp | 
            Rule::context | Rule::module | Rule::defun | 
            Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | Rule::array |
            Rule::assign | Rule::deref_assign | Rule::object | Rule::bind |
            Rule::r#let | Rule::r#loop | Rule::r#for | Rule::try_catch |
            Rule::lambda | Rule::r#break | Rule::throw |
            Rule::infix_identifier | Rule::r#return => {
                let rule = parsed.as_rule();
                let str = parsed.as_str().to_owned();
                let inner : Vec<Pair<Rule>> = parsed.into_inner().collect();
                self.parse_vec(rule,str,inner)?
            }
            _ => {
                Err(anyhow!("TODO: [{:?}] {}",parsed.as_rule(), parsed.as_str()))?
            }
        })
    }

}
