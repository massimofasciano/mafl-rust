use std::cell::RefCell;
use pest::iterators::Pair;
use anyhow::{anyhow, Result};
use crate::{expression::{Rule, ExpressionType, Expression, self, Operator, BlockType}, unescape_string, Interpreter};

impl Interpreter {

    fn find_tag<'a>(tag: &'a str, pairs: &'a [Pair<Rule>]) -> impl Iterator<Item=&'a Pair<'a,Rule>> + 'a {
        pairs.iter().filter(move |p| {
            let ptag = p.as_node_tag();
            ptag.is_some() && ptag.unwrap() == tag
        })
    }

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
            _ => match rule { 
                Rule::block => ExpressionType::Block{r#type: BlockType::Block, body: sequence}.into(),
                Rule::function_block => ExpressionType::Block{r#type: BlockType::Function, body: sequence}.into(),
                Rule::if_block => ExpressionType::Block{r#type: BlockType::If, body: sequence}.into(),
                Rule::block_syntax => ExpressionType::Block{r#type: BlockType::Sequence, body: sequence}.into(),
                Rule::file => ExpressionType::Block{r#type: BlockType::Sequence, body: sequence}.into(),
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
                Rule::expr_prefix | Rule::expr_ref => {
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
                                let field = self.ident(inner[0].as_str());
                                Ok(ExpressionType::Field(ast, field).into())
                            }
                            _ => Err(anyhow!("parse error expr_apply_or_access"))
                        }
                    })?
                } 
                Rule::fun => {
                    let args = Self::find_tag("arg", &inner)
                        .map(|x|self.ident(x.as_str())).collect::<Vec<_>>();
                    // let with = Self::find_tag("with", &inner)
                    //     .map(|pair| -> Result<_> {
                    //         let mut inner = pair.to_owned().into_inner();
                    //         let alias = self.ident(inner.next().expect("need identifier alias").as_str());
                    //         let var = {
                    //             if let Some(val) = inner.next() {
                    //                 self.ident(val.as_str())
                    //             } else {
                    //                 alias.to_owned()
                    //             }
                    //         };
                    //         Ok((alias, var))
                    //     }).collect::<Result<Vec<_>>>()?;
                    // let persist = Self::find_tag("persist", &inner)
                    //     .map(|pair| -> Result<_> {
                    //         let mut inner = pair.to_owned().into_inner();
                    //         let alias = self.ident(inner.next().expect("need identifier alias").as_str());
                    //         let value = {
                    //             if let Some(val) = inner.next() {
                    //                 self.parse_rule(val)?
                    //             } else {
                    //                 ExpressionType::Nil.into()
                    //             }
                    //         };
                    //         Ok((alias, value))
                    //     }).collect::<Result<Vec<_>>>()?;
                    // let as_list = Self::find_tag("as", &inner)
                    //     .map(|x|self.ident(x.as_str())).collect::<Vec<_>>();
                    let body = Self::find_tag("body", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .expect("missing body")?;
                    // ExpressionType::Fun(args,with,as_list,persist,body).into()
                    ExpressionType::Fun(args,body).into()
                }
                Rule::defun => {
                    let name = Self::find_tag("name", &inner).next()
                        .map(|x|self.ident(x.as_str())).expect("missing name");
                    let args = Self::find_tag("arg", &inner)
                        .map(|x|self.ident(x.as_str())).collect::<Vec<_>>();
                    // let with = Self::find_tag("with", &inner)
                    //     .map(|pair| -> Result<_> {
                    //         let mut inner = pair.to_owned().into_inner();
                    //         let alias = self.ident(inner.next().expect("need identifier alias").as_str());
                    //         let var = {
                    //             if let Some(val) = inner.next() {
                    //                 self.ident(val.as_str())
                    //             } else {
                    //                 alias.to_owned()
                    //             }
                    //         };
                    //         Ok((alias, var))
                    //     }).collect::<Result<Vec<_>>>()?;
                    // let persist = Self::find_tag("persist", &inner)
                    //     .map(|pair| -> Result<_> {
                    //         let mut inner = pair.to_owned().into_inner();
                    //         let alias = self.ident(inner.next().expect("need identifier alias").as_str());
                    //         let value = {
                    //             if let Some(val) = inner.next() {
                    //                 self.parse_rule(val)?
                    //             } else {
                    //                 ExpressionType::Nil.into()
                    //             }
                    //         };
                    //         Ok((alias, value))
                    //     }).collect::<Result<Vec<_>>>()?;
                    // let in_list = Self::find_tag("in", &inner)
                    //     .map(|x|self.ident(x.as_str())).collect::<Vec<_>>();
                    let body = Self::find_tag("body", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .expect("missing body")?;
                    // let definition : Expression = ExpressionType::Let(name.to_owned(), 
                    //     // ExpressionType::Fun(args,with,vec![name.to_owned()],persist,body).into()
                    //     ExpressionType::Fun(args,body).into()
                    // ).into();
                    // let mutual_assigns = in_list.into_iter().map(|mutname| {
                    //     ExpressionType::AssignToExpression(
                    //         ExpressionType::Field(ExpressionType::Variable(mutname.to_owned()).into(), name.to_owned()).into(),
                    //         ExpressionType::Variable(name.to_owned()).into()
                    //     ).into()
                    // });
                    // let mut sequence = vec![definition.to_owned()];
                    // sequence.extend(mutual_assigns);
                    // if sequence.len() == 1 {
                    //     definition
                    // } else {
                    //     ExpressionType::Block{r#type: BlockType::Sequence, body: sequence}.into()
                    // }
                    ExpressionType::Block{r#type: BlockType::Sequence, body: vec![
                        ExpressionType::Let(name.to_owned(), ExpressionType::Nil.into()).into(), 
                        ExpressionType::AssignToExpression(
                            ExpressionType::Variable(name.to_owned()).into(),
                            ExpressionType::Fun(args,body).into()
                        ).into()    
                    ]}.into()
                }
                Rule::array => {
                    ExpressionType::Array(RefCell::new(
                        inner.iter().map(|e| self.parse_rule(e.to_owned())).collect::<Result<Vec<_>>>()?
                    )).into()
                } 
                Rule::context => {
                    assert!(inner.len() == 2);
                    assert!(inner[0].as_rule() == Rule::function_args);
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
                    let var = self.ident(inner[0].as_str());
                    assert!(inner[1].as_rule() == Rule::function_args);
                    assert!(inner[2].as_rule() == Rule::block_syntax);
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
                    assert!(inner[1].as_rule() == Rule::if_block);
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
                    let var = self.ident(inner[0].as_str());
                    if inner.len() == 1 {
                        let value = ExpressionType::Variable(var.to_owned()).into();
                        let body = ExpressionType::Variable(var.to_owned()).into();
                        ExpressionType::BindIn(var, value, body).into()
                    } else if inner.len() == 2 {
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
                Rule::assign => {
                    assert!(inner.len() >= 3);
                    let var_str = inner[0].as_str().to_owned();
                    let var = ExpressionType::Variable(self.ident(&var_str)).into();
                    if inner.len() == 3 {
                        let val = self.parse_rule(inner[2].clone())?;
                        match inner[1].as_rule() {
                            Rule::equal => ExpressionType::AssignToExpression(var, val).into(),
                            Rule::left_arrow => ExpressionType::AssignToDeRefExpression(var, val).into(),
                            _ => {
                                let op = 
                                    self.parse_rule(inner[1].clone().into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?)?;
                                if let ExpressionType::ParsedOperator(op) = op.as_ref() {
                                    ExpressionType::OpAssignToExpression(op.to_owned(), var, val).into()
                                } else {
                                    Err(anyhow!("parse error: operator expected"))?
                                }
                            }
                        }
                    } else {
                        let chain = inner[1..inner.len()-2].iter().try_fold(var, |acc, pair| {
                            match pair.as_rule() {
                                Rule::array_access => Ok(ExpressionType::ArrayAccess(acc, self.parse_rule(pair.clone())?).into()),
                                Rule::field_access => {
                                    let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                    assert!(inner.len() == 1);
                                    let field = self.ident(inner[0].as_str());
                                    Ok(ExpressionType::Field(acc, field).into())
                                }
                                _ => Err(anyhow!("bad assign chain")),
                            }
                        })?;
                        let val = self.parse_rule(inner[inner.len()-1].clone())?;
                        match inner[inner.len()-2].as_rule() {
                            Rule::equal => ExpressionType::AssignToExpression(chain, val).into(),
                            Rule::left_arrow => ExpressionType::AssignToDeRefExpression(chain, val).into(),
                            _ => {
                                let op = 
                                    self.parse_rule(inner[inner.len()-2].clone().into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?)?;
                                if let ExpressionType::ParsedOperator(op) = op.as_ref() {
                                    ExpressionType::OpAssignToExpression(op.to_owned(), chain, val).into()
                                } else {
                                    Err(anyhow!("parse error: operator expected"))?
                                }
                            }
                        }
                    }
                }
                Rule::r#while => {
                    assert!(inner.len() == 2);
                    let precond = self.parse_rule(inner[0].clone())?;
                    assert!(inner[1].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[1].clone())?;
                    let code = vec![
                        ExpressionType::If(precond,
                            ExpressionType::Nil.into(),
                            ExpressionType::Break(ExpressionType::Nil.into()).into()
                        ).into(),
                        body,
                    ];
                    let block_seq = ExpressionType::Block{ r#type: BlockType::Sequence, body: code }.into();
                    ExpressionType::Loop(block_seq).into()
                } 
                Rule::do_while => {
                    assert!(inner.len() == 2);
                    let postcond = self.parse_rule(inner[1].clone())?;
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    let code = vec![
                        body,
                        ExpressionType::If(postcond,
                            ExpressionType::Nil.into(),
                            ExpressionType::Break(ExpressionType::Nil.into()).into()
                        ).into(),
                    ];
                    let block_seq = ExpressionType::Block{ r#type: BlockType::Sequence, body: code }.into();
                    ExpressionType::Loop(block_seq).into()
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
                    let var = self.ident(inner[0].as_str());
                    let expr = self.parse_rule(inner[1].clone())?;
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    ExpressionType::Iterate(var, expr, body).into()
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
                Rule::endblock => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        ExpressionType::Nil.into()
                    };
                    ExpressionType::EndBlock(body).into() 
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
            Rule::string => { ExpressionType::String(unescape_string(parsed.as_str())?).into() },
            Rule::identifier => { 
                ExpressionType::ParsedIdentifier(self.ident(parsed.as_str())).into() 
            },
            Rule::character => { 
                assert!(!parsed.as_str().is_empty());
                ExpressionType::Character(unescape_string(parsed.as_str())?.chars().next().unwrap()).into() 
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
            Rule::block_syntax | Rule::block | Rule::file | Rule::if_block |
            Rule::function_block  => { self.parse_block(parsed)? },
            Rule::string_literal | Rule::char_literal  | Rule::array_access => {
                let inner = parsed.into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?;
                self.parse_rule(inner)?
            }
            Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | 
            Rule::expr_eq | Rule::expr_rel | Rule::expr_add | 
            Rule::expr_mul | Rule::expr_apply_or_access | Rule::expr_post | 
            Rule::expr_prefix | Rule::expr_exp | Rule::expr_ref |
            Rule::context | Rule::module | Rule::defun |
            Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | Rule::array |
            Rule::assign | Rule::object | Rule::bind | Rule::fun |
            Rule::r#let | Rule::r#loop | Rule::r#for | Rule::try_catch |
            Rule::endblock | Rule::r#break | Rule::throw | 
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
