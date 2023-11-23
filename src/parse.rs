use pest::iterators::Pair;
use anyhow::{anyhow, Result};
use crate::{expression::{Operator, BlockType, Syntax}, unescape_string, Interpreter, Rule};

impl Interpreter {

    fn find_tag<'a>(tag: &'a str, pairs: &'a [Pair<Rule>]) -> impl Iterator<Item=&'a Pair<'a,Rule>> + 'a {
        pairs.iter().filter(move |p| {
            let ptag = p.as_node_tag();
            ptag.is_some() && ptag.unwrap() == tag
        })
    }

    fn parse_block(&self, parsed: Pair<Rule>) -> Result<Syntax> {
        let rule = parsed.as_rule().to_owned();
        let sequence = parsed.into_inner()
            .filter_map(|e| {
                if e.as_rule() == Rule::EOI { None } 
                else { Some(self.parse_rule(e)) }
            })
            .collect::<Result<Vec<Syntax>>>()?;
        Ok(match sequence.len() {
            0 => Syntax::Nil,
            _ => match rule { 
                Rule::block => Syntax::Block{r#type: BlockType::Block, body: sequence},
                Rule::function_block => Syntax::Block{r#type: BlockType::Function, body: sequence},
                Rule::if_block => Syntax::Block{r#type: BlockType::If, body: sequence},
                Rule::block_syntax => Syntax::Block{r#type: BlockType::Sequence, body: sequence},
                Rule::file => Syntax::Block{r#type: BlockType::Sequence, body: sequence},
                _ => Err(anyhow!("parse error block type: {rule:?}"))?,
            },
        })
    }

    fn parse_vec(&self, rule: Rule, string: String, inner: Vec<Pair<Rule>>) -> Result<Syntax> {
        Ok(match rule {
                Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
                Rule::expr_rel | Rule::expr_add | Rule::expr_mul | Rule::expr_exp => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(inner.len() > 2);
                    assert!(inner.len() % 2 == 1);
                    let left = self.parse_rule(inner[0].clone())?;
                    inner[1..].chunks_exact(2).try_fold(left, |ast, pair| -> Result<Syntax> {
                        let op = self.parse_rule(pair[0].clone())?;
                        let right = self.parse_rule(pair[1].clone())?;
                        if let Syntax::Operator(op) = op {
                            Ok(Syntax::BinOpCall(op.to_owned(), ast.into(), right.into()))
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_post => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(!inner.is_empty());
                    let expr = self.parse_rule(inner[0].clone())?;
                    inner[1..].iter().try_fold(expr, |ast, pair| -> Result<Syntax> {
                        let op = self.parse_rule(pair.clone())?;
                        if let Syntax::Operator(op) = op {
                            Ok(Syntax::UnaryOpCall(op.to_owned(), ast.into()))
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
                    rinner[1..].iter().try_fold(expr, |ast, pair| -> Result<Syntax> {
                        let op = self.parse_rule(pair.clone())?;
                        if let Syntax::Operator(op) = op {
                            Ok(Syntax::UnaryOpCall(op.to_owned(), ast.into()))
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_apply_or_access => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(inner.len() > 1);
                    let target = self.parse_rule(inner[0].clone())?;
                    inner[1..].iter().try_fold(target, |ast, pair| -> Result<Syntax> {
                        match pair.as_rule() {
                            Rule::apply_args => {
                                let args = pair.clone().into_inner()
                                    .map(|e| (self.parse_rule(e.clone()))).collect::<Result<Vec<_>>>()?;
                                Ok(Syntax::FunctionCall(ast.into(), args))
                            }
                            Rule::array_access => Ok(Syntax::ArrayAccess(ast.into(), self.parse_rule(pair.clone())?.into())),
                            Rule::field_access => {
                                let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                assert!(inner.len() == 1);
                                let field = inner[0].as_str().to_owned();
                                Ok(Syntax::Field(ast.into(), field))
                            }
                            _ => Err(anyhow!("parse error expr_apply_or_access"))
                        }
                    })?
                } 
                Rule::fun => {
                    let fun_type = Self::find_tag("fun_type", &inner).next()
                        .map(|x|x.as_str().to_owned()).expect("missing fun type");
                    let args = Self::find_tag("arg", &inner)
                        .map(|x|x.as_str().to_owned()).collect::<Vec<_>>();
                    let mut body = Self::find_tag("body", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .expect("missing body")?;
                    if fun_type == "cons" || fun_type == "module" {
                        // @self injected at end of function body
                        let new_body_vec = match body {
                            Syntax::Block{r#type: BlockType::Function, body} => {
                                let mut new = body.clone();
                                new.push(Syntax::Builtin("self".to_owned()));
                                new
                            }
                            _ => Err(anyhow!("not a function body"))?
                        };
                        body = Syntax::Block{r#type: BlockType::Function, body: new_body_vec};
                    }
                    if fun_type == "module" {
                        if !args.is_empty() {
                            Err(anyhow!("no arguments allowed for modules"))?
                        }
                        // if it's a module, we call the function to instantiate
                        Syntax::FunctionCall(Syntax::Fun(vec![],body.into()).into(),vec![])
                    } else if fun_type == "dynmut" {
                        // a context-mutating dynamic function
                        Syntax::Dyn(true, args,body.into())
                    } else if fun_type == "dyn" {
                        // a normal dynamic function
                        Syntax::Dyn(false, args,body.into())
                    } else {
                        // a normal function (lambda)
                        Syntax::Fun(args,body.into())
                    }
                }
                Rule::closed => {
                    let vars = Self::find_tag("var", &inner)
                        .map(|x|x.as_str().to_owned()).collect::<Vec<_>>();
                    let body = Self::find_tag("body", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .expect("missing body")?;
                    Syntax::Closed(vars,body.into())
                }
                Rule::array => {
                    Syntax::ArrayLiteral(
                        inner.iter().map(|e| self.parse_rule(e.to_owned())).collect::<Result<Vec<_>>>()?
                    )
                } 
                Rule::r#use => {
                    assert!(!inner.is_empty());
                    let opt_source = Self::find_tag("source", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .map_or(Ok(None), |v| v.map(Some))?;
                    let vars = Self::find_tag("var", &inner)
                        .map(|x|x.as_str().to_owned()).collect::<Vec<_>>();
                    Syntax::Use(opt_source.map(Into::into), vars)
                } 
                Rule::r#if => {
                    assert!(inner.len() >= 2);
                    let mut result;
                    let mut i = inner.len()-1;
                    if inner.len() % 2 == 1 {
                        assert!(inner[i].as_rule() == Rule::if_block);
                        result = self.parse_rule(inner[i].clone())?;
                        i -= 1;
                    } else {
                        result = Syntax::Nil;
                    };
                    i -= 1;
                    loop {
                        let cond = self.parse_rule(inner[i].clone())?;
                        assert!(inner[i+1].as_rule() == Rule::if_block);
                        let then = self.parse_rule(inner[i+1].clone())?;
                        result = Syntax::If(cond.into(), then.into(), result.into());
                        if i < 2 { break; }
                        i -= 2;
                    }
                    result
                } 
                Rule::forget => {
                    assert!(!inner.is_empty());
                    let ids = inner.iter().map(|x|x.as_str().to_owned()).collect();
                    Syntax::Forget(ids)
                } 
                Rule::r#let => {
                    assert!(inner.len() == 1 || inner.len() == 2 || inner.len() == 3);
                    let opt_type =  Self::find_tag("let_type", &inner).next()
                        .map(|x|x.as_str());
                    let var_rule = Self::find_tag("var", &inner).next()
                        .expect("missing var");
                    let opt_val = Self::find_tag("val", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .map_or(Ok(None), |v| v.map(Some))?;
                    match var_rule.as_rule() {
                        Rule::identifier => {
                            let var = var_rule.as_str().to_owned();
                            let val = opt_val.unwrap_or(Syntax::Nil);
                            match opt_type {
                                Some("rec") => {
                                // let f; f = ... (recursive binding)
                                Syntax::Block{r#type: BlockType::Sequence, body: vec![
                                    Syntax::Let(var.to_owned(), Syntax::Nil.into()), 
                                    Syntax::AssignToExpression(
                                        Syntax::Variable(var.to_owned()).into(), val.into()
                                    )    
                                ]}
                                }
                                Some("ref") => {
                                    // bind (let by reference)
                                    Syntax::LetRef(var, val.into())
                                }
                                Some(_) => {
                                    Err(anyhow!("invalid let type"))?
                                }
                                None => {
                                    // normal let (non-recursive)
                                    Syntax::Let(var, val.into())
                                }
                            }
                        }
                        Rule::identifier_array => {
                            let vars = var_rule.clone().into_inner().map(|pair| {
                                pair.as_str().to_owned()
                            }).collect();
                            let val = opt_val.expect("need a value for array let");
                            Syntax::LetArray(vars, val.into())
                        }
                        _ => Err(anyhow!("bad let syntax {:?}", var_rule.as_rule()))?,
                    }
                } 
                Rule::assign => {
                    assert!(inner.len() >= 3);
                    let var_str = inner[0].as_str().to_owned();
                    let var = Syntax::Variable(var_str);
                    if inner.len() == 3 {
                        let val = self.parse_rule(inner[2].clone())?;
                        match inner[1].as_rule() {
                            Rule::equal => Syntax::AssignToExpression(var.into(), val.into()),
                            Rule::left_arrow => Syntax::AssignToDeRefExpression(var.into(), val.into()),
                            _ => {
                                let op = 
                                    self.parse_rule(inner[1].clone().into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?)?;
                                if let Syntax::Operator(op) = op {
                                    Syntax::OpAssignToExpression(op.to_owned(), var.into(), val.into())
                                } else {
                                    Err(anyhow!("parse error: operator expected"))?
                                }
                            }
                        }
                    } else {
                        let chain = inner[1..inner.len()-2].iter().try_fold(var, |acc, pair| {
                            match pair.as_rule() {
                                Rule::array_access => Ok(Syntax::ArrayAccess(acc.into(), self.parse_rule(pair.clone())?.into())),
                                Rule::field_access => {
                                    let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                    assert!(inner.len() == 1);
                                    let field = inner[0].as_str().to_owned();
                                    Ok(Syntax::Field(acc.into(), field))
                                }
                                _ => Err(anyhow!("bad assign chain")),
                            }
                        })?;
                        let val = self.parse_rule(inner[inner.len()-1].clone())?;
                        match inner[inner.len()-2].as_rule() {
                            Rule::equal => Syntax::AssignToExpression(chain.into(), val.into()),
                            Rule::left_arrow => Syntax::AssignToDeRefExpression(chain.into(), val.into()),
                            _ => {
                                let op = 
                                    self.parse_rule(inner[inner.len()-2].clone().into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?)?;
                                if let Syntax::Operator(op) = op {
                                    Syntax::OpAssignToExpression(op.to_owned(), chain.into(), val.into())
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
                        Syntax::If(precond.into(),
                            Syntax::Nil.into(),
                            Syntax::Break(Syntax::Nil.into()).into()
                        ),
                        body,
                    ];
                    let block_seq = Syntax::Block{ r#type: BlockType::Sequence, body: code };
                    Syntax::Loop(block_seq.into())
                } 
                Rule::do_while => {
                    assert!(inner.len() == 2);
                    let postcond = self.parse_rule(inner[1].clone())?;
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    let code = vec![
                        body,
                        Syntax::If(postcond.into(),
                            Syntax::Nil.into(),
                            Syntax::Break(Syntax::Nil.into()).into()
                        ),
                    ];
                    let block_seq = Syntax::Block{ r#type: BlockType::Sequence, body: code };
                    Syntax::Loop(block_seq.into())
                } 
                Rule::r#loop => {
                    assert!(inner.len() == 1);
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    Syntax::Loop(body.into())
                }
                Rule::r#for => {
                    assert!(inner.len() == 3);
                    assert!(inner[0].as_rule() == Rule::identifier);
                    let var = inner[0].as_str().to_owned();
                    let expr = self.parse_rule(inner[1].clone())?;
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    Syntax::Iterate(var, expr.into(), body.into())
                } 
                Rule::try_catch => {
                    assert!(inner.len() == 3);
                    let expr = self.parse_rule(inner[0].clone())?;
                    assert!(inner[1].as_rule() == Rule::identifier);
                    let var = inner[1].as_str().to_owned();
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    Syntax::TryCatch(expr.into(), var, body.into())
                } 
                Rule::r#return => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Syntax::Nil
                    };
                    Syntax::Return(body.into()) 
                },
                Rule::r#break => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Syntax::Nil
                    };
                    Syntax::Break(body.into()) 
                },
                Rule::exit => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Syntax::Nil
                    };
                    Syntax::Exit(body.into()) 
                },
                Rule::throw => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Syntax::Nil
                    };
                    Syntax::Throw(body.into()) 
                },
                Rule::test => { 
                    let body = self.parse_rule(inner[0].clone())?;
                    let source = inner[0].as_str().to_owned();
                    let expected = inner.get(1)
                        .map(|e|self.parse_rule(e.to_owned()))
                        .unwrap_or(Ok(Syntax::Boolean(true)))?;
                    Syntax::Test(source,body.into(),expected.into()) 
                },
                Rule::infix_identifier => { 
                    assert!(inner.len() == 1);
                    let id = inner[0].as_str().to_owned();
                    Syntax::Operator(Operator::Identifier(id)) 
                },
                _ => {
                    Err(anyhow!("TODO: [{:?}] {}",rule,string))?
                }
            })
    }

    pub fn parse_rule(&self, parsed: Pair<Rule>) -> Result<Syntax> {
        Ok(match parsed.as_rule() {
            Rule::integer => { Syntax::Integer(parsed.as_str().parse()?) },
            Rule::float => { Syntax::Float(parsed.as_str().parse()?) },
            Rule::string => { Syntax::String(unescape_string(parsed.as_str())?) },
            Rule::identifier => { 
                Syntax::Identifier(parsed.as_str().to_owned()) 
            },
            Rule::character => { 
                assert!(!parsed.as_str().is_empty());
                Syntax::Character(unescape_string(parsed.as_str())?.chars().next().unwrap()) 
            },
            Rule::variable => { 
                if parsed.as_str().starts_with('@') {
                    Syntax::Builtin(parsed.as_str().strip_prefix('@').unwrap().to_owned()) 
                } else {
                    Syntax::Variable(parsed.as_str().to_owned()) 
                }
            },
            Rule::nil_literal => { Syntax::Nil },
            Rule::nil_implicit => { Syntax::Nil },
            Rule::r#true => { Syntax::Boolean(true) },
            Rule::r#false => { Syntax::Boolean(false) },
            Rule::r#ref => { Syntax::Operator(Operator::Ref) },
            Rule::deref => { Syntax::Operator(Operator::DeRef) },
            Rule::question => { Syntax::Operator(Operator::Question) },
            Rule::exclam => { Syntax::Operator(Operator::Exclam) },
            Rule::pipe => { Syntax::Operator(Operator::Pipe) },
            Rule::neg => { Syntax::Operator(Operator::Neg) },
            Rule::add => { Syntax::Operator(Operator::Add) },
            Rule::mult => { Syntax::Operator(Operator::Mul) },
            Rule::sub => { Syntax::Operator(Operator::Sub) },
            Rule::div => { Syntax::Operator(Operator::Div) },
            Rule::intdiv => { Syntax::Operator(Operator::IntDiv) },
            Rule::r#mod => { Syntax::Operator(Operator::Mod) },
            Rule::exp => { Syntax::Operator(Operator::Exp) },
            Rule::or => { Syntax::Operator(Operator::Or) },
            Rule::and => { Syntax::Operator(Operator::And) },
            Rule::not => { Syntax::Operator(Operator::Not) },
            Rule::gt => { Syntax::Operator(Operator::Gt) },
            Rule::ge => { Syntax::Operator(Operator::Ge) },
            Rule::lt => { Syntax::Operator(Operator::Lt) },
            Rule::le => { Syntax::Operator(Operator::Le) },
            Rule::ne => { Syntax::Operator(Operator::Ne) },
            Rule::eq => { Syntax::Operator(Operator::Eq) },
            Rule::r#continue => { Syntax::Continue },
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
            Rule::r#use | Rule::forget |
            Rule::r#if | Rule::r#while | Rule::do_while | Rule::array |
            Rule::assign | Rule::fun | Rule::closed |
            Rule::r#let | Rule::r#loop | Rule::r#for | Rule::try_catch |
            Rule::exit | Rule::r#break | Rule::throw | Rule::test |
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