use pest::iterators::Pair;
use anyhow::{anyhow, Result};
use crate::{Ptr, PtrCell, expression::{Rule, Expr, self, Operator, BlockType}, unescape_string, Interpreter};

impl Interpreter {

    fn find_tag<'a>(tag: &'a str, pairs: &'a [Pair<Rule>]) -> impl Iterator<Item=&'a Pair<'a,Rule>> + 'a {
        pairs.iter().filter(move |p| {
            let ptag = p.as_node_tag();
            ptag.is_some() && ptag.unwrap() == tag
        })
    }

    fn parse_block(&self, parsed: Pair<Rule>) -> Result<Ptr<Expr>> {
        let rule = parsed.as_rule().to_owned();
        let sequence = parsed.into_inner()
            .filter_map(|e| {
                if e.as_rule() == Rule::EOI { None } 
                else { Some(self.parse_rule(e)) }
            })
            .collect::<Result<Vec<Ptr<Expr>>>>()?;
        Ok(match sequence.len() {
            0 => Expr::Nil.into(),
            _ => match rule { 
                Rule::block => Expr::Block{r#type: BlockType::Block, body: sequence}.into(),
                Rule::function_block => Expr::Block{r#type: BlockType::Function, body: sequence}.into(),
                Rule::if_block => Expr::Block{r#type: BlockType::If, body: sequence}.into(),
                Rule::block_syntax => Expr::Block{r#type: BlockType::Sequence, body: sequence}.into(),
                Rule::file => Expr::Block{r#type: BlockType::Sequence, body: sequence}.into(),
                _ => Err(anyhow!("parse error block type: {rule:?}"))?,
            },
        })
    }

    fn parse_vec(&self, rule: Rule, string: String, inner: Vec<Pair<Rule>>) -> Result<Ptr<Expr>> {
        Ok(match rule {
                Rule::expr_infix_id | Rule::expr_infix_pipe | Rule::expr_or | Rule::expr_and | Rule::expr_eq | 
                Rule::expr_rel | Rule::expr_add | Rule::expr_mul | Rule::expr_exp => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(inner.len() > 2);
                    assert!(inner.len() % 2 == 1);
                    let left = self.parse_rule(inner[0].clone())?;
                    inner[1..].chunks_exact(2).try_fold(left, |ast, pair| -> Result<Ptr<Expr>> {
                        let op = self.parse_rule(pair[0].clone())?;
                        let right = self.parse_rule(pair[1].clone())?;
                        if let Expr::ParsedOperator(op) = op.as_ref() {
                            Ok(Expr::BinOpCall(op.to_owned(), ast, right).into())
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_post => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(!inner.is_empty());
                    let expr = self.parse_rule(inner[0].clone())?;
                    inner[1..].iter().try_fold(expr, |ast, pair| -> Result<Ptr<Expr>> {
                        let op = self.parse_rule(pair.clone())?;
                        if let Expr::ParsedOperator(op) = op.as_ref() {
                            Ok(Expr::UnaryOpCall(op.to_owned(), ast).into())
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
                    rinner[1..].iter().try_fold(expr, |ast, pair| -> Result<Ptr<Expr>> {
                        let op = self.parse_rule(pair.clone())?;
                        if let Expr::ParsedOperator(op) = op.as_ref() {
                            Ok(Expr::UnaryOpCall(op.to_owned(), ast).into())
                        } else {
                            Err(anyhow!("parse error: operator expected"))
                        }
                    })?
                }
                Rule::expr_apply_or_access => {
                    if inner.len() == 1 { return self.parse_rule(inner[0].clone()) }
                    assert!(inner.len() > 1);
                    let target = self.parse_rule(inner[0].clone())?;
                    inner[1..].iter().try_fold(target, |ast, pair| -> Result<Ptr<Expr>> {
                        match pair.as_rule() {
                            Rule::apply_args => {
                                let args = pair.clone().into_inner()
                                    .map(|e| (self.parse_rule(e.clone()))).collect::<Result<Vec<_>>>()?;
                                Ok(Expr::FunctionCall(ast, args).into())
                            }
                            Rule::array_access => Ok(Expr::ArrayAccess(ast, self.parse_rule(pair.clone())?).into()),
                            Rule::field_access => {
                                let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                assert!(inner.len() == 1);
                                let field = inner[0].as_str().to_owned();
                                Ok(Expr::Field(ast, field).into())
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
                        let new_body_vec = match body.as_ref() {
                            Expr::Block{r#type: BlockType::Function, body} => {
                                let mut new = body.clone();
                                new.push(Expr::BuiltinVariable("self".to_owned()).into());
                                new
                            }
                            _ => Err(anyhow!("not a function body"))?
                        };
                        body = Expr::Block{r#type: BlockType::Function, body: new_body_vec}.into();
                    }
                    if fun_type == "module" {
                        if !args.is_empty() {
                            Err(anyhow!("no arguments allowed for modules"))?
                        }
                        // if it's a module, we call the function to instantiate
                        Expr::FunctionCall(Expr::Fun(vec![],body).into(),vec![]).into()
                    } else if fun_type == "dyn" {
                        // a dynamic function
                        Expr::Dyn(args,body).into()
                    } else {
                        // a normal function (lambda)
                        Expr::Fun(args,body).into()
                    }
                }
                Rule::closed => {
                    let vars = Self::find_tag("var", &inner)
                        .map(|x|x.as_str().to_owned()).collect::<Vec<_>>();
                    let body = Self::find_tag("body", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .expect("missing body")?;
                    Expr::Closed(vars,body).into()
                }
                Rule::array => {
                    Expr::Array(PtrCell::new(
                        inner.iter().map(|e| self.parse_rule(e.to_owned())).collect::<Result<Vec<_>>>()?
                    )).into()
                } 
                Rule::r#use => {
                    assert!(!inner.is_empty());
                    let opt_source = Self::find_tag("source", &inner).next()
                        .map(|x| self.parse_rule(x.to_owned()))
                        .map_or(Ok(None), |v| v.map(Some))?;
                    let vars = Self::find_tag("var", &inner)
                        .map(|x|x.as_str().to_owned()).collect::<Vec<_>>();
                    Expr::Use(opt_source, vars).into()
                } 
                Rule::r#if | Rule::unless => {
                    assert!(inner.len() == 2 || inner.len() == 3);
                    let expr = self.parse_rule(inner[0].clone())?;
                    let cond = if rule == Rule::unless {
                        Expr::UnaryOpCall(Operator::Not,expr).into()
                    } else {
                        expr
                    };
                    assert!(inner[1].as_rule() == Rule::if_block);
                    let then = self.parse_rule(inner[1].clone())?;
                    let r#else = if inner.len() < 3 {
                        Expr::Nil.into()
                    } else {
                        self.parse_rule(inner[2].clone())?
                    };
                    Expr::If(cond, then, r#else).into()
                } 
                Rule::forget => {
                    assert!(inner.len() == 1);
                    let id = inner[0].as_str().to_owned();
                    Expr::Forget(id).into()
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
                            let val = opt_val.unwrap_or(expression::nil());
                            match opt_type {
                                Some("rec") => {
                                // let f; f = ... (recursive binding)
                                Expr::Block{r#type: BlockType::Sequence, body: vec![
                                    Expr::Let(var.to_owned(), Expr::Nil.into()).into(), 
                                    Expr::AssignToExpression(
                                        Expr::Variable(var.to_owned()).into(), val
                                    ).into()    
                                ]}.into()
                                }
                                Some("ref") => {
                                    // bind (let by reference)
                                    Expr::LetRef(var, val).into()
                                }
                                Some(_) => {
                                    Err(anyhow!("invalid let type"))?
                                }
                                None => {
                                    // normal let (non-recursive)
                                    Expr::Let(var, val).into()
                                }
                            }
                        }
                        Rule::identifier_array => {
                            let vars = var_rule.clone().into_inner().map(|pair| {
                                pair.as_str().to_owned()
                            }).collect();
                            let val = opt_val.expect("need a value for array let");
                            Expr::LetArray(vars, val).into()
                        }
                        _ => Err(anyhow!("bad let syntax {:?}", var_rule.as_rule()))?,
                    }
                } 
                Rule::assign => {
                    assert!(inner.len() >= 3);
                    let var_str = inner[0].as_str().to_owned();
                    let var = Expr::Variable(var_str).into();
                    if inner.len() == 3 {
                        let val = self.parse_rule(inner[2].clone())?;
                        match inner[1].as_rule() {
                            Rule::equal => Expr::AssignToExpression(var, val).into(),
                            Rule::left_arrow => Expr::AssignToDeRefExpression(var, val).into(),
                            _ => {
                                let op = 
                                    self.parse_rule(inner[1].clone().into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?)?;
                                if let Expr::ParsedOperator(op) = op.as_ref() {
                                    Expr::OpAssignToExpression(op.to_owned(), var, val).into()
                                } else {
                                    Err(anyhow!("parse error: operator expected"))?
                                }
                            }
                        }
                    } else {
                        let chain = inner[1..inner.len()-2].iter().try_fold(var, |acc, pair| {
                            match pair.as_rule() {
                                Rule::array_access => Ok(Expr::ArrayAccess(acc, self.parse_rule(pair.clone())?).into()),
                                Rule::field_access => {
                                    let inner : Vec<Pair<Rule>> = pair.clone().into_inner().collect();
                                    assert!(inner.len() == 1);
                                    let field = inner[0].as_str().to_owned();
                                    Ok(Expr::Field(acc, field).into())
                                }
                                _ => Err(anyhow!("bad assign chain")),
                            }
                        })?;
                        let val = self.parse_rule(inner[inner.len()-1].clone())?;
                        match inner[inner.len()-2].as_rule() {
                            Rule::equal => Expr::AssignToExpression(chain, val).into(),
                            Rule::left_arrow => Expr::AssignToDeRefExpression(chain, val).into(),
                            _ => {
                                let op = 
                                    self.parse_rule(inner[inner.len()-2].clone().into_inner().next().ok_or(anyhow!("problem parsing silent rule"))?)?;
                                if let Expr::ParsedOperator(op) = op.as_ref() {
                                    Expr::OpAssignToExpression(op.to_owned(), chain, val).into()
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
                        Expr::If(precond,
                            Expr::Nil.into(),
                            Expr::Break(Expr::Nil.into()).into()
                        ).into(),
                        body,
                    ];
                    let block_seq = Expr::Block{ r#type: BlockType::Sequence, body: code }.into();
                    Expr::Loop(block_seq).into()
                } 
                Rule::do_while => {
                    assert!(inner.len() == 2);
                    let postcond = self.parse_rule(inner[1].clone())?;
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    let code = vec![
                        body,
                        Expr::If(postcond,
                            Expr::Nil.into(),
                            Expr::Break(Expr::Nil.into()).into()
                        ).into(),
                    ];
                    let block_seq = Expr::Block{ r#type: BlockType::Sequence, body: code }.into();
                    Expr::Loop(block_seq).into()
                } 
                Rule::r#loop => {
                    assert!(inner.len() == 1);
                    assert!(inner[0].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[0].clone())?;
                    Expr::Loop(body).into()
                }
                Rule::r#for => {
                    assert!(inner.len() == 3);
                    assert!(inner[0].as_rule() == Rule::identifier);
                    let var = inner[0].as_str().to_owned();
                    let expr = self.parse_rule(inner[1].clone())?;
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    Expr::Iterate(var, expr, body).into()
                } 
                Rule::try_catch => {
                    assert!(inner.len() == 3);
                    let expr = self.parse_rule(inner[0].clone())?;
                    assert!(inner[1].as_rule() == Rule::identifier);
                    let var = inner[1].as_str().to_owned();
                    assert!(inner[2].as_rule() == Rule::block_syntax);
                    let body = self.parse_rule(inner[2].clone())?;
                    Expr::TryCatch(expr, var, body).into()
                } 
                Rule::r#return => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Expr::Nil.into()
                    };
                    Expr::Return(body).into() 
                },
                Rule::r#break => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Expr::Nil.into()
                    };
                    Expr::Break(body).into() 
                },
                Rule::exit => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Expr::Nil.into()
                    };
                    Expr::Exit(body).into() 
                },
                Rule::throw => { 
                    let body = if inner.len() == 1 {
                        self.parse_rule(inner[0].clone())?
                    } else {
                        Expr::Nil.into()
                    };
                    Expr::Throw(body).into() 
                },
                Rule::test => { 
                    let body = self.parse_rule(inner[0].clone())?;
                    let source = inner[0].as_str().to_owned();
                    let expected = inner.get(1)
                        .map(|e|self.parse_rule(e.to_owned()))
                        .unwrap_or(Ok(Expr::Boolean(true).into()))?;
                    Expr::Test(source,body,expected).into() 
                },
                Rule::infix_identifier => { 
                    assert!(inner.len() == 1);
                    let id = inner[0].as_str().to_owned();
                    Expr::ParsedOperator(Operator::Identifier(id)).into() 
                },
                _ => {
                    Err(anyhow!("TODO: [{:?}] {}",rule,string))?
                }
            })
    }

    pub fn parse_rule(&self, parsed: Pair<Rule>) -> Result<Ptr<Expr>> {
        Ok(match parsed.as_rule() {
            Rule::integer => { Expr::Integer(parsed.as_str().parse()?).into() },
            Rule::float => { Expr::Float(parsed.as_str().parse()?).into() },
            Rule::string => { Expr::String(unescape_string(parsed.as_str())?).into() },
            Rule::identifier => { 
                Expr::ParsedIdentifier(parsed.as_str().to_owned()).into() 
            },
            Rule::character => { 
                assert!(!parsed.as_str().is_empty());
                Expr::Character(unescape_string(parsed.as_str())?.chars().next().unwrap()).into() 
            },
            Rule::variable => { 
                if parsed.as_str().starts_with('@') {
                    Expr::BuiltinVariable(parsed.as_str().strip_prefix('@').unwrap().to_owned()).into() 
                } else {
                    Expr::Variable(parsed.as_str().to_owned()).into() 
                }
            },
            Rule::nil_literal => { Expr::Nil.into() },
            Rule::nil_implicit => { Expr::Nil.into() },
            Rule::r#true => { Expr::Boolean(true).into() },
            Rule::r#false => { Expr::Boolean(false).into() },
            Rule::r#ref => { Expr::ParsedOperator(Operator::Ref).into() },
            Rule::deref => { Expr::ParsedOperator(Operator::DeRef).into() },
            Rule::question => { Expr::ParsedOperator(Operator::Question).into() },
            Rule::exclam => { Expr::ParsedOperator(Operator::Exclam).into() },
            Rule::pipe => { Expr::ParsedOperator(Operator::Pipe).into() },
            Rule::neg => { Expr::ParsedOperator(Operator::Neg).into() },
            Rule::add => { Expr::ParsedOperator(Operator::Add).into() },
            Rule::mult => { Expr::ParsedOperator(Operator::Mul).into() },
            Rule::sub => { Expr::ParsedOperator(Operator::Sub).into() },
            Rule::div => { Expr::ParsedOperator(Operator::Div).into() },
            Rule::intdiv => { Expr::ParsedOperator(Operator::IntDiv).into() },
            Rule::r#mod => { Expr::ParsedOperator(Operator::Mod).into() },
            Rule::exp => { Expr::ParsedOperator(Operator::Exp).into() },
            Rule::or => { Expr::ParsedOperator(Operator::Or).into() },
            Rule::and => { Expr::ParsedOperator(Operator::And).into() },
            Rule::not => { Expr::ParsedOperator(Operator::Not).into() },
            Rule::gt => { Expr::ParsedOperator(Operator::Gt).into() },
            Rule::ge => { Expr::ParsedOperator(Operator::Ge).into() },
            Rule::lt => { Expr::ParsedOperator(Operator::Lt).into() },
            Rule::le => { Expr::ParsedOperator(Operator::Le).into() },
            Rule::ne => { Expr::ParsedOperator(Operator::Ne).into() },
            Rule::eq => { Expr::ParsedOperator(Operator::Eq).into() },
            Rule::r#continue => { Expr::Continue.into() },
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
            Rule::r#if | Rule::r#while | Rule::unless | Rule::do_while | Rule::array |
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
