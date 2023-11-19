use std::{ops::Deref, collections::{HashSet, HashMap}};
use anyhow::{anyhow, Result};
use pest_derive::Parser;
use crate::{context::{Context, MemCell, ScopeID}, RefC, R};

#[cfg(feature = "gc")]
use gc::{Finalize, Trace};

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "gc", derive(Trace, Finalize))]
pub enum Operator {
    Add, Mul, Sub, Div, IntDiv, Exp, Mod,
    Not, And, Or, Pipe,
    Gt, Ge, Lt, Le, Ne, Eq,
    Neg, Ref, DeRef, Question, Exclam,
    Identifier(String)
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "gc", derive(Trace, Finalize))]
pub enum BlockType {
    Sequence,
    Block,
    If,
    Function
}

#[derive(Debug,Clone)]
#[cfg_attr(feature = "gc", derive(Trace, Finalize))]
pub enum Expr {
    Integer(i64),
    Float(f64),
    Character(char),
    Boolean(bool),
    String(String),
    Nil,
    Error(String),
    Variable(String),
    BuiltinVariable(String),
    BuiltinFunction(String),
    FunctionCall(R<Expr>,Vec<R<Expr>>),
    Field(R<Expr>,String),
    BinOpCall(Operator,R<Expr>,R<Expr>),
    UnaryOpCall(Operator,R<Expr>),
    If(R<Expr>,R<Expr>,R<Expr>),
    Iterate(String,R<Expr>,R<Expr>),
    TryCatch(R<Expr>,String,R<Expr>),
    Let(String,R<Expr>),
    LetArray(Vec<String>,R<Expr>),
    LetRef(String,R<Expr>),
    Forget(String),
    AssignToExpression(R<Expr>,R<Expr>),
    AssignToDeRefExpression(R<Expr>,R<Expr>),
    OpAssignToExpression(Operator,R<Expr>,R<Expr>),
    ArrayAccess(R<Expr>,R<Expr>),
    Block{r#type: BlockType, body: Vec<R<Expr>>},
    Loop(R<Expr>),
    Fun(Vec<String>,R<Expr>),
    Dyn(Vec<String>,R<Expr>),
    Closed(Vec<String>,R<Expr>),
    Use(Option<R<Expr>>,Vec<String>),
    Closure(Context,Vec<String>,R<Expr>),
    Array(RefC<Vec<R<Expr>>>),
    Return(R<Expr>),
    Break(R<Expr>),
    Exit(R<Expr>),
    Throw(R<Expr>),
    Test(String,R<Expr>,R<Expr>),
    Continue, 
    Ref(R<MemCell>),
    Scope(ScopeID,HashMap<String,R<Expr>>),
    ScopeCycle(ScopeID),
    ClosurePrintable(R<Expr>,Vec<String>,R<Expr>),
    ArrayPrintable(Vec<R<Expr>>),
    ExceptionPrintable(R<Expr>),
    ParsedOperator(Operator),
    ParsedIdentifier(String),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (Expr::Integer(a),Expr::Integer(b)) => a == b,
            (Expr::Float(a),Expr::Float(b)) => a == b,
            (Expr::Float(a), Expr::Integer(b)) => (*a) == (*b as f64),
            (Expr::Integer(a), Expr::Float(b)) => (*a as f64) == (*b),
            (Expr::Character(a),Expr::Character(b)) => a == b,
            (Expr::String(a),Expr::String(b)) => a == b,
            (Expr::Error(a),Expr::Error(b)) => a == b,
            (Expr::Boolean(a),Expr::Boolean(b)) => a == b,
            (Expr::Nil,Expr::Nil) => true,
            (Expr::Array(a),Expr::Array(b)) => a == b,
            (Expr::ParsedOperator(a),Expr::ParsedOperator(b)) => a == b,
            _ => false,
        }
    }
}

impl Expr {
    pub fn to_error(&self) -> Result<R<Expr>> {
        Err(anyhow!("{:#?}",self))
    }
}

fn decycle(e: R<Expr>, env: &mut HashSet<ScopeID>) -> R<Expr> {
    match e.as_ref() {
        Expr::Break(rc) =>
            Expr::ExceptionPrintable(decycle(rc.to_owned(),env)).into(),
        Expr::Return(rc) => 
            Expr::ExceptionPrintable(decycle(rc.to_owned(),env)).into(),
        Expr::Throw(rc) =>
            Expr::ExceptionPrintable(decycle(rc.to_owned(),env)).into(),
        Expr::Array(arr) => {
            let mut safe = vec![];
            for rc in arr.borrow().iter() {
                match rc.as_ref() {
                    Expr::Closure(_,_ ,_) => {
                        safe.push(decycle(rc.to_owned(),env));
                    }
                    Expr::Array(_) => {
                        safe.push(decycle(rc.to_owned(),env));
                    }
                    _ => safe.push(rc.to_owned()),
                }
            }
            Expr::ArrayPrintable(safe).into()
        }
        Expr::Closure(ctx, args, body) => {
            let scope_id = ctx.scope_id();
            if env.contains(&scope_id) {
                Expr::ClosurePrintable(
                    Expr::ScopeCycle(scope_id).into(), 
                    args.to_owned(), body.to_owned()
                ).into()
            } else {
                env.insert(scope_id);
                let mut safe = HashMap::new();
                for (k, v) in ctx.bindings_ref() {
                    let value = v.get();
                    match value.as_ref() {
                        Expr::Closure(_, _, _) => {
                            safe.insert(k, decycle(value, env));
                        }
                        Expr::Array(_) => {
                            safe.insert(k, decycle(value, env));
                        }
                        _ => {
                            safe.insert(k, value);
                        }
                    }
                }
                Expr::ClosurePrintable(
                    Expr::Scope(scope_id, safe).into(),
                    args.to_owned(),body.to_owned()
                ).into()
            }
        }
        _ => e.to_owned(),
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Nil => write!(f,"nil"),
            Expr::Boolean(a) => write!(f,"{a}"),
            Expr::Float(a) => write!(f,"{a}"),
            Expr::Integer(a) => write!(f,"{a}"),
            Expr::Character(a) => write!(f,"{a}"),
            Expr::String(a) => write!(f,"{a}"),
            Expr::Error(a) => write!(f,"Error<{a}>"),
            Expr::ArrayPrintable(v) =>
                write!(f,"[{}]",v.iter().map(|x|x.to_string()).collect::<Vec<_>>().join(", ")),
            Expr::Ref(mc) => write!(f,"->{}",mc.get()),
            Expr::ExceptionPrintable(a) => write!(f,"Exception<{a}>"),
            Expr::Throw(_) | 
            Expr::Return(_) | 
            Expr::Break(_) | 
            Expr::Array(_) | 
            Expr::Closure(_, _, _) => 
                write!(f,"{}",decycle(self.to_owned().into(),&mut HashSet::new()).deref()),
            _ => write!(f,"{:#?}",self),
        }
    }
}

pub fn integer(i: i64) -> R<Expr> {
    Expr::Integer(i).into()
}

pub fn float(f: f64) -> R<Expr> {
    Expr::Float(f).into()
}

pub fn boolean(b: bool) -> R<Expr> {
    Expr::Boolean(b).into()
}

pub fn character(c: char) -> R<Expr> {
    Expr::Character(c).into()
}

pub fn string(s: String) -> R<Expr> {
    Expr::String(s).into()
}

pub fn nil() -> R<Expr> {
    Expr::Nil.into()
}

pub fn closure(ctx: Context, args: Vec<String>, body: R<Expr>) -> R<Expr> {
    Expr::Closure(ctx,args,body).into()
}

pub fn context(ctx: Context) -> R<Expr> {
    closure(ctx,vec![],nil())
}

pub fn array(vals: Vec<R<Expr>>) -> R<Expr> {
    Expr::Array(RefC::new(vals)).into()
}

#[derive(Debug,Clone)]
pub enum Value {
    Nil,
    Integer(i64),
    Float(f64),
    Character(char),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    Dict(HashMap<String,Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f,"nil"),
            Value::Boolean(b) => write!(f,"{b}"),
            Value::Float(x) => write!(f,"{x}"),
            Value::Integer(i) => write!(f,"{i}"),
            Value::Character(c) => write!(f,"{c:?}"),
            Value::String(s) => write!(f,"{s:?}"),
            Value::Array(v) => 
                write!(f,"[{}]",v.iter().map(Self::to_string).collect::<Vec<_>>().join(", ")),
            Value::Dict(d) => 
                write!(f,"{{{}}}",d.iter().map(|(k,v)| { format!("{k}: {v}") }).collect::<Vec<_>>().join(", ")),
        }
    }
}

impl TryFrom<R<Expr>> for Value {
    type Error = anyhow::Error;
    fn try_from(expr: R<Expr>) -> Result<Self> {
        Ok(match expr.as_ref() {
            Expr::Nil => Value::Nil,
            Expr::Integer(i) => Value::Integer(*i),
            Expr::Float(f) => Value::Float(*f),
            Expr::Character(c) => Value::Character(*c),
            Expr::Boolean(b) => Value::Boolean(*b),
            Expr::String(s) => Value::String(s.to_owned()),
            Expr::Array(rc) => {
                let v = rc.borrow().iter()
                    .map(|e| { Self::try_from(e.to_owned()) })
                    .collect::<Result<Vec<_>>>()?;
                Value::Array(v)
            }
            Expr::Closure(ctx,_,_) => {
                let d = ctx.bindings_cloned().iter()
                    .map(|(id, mc)| -> Result<(String,Value)> { 
                        Ok((id.to_owned(), Self::try_from(mc.get())?)) 
                    })
                    .collect::<Result<HashMap<String,Value>>>()?;
                Value::Dict(d)
            }
            _ => Err(anyhow!("can't convert this expression to a value"))?
        })
    }
}

