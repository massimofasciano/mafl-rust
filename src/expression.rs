use std::{ops::Deref, collections::{HashSet, HashMap}};
use anyhow::{anyhow, Result};
use pest_derive::Parser;
use crate::{context::{Context, MemCell, ScopeID}, PtrCell, Ptr, Interpreter};

#[cfg(feature = "gc")]
use gc::{Finalize, Trace};

#[derive(Parser)]
#[grammar = "mafl.pest"]
pub struct MaflParser;

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

pub type Builtin = fn (&Interpreter, &Context, &[Ptr<Expr>]) -> Result<Ptr<Expr>>;

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
    FunctionCall(Ptr<Expr>,Vec<Ptr<Expr>>),
    Field(Ptr<Expr>,String),
    BinOpCall(Operator,Ptr<Expr>,Ptr<Expr>),
    UnaryOpCall(Operator,Ptr<Expr>),
    If(Ptr<Expr>,Ptr<Expr>,Ptr<Expr>),
    Iterate(String,Ptr<Expr>,Ptr<Expr>),
    TryCatch(Ptr<Expr>,String,Ptr<Expr>),
    Let(String,Ptr<Expr>),
    LetArray(Vec<String>,Ptr<Expr>),
    LetRef(String,Ptr<Expr>),
    Forget(Vec<String>),
    AssignToExpression(Ptr<Expr>,Ptr<Expr>),
    AssignToDeRefExpression(Ptr<Expr>,Ptr<Expr>),
    OpAssignToExpression(Operator,Ptr<Expr>,Ptr<Expr>),
    ArrayAccess(Ptr<Expr>,Ptr<Expr>),
    Block{r#type: BlockType, body: Vec<Ptr<Expr>>},
    Loop(Ptr<Expr>),
    Fun(Vec<String>,Ptr<Expr>),
    Dyn(bool,Vec<String>,Ptr<Expr>),
    Closed(Vec<String>,Ptr<Expr>),
    Use(Option<Ptr<Expr>>,Vec<String>),
    Closure(Context,Vec<String>,Ptr<Expr>),
    Array(PtrCell<Vec<Ptr<Expr>>>),
    Return(Ptr<Expr>),
    Break(Ptr<Expr>),
    Exit(Ptr<Expr>),
    Throw(Ptr<Expr>),
    Test(String,Ptr<Expr>,Ptr<Expr>),
    Continue, 
    Ref(Ptr<MemCell>),
    Scope(ScopeID,HashMap<String,Ptr<Expr>>),
    ScopeCycle(ScopeID),
    ClosureSafe(Ptr<Expr>,Vec<String>,Ptr<Expr>),
    ArraySafe(Vec<Ptr<Expr>>),
    ExceptionSafe(Ptr<Expr>),
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
    pub fn to_error(&self) -> Result<Ptr<Expr>> {
        Err(anyhow!("{:#?}",self))
    }
}

fn decycle(e: Ptr<Expr>, env: &mut HashSet<ScopeID>) -> Ptr<Expr> {
    match e.as_ref() {
        Expr::Exit(rc) =>
            Expr::ExceptionSafe(decycle(rc.to_owned(),env)).into(),
        Expr::Break(rc) =>
            Expr::ExceptionSafe(decycle(rc.to_owned(),env)).into(),
        Expr::Return(rc) => 
            Expr::ExceptionSafe(decycle(rc.to_owned(),env)).into(),
        Expr::Throw(rc) =>
            Expr::ExceptionSafe(decycle(rc.to_owned(),env)).into(),
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
            Expr::ArraySafe(safe).into()
        }
        Expr::Closure(ctx, args, body) => {
            let scope_id = ctx.scope_id();
            if env.contains(&scope_id) {
                Expr::ClosureSafe(
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
                Expr::ClosureSafe(
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
            Expr::ArraySafe(v) =>
                write!(f,"[{}]",v.iter().map(|x|x.to_string()).collect::<Vec<_>>().join(", ")),
            Expr::Ref(mc) => write!(f,"->{}",mc.get()),
            Expr::ExceptionSafe(a) => write!(f,"Exception<{a}>"),
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

pub fn integer(i: i64) -> Ptr<Expr> {
    Expr::Integer(i).into()
}

pub fn float(f: f64) -> Ptr<Expr> {
    Expr::Float(f).into()
}

pub fn boolean(b: bool) -> Ptr<Expr> {
    Expr::Boolean(b).into()
}

pub fn character(c: char) -> Ptr<Expr> {
    Expr::Character(c).into()
}

pub fn string(s: String) -> Ptr<Expr> {
    Expr::String(s).into()
}

pub fn nil() -> Ptr<Expr> {
    Expr::Nil.into()
}

pub fn closure(ctx: Context, args: Vec<String>, body: Ptr<Expr>) -> Ptr<Expr> {
    Expr::Closure(ctx,args,body).into()
}

pub fn context(ctx: Context) -> Ptr<Expr> {
    closure(ctx,vec![],nil())
}

pub fn array(vals: Vec<Ptr<Expr>>) -> Ptr<Expr> {
    Expr::Array(PtrCell::new(vals)).into()
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

impl TryFrom<Ptr<Expr>> for Value {
    type Error = anyhow::Error;
    fn try_from(expr: Ptr<Expr>) -> Result<Self> {
        let safe = decycle(expr,&mut HashSet::new());
        Ok(match safe.as_ref() {
            Expr::Nil => Value::Nil,
            Expr::Integer(i) => Value::Integer(*i),
            Expr::Float(f) => Value::Float(*f),
            Expr::Character(c) => Value::Character(*c),
            Expr::Boolean(b) => Value::Boolean(*b),
            Expr::String(s) => Value::String(s.to_owned()),
            Expr::ExceptionSafe(e) => Self::try_from(e.to_owned())?,
            Expr::ArraySafe(vec) => {
                let v = vec.iter()
                    .map(|e| { Self::try_from(e.to_owned()) })
                    .collect::<Result<Vec<_>>>()?;
                Value::Array(v)
            }
            Expr::ClosureSafe(sc,_,_) => {
                if let Expr::Scope(_, de) = sc.as_ref() {
                    let d = de.iter()
                        .map(|(id, e)| -> Result<(String,Value)> { 
                            Ok((id.to_owned(), Self::try_from(e.to_owned())?)) 
                        })
                        .collect::<Result<HashMap<String,Value>>>()?;
                    Value::Dict(d)
                } else {
                    Value::Dict(HashMap::new())
                }
            }
            _ => Err(anyhow!("can't convert this expression to a value"))?
        })
    }
}

impl From<Value> for Expr {
    fn from(value: Value) -> Self {
        match value {
            Value::Nil => Expr::Nil,
            Value::Integer(i) => Expr::Integer(i),
            Value::Float(f) => Expr::Float(f),
            Value::Character(c) => Expr::Character(c),
            Value::Boolean(b) => Expr::Boolean(b),
            Value::String(s) => Expr::String(s),
            Value::Array(v) => 
                Expr::Array(PtrCell::new(v.into_iter().map(|x|Expr::from(x).into()).collect())),
            Value::Dict(dict) => {
                Expr::Closure(Context::from(dict),vec![],Expr::Nil.into())
            }
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Integer(i) => Ok(i),
            _ => Err(anyhow!("can't convert this value to an i64"))
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Integer(i) => Ok(i as f64),
            Value::Float(f) => Ok(f),
            _ => Err(anyhow!("can't convert this value to an f64"))
        }
    }
}

impl TryFrom<Value> for String {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(anyhow!("can't convert this value to a String"))
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Boolean(b) => Ok(b),
            _ => Err(anyhow!("can't convert this value to a bool"))
        }
    }
}

impl TryFrom<Value> for char {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Character(c) => Ok(c),
            _ => Err(anyhow!("can't convert this value to a char"))
        }
    }
}

impl TryFrom<Value> for Vec<Value> {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Array(v) => Ok(v),
            _ => Err(anyhow!("can't convert this value to a Vec<Value>"))
        }
    }
}

impl TryFrom<Value> for HashMap<String,Value> {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Dict(d) => Ok(d),
            _ => Err(anyhow!("can't convert this value to a HashMap<String,Value>"))
        }
    }
}


