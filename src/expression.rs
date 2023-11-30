use std::{ops::Deref, collections::{HashSet, HashMap}};
use anyhow::{anyhow, Result};
use crate::{context::{Context, MemCell, ScopeID}, PtrCell, Ptr, Interpreter};

#[cfg(feature = "gc")]
use gc::{Finalize, Trace};

#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "operator", content = "value"))]
#[cfg_attr(feature = "gc", derive(Finalize))]
pub enum Operator {
    Add, Mul, Sub, Div, IntDiv, Exp, Mod,
    Not, And, Or, Pipe,
    Gt, Ge, Lt, Le, Ne, Eq,
    Neg, Ref, DeRef, Question, Exclam,
    Identifier(String)
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "block_type", content = "value"))]
#[cfg_attr(feature = "gc", derive(Finalize))]
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
    Block(BlockType,Vec<Ptr<Expr>>),
    Loop(Ptr<Expr>),
    Fun(Vec<String>,Ptr<Expr>),
    Dyn(bool,Vec<String>,Ptr<Expr>),
    Closed(Vec<String>,Ptr<Expr>),
    Use(Option<Ptr<Expr>>,Option<Vec<String>>),
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
    Operator(Operator),
    Identifier(String),
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
            (Expr::Operator(a),Expr::Operator(b)) => a == b,
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
        Expr::Ref(mc) =>
            decycle(mc.get(),env),
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
            Expr::Ref(mc) => write!(f,"->{}",decycle(mc.get(),&mut HashSet::new())),
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
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
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

impl<T: Into<Value>> From<HashMap<String,T>> for Value {
    fn from(value: HashMap<String,T>) -> Self {
        Value::Dict(value.into_iter().map(|(k,v)| { (k,v.into()) }).collect())
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Value::Array(value.into_iter().map(Into::into).collect())       
    }
}

impl<T: Into<Value> + Clone> From<&[T]> for Value {
    fn from(value: &[T]) -> Self {
        Value::Array(value.iter().cloned().map(Into::into).collect())       
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)       
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)       
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)       
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Character(value)       
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)       
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(value.to_string())       
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(opt_value: Option<T>) -> Self {
        if let Some(value) = opt_value {
            value.into()
        } else {
            Value::Nil
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

impl TryFrom<Value> for Vec<i64> {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::Array(v) => Ok(v.into_iter().map(|x|x.try_into()).collect::<Result<Vec<_>>>()?),
            _ => Err(anyhow!("can't convert this value to a Vec<i64>"))
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

impl Value {
    #[cfg(feature = "ron")]
    pub fn try_to_ron(&self) -> Result<String> {
        Ok(ron::to_string(self)?)
    }
    #[cfg(feature = "ron")]
    pub fn try_from_ron(ron_str: &str) -> Result<Self> {
        Ok(ron::from_str(ron_str)?)
    }
}

#[derive(Debug,Clone,PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "syntax", content = "value"))]
pub enum Syntax {
    ArrayLiteral(Vec<Syntax>),
    ArrayAccess(Box<Syntax>,Box<Syntax>),
    AssignToDeRefExpression(Box<Syntax>,Box<Syntax>),
    AssignToExpression(Box<Syntax>,Box<Syntax>),
    BinOpCall(Operator,Box<Syntax>,Box<Syntax>),
    Block(BlockType,Vec<Syntax>),
    Boolean(bool),
    Break(Box<Syntax>),
    Builtin(String),
    Character(char),
    Closed(Vec<String>,Box<Syntax>),
    Continue,
    Dyn(bool,Vec<String>,Box<Syntax>),
    Exit(Box<Syntax>),
    Field(Box<Syntax>,String),
    Float(f64),
    Forget(Vec<String>),
    Fun(Vec<String>,Box<Syntax>),
    FunctionCall(Box<Syntax>,Vec<Syntax>),
    Identifier(String),
    If(Box<Syntax>,Box<Syntax>,Box<Syntax>),
    Integer(i64),
    Iterate(String,Box<Syntax>,Box<Syntax>),
    Let(String,Box<Syntax>),
    LetArray(Vec<String>,Box<Syntax>),
    LetRef(String,Box<Syntax>),
    Loop(Box<Syntax>),
    Nil,
    OpAssignToExpression(Operator,Box<Syntax>,Box<Syntax>),
    Operator(Operator),
    Return(Box<Syntax>),
    String(String),
    Test(String,Box<Syntax>,Box<Syntax>),
    Throw(Box<Syntax>),
    TryCatch(Box<Syntax>,String,Box<Syntax>),
    UnaryOpCall(Operator,Box<Syntax>), 
    Use(Option<Box<Syntax>>,Option<Vec<String>>),
    Variable(String),
}

impl TryFrom<Syntax> for Value {
    type Error = anyhow::Error;
    fn try_from(syntax: Syntax) -> Result<Self> {
        Ok(match syntax {
            Syntax::Nil => Value::Nil,
            Syntax::Integer(i) => Value::Integer(i),
            Syntax::Float(f) => Value::Float(f),
            Syntax::Character(c) => Value::Character(c),
            Syntax::Boolean(b) => Value::Boolean(b),
            Syntax::String(s) => Value::String(s),
            Syntax::ArrayLiteral(vec) => {
                let v = vec.iter()
                    .map(|e| { Self::try_from(e.to_owned()) })
                    .collect::<Result<Vec<_>>>()?;
                Value::Array(v)
            }
            _ => Err(anyhow!("can't convert this syntax to a value"))?
        })
    }
}

impl TryFrom<Value> for Syntax {
    type Error = anyhow::Error;
    fn try_from(value: Value) -> Result<Self> {
        Ok(match value {
            Value::Nil => Syntax::Nil,
            Value::Integer(i) => Syntax::Integer(i),
            Value::Float(f) => Syntax::Float(f),
            Value::Character(c) => Syntax::Character(c),
            Value::Boolean(b) => Syntax::Boolean(b),
            Value::String(s) => Syntax::String(s),
            Value::Array(v) => 
                Syntax::ArrayLiteral(v.into_iter().map(Self::try_from).collect::<Result<_>>()?),
            Value::Dict(_) => Err(anyhow!("can't convert this dict value to syntax"))?
        })
    }
}

impl std::fmt::Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Syntax::Nil => write!(f,"nil"),
            Syntax::Boolean(a) => write!(f,"{a}"),
            Syntax::Float(a) => write!(f,"{a}"),
            Syntax::Integer(a) => write!(f,"{a}"),
            Syntax::Character(a) => write!(f,"{a}"),
            Syntax::String(a) => write!(f,"{a}"),
            Syntax::ArrayLiteral(v) =>
                write!(f,"[{}]",v.iter().map(|x|x.to_string()).collect::<Vec<_>>().join(", ")),
            _ => write!(f,"{:#?}",self),
        }
    }
}

impl Syntax {
    #[cfg(feature = "ron")]
    pub fn try_to_ron(&self) -> Result<String> {
        Ok(ron::to_string(self)?)
    }
    #[cfg(feature = "ron")]
    pub fn try_from_ron(ron_str: &str) -> Result<Self> {
        Ok(ron::from_str(ron_str)?)
    }
    #[cfg(feature = "json")]
    pub fn try_to_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }
    #[cfg(feature = "json")]
    pub fn try_from_json(ron_str: &str) -> Result<Self> {
        Ok(serde_json::from_str(ron_str)?)
    }
}

#[cfg(feature = "gc")]
unsafe impl Trace for Syntax {
    gc::unsafe_empty_trace!();
}
#[cfg(feature = "gc")]
impl Finalize for Syntax {
    fn finalize(&self) {}
}
#[cfg(feature = "gc")]
unsafe impl Trace for Operator {
    gc::unsafe_empty_trace!();
}
#[cfg(feature = "gc")]
unsafe impl Trace for BlockType {
    gc::unsafe_empty_trace!();
}

impl From<Box<Syntax>> for Ptr<Expr> {
    fn from(value: Box<Syntax>) -> Self {
        let val = *value;
        let expr = Expr::from(val);
        expr.into()
    }
}

impl From<Syntax> for Ptr<Expr> {
    fn from(syntax: Syntax) -> Self {
        let expr = Expr::from(syntax);
        expr.into()
    }
}

impl From<Syntax> for Expr {
    fn from(syntax: Syntax) -> Self {
        match syntax {
            Syntax::Integer(i) => Expr::Integer(i),
            Syntax::ArrayAccess(a, b) => Expr::ArrayAccess(a.into(), b.into()),
            Syntax::ArrayLiteral(v) => Expr::Array(PtrCell::new(v.into_iter().map(Into::into).collect())),
            Syntax::AssignToDeRefExpression(a,b) => Expr::AssignToDeRefExpression(a.into(),b.into()),
            Syntax::AssignToExpression(b1,b2) => Expr::AssignToExpression(b1.into(),b2.into()),
            Syntax::BinOpCall(op,b1,b2) => Expr::BinOpCall(op,b1.into(),b2.into()),
            Syntax::Block(btyp, v) => 
                Expr::Block(btyp, v.into_iter().map(Into::into).collect()),
            Syntax::Boolean(b) => Expr::Boolean(b),
            Syntax::Break(b) => Expr::Break(b.into()),
            Syntax::Builtin(s) => Expr::BuiltinVariable(s),
            Syntax::Character(c) => Expr::Character(c),
            Syntax::Closed(vstr,b) => Expr::Closed(vstr, b.into()),
            Syntax::Continue => Expr::Continue,
            Syntax::Dyn(b,vstr,b1) => Expr::Dyn(b,vstr,b1.into()),
            Syntax::Exit(b) => Expr::Exit(b.into()),
            Syntax::Field(b,s) => Expr::Field(b.into(), s),
            Syntax::Float(f) => Expr::Float(f),
            Syntax::Forget(vstr) => Expr::Forget(vstr),
            Syntax::Fun(vstr,b) => Expr::Fun(vstr,b.into()),
            Syntax::FunctionCall(b,v) => 
                Expr::FunctionCall(b.into(), v.into_iter().map(Into::into).collect()),
            Syntax::Identifier(s) => Expr::Identifier(s),
            Syntax::If(b1,b2,b3) => Expr::If(b1.into(),b2.into(),b3.into()),
            Syntax::Iterate(s,b1,b2) => Expr::Iterate(s, b1.into(), b2.into()),
            Syntax::Let(s,b) => Expr::Let(s,b.into()),
            Syntax::LetArray(vstr,b) => Expr::LetArray(vstr, b.into()),
            Syntax::LetRef(s,b) => Expr::LetRef(s, b.into()),
            Syntax::Loop(b) => Expr::Loop(b.into()),
            Syntax::Nil => Expr::Nil,
            Syntax::OpAssignToExpression(op,b1,b2) => Expr::OpAssignToExpression(op, b1.into(), b2.into()),
            Syntax::Operator(op) => Expr::Operator(op),
            Syntax::Return(b) => Expr::Return(b.into()),
            Syntax::String(s) => Expr::String(s),
            Syntax::Test(s,b1,b2) => Expr::Test(s,b1.into(),b2.into()),
            Syntax::Throw(b) => Expr::Throw(b.into()),
            Syntax::TryCatch(b1,s,b2) => Expr::TryCatch(b1.into(), s, b2.into()),
            Syntax::UnaryOpCall(op,b) => Expr::UnaryOpCall(op, b.into()),
            Syntax::Use(opt_b,vstr) => Expr::Use(opt_b.map(Into::into),vstr),
            Syntax::Variable(s) => Expr::Variable(s),
        }
    }    
}

