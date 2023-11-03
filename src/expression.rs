use std::{cell::RefCell, rc::Rc};
use anyhow::{anyhow, Result};
use pest_derive::Parser;
use crate::context::Context;

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

pub type Expression = Rc<ExpressionType>;
pub type Expressions = Vec<Expression>;
pub type Strings = Vec<String>;

#[derive(Debug,Clone,PartialEq)]
pub enum ExpressionType {
    Integer(i64),
    Float(f64),
    Character(char),
    Boolean(bool),
    String(String),
    Error(String),
    Variable(String),
    InfixOp(String),
    BuiltinFunction(String),
    Unit,
    Block(Expressions),
    Sequence(Expressions),
    FunctionCall(Expression,Expressions),
    Field(Expression,String),
    BinOpCall(Expression,Expression,Expression),
    UnaryOpCall(Expression,Expression),
    AddOp, MultOp, SubOp, DivOp, ExpOp, ModOp,
    NotOp, AndOp, OrOp, PipeOp,
    GtOp, GeOp, LtOp, LeOp, NeOp, EqOp,
    NegOp, RefOp, DeRefOp, QuestionOp, ExclamOp,
    If(Expression,Expression,Expression),
    While(Expression,Expression),
    DoWhile(Expression,Expression),
    For(String,Expression,Expression),
    Let(String,Expression),
    LetIn(String,Expression,Expression),
    Assign(String,Expression),
    AssignToExpression(Expression,Expression),
    Loop(Expression),
    Context(Strings,Expression),
    Module(Strings,Expression),
    FunctionStatic(Strings,Expression),
    FunctionDynamic(Strings,Expression),
    FunctionClosure(Strings,Expression),
    Closure(Context,Strings,Expression),
    Array(RefCell<Expressions>),
    Return(Expression),
    Continue, Break,
}

impl ExpressionType {
    pub fn to_error(&self) -> Result<Expression> {
        Err(anyhow!("{:#?}",self))
    }
}

impl std::fmt::Display for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionType::Unit => write!(f,"()"),
            ExpressionType::Boolean(a) => write!(f,"{a}"),
            ExpressionType::Float(a) => write!(f,"{a}"),
            ExpressionType::Integer(a) => write!(f,"{a}"),
            ExpressionType::Character(a) => write!(f,"{a}"),
            ExpressionType::String(a) => write!(f,"{a}"),
            ExpressionType::Error(a) => write!(f,"Error: {a}"),
            ExpressionType::Array(a) => 
                write!(f,"[{}]",a.borrow().iter().map(|x|x.to_string()).collect::<Vec<_>>().join(",")),
            _ => write!(f,"{:#?}",self),
        }
    }
}

pub fn error(msg: String) -> Expression {
    ExpressionType::Error(msg).into()
}

pub fn integer(i: i64) -> Expression {
    ExpressionType::Integer(i).into()
}

pub fn float(f: f64) -> Expression {
    ExpressionType::Float(f).into()
}

pub fn boolean(b: bool) -> Expression {
    ExpressionType::Boolean(b).into()
}

pub fn character(c: char) -> Expression {
    ExpressionType::Character(c).into()
}

pub fn string(s: String) -> Expression {
    ExpressionType::String(s).into()
}

pub fn unit() -> Expression {
    ExpressionType::Unit.into()
}

pub fn closure(ctx: Context, args: Strings, body: Expression) -> Expression {
    ExpressionType::Closure(ctx,args,body).into()
}

pub fn array(vals: Expressions) -> Expression {
    ExpressionType::Array(RefCell::new(vals)).into()
}
