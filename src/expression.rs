use std::{cell::RefCell, rc::Rc};

use anyhow::{anyhow, Result};

use pest_derive::Parser;

use crate::context::Context;

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

#[derive(Debug,Clone,PartialEq)]
pub enum Expression {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Error(String),
    Variable(String),
    InfixOp(String),
    Builtin(String),
    Unit,
    Block(Vec<Expression>),
    Sequence(Vec<Expression>),
    FunctionCall(Box<Expression>,Vec<Expression>),
    Field(Box<Expression>,String),
    BinOpCall(Box<Expression>,Box<Expression>,Box<Expression>),
    UnaryOpCall(Box<Expression>,Box<Expression>),
    AddOp, MultOp, SubOp, DivOp, ExpOp, ModOp,
    NotOp, AndOp, OrOp, PipeOp,
    GtOp, GeOp, LtOp, LeOp, NeOp, EqOp,
    NegOp, RefOp, DeRefOp, QuestionOp, ExclamOp,
    If(Box<Expression>,Box<Expression>,Box<Expression>),
    While(Box<Expression>,Box<Expression>),
    DoWhile(Box<Expression>,Box<Expression>),
    Let(String,Box<Expression>),
    LetIn(String,Box<Expression>,Box<Expression>),
    Assign(String,Box<Expression>),
    AssignToExpression(Box<Expression>,Box<Expression>),
    Loop(Box<Expression>),
    ContextSyntax(Vec<String>,Box<Expression>),
    CaptureSyntax(Vec<String>,Box<Expression>),
    FunctionStatic(Vec<String>,Box<Expression>),
    FunctionDynamic(Vec<String>,Box<Expression>),
    FunctionClosure(Vec<String>,Box<Expression>),
    Closure(Context,Vec<String>,Box<Expression>),
    Array(Rc<RefCell<Vec<Expression>>>),
    Return(Box<Expression>),
    Continue, Break,
}

impl Expression {
    pub fn to_error(&self) -> Result<Expression> {
        Err(anyhow!("{:#?}",self))
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Unit => write!(f,"()"),
            Expression::Boolean(a) => write!(f,"{a}"),
            Expression::Float(a) => write!(f,"{a}"),
            Expression::Integer(a) => write!(f,"{a}"),
            Expression::String(a) => write!(f,"{a}"),
            Expression::Error(a) => write!(f,"Error: {a}"),
            Expression::Array(a) => write!(f,"{:?}",a.borrow()),
            _ => write!(f,"{:#?}",self),
        }
    }
}
