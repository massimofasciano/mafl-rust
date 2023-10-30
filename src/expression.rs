use std::ops::{Add, Sub, Mul, Neg};
use anyhow::{anyhow, Result};

use pest_derive::Parser;

use crate::context::Context;

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

#[derive(Debug,Clone,PartialEq)]
pub enum Expression {
    Error(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier(String),
    String(String),
    Variable(String),
    InfixOp(String),
    Builtin(String),
    Unit,
    Block(Vec<Expression>),
    FunctionCall(Box<Expression>,Vec<Expression>),
    Field(Box<Expression>,Box<Expression>),
    BinOpCall(Box<Expression>,Box<Expression>,Box<Expression>),
    UnaryOpCall(Box<Expression>,Box<Expression>),
    AddOp, MultOp, SubOp, DivOp, ExpOp,
    NotOp, AndOp, OrOp, PipeOp,
    GtOp, GeOp, LtOp, LeOp, NeOp, EqOp,
    NegOp, RefOp, DeRefOp, QuestionOp, ExclamOp,
    If(Box<Expression>,Box<Expression>,Box<Expression>),
    While(Box<Expression>,Box<Expression>),
    DoWhile(Box<Expression>,Box<Expression>),
    Var(String,Box<Expression>),
    Let(String,Box<Expression>,Box<Expression>),
    Assign(String,Box<Expression>),
    Loop(Box<Expression>),
    Function(Vec<String>,Box<Expression>),
    Closure(Context,Vec<String>,Box<Expression>),
    Return(Box<Expression>),
    Continue, Break,
}

impl From<Expression> for AtomicExpression {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Integer(i) => Self::Integer(i),
            Expression::Float(f) => Self::Float(f),
            Expression::Boolean(b) => Self::Boolean(b),
            Expression::String(s) => Self::String(s),
            _ => Self::Unit,
        }
    }
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum AtomicExpression {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Unit,
}

impl Expression {
    pub fn as_error(&self) -> Result<Expression> {
        Err(anyhow!(self.to_owned()))
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Float(a) => write!(f,"{a}"),
            Expression::Integer(a) => write!(f,"{a}"),
            Expression::String(a) => write!(f,"{a}"),
            _ => write!(f,"{:?}",self),
        }
    }
}

impl Add for Expression {
    type Output = Expression;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expression::Float(a), Expression::Float(b)) => Expression::Float(a+b),
            (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a+b as f64),
            (Expression::Integer(a), Expression::Float(b)) => Expression::Float(a as f64+b),
            (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a+b),
            (Expression::String(a), Expression::String(b)) => Expression::String(format!("{a}{b}")),
            (Expression::String(a), Expression::Integer(b)) => Expression::String(format!("{a}{b}")),
            (Expression::Integer(a), Expression::String(b)) => Expression::String(format!("{a}{b}")),
            (Expression::String(a), Expression::Float(b)) => Expression::String(format!("{a}{b}")),
            (Expression::Float(a), Expression::String(b)) => Expression::String(format!("{a}{b}")),
            _ => Self::Error("add".to_owned()),
        }
    }
}

impl Sub for Expression {
    type Output = Expression;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expression::Float(a), Expression::Float(b)) => Expression::Float(a-b),
            (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a-b as f64),
            (Expression::Integer(a), Expression::Float(b)) => Expression::Float(a as f64-b),
            (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a-b),
            _ => Self::Error("sub".to_owned()),
        }
    }
}

impl Mul for Expression {
    type Output = Expression;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Expression::Float(a), Expression::Float(b)) => Expression::Float(a*b),
            (Expression::Float(a), Expression::Integer(b)) => Expression::Float(a*b as f64),
            (Expression::Integer(a), Expression::Float(b)) => Expression::Float(a as f64*b),
            (Expression::Integer(a), Expression::Integer(b)) => Expression::Integer(a*b),
            (Expression::String(a), Expression::Integer(b)) => Expression::String(a.repeat(b as usize)),
            (Expression::Integer(a), Expression::String(b)) => Expression::String(b.repeat(a as usize)),
            _ => Self::Error("mul".to_owned()),
        }
    }
}

impl Neg for Expression {
    type Output = Expression;
    fn neg(self) -> Self::Output {
        match self {
            Expression::Float(a) => Expression::Float(-a),
            Expression::Integer(a) => Expression::Integer(-a),
            Expression::String(a) => Expression::String(a.chars().rev().collect::<String>()),
            _ => Self::Error("neg".to_owned()),
        }
    }
}
