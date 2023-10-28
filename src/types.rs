use std::{collections::HashMap, ops::{Add, Sub, Mul, Neg}};

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

#[derive(Debug,Clone)]
pub enum Ast {
    Error(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier(String),
    String(String),
    Variable(String),
    Unit,
    Block(Vec<Ast>),
    FunctionCall(Box<Ast>,Vec<Ast>),
    Field(Box<Ast>,Box<Ast>),
    BinOpCall(Box<Ast>,Box<Ast>,Box<Ast>),
    UnaryOpCall(Box<Ast>,Box<Ast>),
    AddOp, MultOp, SubOp, DivOp, ExpOp,
    NotOp, AndOp, OrOp, 
    GtOp, GeOp, LtOp, LeOp, NeOp, EqOp,
    NegOp, DollarOp, QuestionOp, ExclamOp,
    If(Box<Ast>,Box<Ast>,Box<Ast>),
    While(Box<Ast>,Box<Ast>),
    DoWhile(Box<Ast>,Box<Ast>),
    Let(String,Box<Ast>),
    Assign(String,Box<Ast>),
    Loop(Box<Ast>),
    Function(Vec<String>,Box<Ast>),
    Closure(Scope,Vec<String>,Box<Ast>),
    Return(Box<Ast>),
    Continue, Break,
}

impl From<Ast> for AstAtom {
    fn from(value: Ast) -> Self {
        match value {
            Ast::Integer(i) => Self::Integer(i),
            Ast::Float(f) => Self::Float(f),
            Ast::Boolean(b) => Self::Boolean(b),
            Ast::String(s) => Self::String(s),
            _ => Self::Unit,
        }
    }
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum AstAtom {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Unit,
}

pub type Scope = HashMap<String,Value>;
pub type Context = Scope;

// #[derive(Debug,Clone,PartialEq,PartialOrd)]
// pub enum Value {
//     Integer(i64),
//     Float(f64),
//     String(String),
//     Boolean(bool),
//     Unit,
//     Ast(Ast),
// }

pub type Value = Ast;
impl From<&Ast> for Value {
    fn from(value: &Ast) -> Self {
        value.to_owned()
    }
}

impl Value {
    pub fn pow(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(b)),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a.powf(b as f64)),
            (Value::Integer(a), Value::Float(b)) => Value::Float((a as f64).powf(b)),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a.pow(b as u32)),
            _ => Self::Unit,
        }
    }
}

impl Add for Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a+b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a+b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as f64+b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a+b),
            (Value::String(a), Value::String(b)) => Value::String(format!("{a}{b}")),
            (Value::String(a), Value::Integer(b)) => Value::String(format!("{a}{b}")),
            (Value::Integer(a), Value::String(b)) => Value::String(format!("{a}{b}")),
            (Value::String(a), Value::Float(b)) => Value::String(format!("{a}{b}")),
            (Value::Float(a), Value::String(b)) => Value::String(format!("{a}{b}")),
            _ => Self::Unit,
        }
    }
}

impl Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a-b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a-b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as f64-b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a-b),
            _ => Self::Unit,
        }
    }
}

impl Mul for Value {
    type Output = Value;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a*b),
            (Value::Float(a), Value::Integer(b)) => Value::Float(a*b as f64),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as f64*b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a*b),
            (Value::String(a), Value::Integer(b)) => Value::String(a.repeat(b as usize)),
            (Value::Integer(a), Value::String(b)) => Value::String(b.repeat(a as usize)),
            _ => Self::Unit,
        }
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> Self::Output {
        match self {
            Value::Float(a) => Value::Float(-a),
            Value::Integer(a) => Value::Integer(-a),
            Value::String(a) => Value::String(a.chars().rev().collect::<String>()),
            _ => Self::Unit,
        }
    }
}
