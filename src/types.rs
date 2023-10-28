use std::collections::HashMap;

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum Ast {
    // Todo(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Unit,
    Block(Vec<Ast>),
    Identifier(String),
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
    String(String),
    Return(Box<Ast>),
    Variable(String),
    Continue, Break,
}

pub type Context = HashMap<String,Value>;

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit,
    Ast(Ast),
}