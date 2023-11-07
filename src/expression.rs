use std::{cell::RefCell, rc::Rc, ops::Deref, collections::{HashSet, HashMap}};
use anyhow::{anyhow, Result};
use pest_derive::Parser;
use crate::context::{Context, MemCell, ScopeID};

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

pub type Expression = Rc<ExpressionType>;
pub type Expressions = Vec<Expression>;
pub type Strings = Vec<String>;

#[derive(Debug,Clone)]
pub enum ExpressionType {
    Integer(i64),
    Float(f64),
    Character(char),
    Boolean(bool),
    String(String),
    Identifier(String),
    Error(String),
    Variable(String),
    InfixOp(String),
    BuiltinFunction(String),
    Nil,
    Block(Expressions),
    Sequence(Expressions),
    FunctionCall(Expression,Expressions),
    Field(Expression,String),
    BinOpCall(Expression,Expression,Expression),
    UnaryOpCall(Expression,Expression),
    AddOp, MultOp, SubOp, DivOp, IntDivOp, ExpOp, ModOp,
    NotOp, AndOp, OrOp, PipeOp,
    GtOp, GeOp, LtOp, LeOp, NeOp, EqOp,
    NegOp, RefOp, DeRefOp, QuestionOp, ExclamOp,
    If(Expression,Expression,Expression),
    While(Expression,Expression),
    DoWhile(Expression,Expression),
    For(String,Expression,Expression),
    Let(String,Expression),
    LetArray(Strings,Expression),
    LetIn(String,Expression,Expression),
    // Assign(String,Expression), // deprecated in favor of AssignToExpression
    AssignToExpression(Expression,Expression),
    ArrayAccess(Expression,Expression),
    Loop(Expression),
    Context(Strings,Expression),
    Module(String,Strings,Expression),
    Defun(String,Strings,Expression),
    Lambda(Strings,Expression),
    Closure(Context,Strings,Expression),
    ClosurePrintable(Expression,Strings,Expression),
    Array(RefCell<Expressions>),
    ArrayPrintable(Expressions),
    Return(Expression),
    Ref(Rc<MemCell>),
    Continue, Break,
    Scope(ScopeID,HashMap<String,Expression>),
    ScopeCycle(ScopeID),
}

impl PartialEq for ExpressionType {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (ExpressionType::Integer(a),ExpressionType::Integer(b)) => a == b,
            (ExpressionType::Float(a),ExpressionType::Float(b)) => a == b,
            (ExpressionType::Character(a),ExpressionType::Character(b)) => a == b,
            (ExpressionType::String(a),ExpressionType::String(b)) => a == b,
            (ExpressionType::Error(a),ExpressionType::Error(b)) => a == b,
            (ExpressionType::Boolean(a),ExpressionType::Boolean(b)) => a == b,
            (ExpressionType::Nil,ExpressionType::Nil) => true,
            (ExpressionType::Array(a),ExpressionType::Array(b)) => a == b,
            (ExpressionType::GtOp,ExpressionType::GtOp) => true,
            (ExpressionType::GeOp,ExpressionType::GeOp) => true,
            (ExpressionType::LtOp,ExpressionType::LtOp) => true,
            (ExpressionType::LeOp,ExpressionType::LeOp) => true,
            (ExpressionType::EqOp,ExpressionType::EqOp) => true,
            (ExpressionType::NeOp,ExpressionType::NeOp) => true,
            (ExpressionType::NotOp,ExpressionType::NotOp) => true,
            (ExpressionType::NegOp,ExpressionType::NegOp) => true,
            (ExpressionType::AndOp,ExpressionType::AndOp) => true,
            (ExpressionType::AddOp,ExpressionType::AddOp) => true,
            (ExpressionType::SubOp,ExpressionType::SubOp) => true,
            (ExpressionType::MultOp,ExpressionType::MultOp) => true,
            (ExpressionType::DivOp,ExpressionType::DivOp) => true,
            (ExpressionType::IntDivOp,ExpressionType::IntDivOp) => true,
            (ExpressionType::PipeOp,ExpressionType::PipeOp) => true,
            (ExpressionType::RefOp,ExpressionType::RefOp) => true,
            (ExpressionType::DeRefOp,ExpressionType::DeRefOp) => true,
            (ExpressionType::QuestionOp,ExpressionType::QuestionOp) => true,
            (ExpressionType::ModOp,ExpressionType::ModOp) => true,
            (ExpressionType::ExpOp,ExpressionType::ExpOp) => true,
            _ => false,
        }
    }
}

impl ExpressionType {
    pub fn to_error(&self) -> Result<Expression> {
        Err(anyhow!("{:#?}",self))
    }
}

fn decycle(e: Expression, env: &mut HashSet<ScopeID>) -> Expression {
    match e.as_ref() {
        ExpressionType::Array(arr) => {
            let mut safe = vec![];
            for rc in arr.borrow().iter() {
                match rc.as_ref() {
                    ExpressionType::Closure(_,_ ,_) => {
                        safe.push(decycle(rc.to_owned(),env));
                    }
                    ExpressionType::Array(_) => {
                        safe.push(decycle(rc.to_owned(),env));
                    }
                    _ => safe.push(rc.to_owned()),
                }
            }
            ExpressionType::ArrayPrintable(safe).into()
        }
        ExpressionType::Closure(ctx, args, body) => {
            let scope_id = ctx.scope_id();
            if env.contains(&scope_id) {
                ExpressionType::ClosurePrintable(
                    ExpressionType::ScopeCycle(scope_id).into(), 
                    args.to_owned(), body.to_owned()
                ).into()
            } else {
                env.insert(scope_id);
                let mut safe = HashMap::new();
                for (k, v) in ctx.bindings_ref() {
                    let value = v.get();
                    match value.as_ref() {
                        ExpressionType::Closure(_, _, _) => {
                            safe.insert(k, decycle(value, env));
                        }
                        ExpressionType::Array(_) => {
                            safe.insert(k, decycle(value, env));
                        }
                        _ => {
                            safe.insert(k, value);
                        }
                    }
                }
                ExpressionType::ClosurePrintable(
                    ExpressionType::Scope(scope_id, safe).into(),
                    args.to_owned(),body.to_owned()
                ).into()
            }
        }
        _ => e.to_owned(),
    }
}

impl std::fmt::Display for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionType::Nil => write!(f,"nil"),
            ExpressionType::Boolean(a) => write!(f,"{a}"),
            ExpressionType::Float(a) => write!(f,"{a}"),
            ExpressionType::Integer(a) => write!(f,"{a}"),
            ExpressionType::Character(a) => write!(f,"{a}"),
            ExpressionType::String(a) => write!(f,"{a}"),
            ExpressionType::Error(a) => write!(f,"Error: {a}"),
            ExpressionType::ArrayPrintable(a) =>
                write!(f,"[{}]",a.iter().map(|x|x.to_string()).collect::<Vec<_>>().join(",")),
            ExpressionType::Array(_) | 
            ExpressionType::Closure(_, _, _) => 
                write!(f,"{}",decycle(self.to_owned().into(),&mut HashSet::new()).deref()),
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

pub fn nil() -> Expression {
    ExpressionType::Nil.into()
}

pub fn closure(ctx: Context, args: Strings, body: Expression) -> Expression {
    ExpressionType::Closure(ctx,args,body).into()
}

pub fn array(vals: Expressions) -> Expression {
    ExpressionType::Array(RefCell::new(vals)).into()
}
