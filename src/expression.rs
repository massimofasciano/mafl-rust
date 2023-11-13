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
pub type Ident = String;
// pub type Ident = usize;
pub type Idents = Vec<Ident>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Add, Mul, Sub, Div, IntDiv, Exp, Mod,
    Not, And, Or, Pipe,
    Gt, Ge, Lt, Le, Ne, Eq,
    Neg, Ref, DeRef, Question, Exclam,
    Identifier(Ident)
}

#[derive(Debug,Clone)]
pub enum ExpressionType {
    Integer(i64),
    Float(f64),
    Character(char),
    Boolean(bool),
    String(String),
    Nil,
    Error(String),
    Variable(Ident),
    BuiltinVariable(String),
    BuiltinFunction(String),
    Block(Expressions),
    FunctionBlock(Expressions),
    IfBlock(Expressions),
    Sequence(Expressions),
    FunctionCall(Expression,Expressions),
    Field(Expression,Ident),
    BinOpCall(Operator,Expression,Expression),
    UnaryOpCall(Operator,Expression),
    If(Expression,Expression,Expression),
    While(Expression,Expression),
    DoWhile(Expression,Expression),
    For(Ident,Expression,Expression),
    TryCatch(Expression,Ident,Expression),
    Let(Ident,Expression),
    LetArray(Idents,Expression),
    BindIn(Ident,Expression,Expression),
    AssignToExpression(Expression,Expression),
    AssignToDeRefExpression(Expression,Expression),
    OpAssignToExpression(Operator,Expression,Expression),
    ArrayAccess(Expression,Expression),
    Loop(Expression),
    Object(Expression),
    Context(Idents,Expression),
    Module(Ident,Idents,Expression),
    Fun(Idents,Idents,Idents,Expression),
    Closure(Context,Idents,Expression),
    Array(RefCell<Expressions>),
    Return(Expression),
    Break(Expression),
    EndBlock(Expression),
    Throw(Expression),
    Continue, 
    Ref(Rc<MemCell>),
    Scope(ScopeID,HashMap<Ident,Expression>),
    ScopeCycle(ScopeID),
    ClosurePrintable(Expression,Idents,Expression),
    ArrayPrintable(Expressions),
    ExceptionPrintable(Expression),
    ParsedOperator(Operator),
    ParsedIdentifier(Ident),
}

impl PartialEq for ExpressionType {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (ExpressionType::Integer(a),ExpressionType::Integer(b)) => a == b,
            (ExpressionType::Float(a),ExpressionType::Float(b)) => a == b,
            (ExpressionType::Float(a), ExpressionType::Integer(b)) => (*a) == (*b as f64),
            (ExpressionType::Integer(a), ExpressionType::Float(b)) => (*a as f64) == (*b),
            (ExpressionType::Character(a),ExpressionType::Character(b)) => a == b,
            (ExpressionType::String(a),ExpressionType::String(b)) => a == b,
            (ExpressionType::Error(a),ExpressionType::Error(b)) => a == b,
            (ExpressionType::Boolean(a),ExpressionType::Boolean(b)) => a == b,
            (ExpressionType::Nil,ExpressionType::Nil) => true,
            (ExpressionType::Array(a),ExpressionType::Array(b)) => a == b,
            (ExpressionType::ParsedOperator(a),ExpressionType::ParsedOperator(b)) => a == b,
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
        ExpressionType::Break(rc) =>
            ExpressionType::ExceptionPrintable(decycle(rc.to_owned(),env)).into(),
        ExpressionType::Return(rc) => 
            ExpressionType::ExceptionPrintable(decycle(rc.to_owned(),env)).into(),
        ExpressionType::Throw(rc) =>
            ExpressionType::ExceptionPrintable(decycle(rc.to_owned(),env)).into(),
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
            ExpressionType::Error(a) => write!(f,"Error<{a}>"),
            ExpressionType::ArrayPrintable(a) =>
                write!(f,"[{}]",a.iter().map(|x|x.to_string()).collect::<Vec<_>>().join(",")),
            ExpressionType::Ref(mc) => write!(f,"->{}",mc.get()),
            ExpressionType::ExceptionPrintable(a) => write!(f,"Exception<{a}>"),
            ExpressionType::Throw(_) | 
            ExpressionType::Return(_) | 
            ExpressionType::Break(_) | 
            ExpressionType::Array(_) | 
            ExpressionType::Closure(_, _, _) => 
                write!(f,"{}",decycle(self.to_owned().into(),&mut HashSet::new()).deref()),
            _ => write!(f,"{:#?}",self),
        }
    }
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

pub fn closure(ctx: Context, args: Idents, body: Expression) -> Expression {
    ExpressionType::Closure(ctx,args,body).into()
}

pub fn context(ctx: Context) -> Expression {
    closure(ctx,vec![],nil())
}

pub fn array(vals: Expressions) -> Expression {
    ExpressionType::Array(RefCell::new(vals)).into()
}
