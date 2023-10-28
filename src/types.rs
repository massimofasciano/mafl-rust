use std::ops::{Add, Sub, Mul, Neg};

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "mfel.pest"]
pub struct MfelParser;

#[derive(Debug,Clone,PartialEq)]
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
    Closure(Context,Vec<String>,Box<Ast>),
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

#[derive(Debug,Clone,PartialEq)]
pub struct Context {
    stack: Vec<ContextItem>
}

#[derive(Debug,Clone,PartialEq)]
pub enum ContextItem {
    NewScope,
    Binding(String,Value),
}

impl ContextItem {
    pub fn new_scope() -> Self {
        Self::NewScope
    }
    pub fn new_binding(var: String, value: Value) -> Self {
        Self::Binding(var, value)
    }
    pub fn value(&self, search_var: &str) -> Option<&Value> {
        match self {
            Self::NewScope => None,
            Self::Binding(var, value) => if var == search_var {
                Some(value)
            } else {
                None
            }
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            stack: vec![]
        }
    }
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
    pub fn start_scope(&mut self) {
        self.stack.push(ContextItem::new_scope())
    }
    pub fn end_scope(&mut self) {
        let st = &mut self.stack;
        while !st.is_empty() && *st.last().unwrap() != ContextItem::new_scope() { st.pop(); }
        if !st.is_empty() { st.pop();}
    }
    pub fn add_binding(&mut self, var: String, value: Value) {
        self.stack.push(ContextItem::new_binding(var, value))
    }
    pub fn get_binding(&self, var: &str) -> Option<&Value> {
        let st = &self.stack;
        for idx in (0..st.len()).rev() {
            if let Some(value) = st[idx].value(var) {
                return Some(value);
            }
        }
        None
    }
    pub fn set_binding(&mut self, var: String, value: Value) -> bool {
        let st = &mut self.stack;
        for idx in (0..st.len()).rev() {
            if st[idx].value(&var).is_some() {
                st[idx]=ContextItem::new_binding(var, value);
                return true;
            }
        }
        false
    }
    pub fn add_context(&mut self, ctx: Context) {
        for item in ctx.stack {
            match item {
                ContextItem::NewScope => {},
                ContextItem::Binding(var, value) => self.add_binding(var, value),
            }
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::{Value,Context};

    fn test_val(i: i64) -> Value {
        Value::Integer(i)
    }

    #[test]
    fn stack_test() {
        let mut ctx = Context::new();
        ctx.add_binding("v1".to_owned(), test_val(1));
        ctx.add_binding("v2".to_owned(), test_val(2));
            ctx.start_scope();
            ctx.add_binding("v3".to_owned(), test_val(3));
                ctx.start_scope();
                ctx.add_binding("v4".to_owned(), test_val(4));
                ctx.add_binding("v5".to_owned(), test_val(5));
                ctx.add_binding("v1".to_owned(), test_val(11));
                assert_eq!(ctx.get_binding("v5"),Some(&test_val(5)));
                assert_eq!(ctx.get_binding("v2"),Some(&test_val(2)));
                assert_eq!(ctx.get_binding("v3"),Some(&test_val(3)));
                ctx.set_binding("v3".to_owned(), test_val(13));
                assert_eq!(ctx.get_binding("v3"),Some(&test_val(13)));
                assert_eq!(ctx.get_binding("v1"),Some(&test_val(11)));
                ctx.end_scope();
            assert_eq!(ctx.get_binding("v5"),None);
            assert_eq!(ctx.get_binding("v1"),Some(&test_val(1)));
            ctx.add_binding("v6".to_owned(), test_val(6));
            assert_eq!(ctx.get_binding("v3"),Some(&test_val(13)));
            ctx.end_scope();
        ctx.end_scope();
        assert!(ctx.is_empty());
    }
}
