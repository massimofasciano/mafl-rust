use std::ops::{Add, Sub, Mul, Neg};

use pest_derive::Parser;

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

// pub type Expression = Expression;
impl From<&Expression> for Expression {
    fn from(value: &Expression) -> Self {
        value.to_owned()
    }
}

// impl Value {
//     pub fn pow(self, rhs: Self) -> Self {
//         match (self, rhs) {
//             (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(b)),
//             (Value::Float(a), Value::Integer(b)) => Value::Float(a.powf(b as f64)),
//             (Value::Integer(a), Value::Float(b)) => Value::Float((a as f64).powf(b)),
//             (Value::Integer(a), Value::Integer(b)) => Value::Integer(a.pow(b as u32)),
//             _ => Self::Unit,
//         }
//     }
// }

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
            _ => Self::Unit,
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
            _ => Self::Unit,
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
            _ => Self::Unit,
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
    Binding(String,Expression),
}

impl ContextItem {
    pub fn new_scope() -> Self {
        Self::NewScope
    }
    pub fn new_binding(var: String, value: Expression) -> Self {
        Self::Binding(var, value)
    }
    pub fn value(&self, search_var: &str) -> Option<&Expression> {
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
    pub fn add_binding(&mut self, var: String, value: Expression) {
        self.stack.push(ContextItem::new_binding(var, value))
    }
    pub fn get_binding(&self, var: &str) -> Option<&Expression> {
        let st = &self.stack;
        for idx in (0..st.len()).rev() {
            if let Some(value) = st[idx].value(var) {
                return Some(value);
            }
        }
        None
    }
    pub fn set_binding(&mut self, var: String, value: Expression) -> bool {
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
    use super::{Expression,Context};

    fn test_val(i: i64) -> Expression {
        Expression::Integer(i)
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
