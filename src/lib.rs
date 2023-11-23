use std::{cell::RefCell, collections::HashMap};
use context::Context;
use expression::{Expr, Builtin, Value};
use pest::Parser as _;
use pest_derive::Parser;

use anyhow::{anyhow, Result};
#[cfg(feature = "gc")]
use gc::{Gc, GcCell};
#[cfg(not(feature = "gc"))]
use std::rc::Rc;

#[derive(Parser)]
#[grammar = "mafl.pest"]
pub struct MaflParser;

pub mod eval;
pub mod expression;
pub mod context;
pub mod builtin;
pub mod parse;
pub mod open;

#[cfg(not(feature = "gc"))]
pub type CellRefMut<'a,T> = std::cell::RefMut<'a,T>;
#[cfg(not(feature = "gc"))]
pub type Ptr<T> = Rc<T>;
#[cfg(not(feature = "gc"))]
pub type PtrCell<T> = RefCell<T>;
#[cfg(feature = "gc")]
pub type CellRefMut<'a,T> = gc::GcCellRefMut<'a,T>;
#[cfg(feature = "gc")]
pub type Ptr<T> = Gc<T>;
#[cfg(feature = "gc")]
pub type PtrCell<T> = GcCell<T>;

#[derive(Debug,Clone)]
pub struct Interpreter {
    args: Ptr<Expr>,
    std: Ptr<Expr>,
    ctx: Context,
    test_pass_count: RefCell<usize>,
    test_fail_count: RefCell<usize>,
    pragma_shadow_local: RefCell<PragmaLevel>,
    builtin_vars: HashMap<String,Builtin>,
    builtin_fns: HashMap<String,Builtin>,
}

#[derive(Debug,Clone)]
pub enum PragmaLevel {
    Allow,
    Warn,
    Error,
}

#[cfg(not(feature = "std_internal"))]
const STD_MAFL : &str = "src/std.mafl";
#[cfg(feature = "std_internal")]
static _STD_STR : &str = include_str!("std.mafl");

impl Interpreter {
    pub fn new() -> Result<Self> {
        let mut interpreter = Self::default();
        interpreter.init_std()?;
        Ok(interpreter)
    }
    pub fn set_args(&mut self, env: Vec<String>) {
        self.args = expression::array(env.into_iter().map(expression::string).collect());
    }
    pub fn set_bindings(&mut self, values: HashMap<String,Value>) {
        self.ctx = Context::from(values);
    }
    pub fn get_bindings(&self) -> Result<HashMap<String,Value>> {
        self.ctx.bindings_cloned().into_iter().map(|(k,v)| -> Result<(String,Value)> {
            Ok((k, Value::try_from(v.get())?))
        }).collect()
    }
    pub fn init_std(&mut self) -> Result<()> {
        let ctx = Context::new();
        #[cfg(feature = "std_internal")]
        builtin::include_str(self, &ctx, _STD_STR)?;
        #[cfg(not(feature = "std_internal"))]
        {
            let std_str = std::fs::read_to_string(STD_MAFL)?;
            builtin::include_str(self, &ctx, &std_str)?;
        }
        self.std = expression::closure(ctx, vec![], expression::nil());
        Ok(())
    }
    pub fn run(&self, source: &str) -> Result<Ptr<Expr>> {
        let expr = self.parse_source(source)?;
        // println!("{expr:#?}");
        self.eval(&self.ctx,&expr)
    }
    pub fn print(&self, e: Ptr<Expr>) -> Result<Ptr<Expr>> {
        self.builtin_fn(&self.ctx, "print", &[e])
    }
    pub fn println(&self, e: Ptr<Expr>) -> Result<Ptr<Expr>> {
        self.builtin_fn(&self.ctx, "println", &[e])
    }
    pub fn parse_source(&self, source: &str) -> Result<Ptr<Expr>> {
        let parsed = MaflParser::parse(Rule::file, source)?
            .next().ok_or(anyhow!("parse error"))?; 
        // println!("{:#?}",parsed);
        let syntax = self.parse_rule(parsed)?;
        // println!("{:#?}",syntax);
        let expr = Expr::from(syntax);
        Ok(expr.into())
    }
    pub fn test_report(&self) -> (usize, usize) {
        let pass_count = *self.test_pass_count.borrow();
        let fail_count = *self.test_fail_count.borrow();
        (pass_count, fail_count)
    }
    pub fn add_builtin_fn(&mut self, name: String, f: Builtin) {
        self.builtin_fns.insert(name, f);
    }
    pub fn add_builtin_var(&mut self, name: String, f: Builtin) {
        self.builtin_vars.insert(name, f);
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {  
            args: expression::array(vec![]),
            std: expression::nil(),
            ctx: Context::new(),
            test_pass_count: RefCell::new(0),
            test_fail_count: RefCell::new(0),
            pragma_shadow_local: RefCell::new(PragmaLevel::Allow),
            builtin_vars: HashMap::new(),
            builtin_fns: HashMap::new(),
        }
    }
}

pub fn unescape_string(sr: &str) -> Result<String> {
    unescape::unescape(sr).ok_or(anyhow!("error unescaping string: {sr}"))
}

#[cfg(test)]
mod tests {
    use crate::{Interpreter, expression};
    use anyhow::Result;

    #[test]
    fn syntax_test() -> Result<()> {
        // need extra stack during test... (set it at 8MB like the main thread)
        // RUST_MIN_STACK=8388608 cargo test
        let interpreter = Interpreter::new()?;
        // this program returns true if no tests failed
        let file = "mafl/language.mafl";
        // run the program
        let source = std::fs::read_to_string(file)?;
        let result = interpreter.run(&source)?;
        // result should be true
        assert_eq!(result, expression::boolean(true));
        Ok(())
    }
}
