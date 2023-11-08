use std::{collections::HashMap, cell::RefCell, rc::Rc};
use log::debug;
use crate::expression::Expression;
use std::sync::atomic::{AtomicUsize, Ordering};

pub type ScopeID = usize;
static SCOPE_ID_SEQ: AtomicUsize = AtomicUsize::new(0);
fn next_scope_id() -> ScopeID {
    SCOPE_ID_SEQ.fetch_add(1, Ordering::SeqCst);
    SCOPE_ID_SEQ.load(Ordering::SeqCst)
}

pub type MemID = usize;
static MEM_ID_SEQ: AtomicUsize = AtomicUsize::new(0);
fn next_mem_id() -> MemID {
    MEM_ID_SEQ.fetch_add(1, Ordering::SeqCst);
    MEM_ID_SEQ.load(Ordering::SeqCst)
}

#[derive(Debug)]
pub struct MemCell {
    inner: RefCell<Expression>,
    id : MemID,
}

impl MemCell {
    pub fn new(e : Expression) -> Self {
        let new = Self {
            inner: RefCell::new(e),
            id : next_mem_id(),
        };
        debug!("new cell id={}",new.id); 
        new
    }
    pub fn new_ref(e : Expression) -> Rc<Self> {
        Rc::new(Self::new(e))
    }
    pub fn get(&self) -> Expression {
        self.inner.borrow().to_owned()
    }
    pub fn get_refmut(&self) -> std::cell::RefMut<Expression> {
        self.inner.borrow_mut()
    }
    pub fn set(&self, e: Expression) -> Expression {
        let old = self.get();
        *self.inner.borrow_mut() = e;
        old
    }
    fn duplicate(&self) -> Self {
        debug!("duplicating cell id={}", self.id);
        Self::new(self.get())
    }
    fn duplicate_ref(&self) -> Rc<Self> {
        Rc::new(self.duplicate())
    }
}

impl Clone for MemCell {
    fn clone(&self) -> Self {
        self.duplicate()
    }
}

impl Drop for MemCell {
    fn drop(&mut self) {
        debug!("dropping cell id={}", self.id);
    }
}

#[derive(Debug,Clone)]
pub struct Context {
    bindings: Vec<(String,Rc<MemCell>)>,
    scopes: Vec<usize>,
}

impl Context {
    pub fn new() -> Self {
        debug!("new");
        Self {
            bindings: vec![],
            scopes: vec![],
        }
    }
    pub fn start_scope(&mut self) {
        debug!("start scope");
        self.scopes.push(self.bindings.len());
    }
    pub fn end_scope(&mut self) {
        debug!("end scope");
        if let Some(st) = self.scopes.pop() {
            self.bindings.truncate(st);
        }
    }
    pub fn add_context(&mut self, ctx: &Context) {
        debug!("add context");
        self.scopes.push(self.bindings.len());
        self.bindings.extend(ctx.bindings.iter().cloned());
    }
    pub fn capture(&self) -> Self {
        debug!("capture");
        self.flatten_ref()
    }
    pub fn scope_id(&self) -> ScopeID {
        next_scope_id()
    }
    pub fn add_binding(&mut self, var: String, value: Expression) {
        debug!("add binding: {var} <- {value}");
        self.bindings.push((var, MemCell::new_ref(value.to_owned())));
    }
    pub fn add_binding_ref(&mut self, var: String, value: Rc<MemCell>) {
        debug!("add binding ref: {var} <- {}",value.get());
        self.bindings.push((var, value));
    }
    pub fn get_binding(&self, var: &str) -> Option<Expression> {
        debug!("get binding: {var}");
        self.bindings.iter().rev().find(|(k,_)| {
            k == var 
        }).map(|(_,v)| {
            v.get()
        })
    }
    pub fn get_binding_ref(&self, var: &str) -> Option<Rc<MemCell>> {
        debug!("get binding ref: {var}");
        self.bindings.iter().rev().find(|(k,_)| {
            k == var 
        }).map(|(_,v)| {
            v.to_owned()
        })
    }
    pub fn set_binding(&mut self, var: &str, value: Expression) {
        debug!("set binding: {var} <- {value}");
        if let Some(mem) = self.get_binding_ref(var) {
            mem.set(value);
        }
    }
    pub fn set_binding_ref(&mut self, var: &str, value: Rc<MemCell>) {
        debug!("set binding ref: {var} <- {}",value.get());
        self.set_binding(var, value.get())
    }
    pub fn flatten_ref(&self) -> Self {
        debug!("flatten ref");
        Self::from(self.bindings_ref())
    }
    pub fn bindings_ref(&self) -> HashMap<String,Rc<MemCell>> {
        debug!("bindings ref");
        self.bindings.iter().cloned().collect()
    }
    pub fn flatten_clone(&self) -> Self {
        debug!("flatten clone");
        Self::from(self.bindings_cloned())
    }
    pub fn bindings_cloned(&self) -> HashMap<String,Rc<MemCell>> {
        debug!("bindings cloned");
        self.bindings_ref().into_iter().map(| (k, rc) | { (k, rc.duplicate_ref()) }).collect()
    }
    pub fn add_bindings(&mut self, bindings: HashMap<String,Rc<MemCell>>) {
        debug!("add bindings");
        self.add_context(&bindings.into());
    }
}

impl From<HashMap<String,Rc<MemCell>>> for Context {
    fn from(bindings: HashMap<String,Rc<MemCell>>) -> Self {
        Self {
            bindings: bindings.into_iter().collect(),
            ..Default::default()
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
    use crate::expression::integer;
    use super::Context;

    #[test]
    fn ref_test() {
        let mut ctx1 = Context::new();
        ctx1.start_scope();
        ctx1.add_binding("v1".to_owned(), integer(1));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(1)));

        ctx1.start_scope();
        ctx1.add_binding("v1".to_owned(), integer(11));
        ctx1.add_binding("v2".to_owned(), integer(2));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx1.get_binding("v2"),Some(integer(2)));
        let ctx2 = ctx1.capture();
        ctx1.end_scope();

        assert_eq!(ctx1.get_binding("v1"),Some(integer(1)));
        assert_eq!(ctx1.get_binding("v2"),None);

        assert_eq!(ctx2.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx2.get_binding("v2"),Some(integer(2)));

        ctx1.add_context(&ctx2);

        assert_eq!(ctx1.get_binding("v1"),Some(integer(11)));
        ctx1.set_binding("v1", integer(21));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(21)));
        assert_eq!(ctx1.get_binding("v2"),Some(integer(2)));

        let mut ctx3 = ctx1.flatten_clone();
        let mut ctx4 = ctx1.flatten_ref();
        ctx3.set_binding("v1", integer(31));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(21)));
        ctx4.set_binding("v1", integer(41));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(41)));

        ctx1.end_scope();

        assert_eq!(ctx1.get_binding("v1"),Some(integer(1)));
        assert_eq!(ctx1.get_binding("v2"),None);

        ctx1.add_binding("v3".to_owned(), integer(3));
        ctx1.start_scope();
        ctx1.start_scope();
        ctx1.start_scope();
        ctx1.set_binding("v3", integer(13));
        ctx1.end_scope();
        ctx1.end_scope();
        ctx1.end_scope();
        assert_eq!(ctx1.get_binding("v3"),Some(integer(13)));

        ctx1.end_scope();

        // println!("{:#?}",ctx4);
        // panic!("*** to force output ***");
    }
}
