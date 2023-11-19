use std::collections::HashMap;
use log::debug;
use crate::{R, RefC, Ident, expression::Expr, CellRefMut};
use std::sync::atomic::{AtomicUsize, Ordering};

#[cfg(feature = "gc")]
use gc::{Finalize, Trace};

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
#[cfg_attr(feature = "gc", derive(Trace))]
pub struct MemCell {
    inner: RefC<R<Expr>>,
    id : MemID,
}

impl MemCell {
    pub fn new(e : R<Expr>) -> Self {
        let new = Self {
            inner: RefC::new(e),
            id : next_mem_id(),
        };
        debug!("new cell id={}",new.id); 
        new
    }
    pub fn new_ref(e : R<Expr>) -> R<Self> {
        R::new(Self::new(e))
    }
    pub fn get(&self) -> R<Expr> {
        self.inner.borrow().to_owned()
    }
    pub fn get_refmut(&self) -> CellRefMut<R<Expr>> {
            self.inner.borrow_mut()
    }
    pub fn set(&self, e: R<Expr>) -> R<Expr> {
        let old = self.get();
        *self.inner.borrow_mut() = e;
        old
    }
    fn duplicate(&self) -> Self {
        debug!("duplicating cell id={}", self.id);
        Self::new(self.get())
    }
    fn duplicate_ref(&self) -> R<Self> {
        R::new(self.duplicate())
    }
}

impl Clone for MemCell {
    fn clone(&self) -> Self {
        self.duplicate()
    }
}

#[cfg(not(feature = "gc"))]
impl Drop for MemCell {
    fn drop(&mut self) {
        debug!("dropping cell id={}", self.id);
    }
}
#[cfg(feature = "gc")]
impl Finalize for MemCell {
    fn finalize(&self) {
        debug!("dropping cell id={}", self.id);
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "gc", derive(Trace, Finalize))]
pub struct Context {
    inner: R<Scope>,
}

pub type Bindings = HashMap<Ident,R<MemCell>>;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "gc", derive(Trace))]
struct Scope {
    bindings: RefC<Bindings>,
    parent: RefC<Option<Context>>,
    id: ScopeID,
}

impl Context {
    pub fn new() -> Self {
        Self{inner:R::new(Scope::new())}
    }
    pub fn with_new_context(&self) -> Self {
        let scope = Scope::new();
        *scope.parent.borrow_mut() = Some(self.to_owned());
        Self { inner: scope.into() }
    }
    pub fn with_context(&self, ctx: Context) -> Self {
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = ctx.bindings_ref();
        *scope.parent.borrow_mut() = Some(self.to_owned());
        Self { inner: scope.into() }
    }
    pub fn capture(&self) -> Self {
        self.flatten_ref()
    }
    pub fn scope_id(&self) -> ScopeID {
        self.inner.id
    }
    pub fn append(&self, ctx: &Self) {
        let mut end = self.to_owned();
        loop {
            let parent = end.parent();
            if let Some(parent) = parent {
                end = parent;
            } else {
                break;
            }
        }
        *end.inner.parent.borrow_mut() = Some(ctx.to_owned());
    }
    fn parent(&self) -> Option<Self> {
        let scope = &self.inner;
        let parent = scope.parent.borrow();
        if parent.is_some() {
            Some(parent.as_ref().unwrap().to_owned())
        } else {
            None
        }
    }
    pub fn remove_binding(&self, var: &Ident) -> Option<R<Expr>> {
        let scope = &self.inner;
        scope.bindings.borrow_mut().remove(var).map(|old| old.get())
    }
    pub fn add_binding(&self, var: Ident, value: R<Expr>) -> Option<R<Expr>> {
        let scope = &self.inner;
        scope.bindings.borrow_mut().insert(var, MemCell::new_ref(value)).map(|old| old.get())
    }
    pub fn add_binding_ref(&self, var: Ident, value: R<MemCell>) -> Option<R<MemCell>> {
        let scope = &self.inner;
        scope.bindings.borrow_mut().insert(var, value)
    }
    pub fn get_binding(&self, var: &Ident) -> Option<R<Expr>> {
        let scope = &self.inner;
        if let Some(rc) = scope.bindings.borrow().get(var) {
            Some(rc.get())
        } else if let Some(parent) = scope.parent.borrow().as_ref() {
            parent.get_binding(var)
        } else {
            None
        }
    }
    pub fn get_binding_ref(&self, var: &Ident) -> Option<R<MemCell>> {
        let scope = &self.inner;
        if let Some(rc) = scope.bindings.borrow().get(var) {
            Some(rc.to_owned())
        } else if let Some(parent) = scope.parent.borrow().as_ref() {
            parent.get_binding_ref(var)
        } else {
            None
        }
    }
    pub fn set_binding(&self, var: Ident, value: R<Expr>) -> Option<R<Expr>> {
        let scope = &self.inner;
        if let Some(rc) = scope.bindings.borrow().get(&var) {
            Some(rc.set(value))
        } else if scope.parent.borrow().is_some() {
            scope.parent.borrow().as_ref().unwrap().set_binding(var,value)
        } else {
            None
        }
    }
    pub fn set_binding_ref(&self, var: Ident, value: R<MemCell>) -> Option<R<MemCell>> {
        let scope = &self.inner;
        if scope.bindings.borrow().contains_key(&var) {
            scope.bindings.borrow_mut().insert(var, value)
        } else if scope.parent.borrow().is_some() {
            scope.parent.borrow().as_ref().unwrap().set_binding_ref(var,value)
        } else {
            None
        }
    }
    pub fn flatten_ref(&self) -> Self {
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = self.bindings_ref();
        scope.into()
    }
    pub fn bindings_ref(&self) -> Bindings {
        let mut bindings = HashMap::new();
        let mut current = self.to_owned();
        loop {
            let kv = current.inner.bindings.borrow().to_owned();
            for (k,v) in kv {
                bindings.entry(k).or_insert(v);
            }
            let parent = current.parent();
            if let Some(parent) = parent {
                current = parent;
            } else {
                break;
            }
        }
        bindings
    }
    pub fn flatten_clone(&self) -> Self {
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = self.bindings_cloned();
        scope.into()
    }
    pub fn bindings_cloned(&self) -> Bindings {
        self.bindings_ref().into_iter().map(| (k, rc) | { (k, rc.duplicate_ref()) }).collect()
    }
    pub fn from_bindings(bindings: Bindings) -> Self {
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = bindings;
        scope.into()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self{ inner:Scope::new().into() }
    }
}

impl From<Scope> for Context {
    fn from(scope: Scope) -> Self {
        Self{ inner: R::new(scope) }
    }
}

impl From<R<Scope>> for Context {
    fn from(rc_scope: R<Scope>) -> Self {
        Self{ inner: rc_scope }
    }
}

impl Scope {
    pub fn new() -> Self {
        let scope = Self::default();
        debug!("new scope id={}",scope.id);
        scope
    }
}

impl Default for Scope {
    fn default() -> Self {
        let id = next_scope_id();
        Self {
            bindings: RefC::new(HashMap::new()),
            parent: RefC::new(None),
            id,
        }
    }
}

#[cfg(not(feature = "gc"))]
impl Drop for Scope {
    fn drop(&mut self) {
        debug!("dropping scope id={}", self.id);
    }
}
#[cfg(feature = "gc")]
impl Finalize for Scope {
    fn finalize(&self) {
        debug!("dropping scope id={}", self.id);
    }
}

#[cfg(test)]
mod tests {
    use crate::{expression::integer, Interpreter};
    use super::Context;
    use anyhow::{Result, Ok};

    #[test]
    fn ref_test() -> Result<()> {
        // don't need to call new because we don't use the full interpreter with stdlib
        // we just need the ident feature.
        let interpreter = Interpreter::default();
        let v1 = interpreter.ident("v1");
        let v2 = interpreter.ident("v2");
        let v3 = interpreter.ident("v3");
        let v4 = interpreter.ident("v4");
        let v10 = interpreter.ident("v10");
        let v11 = interpreter.ident("v11");
        let v12 = interpreter.ident("v12");
        let ctx1 = Context::new();
        ctx1.add_binding(v1.to_owned(), integer(1));
        let ctx2 = ctx1.with_new_context();
        ctx2.add_binding(v2.to_owned(), integer(2));
        assert_eq!(ctx2.get_binding(&v1),Some(integer(1)));
        assert_eq!(ctx2.get_binding(&v2),Some(integer(2)));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(1)));
        assert_eq!(ctx1.get_binding(&v2),None);
        ctx2.set_binding(v1.to_owned(),integer(11));
        ctx2.set_binding(v2.to_owned(),integer(12));
        assert_eq!(ctx2.get_binding(&v1),Some(integer(11)));
        assert_eq!(ctx2.get_binding(&v2),Some(integer(12)));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(11)));
        assert_eq!(ctx1.get_binding(&v2),None);

        let ctx3 = Context::new();
        ctx3.add_binding(v3.to_owned(), integer(3));
        assert_eq!(ctx3.get_binding(&v3),Some(integer(3)));

        let ctx4 = ctx3.with_new_context();
        ctx4.add_binding(v4.to_owned(), integer(4));
        assert_eq!(ctx4.get_binding(&v4),Some(integer(4)));
        ctx4.append(&ctx1);

        assert_eq!(ctx4.get_binding(&v4),Some(integer(4)));
        assert_eq!(ctx4.get_binding(&v3),Some(integer(3)));
        assert_eq!(ctx4.get_binding(&v1),Some(integer(11)));
        assert_eq!(ctx4.get_binding(&v2),None);
        assert_eq!(ctx2.get_binding(&v1),Some(integer(11)));
        assert_eq!(ctx2.get_binding(&v2),Some(integer(12)));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(11)));
        assert_eq!(ctx1.get_binding(&v2),None);

        ctx1.add_binding(v10.to_owned(), integer(10));
        assert_eq!(ctx1.get_binding(&v10),Some(integer(10)));
        assert_eq!(ctx2.get_binding(&v10),Some(integer(10)));
        assert_eq!(ctx3.get_binding(&v10),Some(integer(10)));
        assert_eq!(ctx4.get_binding(&v10),Some(integer(10)));

        ctx4.set_binding(v2.to_owned(), integer(22));
        assert_eq!(ctx1.get_binding(&v2),None);
        assert_eq!(ctx2.get_binding(&v2),Some(integer(12)));
        assert_eq!(ctx3.get_binding(&v2),None);
        assert_eq!(ctx4.get_binding(&v2),None);

        let ctx1_cap = ctx1.capture();
        ctx1_cap.set_binding(v10.to_owned(), integer(110));
        ctx1.add_binding(v11.to_owned(), integer(111));
        ctx1_cap.add_binding(v12.to_owned(), integer(112));
        assert_eq!(ctx1_cap.get_binding(&v10),Some(integer(110)));
        assert_eq!(ctx1_cap.get_binding(&v11),None);
        assert_eq!(ctx1_cap.get_binding(&v12),Some(integer(112)));
        assert_eq!(ctx1.get_binding(&v10),Some(integer(110)));
        assert_eq!(ctx1.get_binding(&v11),Some(integer(111)));
        assert_eq!(ctx1.get_binding(&v12),None);

        let ctx4m = ctx4.flatten_clone();
        assert_eq!(ctx1.get_binding(&v1),Some(integer(11)));
        assert_eq!(ctx4m.get_binding(&v1),Some(integer(11)));
        ctx4.set_binding(v1.to_owned(), integer(411));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(411)));
        assert_eq!(ctx4m.get_binding(&v1),Some(integer(11)));
        ctx4m.set_binding(v1.to_owned(), integer(1411));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(411)));
        assert_eq!(ctx4m.get_binding(&v1),Some(integer(1411)));

        let ctx4r = ctx4.flatten_ref();
        assert_eq!(ctx1.get_binding(&v1),Some(integer(411)));
        assert_eq!(ctx4r.get_binding(&v1),Some(integer(411)));
        ctx4.set_binding(v1.to_owned(), integer(2411));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(2411)));
        assert_eq!(ctx4r.get_binding(&v1),Some(integer(2411)));
        ctx4r.set_binding(v1.to_owned(), integer(3411));
        assert_eq!(ctx1.get_binding(&v1),Some(integer(3411)));
        assert_eq!(ctx4r.get_binding(&v1),Some(integer(3411)));
        Ok(())
    }
}
