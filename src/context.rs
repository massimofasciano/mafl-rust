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

#[repr(transparent)]
#[derive(Debug,Clone)]
pub struct Context {
    inner: Rc<Scope>,
}

#[derive(Debug,Clone)]
struct Scope {
    bindings: RefCell<HashMap<String,Rc<MemCell>>>,
    parent: RefCell<Option<Context>>,
    id: ScopeID,
}

impl Context {
    pub fn new() -> Self {
        debug!("new");
        Self{inner:Rc::new(Scope::new())}
    }
    pub fn with_new_context(&self) -> Self {
        debug!("with new scope");
        let scope = Scope::new();
        *scope.parent.borrow_mut() = Some(self.to_owned());
        Self { inner: scope.into() }
    }
    pub fn with_context(&self, ctx: Context) -> Self {
        debug!("with context");
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = ctx.bindings_ref();
        *scope.parent.borrow_mut() = Some(self.to_owned());
        Self { inner: scope.into() }
    }
    pub fn capture(&self) -> Self {
        debug!("capture");
        self.flatten_ref()
    }
    // pub fn capture(&self) -> Self {
    //     debug!("capture");
    //     let captured = Context::new();
    //     {   // we swap old and new in a block to contain the mut borrows
    //         let new_bindings = &mut *captured.inner.bindings.borrow_mut();
    //         let new_parent = &mut *captured.inner.parent.borrow_mut();
    //         let old_bindings = &mut *self.inner.bindings.borrow_mut();
    //         let old_parent = &mut *self.inner.parent.borrow_mut();
    //         swap(old_bindings,new_bindings);
    //         swap(old_parent,new_parent);
    //         *old_parent = Some(captured.to_owned());
    //     }
    //     captured
    // }
    pub fn append(&self, ctx: &Self) {
        debug!("append");
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
    pub fn add_binding(&self, var: String, value: Expression) -> Option<Expression> {
        debug!("add binding: {var} <- {value}");
        let scope = &self.inner;
        scope.bindings.borrow_mut().insert(var, MemCell::new_ref(value)).map(|old| old.get())
    }
    pub fn add_binding_ref(&self, var: String, value: Rc<MemCell>) -> Option<Rc<MemCell>> {
        debug!("add binding ref: {var} <- {}",value.get());
        let scope = &self.inner;
        scope.bindings.borrow_mut().insert(var, value)
    }
    pub fn get_binding(&self, var: &str) -> Option<Expression> {
        debug!("get binding: {var}");
        let scope = &self.inner;
        if let Some(rc) = scope.bindings.borrow().get(var) {
            Some(rc.get())
        } else if let Some(parent) = scope.parent.borrow().as_ref() {
            parent.get_binding(var)
        } else {
            None
        }
    }
    pub fn get_binding_ref(&self, var: &str) -> Option<Rc<MemCell>> {
        debug!("get binding ref: {var}");
        let scope = &self.inner;
        if let Some(rc) = scope.bindings.borrow().get(var) {
            Some(rc.to_owned())
        } else if let Some(parent) = scope.parent.borrow().as_ref() {
            parent.get_binding_ref(var)
        } else {
            None
        }
    }
    pub fn set_binding(&self, var: String, value: Expression) -> Option<Expression> {
        debug!("set binding: {var} <- {value}");
        let scope = &self.inner;
        if let Some(rc) = scope.bindings.borrow().get(&var) {
            Some(rc.set(value))
        } else if scope.parent.borrow().is_some() {
            scope.parent.borrow().as_ref().unwrap().set_binding(var,value)
        } else {
            None
        }
    }
    pub fn set_binding_ref(&self, var: String, value: Rc<MemCell>) -> Option<Rc<MemCell>> {
        debug!("set binding ref: {var} <- {}",value.get());
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
        debug!("copy merge");
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = self.bindings_ref();
        scope.into()
    }
    pub fn bindings_ref(&self) -> HashMap<String,Rc<MemCell>> {
        debug!("bindings");
        let mut bindings = HashMap::new();
        let mut current = self.to_owned();
        loop {
            let kv = current.inner.bindings.borrow().to_owned();
            bindings.extend(kv);
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
        debug!("copy merge");
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = self.bindings_cloned();
        scope.into()
    }
    pub fn bindings_cloned(&self) -> HashMap<String,Rc<MemCell>> {
        debug!("bindings cloned");
        self.bindings_ref().into_iter().map(| (k, rc) | { (k, rc.duplicate_ref()) }).collect()
    }
    pub fn with_bindings(&self, bindings: HashMap<String,Rc<MemCell>>) -> Self {
        debug!("with bindings");
        let scope = Scope::new();
        *scope.bindings.borrow_mut() = bindings;
        scope.into()
    }
}

impl Default for Context {
    fn default() -> Self {
        debug!("default");
        Self{ inner:Scope::new().into() }
    }
}

impl From<Scope> for Context {
    fn from(scope: Scope) -> Self {
        debug!("from scope");
        Self{ inner:Rc::new(scope) }
    }
}

impl From<Rc<Scope>> for Context {
    fn from(rc_scope: Rc<Scope>) -> Self {
        debug!("from scope");
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
        debug!("default scope id={id}");
        Self {
            bindings: RefCell::new(HashMap::new()),
            parent: RefCell::new(None),
            id,
        }
    }
}

// impl Debug for Context {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.cycle_fmt(f, &mut HashSet::new())
//     }
// }

// impl Context {
//     fn cycle_fmt(&self, f: &mut std::fmt::Formatter<'_>, env: &mut HashSet<ScopeID>) -> std::fmt::Result {
//         let scope = self.inner.as_ref();
//         write!(f,"Context({}",scope.id)?;
//         if env.contains(&scope.id) {
//             write!(f," => ...]]")
//         } else {
//             env.insert(scope.id);
//             let bindings = scope.bindings.borrow();
//             write!(f," => {{")?;
//             let mut iter = bindings.iter().peekable();
//             while let Some((k,v)) = iter.next() {
//                 write!(f,"{k:?} = ")?;
//                 match v.as_ref() {
//                     ExpressionType::Closure(ctx, args, body) => {
//                         write!(f,"Closure(")?;
//                         ctx.cycle_fmt(f, env)?;
//                         write!(f,",{:#?}",args)?;
//                         write!(f,",{:#?}",body)?;
//                         write!(f,")")?;
//                     },
//                     _ => {
//                         write!(f,"{v:?}")?;
//                     },
//                 }
//                 if iter.peek().is_some() { write!(f,",")?; }
//             }
//             write!(f,"}}")?;
//             if let Some(parent) = scope.parent.borrow().as_ref() {
//                 write!(f,",")?; 
//                 parent.cycle_fmt(f,env)?;
//             }
//             write!(f,")")
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use crate::expression::integer;
    use super::Context;

    #[test]
    fn ref_test() {
        let ctx1 = Context::new();
        ctx1.add_binding("v1".to_owned(), integer(1));
        let ctx2 = ctx1.with_new_context();
        ctx2.add_binding("v2".to_owned(), integer(2));
        assert_eq!(ctx2.get_binding("v1"),Some(integer(1)));
        assert_eq!(ctx2.get_binding("v2"),Some(integer(2)));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(1)));
        assert_eq!(ctx1.get_binding("v2"),None);
        ctx2.set_binding("v1".to_owned(),integer(11));
        ctx2.set_binding("v2".to_owned(),integer(12));
        assert_eq!(ctx2.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx2.get_binding("v2"),Some(integer(12)));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx1.get_binding("v2"),None);

        let ctx3 = Context::new();
        ctx3.add_binding("v3".to_owned(), integer(3));
        assert_eq!(ctx3.get_binding("v3"),Some(integer(3)));

        let ctx4 = ctx3.with_new_context();
        ctx4.add_binding("v4".to_owned(), integer(4));
        assert_eq!(ctx4.get_binding("v4"),Some(integer(4)));
        ctx4.append(&ctx1);

        assert_eq!(ctx4.get_binding("v4"),Some(integer(4)));
        assert_eq!(ctx4.get_binding("v3"),Some(integer(3)));
        assert_eq!(ctx4.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx4.get_binding("v2"),None);
        assert_eq!(ctx2.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx2.get_binding("v2"),Some(integer(12)));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx1.get_binding("v2"),None);

        ctx1.add_binding("v10".to_owned(), integer(10));
        assert_eq!(ctx1.get_binding("v10"),Some(integer(10)));
        assert_eq!(ctx2.get_binding("v10"),Some(integer(10)));
        assert_eq!(ctx3.get_binding("v10"),Some(integer(10)));
        assert_eq!(ctx4.get_binding("v10"),Some(integer(10)));

        ctx4.set_binding("v2".to_owned(), integer(22));
        assert_eq!(ctx1.get_binding("v2"),None);
        assert_eq!(ctx2.get_binding("v2"),Some(integer(12)));
        assert_eq!(ctx3.get_binding("v2"),None);
        assert_eq!(ctx4.get_binding("v2"),None);

        let ctx1_cap = ctx1.capture();
        ctx1_cap.set_binding("v10".to_owned(), integer(110));
        ctx1.add_binding("v11".to_owned(), integer(111));
        ctx1_cap.add_binding("v12".to_owned(), integer(112));
        assert_eq!(ctx1_cap.get_binding("v10"),Some(integer(110)));
        assert_eq!(ctx1_cap.get_binding("v11"),None);
        assert_eq!(ctx1_cap.get_binding("v12"),Some(integer(112)));
        assert_eq!(ctx1.get_binding("v10"),Some(integer(110)));
        assert_eq!(ctx1.get_binding("v11"),Some(integer(111)));
        // old capture behavior before Memcells
        // assert_eq!(ctx1.get_binding("v12"),Some(integer(112))); 
        // new capture behavior with Memcells+flatten_ref
        assert_eq!(ctx1.get_binding("v12"),None);

        let ctx4m = ctx4.flatten_clone();
        assert_eq!(ctx1.get_binding("v1"),Some(integer(11)));
        assert_eq!(ctx4m.get_binding("v1"),Some(integer(11)));
        ctx4.set_binding("v1".to_owned(), integer(411));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(411)));
        assert_eq!(ctx4m.get_binding("v1"),Some(integer(11)));
        ctx4m.set_binding("v1".to_owned(), integer(1411));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(411)));
        assert_eq!(ctx4m.get_binding("v1"),Some(integer(1411)));

        let ctx4r = ctx4.flatten_ref();
        assert_eq!(ctx1.get_binding("v1"),Some(integer(411)));
        assert_eq!(ctx4r.get_binding("v1"),Some(integer(411)));
        ctx4.set_binding("v1".to_owned(), integer(2411));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(2411)));
        assert_eq!(ctx4r.get_binding("v1"),Some(integer(2411)));
        ctx4r.set_binding("v1".to_owned(), integer(3411));
        assert_eq!(ctx1.get_binding("v1"),Some(integer(3411)));
        assert_eq!(ctx4r.get_binding("v1"),Some(integer(3411)));
    }
}
