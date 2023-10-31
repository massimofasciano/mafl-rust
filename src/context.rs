use std::{collections::HashMap, cell::{RefCell, RefMut}, rc::Rc};

use crate::expression::Expression;

// we use a stack of (String,Expression) pairs + scope markers instead of a stack of HashMap<String,Expression>

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
    pub fn value_mut(&mut self, search_var: &str) -> Option<&mut Expression> {
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
    pub fn get_mut_binding(&mut self, var: &str) -> Option<&mut Expression> {
        let st = &mut self.stack;
        let mut index = None;
        for idx in (0..st.len()).rev() {
            if st[idx].value(var).is_some() {
                index = Some(idx);
                break;
            }
        }
        index.map(|i|st[i].value_mut(var).unwrap())
    }
    
    pub fn add_context(&mut self, ctx: &Context) {
        for item in ctx.stack.iter() {
            match item {
                ContextItem::NewScope => {},
                ContextItem::Binding(var, value) => self.add_binding(var.to_owned(), value.to_owned()),
            }
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

type ContextInner = Rc<Scope>;

#[repr(transparent)]
#[derive(Debug,Clone,PartialEq)]
pub struct ContextRef {
    inner: ContextInner,
}

#[derive(Debug,Clone,PartialEq)]
pub struct Scope {
    bindings: RefCell<HashMap<String,Expression>>,
    parent: RefCell<Option<ContextRef>>,
}

impl ContextRef {
    pub fn new() -> Self {
        Self{inner:Rc::new(Scope::new())}
    }
    pub fn with_new_scope(&self) -> Self {
        let scope = Scope::new();
        *scope.parent.borrow_mut() = Some(self.to_owned());
        Self{inner:Rc::new(scope)}
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
    pub fn parent(&self) -> Option<Self> {
        let scope = &self.inner;
        let parent = scope.parent.borrow();
        if parent.is_some() {
            Some(parent.as_ref().unwrap().to_owned())
        } else {
            None
        }
    }
    pub fn add_binding(&self, var: String, value: Expression) -> Option<Expression> {
        let scope = &self.inner;
        scope.bindings.borrow_mut().insert(var, value)
    }
    pub fn get_binding(&self, var: &str) -> Option<Expression> {
        let scope = &self.inner;
        if let Some(value) = scope.bindings.borrow().get(var) {
            Some(value.to_owned())
        } else if let Some(parent) = scope.parent.borrow().as_ref() {
            parent.get_binding(var)
        } else {
            None
        }
    }
    pub fn set_binding(&self, var: String, value: Expression) -> Option<Expression> {
        let scope = &self.inner;
        if scope.bindings.borrow().contains_key(&var) {
            scope.bindings.borrow_mut().insert(var, value)
        } else if scope.parent.borrow().is_some() {
            scope.parent.borrow().as_ref().unwrap().set_binding(var,value)
        } else {
            None
        }
    }
}

impl Default for ContextRef {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            bindings: RefCell::new(HashMap::new()),
            parent: RefCell::new(None),
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression,Context, ContextRef};

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

    #[test]
    fn ref_test() {
        let ctx1 = ContextRef::new();
        ctx1.add_binding("v1".to_owned(), test_val(1));
        let ctx2 = ctx1.with_new_scope();
        ctx2.add_binding("v2".to_owned(), test_val(2));
        assert_eq!(ctx2.get_binding("v1"),Some(test_val(1)));
        assert_eq!(ctx2.get_binding("v2"),Some(test_val(2)));
        assert_eq!(ctx1.get_binding("v1"),Some(test_val(1)));
        assert_eq!(ctx1.get_binding("v2"),None);
        ctx2.set_binding("v1".to_owned(),test_val(11));
        ctx2.set_binding("v2".to_owned(),test_val(12));
        assert_eq!(ctx2.get_binding("v1"),Some(test_val(11)));
        assert_eq!(ctx2.get_binding("v2"),Some(test_val(12)));
        assert_eq!(ctx1.get_binding("v1"),Some(test_val(11)));
        assert_eq!(ctx1.get_binding("v2"),None);

        let ctx3 = ContextRef::new();
        ctx3.add_binding("v3".to_owned(), test_val(3));
        assert_eq!(ctx3.get_binding("v3"),Some(test_val(3)));
        let ctx4 = ctx3.with_new_scope();
        ctx4.add_binding("v4".to_owned(), test_val(4));
        assert_eq!(ctx4.get_binding("v4"),Some(test_val(4)));
        ctx4.append(&ctx1);

        assert_eq!(ctx4.get_binding("v4"),Some(test_val(4)));
        assert_eq!(ctx4.get_binding("v3"),Some(test_val(3)));
        assert_eq!(ctx4.get_binding("v1"),Some(test_val(11)));
        assert_eq!(ctx4.get_binding("v2"),None);
        assert_eq!(ctx2.get_binding("v1"),Some(test_val(11)));
        assert_eq!(ctx2.get_binding("v2"),Some(test_val(12)));
        assert_eq!(ctx1.get_binding("v1"),Some(test_val(11)));
        assert_eq!(ctx1.get_binding("v2"),None);

        ctx1.add_binding("v10".to_owned(), test_val(10));
        assert_eq!(ctx1.get_binding("v10"),Some(test_val(10)));
        assert_eq!(ctx2.get_binding("v10"),Some(test_val(10)));
        assert_eq!(ctx3.get_binding("v10"),Some(test_val(10)));
        assert_eq!(ctx4.get_binding("v10"),Some(test_val(10)));

        ctx4.set_binding("v2".to_owned(), test_val(22));
        assert_eq!(ctx1.get_binding("v2"),None);
        assert_eq!(ctx2.get_binding("v2"),Some(test_val(12)));
        assert_eq!(ctx3.get_binding("v2"),None);
        assert_eq!(ctx4.get_binding("v2"),None);

        // assert_eq!(true,false);
    }
}
