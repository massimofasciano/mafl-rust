use std::{collections::HashMap, cell::RefCell, rc::Rc};

use log::debug;

use crate::expression::Expression;

type ContextInner = Rc<Scope>;
type Bindings = HashMap<String,Expression>;

#[repr(transparent)]
#[derive(Debug,Clone,PartialEq)]
pub struct Context {
    inner: ContextInner,
}

#[derive(Debug,Clone,PartialEq)]
struct Scope {
    bindings: RefCell<Bindings>,
    parent: RefCell<Option<Context>>,
}

impl Context {
    pub fn new() -> Self {
        debug!("new");
        Self{inner:Rc::new(Scope::new())}
    }
    pub fn with_new_scope(&self) -> Self {
        debug!("new scope");
        let scope = Scope::new();
        *scope.parent.borrow_mut() = Some(self.to_owned());
        Self{inner:Rc::new(scope)}
    }
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
        debug!("add binding: {var} <- {value:?}");
        let scope = &self.inner;
        scope.bindings.borrow_mut().insert(var, value)
    }
    pub fn get_binding(&self, var: &str) -> Option<Expression> {
        debug!("get binding: {var}");
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
        debug!("set binding: {var} <- {value:?}");
        let scope = &self.inner;
        if scope.bindings.borrow().contains_key(&var) {
            scope.bindings.borrow_mut().insert(var, value)
        } else if scope.parent.borrow().is_some() {
            scope.parent.borrow().as_ref().unwrap().set_binding(var,value)
        } else {
            None
        }
    }
    // pub fn get_binding_context(&self, var: &str) -> Option<ContextRef> {
    //     let scope = &self.inner;
    //     if scope.bindings.borrow().contains_key(var) {
    //         Some(self.to_owned())
    //     } else if let Some(parent) = scope.parent.borrow().as_ref() {
    //         parent.get_binding_context(var)
    //     } else {
    //         None
    //     }
    // }
}

impl Default for Context {
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
    use super::{Expression,Context};

    fn test_val(i: i64) -> Expression {
        Expression::Integer(i)
    }
    
    #[test]
    fn ref_test() {
        let ctx1 = Context::new();
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

        let ctx3 = Context::new();
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
    }
}
