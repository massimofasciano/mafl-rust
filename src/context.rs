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
