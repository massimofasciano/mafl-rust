use std::collections::HashSet;

use crate::{expression::{Expr, R, self, BlockType, Ident}, context::Context, Interpreter};
use anyhow::{Result,anyhow};

impl Interpreter {

    #[allow(clippy::only_used_in_recursion)]
    pub fn open(&self, ctx: &Context, ast: &R<Expr>) -> Result<HashSet<Ident>> {
        Ok(match ast.as_ref() {
            Expr::Variable(s) => {
                let mut open = HashSet::new();
                if ctx.get_binding(s).is_none() {
                    open.insert(s.to_owned());
                }
                open
            }

            Expr::Dyn(_,_) |
            Expr::BuiltinVariable(_) |
            Expr::Continue |
            Expr::Nil |
            Expr::Integer(_) |
            Expr::Float(_) |
            Expr::Character(_) |
            Expr::Boolean(_) |
            Expr::String(_)
                => HashSet::new(),

            Expr::Field(expr, _) |
            Expr::Loop(expr) |
            Expr::UnaryOpCall(_, expr) |
            Expr::Exit(expr) |
            Expr::Return(expr) |
            Expr::Throw(expr) |
            Expr::Break(expr) =>
                self.open(ctx, expr)?,

            Expr::Array(rc_exprs) => {
                let rc = rc_exprs.borrow();
                let exprs = rc.iter();
                let mut open = HashSet::new();
                for expr in exprs {
                    open.extend(self.open(ctx, expr)?);
                }
                open
            }

            Expr::FunctionCall(expr, exprs) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, expr)?);
                for expr in exprs {
                    open.extend(self.open(ctx, expr)?);
                }
                open
            }

            Expr::Test(_,first,second) |
            Expr::AssignToDeRefExpression(first, second) |
            Expr::OpAssignToExpression(_, first, second) |
            Expr::AssignToExpression(first, second) |
            Expr::ArrayAccess(first, second) |
            Expr::BinOpCall(_, first, second) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, first)?);
                open.extend(self.open(ctx, second)?);
                open
            }

            Expr::If(first, second, third) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, first)?);
                open.extend(self.open(ctx, second)?);
                open.extend(self.open(ctx, third)?);
                open
            }

            Expr::Block{r#type: block_type, body: exprs} => {
                let mut open = HashSet::new();
                let block_ctx = match block_type {
                    BlockType::Sequence => ctx.to_owned(),
                    BlockType::Block | BlockType::If | BlockType::Function =>
                        ctx.capture(),
                };
                for expr in exprs {
                    open.extend(self.open(&block_ctx,expr)?);
                }
                // remove from open vars if it now has a binding...
                for open_var in open.clone() {
                    if block_ctx.get_binding(&open_var).is_some() {
                        open.remove(&open_var);
                    }
                }
                open
            }

            Expr::LetRef(id, val) |
            Expr::Let(id, val) => {
                let open = self.open(ctx,val)?;
                ctx.add_binding(id.to_owned(), expression::nil());
                open
            }

            Expr::Forget(id) => {
                ctx.remove_binding(id);
                HashSet::new()
            }

            Expr::LetArray(ids, val) => {
                let open = self.open(ctx,val)?;
                for id in ids {
                    ctx.add_binding(id.to_owned(), expression::nil());
                }
                open
            }

            Expr::TryCatch(iter, closed_var, body) |
            Expr::Iterate(closed_var,iter, body) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, iter)?);
                let new_ctx = ctx.capture();
                new_ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                open.extend(self.open(&new_ctx, body)?);
                open
            }

            
            Expr::Closed(vars, _) => {
                let mut open = HashSet::new();
                open.extend(vars.to_owned());
                open
            }

            Expr::Fun(closed,expr) => {
                let ctx = Context::new();
                for closed_var in closed {
                    ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                }
                let mut open = self.open(&ctx, expr)?;
                // remove the function arguments from open vars...
                for closed_var in closed {
                    open.remove(closed_var);
                }
                open                
            }

            Expr::Use(opt_source,members) => {
                let mut open = HashSet::new();
                if let Some(source) = opt_source {
                    open.extend(self.open(ctx, source)?);
                    for var in members {
                        ctx.add_binding(var.to_owned(), expression::nil()); 
                    }
                } else {
                    for var in members {
                        open.insert(var.to_owned()); 
                    }
                }
                open
            }

            _ => Err(anyhow!("unhandled case for open vars: {:?}",ast))?
        })
    }


}
