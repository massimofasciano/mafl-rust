use std::collections::HashSet;

use crate::{expression::{ExpressionType, Expression, self, BlockType, Ident}, context::Context, Interpreter};
use anyhow::{Result,anyhow};

impl Interpreter {

    #[allow(clippy::only_used_in_recursion)]
    pub fn open(&self, ctx: &Context, ast: &Expression) -> Result<HashSet<Ident>> {
        Ok(match ast.as_ref() {
            ExpressionType::Variable(s) => {
                let mut open = HashSet::new();
                if ctx.get_binding(s).is_none() {
                    open.insert(s.to_owned());
                }
                open
            }

            ExpressionType::BuiltinVariable(_) |
            ExpressionType::Continue |
            ExpressionType::Nil |
            ExpressionType::Integer(_) |
            ExpressionType::Float(_) |
            ExpressionType::Character(_) |
            ExpressionType::Boolean(_) |
            ExpressionType::String(_)
                => HashSet::new(),

            ExpressionType::Field(expr, _) |
            ExpressionType::Loop(expr) |
            ExpressionType::UnaryOpCall(_, expr) |
            ExpressionType::EndBlock(expr) |
            ExpressionType::Return(expr) |
            ExpressionType::Throw(expr) |
            ExpressionType::Break(expr) =>
                self.open(ctx, expr)?,

            ExpressionType::Array(rc_exprs) => {
                let rc = rc_exprs.borrow();
                let exprs = rc.iter();
                let mut open = HashSet::new();
                for expr in exprs {
                    open.extend(self.open(ctx, expr)?);
                }
                open
            }

            ExpressionType::FunctionCall(expr, exprs) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, expr)?);
                for expr in exprs {
                    open.extend(self.open(ctx, expr)?);
                }
                open
            }

            ExpressionType::AssignToDeRefExpression(first, second) |
            ExpressionType::OpAssignToExpression(_, first, second) |
            ExpressionType::AssignToExpression(first, second) |
            ExpressionType::ArrayAccess(first, second) |
            ExpressionType::BinOpCall(_, first, second) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, first)?);
                open.extend(self.open(ctx, second)?);
                open
            }

            ExpressionType::If(first, second, third) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, first)?);
                open.extend(self.open(ctx, second)?);
                open.extend(self.open(ctx, third)?);
                open
            }

            ExpressionType::Block{r#type: block_type, body: exprs} => {
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

            ExpressionType::Let(id, val) => {
                let open = self.open(ctx,val)?;
                ctx.add_binding(id.to_owned(), expression::nil());
                open
            }
            ExpressionType::LetArray(ids, val) => {
                let open = self.open(ctx,val)?;
                for id in ids {
                    ctx.add_binding(id.to_owned(), expression::nil());
                }
                open
            }

            ExpressionType::TryCatch(iter, closed_var, body) |
            ExpressionType::Iterate(closed_var,iter, body) => {
                let mut open = HashSet::new();
                open.extend(self.open(ctx, iter)?);
                let new_ctx = ctx.capture();
                new_ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                open.extend(self.open(&new_ctx, body)?);
                open
            }

            ExpressionType::Fun(closed,expr) => {
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

            ExpressionType::Context(closed, expr) => {
                let ctx = Context::new();
                for closed_var in closed {
                    ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                }
                let mut open = self.open(&ctx, expr)?;
                // remove the context arguments from open vars...
                for closed_var in closed {
                    open.remove(closed_var);
                }
                open                
            }

            ExpressionType::Object(expr) => {
                let ctx = Context::new();
                self.open(&ctx, expr)?                
            }

            ExpressionType::Module(closed_var, closed, expr) => {
                let ctx = Context::new();
                ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                for closed_var in closed {
                    ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                }
                let mut open = self.open(&ctx, expr)?;
                // remove the module name and arguments from open vars...
                open.remove(closed_var);
                for closed_var in closed {
                    open.remove(closed_var);
                }
                open                
            }

            ExpressionType::Proto(closed, opt_expr, _) => {
                let mut open = if let Some(expr) = opt_expr {
                    self.open(ctx, expr)?
                } else {
                    HashSet::new()
                };
                open.extend(closed.iter().cloned());
                open                
            }

            _ => Err(anyhow!("unhandled case for open vars: {:?}",ast))?
        })
    }


}
