use std::collections::HashSet;

use crate::{expression::{ExpressionType, Expression, self, BlockType, Ident}, context::Context, Interpreter};
use anyhow::{Result,anyhow};

impl Interpreter {

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

            ExpressionType::FunctionCall(_, exprs) => {
                let mut open = HashSet::new();
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
                        ctx.with_new_context(),
                };
                for expr in exprs {
                    open.extend(self.open(&block_ctx,expr)?);
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
                let new_ctx = Context::new();
                ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                open.extend(self.open(&new_ctx, body)?);
                open
            }

            ExpressionType::Context(closed, expr) |
            ExpressionType::Fun(closed,_,_,_,expr) => {
                let ctx = Context::new();
                for closed_var in closed {
                    ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                }
                self.open(&ctx, expr)?
            }

            ExpressionType::Object(expr) => {
                let ctx = Context::new();
                self.open(&ctx, expr)?
            }

            ExpressionType::Module(closed_var, closed, expr) => {
                let local_ctx = Context::new();
                local_ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                for closed_var in closed {
                    local_ctx.add_binding(closed_var.to_owned(), expression::nil()); 
                }
                self.open(&local_ctx, expr)?
            }

            _ => {
                // println!("*** unhandled case for open vars, assuming empty");
                // HashSet::new()
                Err(anyhow!("unhandled case for open vars: {:?}",ast))?
            }
        })
    }


}
