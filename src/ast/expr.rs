use crate::ast::context::{NamesContext, TypeContext};
use crate::ast::error::TypingError;
use crate::ast::pattern::Pattern;
use crate::ast::unnamed_expr::UnnamedExpr;
use crate::ast::Type;
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Expr {
    ConstTrue,
    ConstFalse,
    If {
        cond: Arc<Expr>,
        iftrue: Arc<Expr>,
        iffalse: Arc<Expr>,
    },

    ConstZero,
    Succ(Arc<Expr>),
    Pred(Arc<Expr>),
    IsZero(Arc<Expr>),
    Var(Arc<str>),
    Abstraction {
        bound_var: Arc<str>,
        type_: Arc<Type>,
        body: Arc<Expr>,
    },
    Application {
        function: Arc<Expr>,
        argument: Arc<Expr>,
    },
    Record(BTreeMap<Arc<str>, Arc<Expr>>),
    Let {
        clauses: Vec<(Arc<Pattern>, Arc<Expr>)>,
        substituted_expr: Arc<Expr>,
    },
    Variant {
        name: Arc<str>,
        body: Arc<Expr>,
        ty: Arc<Type>,
    },
    Nil {
        ty: Arc<Type>,
    },
    Cons {
        left: Arc<Expr>,
        right: Arc<Expr>,
    },
}

impl Expr {
    pub fn arc(self) -> Arc<Self> {
        Arc::new(self)
    }

    pub fn number(val: u32) -> Arc<Expr> {
        if val == 0 {
            Expr::ConstZero.arc()
        } else {
            Expr::Succ(Expr::number(val - 1)).arc()
        }
    }

    pub fn type_of_ctx(&self, context: &mut TypeContext) -> Result<Arc<Type>, TypingError> {
        use std::ops::Deref;
        use Expr::*;
        match self {
            ConstTrue => Ok(Type::Boolean.arc()),
            ConstFalse => Ok(Type::Boolean.arc()),
            If {
                cond,
                iftrue,
                iffalse,
            } => {
                let cond_ty = cond.type_of_ctx(context)?;
                cond_ty
                    .expect(cond, &Type::Boolean.arc())
                    .map_err(TypingError::IfCond)?;
                let iftrue_ty = iftrue.type_of_ctx(context)?;
                let iffalse_ty = iffalse.type_of_ctx(context)?;
                iftrue_ty.expect(iftrue, &iffalse_ty).map_err(|_| {
                    TypingError::IfBody(
                        iftrue.clone(),
                        iftrue_ty.clone(),
                        iffalse.clone(),
                        iffalse_ty.clone(),
                    )
                })?;
                Ok(iftrue_ty)
            }
            ConstZero => Ok(Type::Nat.arc()),
            Succ(t) => {
                let ty = t.type_of_ctx(context)?;
                ty.expect(t, &Type::Nat.arc())
                    .map_err(TypingError::SuccOf)?;
                Ok(Type::Nat.arc())
            }
            Pred(t) => {
                let ty = t.type_of_ctx(context)?;
                ty.expect(t, &Type::Nat.arc())
                    .map_err(TypingError::PredOf)?;
                Ok(Type::Nat.arc())
            }
            IsZero(t) => {
                let ty = t.type_of_ctx(context)?;
                ty.expect(t, &Type::Nat.arc())
                    .map_err(TypingError::IsZeroOf)?;
                Ok(Type::Boolean.arc())
            }
            Var(name) => {
                let ty = context
                    .iter()
                    .rev()
                    .find(|(nm, _)| nm == name)
                    .map(|(_, t)| t)
                    .cloned();
                ty.ok_or_else(|| TypingError::NoVar(name.clone()))
            }
            Abstraction {
                type_,
                bound_var,
                body,
            } => context.save_frame(|context| {
                context.push((bound_var.clone(), type_.clone()));
                let res = body.type_of_ctx(context)?;

                Ok(Type::Function(type_.clone(), res.clone()).arc())
            }),
            Application { function, argument } => {
                let fun_ty = function.type_of_ctx(context)?;
                match fun_ty.deref() {
                    Type::Function(fun_arg_ty, ty) => {
                        let arg_ty = argument.type_of_ctx(context)?;
                        arg_ty
                            .expect(argument, &fun_arg_ty)
                            .map_err(TypingError::CallArg)?;
                        Ok(ty.clone())
                    }
                    _ => Err(TypingError::CallOf(function.clone(), fun_ty.clone())),
                }
            }
            Record(fields) => {
                let fields: Result<BTreeMap<_, _>, _> = fields
                    .iter()
                    .map(|(nm, val)| -> Result<_, _> {
                        let ty = val.type_of_ctx(context)?;
                        Ok((nm.clone(), ty))
                    })
                    .collect();
                let fields = fields?;
                Ok(Type::Record(fields).arc())
            }
            Let {
                clauses,
                substituted_expr,
            } => {
                let vars = clauses
                    .iter()
                    .map(|(pattern, expr)| -> Result<_, _> {
                        let destr_ty = expr.type_of_ctx(context)?;
                        Ok(pattern.collect_var_types(&destr_ty)?)
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect::<TypeContext>();

                Ok(context.save_frame(|context| {
                    context.extend(vars);
                    substituted_expr.type_of_ctx(context)
                })?)
            }
            Variant { body, name, ty } => {
                let body_ty = body.type_of_ctx(context)?;
                // TODO: this a common idiom that should probably be a macro based on matches! or some other macromagic
                match ty.deref() {
                    Type::Variant(vars) => vars
                        .get(name)
                        .ok_or_else(|| TypingError::UnknownVariant(name.clone(), ty.clone()))
                        .and_then(|v| v.expect(body, &body_ty).map_err(TypingError::Variant)),
                    _ => Err(TypingError::AsNotVariant(ty.clone())),
                }?;
                Ok(ty.clone())
            }
            Nil { ty } => Ok(Type::List(ty.clone()).arc()),
            Cons { left, right } => {
                let left_ty = left.type_of_ctx(context)?;
                let right_ty = right.type_of_ctx(context)?;
                let item_ty = match right_ty.deref() {
                    Type::List(t) => Ok(t),
                    _ => Err(TypingError::ConsRight(right.clone(), right_ty.clone())),
                }?;
                left_ty
                    .expect(right, &item_ty)
                    .map_err(TypingError::ConsLeft)?;
                Ok(right_ty)
            }
        }
    }

    pub fn type_of(&self) -> Result<Arc<Type>, TypingError> {
        let mut context = TypeContext::new();
        self.type_of_ctx(&mut context)
    }

    fn to_unnamed_impl(&self, context: &mut NamesContext) -> Arc<UnnamedExpr> {
        match self {
            Expr::ConstTrue => UnnamedExpr::ConstTrue.arc(),
            Expr::ConstFalse => UnnamedExpr::ConstFalse.arc(),
            Expr::If {
                cond,
                iftrue,
                iffalse,
            } => UnnamedExpr::If {
                cond: cond.to_unnamed_impl(context),
                iftrue: iftrue.to_unnamed_impl(context),
                iffalse: iffalse.to_unnamed_impl(context),
            }
            .arc(),

            Expr::ConstZero => UnnamedExpr::ConstZero.arc(),
            Expr::Succ(t) => UnnamedExpr::Succ(t.to_unnamed_impl(context)).arc(),
            Expr::Pred(t) => UnnamedExpr::Pred(t.to_unnamed_impl(context)).arc(),
            Expr::IsZero(t) => UnnamedExpr::IsZero(t.to_unnamed_impl(context)).arc(),

            Expr::Var(name) => match context.iter().rev().enumerate().find(|(_, s)| s == &name) {
                None => UnnamedExpr::UnboundVar(name.clone()).arc(),
                Some((index, _)) => UnnamedExpr::BoundVar {
                    index: index,
                    old_name: name.clone(),
                }
                .arc(),
            },
            Expr::Abstraction {
                bound_var, body, ..
            } => context.save_frame(|context| {
                context.push(bound_var.clone());
                let r = UnnamedExpr::Abstraction {
                    bound_var: bound_var.clone(),
                    body: body.to_unnamed_impl(context),
                };
                r.arc()
            }),
            Expr::Application { function, argument } => UnnamedExpr::Application {
                function: function.to_unnamed_impl(context),
                argument: argument.to_unnamed_impl(context),
            }
            .arc(),
            Expr::Record(items) => UnnamedExpr::Record(
                items
                    .into_iter()
                    .map(|(k, v)| (k.clone(), v.to_unnamed_impl(context)))
                    .collect(),
            )
            .arc(),
            Expr::Let {
                clauses,
                substituted_expr,
            } => {
                let var_names = clauses
                    .iter()
                    .flat_map(|(pattern, _)| pattern.collect_var_names())
                    .collect::<Vec<Arc<str>>>();
                let subs = context.save_frame(|context| {
                    context.extend(var_names);
                    substituted_expr.to_unnamed_impl(context)
                });
                UnnamedExpr::Let {
                    substituted_expr: subs,
                    clauses: clauses
                        .iter()
                        .map(|(pattern, expr)| {
                            (pattern.to_unnamed(), expr.to_unnamed_impl(context))
                        })
                        .collect(),
                }
                .arc()
            }
            Expr::Variant { name, body, .. } => UnnamedExpr::Variant {
                name: name.clone(),
                body: body.to_unnamed_impl(context),
            }
            .arc(),
            Expr::Nil { .. } => UnnamedExpr::Nil.arc(),
            Expr::Cons { left, right } => UnnamedExpr::Cons {
                left: left.to_unnamed_impl(context),
                right: right.to_unnamed_impl(context),
            }
            .arc(),
        }
    }

    pub fn to_unnamed(&self) -> Arc<UnnamedExpr> {
        let mut context = NamesContext::new();
        self.to_unnamed_impl(&mut context)
    }
}
