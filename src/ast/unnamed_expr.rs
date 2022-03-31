use crate::ast::context::EvaluationContext;
use crate::ast::error::RuntimeError;
use crate::ast::pattern::UnnamedPattern;
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum UnnamedExpr {
    ConstTrue,
    ConstFalse,
    If {
        cond: Arc<UnnamedExpr>,
        iftrue: Arc<UnnamedExpr>,
        iffalse: Arc<UnnamedExpr>,
    },

    ConstZero,
    Succ(Arc<UnnamedExpr>),
    Pred(Arc<UnnamedExpr>),
    IsZero(Arc<UnnamedExpr>),
    UnboundVar(Arc<str>),
    BoundVar {
        index: usize,
        old_name: Arc<str>,
    },
    Abstraction {
        bound_var: Arc<str>,
        body: Arc<UnnamedExpr>,
    },
    Application {
        function: Arc<UnnamedExpr>,
        argument: Arc<UnnamedExpr>,
    },
    Record(BTreeMap<Arc<str>, Arc<UnnamedExpr>>),
    Let {
        clauses: Vec<(Arc<UnnamedPattern>, Arc<UnnamedExpr>)>,
        substituted_expr: Arc<UnnamedExpr>,
    },
    Variant {
        name: Arc<str>,
        body: Arc<UnnamedExpr>,
    },
    Nil,
    Cons {
        left: Arc<UnnamedExpr>,
        right: Arc<UnnamedExpr>,
    },
}

impl UnnamedExpr {
    pub fn arc(self) -> Arc<Self> {
        Arc::new(self)
    }

    /// Closes the term down to variable #k (k-th variable is the last one touched) using the provided context
    ///
    /// # Arguments
    ///
    /// * `context`: context to use for resolving the variables
    /// * `k`: how many variables to consider local (those are not resolved and not saved in the closure)
    fn close(self: &Arc<Self>, context: &EvaluationContext, k: usize) -> Arc<Self> {
        use crate::ast::UnnamedExpr::{ConstFalse, ConstTrue, ConstZero, Nil};
        use std::ops::Deref;
        use UnnamedExpr::*;
        match self.deref() {
            ConstTrue => self.clone(),
            ConstFalse => self.clone(),
            If {
                cond,
                iftrue,
                iffalse,
            } => If {
                cond: cond.close(context, k),
                iftrue: iftrue.close(context, k),
                iffalse: iffalse.close(context, k),
            }
            .arc(),
            ConstZero => self.clone(),
            Succ(t) => Succ(t.close(context, k)).arc(),
            Pred(t) => Pred(t.close(context, k)).arc(),
            IsZero(t) => IsZero(t.close(context, k)).arc(),
            UnboundVar(_) => self.clone(),
            BoundVar { index, old_name } => {
                let index = *index;
                if index >= k {
                    context.get(index - k)
                } else {
                    BoundVar {
                        index,
                        old_name: old_name.clone(),
                    }
                    .arc()
                }
            }
            Abstraction { body, bound_var } => Abstraction {
                body: body.close(context, k + 1),
                bound_var: bound_var.clone(),
            }
            .arc(),
            Application { function, argument } => Application {
                function: function.close(context, k),
                argument: argument.close(context, k),
            }
            .arc(),
            Record(items) => Record(
                items
                    .iter()
                    .map(|(name, val)| (name.clone(), val.close(context, k)))
                    .collect(),
            )
            .arc(),
            Let {
                clauses,
                substituted_expr,
            } => {
                let variable_count: usize =
                    clauses.iter().map(|(pat, _)| pat.variable_count()).sum();
                let clauses = clauses
                    .iter()
                    .map(|(pattern, value)| (pattern.clone(), value.close(context, k)))
                    .collect();
                let substituted_expr = substituted_expr.close(context, k + variable_count);
                Let {
                    clauses,
                    substituted_expr,
                }
                .arc()
            }
            Variant { .. } => todo!(),
            Nil => todo!(),
            Cons { .. } => todo!(),
        }
    }

    pub fn isval(&self) -> bool {
        match self {
            UnnamedExpr::ConstTrue => true,
            UnnamedExpr::ConstFalse => true,
            UnnamedExpr::If { .. } => false,
            UnnamedExpr::ConstZero => true,
            UnnamedExpr::Succ(t) => t.isval(),
            UnnamedExpr::Pred(_) => false,
            UnnamedExpr::IsZero(_) => false,
            UnnamedExpr::UnboundVar(_) => false,
            UnnamedExpr::BoundVar { .. } => false,
            UnnamedExpr::Abstraction { .. } => false,
            UnnamedExpr::Application { .. } => false,
            UnnamedExpr::Record(items) => items.iter().all(|(_, val)| val.isval()),
            UnnamedExpr::Let { .. } => false,
            UnnamedExpr::Variant { .. } => todo!(),
            UnnamedExpr::Nil => true,
            UnnamedExpr::Cons { .. } => todo!(),
        }
    }

    fn evaluate_impl(&self, context: &mut EvaluationContext) -> Result<Arc<Self>, RuntimeError> {
        use crate::ast::UnnamedExpr::{ConstFalse, ConstTrue, ConstZero, Nil};
        use std::ops::Deref;
        use UnnamedExpr::*;

        Ok(match self {
            ConstTrue => ConstTrue.arc(),
            ConstFalse => ConstFalse.arc(),
            ConstZero => ConstZero.arc(),
            If {
                cond,
                iftrue,
                iffalse,
            } => {
                let cond = cond.evaluate_impl(context)?;
                return match cond.deref() {
                    t if !t.isval() => Ok(If {
                        cond,
                        iftrue: iftrue.evaluate_impl(context)?,
                        iffalse: iffalse.evaluate_impl(context)?,
                    }
                    .arc()),
                    ConstTrue => Ok(iftrue.evaluate_impl(context)?),
                    ConstFalse => Ok(iffalse.evaluate_impl(context)?),
                    _ => Err(RuntimeError::IfTypeError(cond)),
                };
            }
            Succ(t) => Succ(t.evaluate_impl(context)?).arc(),
            Pred(t) => {
                let t = t.evaluate_impl(context)?;
                return match t.deref() {
                    t if !t.isval() => Ok(Pred(t.clone().arc()).arc()),
                    ConstZero => Ok(ConstZero.arc()),
                    Succ(t) => Ok(t.clone()),
                    _ => Err(RuntimeError::PredTypeError(t)),
                };
            }
            IsZero(t) => {
                let t = t.evaluate_impl(context)?;
                return match t.deref() {
                    t if !t.isval() => Ok(IsZero(t.clone().arc()).arc()),
                    ConstZero => Ok(ConstTrue.arc()),
                    Succ(_) => Ok(ConstFalse.arc()),
                    _ => Err(RuntimeError::IsZeroTypeError(t)),
                };
            }
            BoundVar { index, .. } => context.get(*index),
            Abstraction { bound_var, body } =>
            // don't touch the abstraction, we use call-by-value
            {
                Abstraction {
                    bound_var: bound_var.clone(),
                    body: body.close(context, 1),
                }
                .arc()

                // context.save_frame(|context| {
                //     context.push(None);
                //     body.evaluate_impl(context).map(|body| {
                //         Abstraction {
                //             bound_var: bound_var.clone(),
                //             body,
                //         }
                //         .arc()
                //     })
                // })?
            }
            Application { function, argument } => {
                let function = function.evaluate_impl(context)?;
                return match function.deref() {
                    Abstraction { body, .. } => {
                        let argument = argument.evaluate_impl(context)?;
                        context.save_frame(|context| {
                            context.push(argument);
                            body.evaluate_impl(context)
                        })
                    }
                    UnboundVar(name) => Ok(UnboundVar(name.clone()).arc()), // it's ok to call an external value
                    _ => {
                        todo!()
                    }
                };
            }
            Let {
                clauses,
                substituted_expr,
            } => {
                let clauses = clauses
                    .iter()
                    .map(|(pattern, expr)| -> Result<_, _> {
                        Ok((pattern.clone(), expr.evaluate_impl(context)?))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if clauses
                    .iter()
                    .all(|(pattern, expr)| pattern.expandable(expr))
                {
                    let vars = clauses
                        .iter()
                        .map(|(pattern, expr)| -> Result<_, _> {
                            let destructed_expr = expr.evaluate_impl(context)?;

                            pattern.collect_var_vals(&destructed_expr)
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect::<Vec<_>>();
                    context.save_frame(|context| {
                        context.extend(vars.into_iter());
                        substituted_expr.evaluate_impl(context)
                    })?
                } else {
                    Let {
                        clauses,
                        substituted_expr: substituted_expr.clone(),
                    }
                    .arc()
                }
            }
            UnboundVar(_) => panic!("Evaluate UnboundVar? No, thank you"),
            Record(items) => Record(
                items
                    .iter()
                    .map(|(name, val)| val.evaluate_impl(context).map(|val| (name.clone(), val)))
                    .collect::<Result<_, _>>()?,
            )
            .arc(),
            Variant { .. } => todo!(),
            Nil => Nil.arc(),
            Cons { left, right } => {
                let left = left.evaluate_impl(context)?;
                let right = right.evaluate_impl(context)?;
                Cons { left, right }.arc()
            }
        })
    }

    pub fn evaluate(&self) -> Result<Arc<Self>, RuntimeError> {
        // TODO: context is actually not needed
        let mut context = EvaluationContext::new();
        UnnamedExpr::evaluate_impl(self, &mut context)
    }

    pub fn equivalent(one: &Self, other: &Self) -> bool {
        use crate::ast::UnnamedExpr::{ConstFalse, ConstTrue, ConstZero, Nil};
        use UnnamedExpr::*;
        match (one, other) {
            (ConstTrue, ConstTrue) => true,
            (ConstFalse, ConstFalse) => true,
            (
                If {
                    cond: cond1,
                    iftrue: iftrue1,
                    iffalse: iffalse1,
                },
                If {
                    cond: cond2,
                    iftrue: iftrue2,
                    iffalse: iffalse2,
                },
            ) => {
                UnnamedExpr::equivalent(cond1, cond2)
                    && UnnamedExpr::equivalent(iftrue1, iftrue2)
                    && UnnamedExpr::equivalent(iffalse1, iffalse2)
            }
            (ConstZero, ConstZero) => true,
            (Succ(t1), Succ(t2)) => UnnamedExpr::equivalent(t1, t2),
            (Pred(t1), Pred(t2)) => UnnamedExpr::equivalent(t1, t2),
            (IsZero(t1), IsZero(t2)) => UnnamedExpr::equivalent(t1, t2),
            (UnboundVar(v1), UnboundVar(v2)) => v1 == v2,
            (BoundVar { index: index1, .. }, BoundVar { index: index2, .. }) => index1 == index2,
            (Abstraction { body: body1, .. }, Abstraction { body: body2, .. }) => {
                UnnamedExpr::equivalent(body1, body2)
            }
            (
                Application {
                    argument: argument1,
                    function: function1,
                },
                Application {
                    argument: argument2,
                    function: function2,
                },
            ) => {
                UnnamedExpr::equivalent(argument1, argument2)
                    && UnnamedExpr::equivalent(function1, function2)
            }
            // Record(BTreeMap<Arc<str>, Arc<UnnamedExpr>>),
            //     Let {
            //         clauses: Vec<(Arc<UnnamedPattern>, Arc<UnnamedExpr>)>,
            //         substituted_expr: Arc<UnnamedExpr>,
            //     },
            //     Variant {
            //         name: Arc<str>,
            //         body: Arc<UnnamedExpr>,
            //     },
            //     Nil,
            //     Cons {
            //         left: Arc<UnnamedExpr>,
            //         right: Arc<UnnamedExpr>,
            //     },
            (Record(items1), Record(items2)) => items1 == items2,
            (
                Let {
                    clauses: clauses1,
                    substituted_expr: substituted_expr1,
                },
                Let {
                    clauses: clauses2,
                    substituted_expr: substituted_expr2,
                },
            ) => clauses1 == clauses2 && substituted_expr1 == substituted_expr2,
            (Variant { .. }, Variant { .. }) => todo!(),
            (Nil, Nil) => true,
            (
                Cons {
                    left: left1,
                    right: right1,
                },
                Cons {
                    left: left2,
                    right: right2,
                },
            ) => left1 == left2 && right1 == right2,
            _ => false,
        }
    }
}

impl PartialEq for UnnamedExpr {
    fn eq(&self, other: &Self) -> bool {
        UnnamedExpr::equivalent(self, other)
    }
}
