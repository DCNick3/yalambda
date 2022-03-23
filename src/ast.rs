use crate::Expr::{IsZero, Pred, Succ};
use std::ops::Deref;
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
        body: Arc<Expr>,
    },
    Application {
        function: Arc<Expr>,
        argument: Arc<Expr>,
    },
}

impl Expr {
    // pub fn transform<F, R>(self, trans: F) -> R {}

    fn to_unnamed_impl(&self, context: &mut Vec<Arc<str>>) -> Arc<UnnamedExpr> {
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
                    index: index as u32,
                    old_name: name.clone(),
                }
                .arc(),
            },
            Expr::Abstraction { bound_var, body } => {
                context.push(bound_var.clone());
                let r = UnnamedExpr::Abstraction {
                    bound_var: bound_var.clone(),
                    body: body.to_unnamed_impl(context),
                };
                context.pop().unwrap();
                r.arc()
            }
            Expr::Application { function, argument } => UnnamedExpr::Application {
                function: function.to_unnamed_impl(context),
                argument: argument.to_unnamed_impl(context),
            }
            .arc(),
        }
    }

    pub fn to_unnamed(&self) -> Arc<UnnamedExpr> {
        let mut context = Vec::new();
        self.to_unnamed_impl(&mut context)
    }
}

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
        index: u32,
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
}

#[derive(Debug)]
pub enum Error {
    Dicks,
}

impl UnnamedExpr {
    pub fn arc(self) -> Arc<Self> {
        Arc::new(self)
    }

    fn shift(&self, k: u32) -> Arc<UnnamedExpr> {
        use UnnamedExpr::*;
        match self {
            ConstTrue => ConstTrue.arc(),
            ConstFalse => ConstFalse.arc(),
            If {
                cond,
                iftrue,
                iffalse,
            } => If {
                cond: cond.shift(k),
                iftrue: iftrue.shift(k),
                iffalse: iffalse.shift(k),
            }
            .arc(),
            ConstZero => ConstZero.arc(),
            Succ(t) => Succ(t.shift(k)).arc(),
            Pred(t) => Pred(t.shift(k)).arc(),
            IsZero(t) => IsZero(t.shift(k)).arc(),
            UnboundVar(v) => UnboundVar(v.clone()).arc(),
            BoundVar {
                index: bound_index,
                old_name,
            } => BoundVar {
                index: if *bound_index < k {
                    *bound_index
                } else {
                    *bound_index + 1
                },
                old_name: old_name.clone(),
            }
            .arc(),

            Abstraction { bound_var, body } => Abstraction {
                bound_var: bound_var.clone(),
                body: body.shift(k + 1),
            }
            .arc(),
            Application { function, argument } => Application {
                function: function.shift(k),
                argument: argument.shift(k),
            }
            .arc(),
        }
    }

    fn substitute(&self, index: u32, value: Arc<UnnamedExpr>) -> Arc<UnnamedExpr> {
        use UnnamedExpr::*;
        match self {
            ConstTrue => ConstTrue.arc(),
            ConstFalse => ConstFalse.arc(),
            ConstZero => ConstZero.arc(),
            If {
                cond,
                iftrue,
                iffalse,
            } => If {
                cond: cond.substitute(index, value.clone()),
                iftrue: iftrue.substitute(index, value.clone()),
                iffalse: iffalse.substitute(index, value),
            }
            .arc(),
            Succ(t) => Succ(t.substitute(index, value)).arc(),
            Pred(t) => Pred(t.substitute(index, value)).arc(),
            IsZero(t) => IsZero(t.substitute(index, value)).arc(),
            UnboundVar(v) => UnboundVar(v.clone()).arc(),
            BoundVar {
                index: bound_index,
                old_name,
            } => {
                if *bound_index == index {
                    value
                } else {
                    BoundVar {
                        index: *bound_index,
                        old_name: old_name.clone(),
                    }
                    .arc()
                }
            }
            Abstraction { bound_var, body } => Abstraction {
                bound_var: bound_var.clone(),
                body: body.substitute(index + 1, body.shift(0)),
            }
            .arc(),
            Application { function, argument } => Application {
                function: function.substitute(index, value.clone()),
                argument: argument.substitute(index, value),
            }
            .arc(),
        }
    }

    fn evaluate_impl(&self, context: &mut Vec<Arc<UnnamedExpr>>) -> Result<Arc<Self>, Error> {
        use UnnamedExpr::*;

        Ok(match self {
            // If { cond, iftrue, iffalse } => {}
            Succ(t) => Succ(t.evaluate_impl(context)?).arc(),
            Pred(t) => Pred(t.evaluate_impl(context)?).arc(),
            IsZero(t) => IsZero(t.evaluate_impl(context)?).arc(),
            BoundVar { index, .. } => context[context.len() - 1 - *index as usize].clone(),
            // Abstraction { bound_var, body } => todo!(),
            Application { function, argument } => {
                let function = function.evaluate_impl(context)?;
                match function.deref() {
                    UnnamedExpr::Abstraction { body, .. } => body.substitute(0, argument.clone()),
                    _ => {
                        todo!()
                    }
                }
            }
            _ => self.clone().arc(),
        })
    }

    pub fn evaluate(&self) -> Result<Arc<Self>, Error> {
        let mut context = Vec::new();
        UnnamedExpr::evaluate_impl(self, &mut context)
    }

    pub fn equivalent(one: &Self, other: &Self) -> bool {
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
            _ => false,
        }
    }
}
