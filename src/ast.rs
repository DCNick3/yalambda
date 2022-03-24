use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    IfTypeError(Arc<UnnamedExpr>),
    IsZeroTypeError(Arc<UnnamedExpr>),
    PredTypeError(Arc<UnnamedExpr>),
}

#[derive(Debug)]
pub struct TypeMismatch {
    pub expression: Arc<Expr>,
    pub actual_type: Arc<Type>,
    pub expected_type: Arc<Type>,
}

#[derive(Debug)]
pub enum TypingError {
    IfCond(TypeMismatch),
    IfBody(Arc<Expr>, Arc<Type>, Arc<Expr>, Arc<Type>),
    SuccOf(TypeMismatch),
    PredOf(TypeMismatch),
    IsZeroOf(TypeMismatch),
    NoVar(Arc<str>),
    CallOf(Arc<Expr>, Arc<Type>),
    CallArg(TypeMismatch),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nat,
    Boolean,
    Function(Arc<Type>, Arc<Type>),
}

impl Type {
    pub fn arc(self) -> Arc<Self> {
        Arc::new(self)
    }

    pub fn expect(
        self: &Arc<Self>,
        expr: &Arc<Expr>,
        another: &Arc<Type>,
    ) -> Result<(), TypeMismatch> {
        if self == another {
            Ok(())
        } else {
            Err(TypeMismatch {
                expression: expr.clone(),
                actual_type: self.clone(),
                expected_type: another.clone(),
            })
        }
    }
}

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
}

impl Expr {
    // pub fn transform<F, R>(self, trans: F) -> R {}

    pub fn type_of_ctx(
        &self,
        context: &mut Vec<(Arc<str>, Arc<Type>)>,
    ) -> Result<Arc<Type>, TypingError> {
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
            } => {
                context.push((bound_var.clone(), type_.clone()));
                let res = body.type_of_ctx(context)?;
                context.pop();

                Ok(Type::Function(type_.clone(), res.clone()).arc())
            }
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
        }
    }

    pub fn type_of(&self) -> Result<Arc<Type>, TypingError> {
        let mut context = Vec::new();
        self.type_of_ctx(&mut context)
    }

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
            Expr::Abstraction {
                bound_var, body, ..
            } => {
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
                body: body.substitute(index + 1, value.shift(0)),
            }
            .arc(),
            Application { function, argument } => Application {
                function: function.substitute(index, value.clone()),
                argument: argument.substitute(index, value),
            }
            .arc(),
        }
    }

    fn evaluate_impl(
        &self,
        context: &mut Vec<Arc<UnnamedExpr>>,
    ) -> Result<Arc<Self>, RuntimeError> {
        use UnnamedExpr::*;

        Ok(match self {
            If {
                cond,
                iftrue,
                iffalse,
            } => {
                let cond = cond.evaluate_impl(context)?;
                return match cond.deref() {
                    ConstTrue => Ok(iftrue.evaluate_impl(context)?),
                    ConstFalse => Ok(iffalse.evaluate_impl(context)?),
                    UnboundVar(_) => Ok(If {
                        cond,
                        iftrue: iftrue.evaluate_impl(context)?,
                        iffalse: iffalse.evaluate_impl(context)?,
                    }
                    .arc()),
                    _ => Err(RuntimeError::IfTypeError(cond)),
                };
            }
            Succ(t) => Succ(t.evaluate_impl(context)?).arc(),
            Pred(t) => {
                let t = t.evaluate_impl(context)?;
                return match t.deref() {
                    ConstZero => Ok(ConstZero.arc()),
                    Succ(t) => Ok(t.clone()),
                    _ => Err(RuntimeError::PredTypeError(t)),
                };
            }
            IsZero(t) => {
                let t = t.evaluate_impl(context)?;
                return match t.deref() {
                    ConstZero => Ok(ConstTrue.arc()),
                    Succ(_) => Ok(ConstFalse.arc()),
                    _ => Err(RuntimeError::IsZeroTypeError(t)),
                };
            }
            BoundVar { .. } => unreachable!(), // should already be substituted
            // Abstraction { bound_var, body } => todo!(),
            Application { function, argument } => {
                let function = function.evaluate_impl(context)?;
                return match function.deref() {
                    Abstraction { body, .. } => Ok(body
                        .substitute(0, argument.clone())
                        .evaluate_impl(context)?),
                    UnboundVar(name) => Ok(UnboundVar(name.clone()).arc()), // it's ok to call an external value
                    _ => {
                        todo!()
                    }
                };
            }
            _ => self.clone().arc(),
        })
    }

    pub fn evaluate(&self) -> Result<Arc<Self>, RuntimeError> {
        // TODO: context is actually not needed
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

impl PartialEq for UnnamedExpr {
    fn eq(&self, other: &Self) -> bool {
        UnnamedExpr::equivalent(self, other)
    }
}
