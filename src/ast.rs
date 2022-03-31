use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    IfTypeError(Arc<UnnamedExpr>),
    IsZeroTypeError(Arc<UnnamedExpr>),
    PredTypeError(Arc<UnnamedExpr>),
    DestructTypeError(Arc<UnnamedExpr>),
    UnknownVariant(Arc<str>, Arc<UnnamedExpr>),
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
    UnknownVariant(Arc<str>, Arc<Type>),
    AsNotVariant(Arc<Type>),
    Variant(TypeMismatch),
    ConsRight(Arc<Expr>, Arc<Type>),
    ConsLeft(TypeMismatch),
    DestructNotRecord(/*Arc<Expr>, impl difficulties*/ Arc<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nat,
    Boolean,
    Function(Arc<Type>, Arc<Type>),
    Record(BTreeMap<Arc<str>, Arc<Type>>),
    List(Arc<Type>),
    Option(Arc<Type>),
    Variant(BTreeMap<Arc<str>, Arc<Type>>),
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

pub struct Context<T: Clone>(Vec<T>);

impl<T: Clone> Context<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value)
    }

    pub fn get(&self, index: usize) -> T {
        if index >= self.0.len() {
            panic!("Context index out of bounds")
        }
        self.0[self.0.len() - 1 - index].clone()
    }

    fn save_frame<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        let val = self.0.len();

        let r = f(self);

        self.0.truncate(val);
        r
    }

    fn iter(&self) -> std::slice::Iter<T> {
        self.0.iter()
    }
}

impl<T: Clone> Extend<T> for Context<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.0.extend(iter)
    }
}

impl<T: Clone> IntoIterator for Context<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: Clone> FromIterator<T> for Context<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<T: Clone> From<Vec<T>> for Context<T> {
    fn from(v: Vec<T>) -> Self {
        Self(v)
    }
}

type TypeContext = Context<(Arc<str>, Arc<Type>)>;
type NamesContext = Context<Arc<str>>;

#[derive(Debug, Clone)]
pub enum Pattern {
    Var(Arc<str>),
    Record(Vec<(Arc<str>, Arc<Pattern>)>),
}

impl Pattern {
    pub fn arc(self) -> Arc<Self> {
        Arc::new(self)
    }

    pub fn to_unnamed(&self) -> Arc<UnnamedPattern> {
        match self {
            Pattern::Var(name) => UnnamedPattern::Var(name.clone()).arc(),
            Pattern::Record(items) => UnnamedPattern::Record(
                items
                    .iter()
                    .map(|(nm, pat)| (nm.clone(), pat.to_unnamed()))
                    .collect(),
            )
            .arc(),
        }
    }

    pub fn collect_var_names(self: &Arc<Pattern>) -> Vec<Arc<str>> {
        match self.deref() {
            Pattern::Var(v) => vec![v.clone()],
            Pattern::Record(items) => {
                let mut r = Vec::new();

                for (_, pattern) in items {
                    r.extend(pattern.collect_var_names());
                }

                r
            }
        }
    }

    pub fn collect_var_types(
        self: &Arc<Pattern>,
        ty: &Arc<Type>,
    ) -> Result<TypeContext, TypingError> {
        match self.deref() {
            Pattern::Var(v) => Ok(vec![(v.clone(), ty.clone())].into()),
            Pattern::Record(items) => match ty.deref() {
                Type::Record(items_ty) => {
                    let mut r = TypeContext::new();

                    for (field, pattern) in items {
                        let item_ty = items_ty.get(field).ok_or_else(|| {
                            TypingError::UnknownVariant(field.clone(), ty.clone())
                        })?;
                        r.extend(pattern.collect_var_types(item_ty)?);
                    }

                    Ok(r)
                }
                _ => Err(TypingError::DestructNotRecord(ty.clone())),
            },
        }
    }
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

#[derive(Debug, Clone)]
pub enum UnnamedPattern {
    Var(Arc<str>), // this is an "old" name now, not really meaning anything
    Record(BTreeMap<Arc<str>, Arc<UnnamedPattern>>),
}

impl UnnamedPattern {
    pub fn arc(self) -> Arc<Self> {
        Arc::new(self)
    }

    pub fn collect_var_vals(
        self: &Arc<UnnamedPattern>,
        val: &Arc<UnnamedExpr>,
    ) -> Result<Vec<Arc<UnnamedExpr>>, RuntimeError> {
        match self.deref() {
            UnnamedPattern::Var(_) => Ok(vec![val.clone()]),
            UnnamedPattern::Record(items) => match val.deref() {
                UnnamedExpr::Record(items_ty) => {
                    let mut r = Vec::new();

                    for (field, pattern) in items {
                        let item_ty = items_ty.get(field).ok_or_else(|| {
                            RuntimeError::UnknownVariant(field.clone(), val.clone())
                        })?;
                        r.extend(pattern.collect_var_vals(item_ty)?);
                    }

                    Ok(r)
                }
                _ => Err(RuntimeError::DestructTypeError(val.clone())),
            },
        }
    }

    pub fn expandable(&self, val: &Arc<UnnamedExpr>) -> bool {
        match self {
            UnnamedPattern::Var(_) => true,
            UnnamedPattern::Record(_) => val.isval(),
        }
    }

    pub fn variable_count(&self) -> usize {
        match self {
            UnnamedPattern::Var(_) => 1,
            UnnamedPattern::Record(items) => {
                items.iter().map(|(_, pat)| pat.variable_count()).sum()
            }
        }
    }
}

impl PartialEq for UnnamedPattern {
    fn eq(&self, other: &Self) -> bool {
        use UnnamedPattern::*;
        match (self, other) {
            (Var(_), Var(_)) => true,
            (Record(items1), Record(items2)) => items1 == items2,
            _ => false,
        }
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

type EvaluationContext = Context<Arc<UnnamedExpr>>;

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

    fn isval(&self) -> bool {
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
