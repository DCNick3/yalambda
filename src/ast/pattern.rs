use crate::ast::context::TypeContext;
use crate::ast::error::{RuntimeError, TypingError};
use crate::ast::unnamed_expr::UnnamedExpr;
use crate::ast::Type;
use std::collections::BTreeMap;
use std::ops::Deref;
use std::sync::Arc;

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

#[derive(Debug, Clone)]
pub enum UnnamedPattern {
    Var(Arc<str>), // this is an "old" name now, not really meaning anything
    Record(BTreeMap<Arc<str>, Arc<UnnamedPattern>>),
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
