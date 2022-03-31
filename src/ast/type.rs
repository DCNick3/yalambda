use crate::ast::error::TypeMismatch;
use crate::ast::expr::Expr;
use std::collections::BTreeMap;
use std::sync::Arc;

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
