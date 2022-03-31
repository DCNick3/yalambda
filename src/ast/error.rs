use crate::ast::{Type, UnnamedExpr};
use crate::Expr;
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
