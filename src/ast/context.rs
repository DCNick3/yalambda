use crate::ast::{Type, UnnamedExpr};
use std::iter::FromIterator;
use std::sync::Arc;

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

    pub fn save_frame<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        let val = self.0.len();

        let r = f(self);

        self.0.truncate(val);
        r
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
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

pub type TypeContext = Context<(Arc<str>, Arc<Type>)>;
pub type NamesContext = Context<Arc<str>>;

pub type EvaluationContext = Context<Arc<UnnamedExpr>>;
