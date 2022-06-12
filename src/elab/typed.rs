use super::typ::Type;
use bumpalo::collections::{String, Vec};

#[derive(Clone)]
pub struct Var {
    id: usize,
    pub typ: Type,
}

#[derive(Default)]
pub struct VarBuilder {
    counter: usize,
}

impl VarBuilder {
    pub fn fresh(&mut self, typ: Type) -> Var {
        let id = self.counter;
        self.counter = id + 1;
        Var { id, typ }
    }
}

pub struct Pat<'a> {
    pub inner: PatInner<'a>,
    pub vars: Vec<'a, Var>,
}

pub enum PatInner<'a> {
    Or(&'a Pat<'a>, &'a Pat<'a>),
    Wild,
}

pub enum Exp<'a> {
    Apply(&'a Exp<'a>, &'a Exp<'a>),
    Case(&'a Exp<'a>, &'a [Case<'a>]),
    Integer(&'a str),
    Lambda(&'a [Case<'a>]),
    Let(&'a [Dec<'a>], &'a Exp<'a>),
    String(String<'a>),
    Var(Var),
}

pub struct Case<'a> {
    pub lhs: Pat<'a>,
    pub rhs: Exp<'a>,
}

pub enum Dec<'a> {
    Val(Pat<'a>, Exp<'a>),
}
