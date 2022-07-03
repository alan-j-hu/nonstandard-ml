use super::typ::Type;
use bumpalo::collections::{String, Vec};

#[derive(Clone)]
pub struct Var<'a> {
    id: usize,
    name: &'a str,
}

#[derive(Default)]
pub struct VarBuilder {
    counter: usize,
}

impl VarBuilder {
    pub fn fresh<'a>(&mut self, name: &'a str) -> Var<'a> {
        let id = self.counter;
        self.counter = id + 1;
        Var { id, name }
    }
}

pub struct Pat<'a> {
    pub inner: PatInner<'a>,
    pub vars: Vec<'a, Var<'a>>,
}

pub enum PatInner<'a> {
    Or(&'a Pat<'a>, &'a Pat<'a>),
    Wild,
}

pub enum Exp<'a> {
    Apply(&'a Exp<'a>, &'a Exp<'a>),
    Case(&'a Exp<'a>, &'a [Case<'a>]),
    Integer(i64),
    Lambda(&'a [Case<'a>]),
    Let(&'a Dec<'a>, &'a Exp<'a>),
    String(String<'a>),
    Var(Var<'a>),
}

pub struct Case<'a> {
    pub lhs: Pat<'a>,
    pub rhs: Exp<'a>,
}

pub enum Dec<'a> {
    And(&'a Dec<'a>, &'a Dec<'a>),
    Loc(&'a Dec<'a>, &'a Dec<'a>),
    Seq(&'a Dec<'a>, &'a Dec<'a>),
    Val(&'a Pat<'a>, &'a Exp<'a>),
}
