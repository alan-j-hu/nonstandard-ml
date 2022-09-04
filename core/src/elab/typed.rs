use super::typ::Type;
use crate::stringpool::StringToken;
use bumpalo::collections::Vec;

#[derive(Clone, Copy, Eq)]
pub struct Var<'a> {
    id: usize,
    name: &'a str,
}

impl<'a> PartialEq for Var<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'a> std::hash::Hash for Var<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
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
    Int(i64),
    Or(&'a Pat<'a>, &'a Pat<'a>),
    Wild,
}

pub enum Exp<'a> {
    Apply(&'a Exp<'a>, &'a Exp<'a>),
    Case(Type, &'a Exp<'a>, &'a [Case<'a>]),
    Integer(i64),
    Lambda(Type, Vec<'a, Type>, &'a [Case<'a>]),
    Let(&'a Dec<'a>, &'a Exp<'a>),
    String(StringToken),
    Var(Var<'a>),
}

pub struct Case<'a> {
    pub pat: Pat<'a>,
    pub pats: Vec<'a, Pat<'a>>,
    pub rhs: Exp<'a>,
}

pub enum Dec<'a> {
    And(&'a Dec<'a>, &'a Dec<'a>),
    Loc(&'a Dec<'a>, &'a Dec<'a>),
    Seq(&'a Dec<'a>, &'a Dec<'a>),
    Val(Type, &'a Pat<'a>, &'a Exp<'a>),
}
