use bumpalo::{
    collections::{String, Vec},
    Bump,
};
use std::collections::BTreeMap;

mod convert;
pub use convert::convert;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id(i32);

pub enum CExp<'a> {
    Apply(&'a CExp<'a>, Id),
    Case(Id, Vec<'a, ()>),
    CaseInt(Id, BTreeMap<i64, Id>, Id),
    Continue(Id, Vec<'a, Id>),
    Enter(Id, Id),
    Let(&'a ADef<'a>, &'a CExp<'a>),
    LetCont(Vec<'a, (Id, Vec<'a, Id>, &'a CExp<'a>)>, &'a CExp<'a>),
}

pub struct ADef<'a> {
    pub id: Id,
    pub exp: AExp<'a>,
}

pub enum AExp<'a> {
    Integer(i64),
    Lambda(Lambda<'a>),
    String(String<'a>),
}

pub struct AUse<'a>(&'a ADef<'a>);

pub struct Lambda<'a> {
    params: Vec<'a, Id>,
    ret_addr: Id,
    body: &'a CExp<'a>,
}

pub struct Builder {
    counter: i32,
}

impl Builder {
    pub fn new() -> Self {
        Builder { counter: 0 }
    }

    pub fn fresh_id(&mut self) -> Id {
        let id = Id(self.counter);
        self.counter += 1;
        id
    }
}
