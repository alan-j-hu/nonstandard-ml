use crate::elab::typ;
use crate::stringpool::StringToken;
use bumpalo::collections::Vec;
use std::collections::BTreeMap;

mod convert;
pub use convert::convert;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(i32);

pub enum CExp<'a> {
    Apply(Val, Val, Id),
    Case(Val, Vec<'a, ()>),
    Continue(Id, Vec<'a, Val>),
    Eq(Val, Val, Id, Id),
    Lt(Val, Val, Id, Id),
    Let(&'a ADef<'a>, &'a CExp<'a>),
    LetCont(Vec<'a, (Id, Vec<'a, Id>, &'a CExp<'a>)>, &'a CExp<'a>),
}

#[derive(Copy, Clone)]
pub enum Val {
    Integer(i64),
    String(StringToken),
    Id(Id),
}

pub struct ADef<'a> {
    pub id: Id,
    pub exp: AExp<'a>,
}

pub enum AExp<'a> {
    Box(typ::Type, u64, Vec<'a, Val>),
    Lambda(Lambda<'a>),
}

pub struct AUse<'a>(&'a ADef<'a>);

pub struct Lambda<'a> {
    pub domain: typ::Type,
    pub param: Id,
    pub ret_addr: Id,
    pub body: &'a CExp<'a>,
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
