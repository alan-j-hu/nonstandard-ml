use bumpalo::collections::{String, Vec};
use std::collections::{BTreeMap, HashMap};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id(i32);

pub enum Instr<'a> {
    Push(Id),
    Apply(Id),
    Let(&'a Def<'a>),
}

pub enum Terminator<'a> {
    CaseInt(Id, BTreeMap<i64, Id>, Id),
    Continue(Id, Vec<'a, Id>),
}

pub struct Def<'a> {
    pub id: Id,
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Closure(Id, Vec<'a, Id>),
    Int(i64),
    String(String<'a>),
}

pub struct Block<'a> {
    pub params: Vec<'a, Id>,
    pub instrs: Vec<'a, Instr<'a>>,
    pub terminator: Terminator<'a>,
}

pub struct Fn<'a> {
    pub params: Vec<'a, Id>,
    pub blocks: HashMap<Id, Block<'a>>,
    pub entry: Id,
}

pub struct Program<'a> {
    pub fns: HashMap<Id, Fn<'a>>,
    pub entry: Id,
}
