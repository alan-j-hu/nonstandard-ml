use bumpalo::collections::{String, Vec};
use std::collections::BTreeMap;

mod convert;
pub use convert::compile;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Register(i32);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockName(i32);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnName(i32);

pub enum Instr<'a> {
    Apply(Register, Register, Register),
    Let(Def<'a>),
}

pub enum Terminator<'a> {
    CaseInt(Register, BTreeMap<i64, BlockName>, BlockName),
    Continue(BlockName, Vec<'a, Register>),
    Return(Register),
    TailCall(Register, Register),
}

pub struct Def<'a> {
    pub register: Register,
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Closure(FnName, Vec<'a, Register>),
    Int(i64),
    String(String<'a>),
}

pub struct Block<'a> {
    pub params: Vec<'a, Register>,
    pub instrs: Vec<'a, Instr<'a>>,
    pub terminator: Terminator<'a>,
}

pub struct Fn<'a> {
    pub param: Register,
    pub blocks: BTreeMap<BlockName, Block<'a>>,
    pub entry: BlockName,
}

pub struct Program<'a> {
    pub fns: BTreeMap<FnName, Fn<'a>>,
    pub entry: FnName,
}
