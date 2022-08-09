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
    Apply(Register, Operand<'a>, Operand<'a>),
    Let(Def<'a>),
}

pub enum Terminator<'a> {
    CaseInt(Operand<'a>, BTreeMap<i64, BlockName>, BlockName),
    Continue(BlockName, Vec<'a, Operand<'a>>),
    Return(Operand<'a>),
    TailCall(Operand<'a>, Operand<'a>),
}

pub enum Operand<'a> {
    Register(Register),
    Int(i64),
    String(String<'a>),
}

pub struct Def<'a> {
    pub register: Register,
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Closure(FnName, Vec<'a, Register>),
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
