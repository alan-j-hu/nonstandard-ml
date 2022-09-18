use crate::bytecode::Cmp;
use crate::elab::typ;
use crate::stringpool::StringToken;
use bumpalo::collections::Vec;
use std::collections::{BTreeMap, HashSet};

mod convert;
pub mod liveness;
pub use convert::compile;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(i32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockName(i32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnName(i32);

pub struct Instr<'a> {
    pub op: Op<'a>,
    pub killset: Option<HashSet<Register>>,
}

impl<'a> Instr<'a> {
    pub fn new(op: Op<'a>) -> Self {
        Self { op, killset: None }
    }
}

pub enum Op<'a> {
    Apply(Register, Register, Register, Option<HashSet<Register>>),
    Let(Def<'a>),
}

pub enum Terminator<'a> {
    Cmp(Cmp, Register, Register, BlockName, BlockName),
    Continue(BlockName, Vec<'a, Register>),
    Return(Register),
    TailCall(Register, Register),
}

impl<'a> Terminator<'a> {
    pub fn visit<'s, R, F>(&'s self, mut visit: F) -> R
    where
        F: for<'b> std::ops::FnMut(
            &'b mut dyn Iterator<Item = BlockName>,
            &'b mut dyn Iterator<Item = &'s Register>,
        ) -> R,
    {
        match self {
            Terminator::Cmp(_, ref lhs, ref rhs, tru, fls) => visit(
                &mut std::iter::once(*tru).chain(std::iter::once(*fls)),
                &mut std::iter::once(lhs).chain(std::iter::once(rhs)),
            ),
            Terminator::Continue(block, ref operands) => {
                visit(&mut std::iter::once(*block), &mut operands.iter())
            }
            Terminator::Return(ref a) => visit(&mut std::iter::empty(), &mut std::iter::once(a)),
            Terminator::TailCall(ref a, ref b) => visit(
                &mut std::iter::empty(),
                &mut std::iter::once(a).chain(std::iter::once(b)),
            ),
        }
    }
}

pub struct Def<'a> {
    pub register: Register,
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Box(typ::Type, u64, Vec<'a, Register>),
    Closure(FnName, Vec<'a, Register>),
    Int(i64),
    String(StringToken),
}

pub struct Block<'a> {
    pub params: Vec<'a, Register>,
    pub instrs: Vec<'a, Instr<'a>>,
    pub terminator: Terminator<'a>,
    pub killset: HashSet<Register>,
}

pub struct Fn<'a> {
    pub domain: typ::Type,
    pub param: Register,
    pub blocks: BTreeMap<BlockName, Block<'a>>,
    pub entry: BlockName,
    pub block_order: Vec<'a, BlockName>,
}

pub struct Program<'a> {
    pub fns: BTreeMap<FnName, Fn<'a>>,
    pub entry: FnName,
}
