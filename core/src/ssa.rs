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

pub enum Terminator<'a> {
    Cmp(Cmp, Register, Register, BlockName, BlockName),
    Continue(BlockName, Vec<'a, Register>),
    Return(Register),
    TailCall(Register, Register),
}

impl<'a> Terminator<'a> {
    pub fn visit<R, F>(&self, mut visit: F) -> R
    where
        F: for<'b> std::ops::FnMut(
            &'b mut dyn Iterator<Item = BlockName>,
            &'b mut dyn Iterator<Item = Register>,
        ) -> R,
    {
        match self {
            Terminator::Cmp(_, ref lhs, ref rhs, tru, fls) => visit(
                &mut std::iter::once(*tru).chain(std::iter::once(*fls)),
                &mut std::iter::once(*lhs).chain(std::iter::once(*rhs)),
            ),
            Terminator::Continue(block, ref operands) => {
                visit(&mut std::iter::once(*block), &mut operands.iter().cloned())
            }
            Terminator::Return(ref a) => visit(&mut std::iter::empty(), &mut std::iter::once(*a)),
            Terminator::TailCall(ref a, ref b) => visit(
                &mut std::iter::empty(),
                &mut std::iter::once(*a).chain(std::iter::once(*b)),
            ),
        }
    }
}

pub enum Op<'a> {
    Apply(Register, Register, Register, Option<HashSet<Register>>),
    Box(Register, typ::Type, u64, Vec<'a, Register>),
    Closure(Register, FnName, Vec<'a, Register>),
    Int(Register, i64),
    String(Register, StringToken),
}

impl<'a> Op<'a> {
    pub fn visit<R, F>(&self, mut visit: F) -> R
    where
        F: for<'b> std::ops::FnMut(
            &'b mut dyn Iterator<Item = Register>,
            &'b mut dyn Iterator<Item = Register>,
        ) -> R,
    {
        match self {
            Op::Apply(ref dest, ref f, ref x, _) => visit(
                &mut std::iter::once(*dest),
                &mut std::iter::once(*f).chain(std::iter::once(*x)),
            ),
            Op::Box(ref dest, _, _, ref args) => {
                visit(&mut std::iter::once(*dest), &mut args.iter().cloned())
            }
            Op::Closure(ref dest, _, ref args) => {
                visit(&mut std::iter::once(*dest), &mut args.iter().cloned())
            }
            Op::Int(ref r, _) | Op::String(ref r, _) => {
                visit(&mut std::iter::once(*r), &mut std::iter::empty())
            }
        }
    }
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
