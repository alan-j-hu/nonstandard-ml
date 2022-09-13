use crate::stringpool::StringToken;
use bumpalo::collections::Vec;
use std::collections::BTreeMap;

mod convert;
pub mod liveness;
pub use convert::compile;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(i32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockName(i32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnName(i32);

pub enum Instr<'a> {
    Apply(Register, Operand, Operand),
    Let(Def<'a>),
}

pub enum Terminator<'a> {
    CaseInt(Operand, BTreeMap<i64, BlockName>, BlockName),
    Continue(BlockName, Vec<'a, Operand>),
    Return(Operand),
    TailCall(Operand, Operand),
}

impl<'a> Terminator<'a> {
    pub fn visit<'s, R, F>(&'s self, mut visit: F) -> R
    where
        F: for<'b> std::ops::FnMut(
            &'b mut dyn Iterator<Item = BlockName>,
            &'b mut dyn Iterator<Item = &'s Operand>,
        ) -> R,
    {
        match self {
            Terminator::CaseInt(ref a, ref cases, default) => visit(
                &mut cases
                    .iter()
                    .map(|(_, b)| *b)
                    .chain(std::iter::once(*default)),
                &mut std::iter::once(a),
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

pub enum Operand {
    Register(Register),
    Int(i64),
    String(StringToken),
}

pub struct Def<'a> {
    pub register: Register,
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Box(u64, Vec<'a, Operand>),
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
    pub block_order: Vec<'a, BlockName>,
}

pub struct Program<'a> {
    pub fns: BTreeMap<FnName, Fn<'a>>,
    pub entry: FnName,
}
