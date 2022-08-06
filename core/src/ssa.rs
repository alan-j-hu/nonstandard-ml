use crate::cps::Id;
use bumpalo::collections::{String, Vec};
use std::collections::{BTreeMap, HashMap};

mod convert;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Register(i32);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockName(i32);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnName(i32);

pub enum Instr<'a> {
    Apply(Register, Register, Register),
    Let(&'a Def<'a>),
}

pub enum Terminator<'a> {
    CaseInt(Register, BTreeMap<i64, BlockName>, BlockName),
    Continue(BlockName, Vec<'a, Register>),
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
    pub params: Vec<'a, Register>,
    pub blocks: HashMap<BlockName, Block<'a>>,
    pub entry: BlockName,
}

pub struct Program<'a> {
    pub fns: HashMap<FnName, Fn<'a>>,
    pub entry: FnName,
}

pub struct GlobalContext {
    fn_counter: i32,
}

impl GlobalContext {
    pub fn fresh_fun(&mut self) -> FnName {
        let id = self.fn_counter;
        self.fn_counter = id + 1;
        FnName(id)
    }
}

pub struct FnBuilder<'ssa> {
    block_counter: i32,
    reg_counter: i32,
    free_vars: std::vec::Vec<Id>,
    regs: HashMap<Id, Register>,
    conts: HashMap<Id, BlockName>,
    blocks: HashMap<BlockName, Block<'ssa>>,
}

impl<'ssa> FnBuilder<'ssa> {
    pub fn new() -> Self {
        Self {
            block_counter: 0,
            reg_counter: 0,
            free_vars: std::vec::Vec::new(),
            regs: HashMap::new(),
            conts: HashMap::new(),
            blocks: HashMap::new(),
        }
    }

    fn get_reg(&mut self, id: &Id) -> Register {
        match self.regs.get(id) {
            Some(reg) => *reg,
            None => {
                let reg = self.fresh_register();
                self.free_vars.push(*id);
                self.regs.insert(*id, reg);
                reg
            }
        }
    }

    pub fn fresh_block(&mut self) -> BlockName {
        let id = self.block_counter;
        self.block_counter = id + 1;
        BlockName(id)
    }

    pub fn fresh_register(&mut self) -> Register {
        let id = self.reg_counter;
        self.reg_counter = id + 1;
        Register(id)
    }
}
