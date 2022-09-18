use crate::bytecode::{
    self, BlockIndex, Fun, FunIndex, LocalIndex, StackMapIndex, StringPoolIndex,
};
use crate::ssa::{self, BlockName, Fn, FnName, Op, Register, Terminator};
use crate::stringpool::{StringPool, StringToken};
use std::collections::HashMap;

pub struct Context<'pool> {
    funs: Vec<Fun>,
    fun_map: HashMap<FnName, FunIndex>,
    string_lits: Vec<String>,
    string_pool: &'pool StringPool<'pool>,
    string_pool_map: HashMap<StringToken, StringPoolIndex>,
}

pub struct FnBuilder<'ssa> {
    avail_locals: Vec<LocalIndex>,
    next_local: u16,
    reg_alloc: HashMap<Register, LocalIndex>,
    blocks: Vec<bytecode::Block>,
    block_map: HashMap<BlockName, BlockIndex>,
    next_stack_map: u16,
    fun: &'ssa Fn<'ssa>,
}

impl<'ssa> FnBuilder<'ssa> {
    pub fn new(fun: &'ssa Fn) -> Self {
        Self {
            avail_locals: vec![],
            next_local: 0,
            reg_alloc: HashMap::new(),
            blocks: vec![],
            block_map: HashMap::new(),
            next_stack_map: 0,
            fun,
        }
    }

    pub fn alloc_reg(&mut self, reg: Register) -> LocalIndex {
        match self.avail_locals.pop() {
            Some(idx) => {
                self.reg_alloc.insert(reg, idx);
                idx
            }
            None => {
                let idx = LocalIndex(self.next_local);
                self.reg_alloc.insert(reg, idx);
                self.next_local += 1;
                idx
            }
        }
    }

    fn new_stackmap(&mut self) -> StackMapIndex {
        let idx = StackMapIndex(self.next_stack_map);
        self.next_stack_map += 1;
        idx
    }

    pub fn compile<'pool>(
        &mut self,
        ctx: &Context<'pool>,
        block: &'ssa ssa::Block<'ssa>,
    ) -> Vec<bytecode::Instr> {
        let mut out = Vec::new();
        for instr in &block.instrs {
            match instr.op {
                Op::Apply(ref dest, ref f, ref x, _) => {
                    let dest = self.alloc_reg(*dest);
                    let sm_idx = self.new_stackmap();
                    let f = self.reg_alloc.get(f).unwrap();
                    let x = self.reg_alloc.get(x).unwrap();
                    out.push(bytecode::Instr::Push(*x));
                    out.push(bytecode::Instr::Apply(*f, sm_idx));
                    out.push(bytecode::Instr::Pop(dest));
                }
                Op::Box(ref dest, _, _, ref args) => {
                    for arg in args.iter().rev() {
                        let arg = self.reg_alloc.get(arg).unwrap();
                        out.push(bytecode::Instr::Push(*arg));
                    }
                    let dest = self.alloc_reg(*dest);
                    out.push(bytecode::Instr::Box(dest));
                }
                Op::Closure(ref dest, ref fun_name, ref args) => {
                    for arg in args.iter().rev() {
                        let arg = self.reg_alloc.get(arg).unwrap();
                        out.push(bytecode::Instr::Push(*arg));
                    }
                    let fun_idx = ctx.fun_map.get(fun_name).unwrap();
                    let dest = self.alloc_reg(*dest);
                    out.push(bytecode::Instr::Closure(dest, *fun_idx));
                }
                Op::Int(ref dest, n) => {
                    let n = n as u64;
                    let dest = self.alloc_reg(*dest);
                    out.push(bytecode::Instr::Load64(dest, n));
                }
                Op::String(ref dest, ref string_token) => {
                    let string_idx = ctx.string_pool_map.get(string_token).unwrap();
                    let dest = self.alloc_reg(*dest);
                    out.push(bytecode::Instr::LoadString(dest, *string_idx));
                }
            }
        }
        match &block.terminator {
            Terminator::Cmp(ref op, ref lhs, ref rhs, ref tru, ref fls) => {
                let lhs = self.reg_alloc.get(lhs).unwrap();
                let rhs = self.reg_alloc.get(rhs).unwrap();
                let tru = self.block_map.get(tru).unwrap();
                let fls = self.block_map.get(fls).unwrap();
                out.push(bytecode::Instr::Cmp(*op, *lhs, *rhs, *tru, *fls));
            }
            Terminator::Continue(ref block_name, ref args) => {
                let block = self.fun.blocks.get(block_name).unwrap();
                for (dest, src) in block.params.iter().zip(args.iter()) {
                    let dest = self.reg_alloc.get(dest).unwrap();
                    let src = self.reg_alloc.get(src).unwrap();
                    out.push(bytecode::Instr::Move(*dest, *src));
                }
                let block_idx = self.block_map.get(block_name).unwrap();
                out.push(bytecode::Instr::Break(*block_idx));
            }
            Terminator::Return(ref reg) => {
                let reg = self.reg_alloc.get(reg).unwrap();
                out.push(bytecode::Instr::Push(*reg));
                out.push(bytecode::Instr::Return);
            }
            Terminator::TailCall(ref f, ref arg) => {
                let f = self.reg_alloc.get(f).unwrap();
                let arg = self.reg_alloc.get(arg).unwrap();
                out.push(bytecode::Instr::Push(*arg));
                out.push(bytecode::Instr::TailCall(*f));
            }
        }
        out
    }
}
