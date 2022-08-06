use bumpalo::{collections::Vec, vec, Bump};
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::cps::{AExp, CExp, Id, Lambda};
use crate::ssa::{Block, BlockName, Fn, FnContext, GlobalContext, Instr, Register, Terminator};

pub struct Compiler<'ssa> {
    fn_ctx: FnContext,
    free_vars: std::vec::Vec<Id>,
    regs: HashMap<Id, Register>,
    conts: HashMap<Id, BlockName>,
    blocks: HashMap<BlockName, Block<'ssa>>,
}

impl<'ssa> Compiler<'ssa> {
    fn new() -> Self {
        Self {
            fn_ctx: FnContext::new(),
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
                let reg = self.fn_ctx.fresh_register();
                self.free_vars.push(*id);
                self.regs.insert(*id, reg);
                reg
            }
        }
    }
}

pub fn compile_fn<'cps, 'ssa>(
    global_ctx: &'ssa mut GlobalContext,
    bump: &'ssa Bump,
    lam: &'cps Lambda<'cps>,
) -> Result<(Fn<'ssa>, std::vec::Vec<Id>), ()> {
    let mut instrs = Vec::new_in(bump);
    let mut comp = Compiler::new();
    let entry = comp.fn_ctx.fresh_block();
    let terminator = convert(global_ctx, bump, &mut comp, &mut instrs, lam.body)?;
    let Compiler {
        free_vars, blocks, ..
    } = comp;
    let entry_block = Block {
        params: Vec::new_in(bump),
        instrs,
        terminator,
    };
    let fun = Fn {
        params: Vec::new_in(bump),
        blocks,
        entry,
    };
    Ok((fun, free_vars))
}

pub fn convert<'cps, 'ssa>(
    global_ctx: &'ssa mut GlobalContext,
    bump: &'ssa Bump,
    comp: &mut Compiler<'ssa>,
    out: &mut Vec<'ssa, Instr<'ssa>>,
    exp: &'cps CExp<'cps>,
) -> Result<Terminator<'ssa>, ()> {
    match exp {
        CExp::Apply(f, x, cont) => {
            let f = comp.get_reg(f);
            let x = comp.get_reg(x);
            let reg = comp.fn_ctx.fresh_register();
            out.push(Instr::Apply(reg, f, x));
            let block = match comp.conts.get(cont) {
                Some(block) => block,
                None => return Err(()),
            };
            Ok(Terminator::Continue(*block, vec![in bump; reg]))
        }
        CExp::Case(_, _) => todo!(),
        CExp::CaseInt(scrut, cases, default) => {
            let scrut = comp.get_reg(scrut);
            let mut jump_table = BTreeMap::new();
            for (num, cont) in cases {
                let block = match comp.conts.get(cont) {
                    Some(block) => block,
                    None => return Err(()),
                };
                jump_table.insert(*num, *block);
            }
            let default = match comp.conts.get(default) {
                Some(block) => block,
                None => return Err(()),
            };
            Ok(Terminator::CaseInt(scrut, jump_table, *default))
        }
        CExp::Continue(_, _) => todo!(),
        CExp::Let(def, _) => match def.exp {
            AExp::Integer(_) => todo!(),
            AExp::Lambda(ref lam) => {
                let _ = compile_fn(global_ctx, bump, lam)?;
                todo!()
            }
            AExp::String(_) => todo!(),
        },
        CExp::LetCont(_, _) => todo!(),
    }
}
