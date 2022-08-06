use bumpalo::{collections::Vec, vec, Bump};
use std::collections::{BTreeMap, HashMap};

use crate::cps::{AExp, CExp, Id, Lambda};
use crate::ssa::{Block, BlockName, Fn, FnBuilder, GlobalContext, Instr, Register, Terminator};

pub fn compile_fn<'cps, 'ssa>(
    global_ctx: &'ssa mut GlobalContext,
    bump: &'ssa Bump,
    lam: &'cps Lambda<'cps>,
) -> Result<(Fn<'ssa>, std::vec::Vec<Id>), ()> {
    let mut instrs = Vec::new_in(bump);
    let mut comp = FnBuilder::new();
    let entry = comp.fresh_block();
    let terminator = convert(global_ctx, bump, &mut comp, &mut instrs, lam.body)?;
    let FnBuilder {
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
    comp: &mut FnBuilder<'ssa>,
    out: &mut Vec<'ssa, Instr<'ssa>>,
    exp: &'cps CExp<'cps>,
) -> Result<Terminator<'ssa>, ()> {
    match exp {
        CExp::Apply(f, x, cont) => {
            let f = comp.get_reg(f);
            let x = comp.get_reg(x);
            let reg = comp.fresh_register();
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
