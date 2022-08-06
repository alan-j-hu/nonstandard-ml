use bumpalo::{
    collections::{String, Vec},
    vec, Bump,
};
use std::collections::{BTreeMap, HashMap};

use crate::cps::{AExp, CExp, Id, Lambda};
use crate::ssa::{Block, BlockName, Def, Expr, Fn, FnName, Instr, Register, Terminator};

pub struct Context<'a> {
    fn_counter: i32,
    fns: BTreeMap<FnName, Fn<'a>>,
}

impl<'a> Context<'a> {
    pub fn add_fn(&mut self, fun: Fn<'a>) -> FnName {
        let id = self.fn_counter;
        let name = FnName(id);
        self.fn_counter = id + 1;
        self.fns.insert(name, fun);
        name
    }
}

pub struct FnBuilder<'ssa> {
    block_counter: i32,
    reg_counter: i32,
    free_vars: std::vec::Vec<Id>,
    regs: HashMap<Id, Register>,
    conts: HashMap<Id, BlockName>,
    blocks: BTreeMap<BlockName, Block<'ssa>>,
}

impl<'ssa> FnBuilder<'ssa> {
    pub fn new() -> Self {
        Self {
            block_counter: 0,
            reg_counter: 0,
            free_vars: std::vec::Vec::new(),
            regs: HashMap::new(),
            conts: HashMap::new(),
            blocks: BTreeMap::new(),
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

pub fn compile_fn<'cps, 'ssa>(
    ctx: &mut Context<'ssa>,
    bump: &'ssa Bump,
    lam: &'cps Lambda<'cps>,
) -> Result<(Fn<'ssa>, std::vec::Vec<Id>), ()> {
    let mut instrs = Vec::new_in(bump);
    let mut builder = FnBuilder::new();
    let entry = builder.fresh_block();
    let terminator = convert(ctx, bump, &mut builder, &mut instrs, lam.body)?;
    let FnBuilder {
        free_vars,
        mut blocks,
        ..
    } = builder;
    let entry_block = Block {
        params: Vec::new_in(bump),
        instrs,
        terminator,
    };
    blocks.insert(entry, entry_block);
    let fun = Fn {
        params: Vec::new_in(bump),
        blocks,
        entry,
    };
    Ok((fun, free_vars))
}

pub fn convert<'cps, 'ssa>(
    ctx: &mut Context<'ssa>,
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
        CExp::Continue(cont, args) => match comp.conts.get(cont) {
            None => Err(()),
            Some(block) => {
                let mut regs = Vec::new_in(bump);
                for id in args {
                    match comp.regs.get(id) {
                        None => return Err(()),
                        Some(reg) => regs.push(*reg),
                    }
                }
                Ok(Terminator::Continue(*block, regs))
            }
        },
        CExp::Let(def, cont) => {
            let register = comp.fresh_register();
            let expr = match def.exp {
                AExp::Integer(n) => Expr::Int(n),
                AExp::Lambda(ref lam) => {
                    let (fun, free_vars) = compile_fn(ctx, bump, lam)?;
                    let fn_name = ctx.add_fn(fun);
                    let mut env = Vec::new_in(bump);
                    for id in free_vars {
                        match comp.regs.get(&id) {
                            None => return Err(()),
                            Some(reg) => env.push(*reg),
                        }
                    }
                    Expr::Closure(fn_name, env)
                }
                AExp::String(ref s) => Expr::String(String::from_str_in(s, bump)),
            };
            out.push(Instr::Let(Def { register, expr }));
            convert(ctx, bump, comp, out, cont)
        }
        CExp::LetCont(def_conts, cont) => {
            let mut block_names = std::vec::Vec::new();
            for (name, params, body) in def_conts {
                let block_name = comp.fresh_block();
                comp.conts.insert(*name, block_name);
                block_names.push((block_name, params, body))
            }
            for (name, old_params, body) in block_names {
                let mut params = Vec::new_in(bump);
                for param in old_params {
                    let reg = comp.fresh_register();
                    comp.regs.insert(*param, reg);
                    params.push(reg)
                }
                let mut instrs = Vec::new_in(bump);
                let terminator = convert(ctx, bump, comp, &mut instrs, *body)?;
                let block = Block {
                    params,
                    instrs,
                    terminator,
                };
                comp.blocks.insert(name, block);
            }
            convert(ctx, bump, comp, out, cont)
        }
    }
}
