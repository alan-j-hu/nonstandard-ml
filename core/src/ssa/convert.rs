use bumpalo::{
    collections::{String, Vec},
    vec, Bump,
};
use std::collections::{BTreeMap, HashMap};

use super::*;
use crate::cps::{AExp, CExp, Id, Lambda, Val};

pub fn compile<'cps, 'ssa>(
    bump: &'ssa Bump,
    ret_addr: Id,
    exp: &'cps CExp<'cps>,
) -> Result<Program<'ssa>, ()> {
    let mut ctx = Context {
        fn_counter: 0,
        fns: BTreeMap::new(),
    };
    let mut builder = FnBuilder::new(ret_addr);
    let mut instrs = Vec::new_in(bump);
    let entry_block = builder.fresh_block();
    let terminator = convert(&mut ctx, bump, &mut builder, &mut instrs, exp)?;
    let block = Block {
        params: Vec::new_in(bump),
        instrs,
        terminator,
    };
    let param = builder.fresh_register();
    let FnBuilder { mut blocks, .. } = builder;
    blocks.insert(entry_block, block);
    let fun = Fn {
        param,
        blocks,
        entry: entry_block,
    };
    let entry_fn = ctx.add_fn(fun);
    let Context { fns, .. } = ctx;
    Ok(Program {
        fns,
        entry: entry_fn,
    })
}

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
    ret_addr: Id,
    free_vars: std::vec::Vec<Id>,
    regs: HashMap<Id, Register>,
    conts: HashMap<Id, BlockName>,
    blocks: BTreeMap<BlockName, Block<'ssa>>,
}

impl<'ssa> FnBuilder<'ssa> {
    pub fn new(ret_addr: Id) -> Self {
        Self {
            block_counter: 0,
            reg_counter: 0,
            ret_addr,
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
    let mut builder = FnBuilder::new(lam.ret_addr);
    let param = builder.fresh_register();
    builder.regs.insert(lam.param, param);
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
        param,
        blocks,
        entry,
    };
    Ok((fun, free_vars))
}

pub fn convert_val<'cps, 'ssa>(
    builder: &mut FnBuilder<'ssa>,
    bump: &'ssa Bump,
    val: &'cps Val<'cps>,
) -> Operand<'ssa> {
    match val {
        Val::Integer(n) => Operand::Int(*n),
        Val::String(ref s) => Operand::String(String::from_str_in(s, bump)),
        Val::Id(id) => Operand::Register(builder.get_reg(id)),
    }
}

pub fn convert<'cps, 'ssa>(
    ctx: &mut Context<'ssa>,
    bump: &'ssa Bump,
    builder: &mut FnBuilder<'ssa>,
    out: &mut Vec<'ssa, Instr<'ssa>>,
    exp: &'cps CExp<'cps>,
) -> Result<Terminator<'ssa>, ()> {
    match exp {
        CExp::Apply(f, x, cont) => {
            let f = convert_val(builder, bump, f);
            let x = convert_val(builder, bump, x);
            if *cont == builder.ret_addr {
                Ok(Terminator::TailCall(f, x))
            } else {
                let reg = builder.fresh_register();
                out.push(Instr::Apply(reg, f, x));
                let block = match builder.conts.get(cont) {
                    Some(block) => block,
                    None => panic!("a"),
                };
                Ok(Terminator::Continue(
                    *block,
                    vec![in bump; Operand::Register(reg)],
                ))
            }
        }
        CExp::Case(_, _) => todo!(),
        CExp::CaseInt(scrut, cases, default) => {
            let scrut = convert_val(builder, bump, scrut);
            let mut jump_table = BTreeMap::new();
            for (num, cont) in cases {
                let block = match builder.conts.get(cont) {
                    Some(block) => block,
                    None => panic!("b"),
                };
                jump_table.insert(*num, *block);
            }
            let default = match builder.conts.get(default) {
                Some(block) => block,
                None => panic!("c"),
            };
            Ok(Terminator::CaseInt(scrut, jump_table, *default))
        }
        CExp::Continue(cont, args) if *cont == builder.ret_addr => {
            if args.len() == 1 {
                let v = convert_val(builder, bump, &args[0]);
                Ok(Terminator::Return(v))
            } else {
                Err(())
            }
        }
        CExp::Continue(cont, args) => {
            let block = match builder.conts.get(cont) {
                None => return Err(()),
                Some(block) => *block,
            };
            let mut vals = Vec::new_in(bump);
            for id in args {
                vals.push(convert_val(builder, bump, id))
            }
            Ok(Terminator::Continue(block, vals))
        }
        CExp::Let(def, cont) => {
            let register = builder.fresh_register();
            let expr = match def.exp {
                AExp::Lambda(ref lam) => {
                    let (fun, free_vars) = compile_fn(ctx, bump, lam)?;
                    let fn_name = ctx.add_fn(fun);
                    let mut env = Vec::new_in(bump);
                    for id in free_vars {
                        match builder.regs.get(&id) {
                            None => panic!("e"),
                            Some(reg) => env.push(*reg),
                        }
                    }
                    Expr::Closure(fn_name, env)
                }
            };
            out.push(Instr::Let(Def { register, expr }));
            builder.regs.insert(def.id, register);
            convert(ctx, bump, builder, out, cont)
        }
        CExp::LetCont(def_conts, cont) => {
            let mut block_names = std::vec::Vec::new();
            for (name, params, body) in def_conts {
                let block_name = builder.fresh_block();
                builder.conts.insert(*name, block_name);
                block_names.push((block_name, params, body))
            }
            for (name, old_params, body) in block_names {
                let mut params = Vec::new_in(bump);
                for param in old_params {
                    let reg = builder.fresh_register();
                    builder.regs.insert(*param, reg);
                    params.push(reg)
                }
                let mut instrs = Vec::new_in(bump);
                let terminator = convert(ctx, bump, builder, &mut instrs, *body)?;
                let block = Block {
                    params,
                    instrs,
                    terminator,
                };
                builder.blocks.insert(name, block);
            }
            convert(ctx, bump, builder, out, cont)
        }
    }
}
