use bumpalo::{collections::Vec, vec, Bump};
use std::collections::{BTreeMap, HashMap};

use super::*;
use crate::cps::{AExp, CExp, Id, Lambda, Val};
use crate::diagnostic::Error;
use crate::elab::typ;

pub fn compile<'cps, 'ssa, 'any>(
    bump: &'ssa Bump,
    ret_addr: Id,
    exp: &'cps CExp<'cps>,
) -> Result<Program<'ssa>, Error<'any>> {
    let mut ctx = Context {
        fn_counter: 0,
        fns: BTreeMap::new(),
    };
    let mut builder = FnBuilder::new(ret_addr, bump);
    let mut instrs = Vec::new_in(bump);
    let entry_block = builder.fresh_block();
    let terminator = convert(&mut ctx, bump, &mut builder, &mut instrs, exp)?;
    let block = Block {
        params: Vec::new_in(bump),
        instrs,
        terminator,
        killset: HashSet::new(),
    };
    builder.block_order.push(entry_block);
    let param = builder.fresh_register();
    let FnBuilder {
        mut blocks,
        block_order,
        ..
    } = builder;
    blocks.insert(entry_block, block);
    let fun = Fn {
        domain: typ::Type::solved(typ::Expr::Integer),
        param,
        blocks,
        entry: entry_block,
        block_order,
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
    block_order: Vec<'ssa, BlockName>,
}

impl<'ssa> FnBuilder<'ssa> {
    pub fn new(ret_addr: Id, bump: &'ssa Bump) -> Self {
        Self {
            block_counter: 0,
            reg_counter: 0,
            ret_addr,
            free_vars: std::vec::Vec::new(),
            regs: HashMap::new(),
            conts: HashMap::new(),
            blocks: BTreeMap::new(),
            block_order: Vec::new_in(bump),
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

    fn get_block<'any>(&self, id: &Id) -> Result<BlockName, Error<'any>> {
        match self.conts.get(id) {
            None => Err(Error::Internal(format!("Cont {:?} not found", id))),
            Some(block) => Ok(*block),
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

pub fn compile_fn<'cps, 'ssa, 'any>(
    ctx: &mut Context<'ssa>,
    bump: &'ssa Bump,
    lam: &'cps Lambda<'cps>,
) -> Result<(Fn<'ssa>, std::vec::Vec<Id>), Error<'any>> {
    let mut instrs = Vec::new_in(bump);
    let mut builder = FnBuilder::new(lam.ret_addr, bump);
    let param = builder.fresh_register();
    builder.regs.insert(lam.param, param);
    let entry = builder.fresh_block();
    let terminator = convert(ctx, bump, &mut builder, &mut instrs, lam.body)?;
    builder.block_order.push(entry);
    let FnBuilder {
        free_vars,
        mut blocks,
        block_order,
        ..
    } = builder;
    let entry_block = Block {
        params: Vec::new_in(bump),
        instrs,
        terminator,
        killset: HashSet::new(),
    };
    blocks.insert(entry, entry_block);
    let fun = Fn {
        domain: lam.domain.clone(),
        param,
        blocks,
        entry,
        block_order,
    };
    Ok((fun, free_vars))
}

pub fn convert_val<'cps, 'ssa>(builder: &mut FnBuilder<'ssa>, val: &'cps Val) -> Operand {
    match val {
        Val::Integer(n) => Operand::Int(*n),
        Val::String(st) => Operand::String(*st),
        Val::Id(id) => Operand::Register(builder.get_reg(id)),
    }
}

pub fn convert<'cps, 'ssa, 'any>(
    ctx: &mut Context<'ssa>,
    bump: &'ssa Bump,
    builder: &mut FnBuilder<'ssa>,
    out: &mut Vec<'ssa, Instr<'ssa>>,
    exp: &'cps CExp<'cps>,
) -> Result<Terminator<'ssa>, Error<'any>> {
    match exp {
        CExp::Apply(f, x, cont) => {
            let f = convert_val(builder, f);
            let x = convert_val(builder, x);
            if *cont == builder.ret_addr {
                Ok(Terminator::TailCall(f, x))
            } else {
                let reg = builder.fresh_register();
                out.push(Instr::new(Op::Apply(reg, f, x)));
                let block = builder.get_block(&cont)?;
                Ok(Terminator::Continue(
                    block,
                    vec![in bump; Operand::Register(reg)],
                ))
            }
        }
        CExp::Case(_, _) => todo!(),
        CExp::Continue(cont, args) if *cont == builder.ret_addr => {
            if args.len() == 1 {
                let v = convert_val(builder, &args[0]);
                Ok(Terminator::Return(v))
            } else {
                Err(Error::Internal(format!(
                    "Expected one cont arg, got {:?}",
                    args.len()
                )))
            }
        }
        CExp::Continue(cont, args) => {
            let block = builder.get_block(&cont)?;
            let mut vals = Vec::new_in(bump);
            for arg in args {
                vals.push(convert_val(builder, arg))
            }
            Ok(Terminator::Continue(block, vals))
        }
        CExp::Eq(lhs, rhs, eq, ne) => {
            let lhs = convert_val(builder, lhs);
            let rhs = convert_val(builder, rhs);
            let eq = builder.get_block(&eq)?;
            let ne = builder.get_block(&ne)?;
            Ok(Terminator::Lt(lhs, rhs, eq, ne))
        }
        CExp::Lt(lhs, rhs, lt, ge) => {
            let lhs = convert_val(builder, lhs);
            let rhs = convert_val(builder, rhs);
            let lt = builder.get_block(&lt)?;
            let ge = builder.get_block(&ge)?;
            Ok(Terminator::Lt(lhs, rhs, lt, ge))
        }
        CExp::Let(def, cont) => {
            let register = builder.fresh_register();
            let expr = match def.exp {
                AExp::Box(ref typ, tag, ref subterms) => {
                    let mut new_subterms = Vec::with_capacity_in(subterms.len(), bump);
                    for subterm in subterms {
                        new_subterms.push(convert_val(builder, subterm))
                    }
                    Expr::Box(typ.clone(), tag, new_subterms)
                }
                AExp::Lambda(ref lam) => {
                    let (fun, free_vars) = compile_fn(ctx, bump, lam)?;
                    let fn_name = ctx.add_fn(fun);
                    let mut env = Vec::new_in(bump);
                    for id in free_vars {
                        match builder.regs.get(&id) {
                            None => return Err(Error::Internal(format!("Unknown reg {:?}", &id))),
                            Some(reg) => env.push(*reg),
                        }
                    }
                    Expr::Closure(fn_name, env)
                }
            };
            out.push(Instr::new(Op::Let(Def { register, expr })));
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
                    killset: HashSet::new(),
                };
                builder.blocks.insert(name, block);
                builder.block_order.push(name);
            }
            convert(ctx, bump, builder, out, cont)
        }
    }
}
