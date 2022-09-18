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
    let terminator = builder.compile(&mut ctx, bump, &mut instrs, exp)?;
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

    pub fn compile_val<'cps>(
        &mut self,
        out: &mut Vec<'ssa, Instr<'ssa>>,
        val: &'cps Val,
    ) -> Register {
        match val {
            Val::Integer(n) => {
                let register = self.fresh_register();
                out.push(Instr::new(Op::Int(register, *n)));
                register
            }
            Val::String(st) => {
                let register = self.fresh_register();
                out.push(Instr::new(Op::String(register, *st)));
                register
            }
            Val::Id(id) => self.get_reg(id),
        }
    }

    pub fn compile<'cps, 'any>(
        &mut self,
        ctx: &mut Context<'ssa>,
        bump: &'ssa Bump,
        out: &mut Vec<'ssa, Instr<'ssa>>,
        exp: &'cps CExp<'cps>,
    ) -> Result<Terminator<'ssa>, Error<'any>> {
        match exp {
            CExp::Apply(f, x, cont) => {
                let f = self.compile_val(out, f);
                let x = self.compile_val(out, x);
                if *cont == self.ret_addr {
                    Ok(Terminator::TailCall(f, x))
                } else {
                    let reg = self.fresh_register();
                    out.push(Instr::new(Op::Apply(reg, f, x, None)));
                    let block = self.get_block(&cont)?;
                    Ok(Terminator::Continue(block, vec![in bump; reg]))
                }
            }
            CExp::Case(_, _) => todo!(),
            CExp::Continue(cont, args) if *cont == self.ret_addr => {
                if args.len() == 1 {
                    let v = self.compile_val(out, &args[0]);
                    Ok(Terminator::Return(v))
                } else {
                    Err(Error::Internal(format!(
                        "Expected one cont arg, got {:?}",
                        args.len()
                    )))
                }
            }
            CExp::Cmp(cmp, lhs, rhs, tru, fls) => {
                let lhs = self.compile_val(out, lhs);
                let rhs = self.compile_val(out, rhs);
                let tru = self.get_block(&tru)?;
                let fls = self.get_block(&fls)?;
                Ok(Terminator::Cmp(*cmp, lhs, rhs, tru, fls))
            }
            CExp::Continue(cont, args) => {
                let block = self.get_block(&cont)?;
                let mut vals = Vec::new_in(bump);
                for arg in args {
                    vals.push(self.compile_val(out, arg))
                }
                Ok(Terminator::Continue(block, vals))
            }
            CExp::Let(def, cont) => {
                let register = self.fresh_register();
                match def.exp {
                    AExp::Box(ref typ, tag, ref subterms) => {
                        let mut new_subterms = Vec::with_capacity_in(subterms.len(), bump);
                        for subterm in subterms {
                            new_subterms.push(self.compile_val(out, subterm))
                        }
                        out.push(Instr::new(Op::Box(
                            register,
                            typ.clone(),
                            tag,
                            new_subterms,
                        )))
                    }
                    AExp::Lambda(ref lam) => {
                        let (fun, free_vars) = compile_fn(ctx, bump, lam)?;
                        let fn_name = ctx.add_fn(fun);
                        let mut env = Vec::new_in(bump);
                        for id in free_vars {
                            match self.regs.get(&id) {
                                None => {
                                    return Err(Error::Internal(format!("Unknown reg {:?}", &id)))
                                }
                                Some(reg) => env.push(*reg),
                            }
                        }
                        out.push(Instr::new(Op::Closure(register, fn_name, env)))
                    }
                };
                self.regs.insert(def.id, register);
                self.compile(ctx, bump, out, cont)
            }
            CExp::LetCont(def_conts, cont) => {
                let mut block_names = std::vec::Vec::new();
                for (name, params, body) in def_conts {
                    let block_name = self.fresh_block();
                    self.conts.insert(*name, block_name);
                    block_names.push((block_name, params, body))
                }
                for (name, old_params, body) in block_names {
                    let mut params = Vec::new_in(bump);
                    for param in old_params {
                        let reg = self.fresh_register();
                        self.regs.insert(*param, reg);
                        params.push(reg)
                    }
                    let mut instrs = Vec::new_in(bump);
                    let terminator = self.compile(ctx, bump, &mut instrs, *body)?;
                    let block = Block {
                        params,
                        instrs,
                        terminator,
                        killset: HashSet::new(),
                    };
                    self.blocks.insert(name, block);
                    self.block_order.push(name);
                }
                self.compile(ctx, bump, out, cont)
            }
        }
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
    let terminator = builder.compile(ctx, bump, &mut instrs, lam.body)?;
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
