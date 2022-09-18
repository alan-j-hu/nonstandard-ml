use super::{Block, BlockName, Expr, Fn, Op, Operand, Program, Register};
use std::collections::{HashMap, HashSet};

pub fn analyze<'a>(program: &'a mut Program<'a>) {
    for (_, fun) in &mut program.fns {
        let mut ctx = FnContext::new();
        ctx.visit_fn(fun);
    }
}

pub struct FnContext {
    live: HashMap<BlockName, HashSet<Register>>,
    unused_regs: HashSet<Register>,
}

impl FnContext {
    pub fn new() -> Self {
        Self {
            live: HashMap::new(),
            unused_regs: HashSet::new(),
        }
    }

    pub fn visit_fn<'a>(&mut self, fun: &mut Fn<'a>) {
        for name in &fun.block_order {
            let block = fun.blocks.get_mut(name).unwrap();
            self.visit_block(name, block);
        }
    }

    fn visit_block<'a>(&mut self, name: &BlockName, block: &mut Block<'a>) {
        fn visit_operand<'a>(
            live: &mut HashSet<Register>,
            killset: &mut HashSet<Register>,
            operand: &'a Operand,
        ) {
            match operand {
                Operand::Register(reg) => {
                    if live.insert(*reg) {
                        killset.insert(*reg);
                    }
                }
                _ => {}
            }
        }

        let mut live = HashSet::new();
        block.terminator.visit(|blocks, operands| {
            for name in blocks {
                match self.live.get(&name) {
                    Some(regs) => {
                        for reg in regs {
                            live.insert(*reg);
                        }
                    }
                    None => panic!(),
                }
            }
            for operand in operands {
                visit_operand(&mut live, &mut block.killset, operand)
            }
        });

        for instr in block.instrs.iter_mut().rev() {
            let mut killset = HashSet::new();
            match instr.op {
                Op::Apply(ref dest, ref a, ref b, ref mut roots) => {
                    if !live.remove(dest) {
                        self.unused_regs.insert(*dest);
                    }
                    *roots = Some(live.clone());
                    visit_operand(&mut live, &mut killset, a);
                    visit_operand(&mut live, &mut killset, b);
                }
                Op::Let(ref def) => {
                    if !live.remove(&def.register) {
                        self.unused_regs.insert(def.register);
                    }
                    match def.expr {
                        Expr::Box(_, _, ref operands) => {
                            for operand in operands {
                                visit_operand(&mut live, &mut killset, operand)
                            }
                        }
                        Expr::Closure(_, ref captures) => {
                            for register in captures {
                                if live.insert(*register) {
                                    killset.insert(*register);
                                }
                            }
                        }
                    }
                }
            }
            instr.killset = Some(killset);
        }

        for register in &block.params {
            if !live.remove(register) {
                self.unused_regs.insert(*register);
            }
        }

        self.live.insert(*name, live);
    }
}
