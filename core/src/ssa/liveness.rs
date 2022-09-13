use super::{Block, BlockName, Expr, Fn, Instr, Operand, Program, Register, Terminator};
use std::collections::{HashMap, HashSet};

pub fn analyze_program<'a>(program: &'a Program) {
    for (_, ref fun) in &program.fns {
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

    pub fn visit_fn<'a>(&mut self, fun: &'a Fn<'a>) {
        for name in &fun.block_order {
            let block = fun.blocks.get(name).unwrap();
            self.visit_block(name, block);
        }
    }

    fn visit_block<'a>(&mut self, name: &'a BlockName, block: &'a Block<'a>) {
        fn visit_operand<'a>(live: &mut HashSet<Register>, operand: &'a Operand) {
            match operand {
                Operand::Register(reg) => {
                    live.insert(*reg);
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
                visit_operand(&mut live, operand)
            }
        });

        for instr in block.instrs.iter().rev() {
            match instr {
                Instr::Apply(ref dest, ref a, ref b) => {
                    if !live.remove(dest) {
                        self.unused_regs.insert(*dest);
                    }
                    visit_operand(&mut live, a);
                    visit_operand(&mut live, b);
                }
                Instr::Let(ref def) => {
                    if !live.remove(&def.register) {
                        self.unused_regs.insert(def.register);
                    }
                    match def.expr {
                        Expr::Box(_, ref operands) => {
                            for operand in operands {
                                visit_operand(&mut live, operand)
                            }
                        }
                        Expr::Closure(_, ref captures) => {
                            for register in captures {
                                live.insert(*register);
                            }
                        }
                    }
                }
            }
        }
        self.live.insert(*name, live);
    }
}
