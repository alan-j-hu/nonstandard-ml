use super::{Block, BlockName, Expr, Fn, Instr, Operand, Program, Register, Terminator};
use std::collections::HashSet;

pub fn analyze_program<'a>(program: &'a Program) {
    for (name, ref fun) in &program.fns {
        let mut ctx = FnContext::new();
        ctx.visit_fn(fun);
    }
}

pub struct FnContext {
    live: HashSet<Register>,
    visited: HashSet<BlockName>,
    unused_regs: HashSet<Register>,
}

impl FnContext {
    pub fn new() -> Self {
        Self {
            live: HashSet::new(),
            visited: HashSet::new(),
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
        match &block.terminator {
            Terminator::CaseInt(a, ref cases, ref default) => {
                self.visit_operand(a);
                for (_, ref block) in cases {
                    assert!(self.visited.contains(block))
                }
                assert!(self.visited.contains(default))
            }
            Terminator::Continue(ref block, ref operands) => {
                if !self.visited.contains(block) {
                    println!("Has not visited {:?}", block);
                }
                assert!(self.visited.contains(block));
                for operand in operands {
                    self.visit_operand(operand)
                }
            }
            Terminator::Return(a) => self.visit_operand(a),
            Terminator::TailCall(a, b) => {
                self.visit_operand(a);
                self.visit_operand(b)
            }
        }
        for instr in block.instrs.iter().rev() {
            match instr {
                Instr::Apply(ref dest, ref a, ref b) => {
                    if !self.live.remove(dest) {
                        self.unused_regs.insert(*dest);
                    }
                    self.visit_operand(a);
                    self.visit_operand(b);
                }
                Instr::Let(ref def) => {
                    if !self.live.remove(&def.register) {
                        self.unused_regs.insert(def.register);
                    }
                    match def.expr {
                        Expr::Box(_, ref operands) => {
                            for operand in operands {
                                self.visit_operand(operand)
                            }
                        }
                        Expr::Closure(_, ref captures) => {
                            for register in captures {
                                self.live.insert(*register);
                            }
                        }
                    }
                }
            }
        }
        self.visited.insert(*name);
    }

    fn visit_operand<'a>(&mut self, operand: &'a Operand) {
        match operand {
            Operand::Register(reg) => {
                self.live.insert(*reg);
            }
            _ => {}
        }
    }
}
