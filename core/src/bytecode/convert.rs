use crate::bytecode::{BlockIndex, LocalIndex};
use crate::ssa::{BlockName, Register};
use std::collections::HashMap;

pub struct FnBuilder {
    avail_locals: Vec<LocalIndex>,
    next_local: u16,
    reg_alloc: HashMap<Register, LocalIndex>,
    blocks: HashMap<BlockName, BlockIndex>,
}

impl FnBuilder {
    pub fn new() -> Self {
        Self {
            avail_locals: vec![],
            next_local: 0,
            reg_alloc: HashMap::new(),
            blocks: HashMap::new(),
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
}
