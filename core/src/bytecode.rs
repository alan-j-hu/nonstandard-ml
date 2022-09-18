#[derive(Copy, Clone)]
pub enum Cmp {
    Lt,
    Le,
    Eq,
    Gt,
    Ge,
}

#[derive(Copy, Clone)]
pub struct BlockIndex(u16);

#[derive(Copy, Clone)]
pub struct LocalIndex(u16);

#[derive(Copy, Clone)]
pub struct StackMapIndex(u16);

pub struct Fun {
    local_count: u16,
    blocks: Box<[Block]>,
    stack_maps: Box<[StackMap]>,
}

pub struct Block {
    instrs: Box<[Instr]>,
}

pub enum Instr {
    Apply(StackMapIndex),
    Break(BlockIndex),
    Cmp(Cmp, LocalIndex, LocalIndex, BlockIndex, BlockIndex),
    Load8(LocalIndex, u8),
    Load16(LocalIndex, u16),
    Load32(LocalIndex, u32),
    Load64(LocalIndex, u64),
    Pop(LocalIndex),
    Push(LocalIndex),
    Return(LocalIndex),
}

pub struct StackMap {}
