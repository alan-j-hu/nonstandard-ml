use super::typ::Type;
use bumpalo::collections::Vec;

#[derive(Clone)]
pub struct Var {
    id: usize,
    typ: Type,
}

#[derive(Default)]
pub struct VarBuilder {
    counter: usize,
}

impl VarBuilder {
    pub fn fresh(&mut self, typ: Type) -> Var {
        let id = self.counter;
        self.counter = id + 1;
        Var { id, typ }
    }
}

pub struct Pat<'a> {
    pub inner: PatInner<'a>,
    pub vars: Vec<'a, Var>,
}

pub enum PatInner<'a> {
    Or(&'a Pat<'a>, &'a Pat<'a>),
    Wild,
}
