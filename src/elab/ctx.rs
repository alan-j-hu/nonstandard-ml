use super::{typ, typed};
use std::collections::HashMap;

pub struct Ctx<'a, 'ast: 'a> {
    pub parent: Option<&'a Ctx<'a, 'ast>>,
    pub scope: &'a Scope<'ast>,
}

impl<'a, 'ast> Ctx<'a, 'ast> {
    pub fn new(scope: &'a Scope<'ast>) -> Self {
        Self {
            parent: None,
            scope,
        }
    }

    pub fn extend(&'a self, scope: &'a Scope<'ast>) -> Ctx<'a, 'ast> {
        Ctx {
            parent: Some(self),
            scope,
        }
    }
}

pub struct Scope<'ast> {
    pub vals: HashMap<&'ast str, (typed::Var, typ::Forall)>,
}

impl<'ast> Scope<'ast> {
    pub fn new() -> Self {
        Self {
            vals: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        fn f<'a, 'ast>(n: isize, parent: &'a Ctx<'a, 'ast>) {
            let ctx = Ctx {
                parent: Some(parent),
                scope: &Scope::new(),
            };
            if n > 0 {
                f(n - 1, &ctx)
            }
        }
        let root = Ctx {
            parent: None,
            scope: &Scope::new(),
        };
        f(3, &root)
    }
}
