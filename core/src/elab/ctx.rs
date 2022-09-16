use super::{typ, typed};
use std::collections::HashMap;

pub struct Ctx<'a, 'ast: 'a, 'typed> {
    pub parent: Option<&'a Ctx<'a, 'ast, 'typed>>,
    pub scope: &'a Scope<'ast, 'typed>,
}

impl<'a, 'ast, 'typed> Ctx<'a, 'ast, 'typed> {
    pub fn new(scope: &'a Scope<'ast, 'typed>) -> Self {
        Self {
            parent: None,
            scope,
        }
    }

    pub fn extend(&'a self, scope: &'a Scope<'ast, 'typed>) -> Self {
        Ctx {
            parent: Some(self),
            scope,
        }
    }
}

pub struct Scope<'ast, 'typed> {
    pub vals: HashMap<&'ast str, (typed::Var<'typed>, typ::Scheme)>,
}

impl<'ast, 'typed> Scope<'ast, 'typed> {
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
        fn f<'a, 'ast, 'typed>(n: isize, parent: &'a Ctx<'a, 'ast, 'typed>) {
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
