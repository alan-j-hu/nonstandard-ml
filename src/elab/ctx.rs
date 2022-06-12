use super::typed;
use std::collections::HashMap;

pub struct Ctx<'a, 'ast: 'a> {
    pub parent: Option<&'a Ctx<'a, 'ast>>,
    pub values: &'a HashMap<&'ast str, typed::Var>,
}

impl<'a, 'ast> Ctx<'a, 'ast> {
    pub fn new(values: &'a HashMap<&'ast str, typed::Var>) -> Self {
        Self {
            parent: None,
            values,
        }
    }

    pub fn extend(&'a self, values: &'a HashMap<&'ast str, typed::Var>) -> Ctx<'a, 'ast> {
        Ctx {
            parent: Some(self),
            values,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Ctx;
    use std::collections::HashMap;

    #[test]
    fn test() {
        fn f<'a, 'ast>(n: isize, parent: &'a Ctx<'a, 'ast>) {
            let ctx = Ctx {
                parent: Some(parent),
                values: &HashMap::new(),
            };
            if n > 0 {
                f(n - 1, &ctx)
            }
        }
        let root = Ctx {
            parent: None,
            values: &HashMap::new(),
        };
        f(3, &root)
    }
}
