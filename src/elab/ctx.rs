use std::collections::HashMap;

pub struct Ctx<'a> {
    pub parent: Option<&'a Ctx<'a>>,
    pub values: HashMap<String, ()>,
}

#[cfg(test)]
mod tests {
    use super::Ctx;
    use std::collections::HashMap;

    #[test]
    fn test() {
        fn f<'a>(n: isize, parent: &'a Ctx<'a>) {
            let ctx = Ctx {
                parent: Some(parent),
                values: HashMap::new(),
            };
            if n > 0 {
                f(n - 1, &ctx)
            }
        }
        let root = Ctx {
            parent: None,
            values: HashMap::new(),
        };
        f(3, &root)
    }
}
