use super::elab::typ::Type;
use super::syntax::ast;
use bumpalo::Bump;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub mod ctx;
pub mod typ;
pub mod typed;

#[derive(Default)]
pub struct Elaborator {
    var_counter: u64,
}

impl Elaborator {
    pub fn rename_pat<'ast, 'typed>(
        &mut self,
        bump: &'typed Bump,
        out: &mut HashMap<&'ast str, Type>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
    ) -> Result<&'typed mut typed::Pat<'typed>, ()> {
        match pat.node {
            ast::Pat::Or(pat1, pat2) => {
                let mut map = HashMap::new();
                let pat1 = self.rename_pat(bump, &mut map, pat1)?;
                let mut names: HashMap<&'ast str, (Type, bool)> =
                    map.iter().map(|(k, v)| (*k, (v.clone(), true))).collect();
                let pat2 = self.check_names_same(bump, &mut names, pat2)?;
                for (k, v) in map.iter() {
                    match out.entry(k) {
                        Entry::Occupied(_) => return Err(()),
                        Entry::Vacant(va) => {
                            va.insert(v.clone());
                        }
                    }
                }
                Ok(bump.alloc(typed::Pat {
                    inner: typed::PatInner::Or(pat1, pat2),
                }))
            }
            ast::Pat::Var(name) => match out.entry(name) {
                Entry::Occupied(_) => Err(()),
                Entry::Vacant(va) => {
                    va.insert(Type::unsolved());
                    Ok(bump.alloc(typed::Pat {
                        inner: typed::PatInner::Wild,
                    }))
                }
            },
            ast::Pat::Wild => Ok(bump.alloc(typed::Pat {
                inner: typed::PatInner::Wild,
            })),
        }
    }

    fn check_names_same<'ast, 'typed>(
        &self,
        bump: &'typed Bump,
        names: &mut HashMap<&'ast str, (Type, bool)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
    ) -> Result<&'typed mut typed::Pat<'typed>, ()> {
        let pat = self.remap_pat(bump, names, pat)?;
        for (k, (_, avail)) in names.iter() {
            if *avail {
                return Err(());
            }
        }
        Ok(pat)
    }

    fn remap_pat<'ast, 'typed>(
        &self,
        bump: &'typed Bump,
        names: &mut HashMap<&'ast str, (Type, bool)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
    ) -> Result<&'typed mut typed::Pat<'typed>, ()> {
        match pat.node {
            ast::Pat::Or(pat1, pat2) => {
                let mut map = names
                    .iter()
                    .filter(|(_, (_, b))| *b)
                    .map(|(k, v)| (*k, v.clone()))
                    .collect();
                let pat1 = self.remap_pat(bump, names, pat1)?;
                let pat2 = self.check_names_same(bump, &mut map, pat2)?;
                Ok(bump.alloc(typed::Pat {
                    inner: typed::PatInner::Or(pat1, pat2),
                }))
            }
            ast::Pat::Var(name) => match names.entry(name) {
                Entry::Occupied(mut o) => {
                    if o.get_mut().1 {
                        o.get_mut().1 = false;
                        Ok(bump.alloc(typed::Pat {
                            inner: typed::PatInner::Wild,
                        }))
                    } else {
                        Err(())
                    }
                }
                Entry::Vacant(_) => Err(()),
            },
            ast::Pat::Wild => Ok(bump.alloc(typed::Pat {
                inner: typed::PatInner::Wild,
            })),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::ast;

    fn make_or<'a>(
        bump: &'a Bump,
        pat1: ast::Pat<'a>,
        pat2: ast::Pat<'a>,
    ) -> &'a ast::Located<ast::Pat<'a>> {
        bump.alloc(ast::Located {
            span: Default::default(),
            node: ast::Pat::Or(
                bump.alloc(ast::Located {
                    span: Default::default(),
                    node: pat1,
                }),
                bump.alloc(ast::Located {
                    span: Default::default(),
                    node: pat2,
                }),
            ),
        })
    }

    #[test]
    fn test_ok() {
        let bump = Bump::new();
        let pat = make_or(&bump, ast::Pat::Var("x"), ast::Pat::Var("x"));
        let mut hm = HashMap::new();
        assert!(<Elaborator as Default>::default()
            .rename_pat(&bump, &mut hm, &pat)
            .is_ok());
        assert!(hm.contains_key("x"))
    }

    #[test]
    fn test_missing() {
        let bump = Bump::new();
        let pat = make_or(&bump, ast::Pat::Var("x"), ast::Pat::Wild);
        let mut hm = HashMap::new();
        assert!(<Elaborator as Default>::default()
            .rename_pat(&bump, &mut hm, &pat)
            .is_err())
    }

    #[test]
    fn test_extra() {
        let bump = Bump::new();
        let pat = make_or(&bump, ast::Pat::Wild, ast::Pat::Var("x"));
        let mut hm = HashMap::new();
        assert!(<Elaborator as Default>::default()
            .rename_pat(&bump, &mut hm, &pat)
            .is_err())
    }
}
