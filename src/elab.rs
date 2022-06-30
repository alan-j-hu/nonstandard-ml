use self::typ::Type;
use super::syntax::ast;
use bumpalo::{
    collections::{String, Vec},
    vec, Bump,
};
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub mod ctx;
pub mod typ;
pub mod typed;

fn generalize<'ast>(
    mono: &HashMap<&'ast str, (typed::Var, typ::Type)>,
) -> HashMap<&'ast str, (typed::Var, typ::Forall)> {
    let mut poly = HashMap::new();
    for (k, (var, typ)) in mono.iter() {
        poly.insert(*k, (var.clone(), typ::Forall(typ.clone())));
    }
    poly
}

#[derive(Default)]
pub struct Elaborator {
    var_builder: typed::VarBuilder,
    typ_builder: typ::Builder,
}

impl Elaborator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn elab_dec<'a, 'ast, 'typed>(
        &mut self,
        bump: &'typed Bump,
        out: &mut Vec<'typed, typed::Dec<'typed>>,
        ctx: &ctx::Ctx<'a, 'ast>,
        dec: &'ast ast::Located<ast::Dec<'ast>>,
    ) -> Result<ctx::Scope<'ast>, ()> {
        match dec.node {
            ast::Dec::And(dec1, dec2) => {
                let out1 = self.elab_dec(bump, out, ctx, dec1)?;
                let mut out2 = self.elab_dec(bump, out, ctx, dec2)?;
                for (k, v) in out1.vals.iter() {
                    match out2.vals.entry(k) {
                        Entry::Occupied(_) => return Err(()),
                        Entry::Vacant(va) => {
                            va.insert(v.clone());
                        }
                    }
                }
                Ok(out2)
            }
            ast::Dec::Loc(dec1, dec2) => {
                let out1 = self.elab_dec(bump, out, ctx, dec1)?;
                self.elab_dec(bump, out, &ctx.extend(&out1), dec2)
            }
            ast::Dec::Seq(dec1, dec2) => {
                let out1 = self.elab_dec(bump, out, ctx, dec1)?;
                let mut out2 = self.elab_dec(bump, out, &ctx.extend(&out1), dec2)?;
                for (k, v) in out1.vals.iter() {
                    match out2.vals.entry(k) {
                        Entry::Occupied(_) => {}
                        Entry::Vacant(va) => {
                            va.insert(v.clone());
                        }
                    }
                }
                Ok(out2)
            }
            ast::Dec::Val(pat, exp) => {
                let mut vars = HashMap::new();
                let typ = self.typ_builder.unsolved();
                let pat = self.rename_pat(bump, &mut vars, pat, typ.clone())?;
                let exp = self.elab_exp(bump, ctx, exp, typ)?;
                out.push(typed::Dec::Val(pat, exp));
                Ok(ctx::Scope {
                    vals: generalize(&vars),
                })
            }
        }
    }

    pub fn elab_exp<'a, 'ast, 'typed>(
        &mut self,
        bump: &'typed Bump,
        ctx: &ctx::Ctx<'a, 'ast>,
        exp: &'ast ast::Located<ast::Exp<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Exp<'typed>, ()> {
        match exp.node {
            ast::Exp::Apply(exp1, exp2) => {
                let dom = self.typ_builder.unsolved();
                let arr = self
                    .typ_builder
                    .solved(typ::Expr::Arrow(dom.clone(), typ.clone()));
                let exp1 = self.elab_exp(bump, ctx, exp1, arr)?;
                let exp2 = self.elab_exp(bump, ctx, exp2, dom)?;
                Ok(typed::Exp::Apply(bump.alloc(exp1), bump.alloc(exp2)))
            }
            ast::Exp::Case(exp, cases) => {
                let from = self.typ_builder.unsolved();
                let exp = self.elab_exp(bump, ctx, exp, from.clone())?;
                let cases = self.elab_cases(bump, ctx, cases, from, typ)?;
                Ok(typed::Exp::Case(bump.alloc(exp), cases))
            }
            ast::Exp::Integer(_) => Err(()),
            ast::Exp::Lambda(cases) => {
                let dom = self.typ_builder.unsolved();
                let codom = self.typ_builder.unsolved();
                let arr = typ::Expr::Arrow(dom.clone(), codom.clone());
                match typ.unify(self.typ_builder.solved(arr)) {
                    Ok(()) => {}
                    Err(_) => return Err(()),
                }
                let cases = self.elab_cases(bump, ctx, cases, dom, codom)?;
                Ok(typed::Exp::Lambda(cases))
            }
            ast::Exp::Let(dec, exp) => {
                let mut vec = vec![in bump];
                let scope = self.elab_dec(bump, &mut vec, ctx, dec)?;
                let exp = self.elab_exp(bump, &ctx.extend(&scope), exp, typ)?;
                Ok(typed::Exp::Let(
                    bump.alloc_slice_fill_iter(vec.drain(..)),
                    bump.alloc(exp),
                ))
            }
            ast::Exp::String(s) => {
                match typ.unify(self.typ_builder.solved(typ::Expr::String)) {
                    Err(_) => return Err(()),
                    Ok(()) => {}
                }
                Ok(typed::Exp::String(String::from_str_in(s, bump)))
            }
            ast::Exp::Var(v) => match ctx.scope.vals.get(v) {
                None => Err(()),
                Some((var, _sc)) => Ok(typed::Exp::Var(var.clone())),
            },
        }
    }

    pub fn elab_cases<'a, 'ast, 'typed>(
        &mut self,
        bump: &'typed Bump,
        ctx: &ctx::Ctx<'a, 'ast>,
        cases: &'ast [ast::Case<'ast>],
        from: typ::Type,
        to: typ::Type,
    ) -> Result<&'typed [typed::Case<'typed>], ()> {
        let mut vec = vec![in bump];
        for ast::Case { lhs, rhs } in cases {
            let mut vars = HashMap::new();
            let lhs = self.rename_pat(bump, &mut vars, lhs, from.clone())?;
            let rhs = self.elab_exp(
                bump,
                &ctx.extend(&ctx::Scope {
                    vals: generalize(&vars),
                }),
                rhs,
                to.clone(),
            )?;
            vec.push(typed::Case { lhs, rhs })
        }
        Ok(bump.alloc_slice_fill_iter(vec.drain(..)))
    }

    pub fn rename_pat<'ast, 'typed>(
        &mut self,
        bump: &'typed Bump,
        out: &mut HashMap<&'ast str, (typed::Var, typ::Type)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Pat<'typed>, ()> {
        match pat.node {
            ast::Pat::As(name, pat) => {
                let mut pat = self.rename_pat(bump, out, pat, typ.clone())?;
                match out.entry(name) {
                    Entry::Occupied(_) => Err(()),
                    Entry::Vacant(va) => {
                        let var = self.var_builder.fresh();
                        va.insert((var.clone(), typ));
                        pat.vars.push(var);
                        Ok(pat)
                    }
                }
            }
            ast::Pat::Or(pat1, pat2) => {
                let mut map = HashMap::new();
                let pat1 = self.rename_pat(bump, &mut map, pat1, typ.clone())?;
                let mut names = map
                    .iter()
                    .map(|(k, (va, typ))| (*k, (va.clone(), typ.clone(), true)))
                    .collect();
                let pat2 = self.check_names_same(bump, &mut names, pat2, typ)?;
                for (k, v) in map.iter() {
                    match out.entry(k) {
                        Entry::Occupied(_) => return Err(()),
                        Entry::Vacant(va) => {
                            va.insert(v.clone());
                        }
                    }
                }
                Ok(typed::Pat {
                    inner: typed::PatInner::Or(bump.alloc(pat1), bump.alloc(pat2)),
                    vars: vec![in bump],
                })
            }
            ast::Pat::Var(name) => match out.entry(name) {
                Entry::Occupied(_) => Err(()),
                Entry::Vacant(va) => {
                    let var = self.var_builder.fresh();
                    va.insert((var.clone(), typ));
                    Ok(typed::Pat {
                        inner: typed::PatInner::Wild,
                        vars: vec![in bump; var],
                    })
                }
            },
            ast::Pat::Wild => Ok(typed::Pat {
                inner: typed::PatInner::Wild,
                vars: vec![in bump],
            }),
        }
    }

    fn check_names_same<'ast, 'typed>(
        &self,
        bump: &'typed Bump,
        names: &mut HashMap<&'ast str, (typed::Var, typ::Type, bool)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Pat<'typed>, ()> {
        let pat = self.remap_pat(bump, names, pat, typ)?;
        for (_k, (_, _, avail)) in names.iter() {
            if *avail {
                return Err(());
            }
        }
        Ok(pat)
    }

    fn remap_pat<'ast, 'typed>(
        &self,
        bump: &'typed Bump,
        names: &mut HashMap<&'ast str, (typed::Var, typ::Type, bool)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Pat<'typed>, ()> {
        match pat.node {
            ast::Pat::As(name, pat) => {
                let mut pat = self.remap_pat(bump, names, pat, typ.clone())?;
                match names.entry(name) {
                    Entry::Occupied(mut o) => {
                        if o.get().2 {
                            o.get_mut().2 = false;
                            let (var, typ2, _) = o.get();
                            match typ.unify(typ2.clone()) {
                                Ok(()) => {
                                    pat.vars.push(var.clone());
                                    Ok(pat)
                                }
                                Err(_) => Err(()),
                            }
                        } else {
                            Err(())
                        }
                    }
                    Entry::Vacant(_) => Err(()),
                }
            }
            ast::Pat::Or(pat1, pat2) => {
                let mut map = names
                    .iter()
                    .filter(|(_, (_, _, b))| *b)
                    .map(|(k, v)| (*k, v.clone()))
                    .collect();
                let pat1 = self.remap_pat(bump, names, pat1, typ.clone())?;
                let pat2 = self.check_names_same(bump, &mut map, pat2, typ)?;
                Ok(typed::Pat {
                    inner: typed::PatInner::Or(bump.alloc(pat1), bump.alloc(pat2)),
                    vars: vec![in bump],
                })
            }
            ast::Pat::Var(name) => match names.entry(name) {
                Entry::Occupied(mut o) => {
                    if o.get().2 {
                        o.get_mut().2 = false;
                        let (var, typ2, _) = o.get();
                        match typ.unify(typ2.clone()) {
                            Ok(()) => Ok(typed::Pat {
                                inner: typed::PatInner::Wild,
                                vars: vec![in bump; var.clone()],
                            }),
                            Err(_) => Err(()),
                        }
                    } else {
                        Err(())
                    }
                }
                Entry::Vacant(_) => Err(()),
            },
            ast::Pat::Wild => Ok(typed::Pat {
                inner: typed::PatInner::Wild,
                vars: vec![in bump],
            }),
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
        let mut builder = typ::Builder::default();
        let typ = builder.unsolved();
        assert!(Elaborator::new()
            .rename_pat(&bump, &mut hm, &pat, typ)
            .is_ok());
        assert!(hm.contains_key("x"))
    }

    #[test]
    fn test_missing() {
        let bump = Bump::new();
        let pat = make_or(&bump, ast::Pat::Var("x"), ast::Pat::Wild);
        let mut hm = HashMap::new();
        let mut builder = typ::Builder::default();
        let typ = builder.unsolved();
        assert!(Elaborator::new()
            .rename_pat(&bump, &mut hm, &pat, typ)
            .is_err())
    }

    #[test]
    fn test_extra() {
        let bump = Bump::new();
        let pat = make_or(&bump, ast::Pat::Wild, ast::Pat::Var("x"));
        let mut hm = HashMap::new();
        let mut builder = typ::Builder::default();
        let typ = builder.unsolved();
        assert!(Elaborator::new()
            .rename_pat(&bump, &mut hm, &pat, typ)
            .is_err())
    }
}
