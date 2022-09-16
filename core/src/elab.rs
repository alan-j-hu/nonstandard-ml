use super::syntax::ast;
use crate::diagnostic::Error;
use bumpalo::{collections::Vec, vec, Bump};
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub mod ctx;
pub mod typ;
pub mod typed;

pub fn elab<'ast, 'typed, 'any>(
    bump: &'typed Bump,
    dec: &'ast ast::Located<ast::Dec<'ast>>,
) -> Result<(ctx::Scope<'ast, 'typed>, typed::Dec<'typed>), Error<'any>> {
    let mut elab = Elaborator::new();
    let scope = ctx::Scope::new();
    let ctx = ctx::Ctx::new(&scope);
    elab.elab_dec(&bump, &ctx, &dec)
}

#[derive(Default)]
pub struct Elaborator {
    var_builder: typed::VarBuilder,
    typ_builder: typ::Builder,
}

fn unify<'any>(actual: typ::Type, expected: typ::Type) -> Result<(), Error<'any>> {
    actual.unify(expected).map_err(Error::Unify)
}

impl Elaborator {
    fn new() -> Self {
        Self::default()
    }

    fn elab_dec<'a, 'ast, 'typed, 'any>(
        &mut self,
        bump: &'typed Bump,
        ctx: &ctx::Ctx<'a, 'ast, 'typed>,
        dec: &'ast ast::Located<ast::Dec<'ast>>,
    ) -> Result<(ctx::Scope<'ast, 'typed>, typed::Dec<'typed>), Error<'any>> {
        match dec.node {
            ast::Dec::And(dec1, dec2) => {
                let (scope1, dec1) = self.elab_dec(bump, ctx, dec1)?;
                let (mut scope2, dec2) = self.elab_dec(bump, ctx, dec2)?;
                for (k, v) in scope1.vals.iter() {
                    match scope2.vals.entry(k) {
                        Entry::Occupied(_) => return Err(Error::RedefinedVar(k.to_string())),
                        Entry::Vacant(va) => {
                            va.insert(v.clone());
                        }
                    }
                }
                Ok((scope2, typed::Dec::And(bump.alloc(dec1), bump.alloc(dec2))))
            }
            ast::Dec::Loc(dec1, dec2) => {
                let (scope1, dec1) = self.elab_dec(bump, ctx, dec1)?;
                let (scope2, dec2) = self.elab_dec(bump, &ctx.extend(&scope1), dec2)?;
                Ok((scope2, typed::Dec::Loc(bump.alloc(dec1), bump.alloc(dec2))))
            }
            ast::Dec::Seq(dec1, dec2) => {
                let (scope1, dec1) = self.elab_dec(bump, ctx, dec1)?;
                let (mut scope2, dec2) = self.elab_dec(bump, &ctx.extend(&scope1), dec2)?;
                for (k, v) in scope1.vals.iter() {
                    match scope2.vals.entry(k) {
                        Entry::Occupied(_) => {}
                        Entry::Vacant(va) => {
                            va.insert(v.clone());
                        }
                    }
                }
                Ok((scope2, typed::Dec::Seq(bump.alloc(dec1), bump.alloc(dec2))))
            }
            ast::Dec::Val(pat, exp) => {
                self.typ_builder.push_level();
                let mut vals = HashMap::new();
                let typ = self.typ_builder.unsolved();
                let pat = self.rename_pat(bump, &mut vals, pat, typ.clone())?;
                let exp = self.elab_exp(bump, ctx, exp, typ.clone())?;
                self.typ_builder.pop_level();
                let vals = vals
                    .iter()
                    .map(|(k, (v, t))| (*k, (v.clone(), self.typ_builder.gen(t.clone()))))
                    .collect();
                Ok((
                    ctx::Scope { vals },
                    typed::Dec::Val(typ, bump.alloc(pat), bump.alloc(exp)),
                ))
            }
        }
    }

    fn elab_exp<'a, 'ast, 'typed, 'any>(
        &mut self,
        bump: &'typed Bump,
        ctx: &ctx::Ctx<'a, 'ast, 'typed>,
        exp: &'ast ast::Located<ast::Exp<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Exp<'typed>, Error<'any>> {
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
                self.typ_builder.push_level();
                let exp = self.elab_exp(bump, ctx, exp, from.clone())?;
                let cases = self.elab_cases(bump, ctx, cases, &from, &vec![in bump], typ)?;
                self.typ_builder.pop_level();
                Ok(typed::Exp::Case(from, bump.alloc(exp), cases))
            }
            ast::Exp::Integer(n) => {
                unify(self.typ_builder.solved(typ::Expr::Integer), typ)?;
                Ok(typed::Exp::Integer(n))
            }
            ast::Exp::Lambda(cases) => {
                if cases.len() == 0 {
                    Err(Error::Internal("Empty lambda".to_string()))
                } else {
                    let dom = self.typ_builder.unsolved();
                    let mut doms = Vec::new_in(bump);
                    for _ in cases[0].pats {
                        doms.push(self.typ_builder.unsolved());
                    }
                    let codom = self.typ_builder.unsolved();
                    let mut arr = codom.clone();
                    for typ in doms.iter().rev() {
                        arr = self
                            .typ_builder
                            .solved(typ::Expr::Arrow(typ.clone(), arr.clone()))
                    }
                    arr = self
                        .typ_builder
                        .solved(typ::Expr::Arrow(dom.clone(), arr.clone()));
                    unify(arr, typ)?;
                    let cases = self.elab_cases(bump, ctx, cases, &dom, &doms, codom)?;
                    Ok(typed::Exp::Lambda(dom, doms, cases))
                }
            }
            ast::Exp::Let(dec, exp) => {
                let (scope, dec) = self.elab_dec(bump, ctx, dec)?;
                let exp = self.elab_exp(bump, &ctx.extend(&scope), exp, typ)?;
                Ok(typed::Exp::Let(bump.alloc(dec), bump.alloc(exp)))
            }
            ast::Exp::String(st) => {
                unify(self.typ_builder.solved(typ::Expr::String), typ)?;
                Ok(typed::Exp::String(st))
            }
            ast::Exp::Var(v) => match ctx.scope.vals.get(v) {
                None => Err(Error::UnboundVar(v.to_string())),
                Some((var, vtyp)) => {
                    unify(self.typ_builder.inst(&vtyp), typ)?;
                    Ok(typed::Exp::Var(var.clone()))
                }
            },
        }
    }

    fn elab_cases<'a, 'ast, 'typed, 'any>(
        &mut self,
        bump: &'typed Bump,
        ctx: &ctx::Ctx<'a, 'ast, 'typed>,
        cases: &'ast [ast::Case<'ast>],
        from: &typ::Type,
        froms: &Vec<typ::Type>,
        to: typ::Type,
    ) -> Result<&'typed [typed::Case<'typed>], Error<'any>> {
        let mut vec = vec![in bump];
        for ast::Case { pat, pats, rhs } in cases {
            let mut vals = HashMap::new();
            if pats.len() == froms.len() {
                let new_pat = self.rename_pat(bump, &mut vals, &pat, from.clone())?;
                let mut new_pats = Vec::new_in(bump);
                for (pat, typ) in pats.iter().zip(froms) {
                    let pat = self.rename_pat(bump, &mut vals, &pat, typ.clone())?;
                    new_pats.push(pat);
                }
                let vals = vals
                    .iter()
                    .map(|(k, (v, t))| (*k, (v.clone(), self.typ_builder.gen(t.clone()))))
                    .collect();
                let rhs =
                    self.elab_exp(bump, &ctx.extend(&ctx::Scope { vals }), rhs, to.clone())?;
                vec.push(typed::Case {
                    pat: new_pat,
                    pats: new_pats,
                    rhs,
                })
            } else {
                return Err(Error::Internal("Case row length is wrong".to_string()));
            }
        }
        Ok(bump.alloc_slice_fill_iter(vec.drain(..)))
    }

    fn rename_pat<'ast, 'typed, 'any>(
        &mut self,
        bump: &'typed Bump,
        out: &mut HashMap<&'ast str, (typed::Var<'typed>, typ::Type)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Pat<'typed>, Error<'any>> {
        match pat.node {
            ast::Pat::As(name, pat) => {
                let mut pat = self.rename_pat(bump, out, pat, typ.clone())?;
                match out.entry(name) {
                    Entry::Occupied(_) => Err(Error::Internal("Redefined".to_string())),
                    Entry::Vacant(va) => {
                        let var = self.var_builder.fresh(bump.alloc_str(name));
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
                        Entry::Occupied(_) => return Err(Error::Internal("Redefined".to_string())),
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
                Entry::Occupied(_) => Err(Error::Internal("Redefined".to_string())),
                Entry::Vacant(va) => {
                    let var = self.var_builder.fresh(bump.alloc_str(name));
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

    fn check_names_same<'ast, 'typed, 'any>(
        &self,
        bump: &'typed Bump,
        names: &mut HashMap<&'ast str, (typed::Var<'typed>, typ::Type, bool)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Pat<'typed>, Error<'any>> {
        let pat = self.remap_pat(bump, names, pat, typ)?;
        for (_k, (_, _, avail)) in names.iter() {
            if *avail {
                return Err(Error::Internal("".to_string()));
            }
        }
        Ok(pat)
    }

    fn remap_pat<'ast, 'typed, 'any>(
        &self,
        bump: &'typed Bump,
        names: &mut HashMap<&'ast str, (typed::Var<'typed>, typ::Type, bool)>,
        pat: &'ast ast::Located<ast::Pat<'ast>>,
        typ: typ::Type,
    ) -> Result<typed::Pat<'typed>, Error<'any>> {
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
                                Err(_) => Err(Error::Internal("".to_string())),
                            }
                        } else {
                            Err(Error::Internal("".to_string()))
                        }
                    }
                    Entry::Vacant(_) => Err(Error::Internal("".to_string())),
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
                            Err(_) => Err(Error::Internal("".to_string())),
                        }
                    } else {
                        Err(Error::Internal("".to_string()))
                    }
                }
                Entry::Vacant(_) => Err(Error::Internal("".to_string())),
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
