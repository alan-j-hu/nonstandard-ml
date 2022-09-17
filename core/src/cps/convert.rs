use super::*;
use crate::diagnostic::Error;
use crate::elab::{
    typ,
    typed::{Case, Dec, Exp, Pat, PatInner, Var},
};
use bumpalo::{vec, Bump};
use std::collections::VecDeque;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

pub fn convert<'cps, 'typed, 'any>(
    typed_bump: &'typed Bump,
    cps_bump: &'cps Bump,
    dec: &'typed Dec<'typed>,
) -> Result<(Id, CExp<'cps>), Error<'any>> {
    let mut compiler = Compiler {
        builder: Builder::new(),
        typed_bump,
        cps_bump,
        vars: HashMap::new(),
    };
    let ret_id = compiler.builder.fresh_id();
    let box_id = compiler.builder.fresh_id();
    let exp = compiler.convert_dec(
        dec,
        Box::new(|comp| {
            let vals = Vec::new_in(comp.cps_bump);
            Ok(CExp::Let(
                &*compiler.cps_bump.alloc(ADef {
                    id: box_id,
                    exp: AExp::Box(typ::Type::solved(typ::Expr::Integer), 0, vals),
                }),
                &*compiler.cps_bump.alloc(CExp::Continue(
                    ret_id,
                    vec![in compiler.cps_bump; Val::Id(box_id)],
                )),
            ))
        }),
    )?;
    Ok((ret_id, exp))
}

struct Compiler<'typed, 'cps> {
    builder: Builder,
    typed_bump: &'typed Bump,
    cps_bump: &'cps Bump,
    vars: HashMap<Var<'typed>, Id>,
}

#[derive(Clone)]
struct Clause<'typed> {
    lhs: std::vec::Vec<&'typed Pat<'typed>>,
    rhs: Id,
    vars: HashMap<Var<'typed>, Val>,
    params: Rc<std::vec::Vec<(Var<'typed>, Id)>>,
}

fn find_irrefutable<'typed>(clause: &Clause<'typed>) -> Option<usize> {
    for (i, elt) in clause.lhs.iter().enumerate() {
        match elt.inner {
            PatInner::Int(_) => return Some(i),
            PatInner::Or(_, _) => {}
            PatInner::Wild => {}
        }
    }
    None
}

fn find_nums<'typed>(out: &mut HashSet<i64>, bump: &'typed Bump, pat: &'typed Pat<'typed>) {
    match pat.inner {
        PatInner::Int(n) => {
            out.insert(n);
        }
        PatInner::Or(pat1, pat2) => {
            find_nums(out, bump, pat1);
            find_nums(out, bump, pat2);
        }
        PatInner::Wild => {}
    }
}

fn remove_pat<'typed>(clause: Clause<'typed>, idx: usize, scrut: Val) -> Clause<'typed> {
    let mut lhs = clause.lhs.clone();
    lhs.remove(idx);
    let mut vars = clause.vars;
    for var in &clause.lhs[idx].vars {
        vars.insert(var.clone(), scrut.clone());
    }
    Clause {
        lhs,
        rhs: clause.rhs,
        vars,
        params: clause.params.clone(),
    }
}

impl<'cps, 'typed> Compiler<'typed, 'cps> {
    fn convert_dec<'s, 'any>(
        &'s mut self,
        dec: &'typed Dec<'typed>,
        cont: Box<dyn for<'a> FnOnce(&'a mut Self) -> Result<CExp<'cps>, Error<'any>> + '_>,
    ) -> Result<CExp<'cps>, Error<'any>> {
        match dec {
            Dec::And(dec1, dec2) | Dec::Loc(dec1, dec2) | Dec::Seq(dec1, dec2) => {
                self.convert_dec(dec1, Box::new(|comp| comp.convert_dec(dec2, cont)))
            }
            Dec::Val(typ, pat, exp) => {
                let mut params = std::vec::Vec::new();
                self.gather_bindings(pat, &mut params);
                let mut vars: Vec<Id> = Vec::new_in(self.cps_bump);
                for (_, id) in &params {
                    vars.push(*id)
                }
                let cont_id = self.builder.fresh_id();
                let mut matrix = std::vec::Vec::new();
                matrix.push(Clause {
                    lhs: std::vec![pat],
                    rhs: cont_id,
                    vars: HashMap::new(),
                    params: Rc::new(params),
                });
                Ok(CExp::LetCont(
                    vec![in self.cps_bump;
                         (cont_id, vars, &*self.cps_bump.alloc(cont(self)?))],
                    &*self.cps_bump.alloc(self.convert_exp(
                        exp,
                        Box::new(|comp, scrut| comp.compile_pats(&[(scrut, typ.clone())], matrix)),
                    )?),
                ))
            }
        }
    }

    fn convert_exp<'any>(
        &mut self,
        exp: &'typed Exp<'typed>,
        cont: Box<dyn FnOnce(&mut Self, Val) -> Result<CExp<'cps>, Error<'any>> + '_>,
    ) -> Result<CExp<'cps>, Error<'any>> {
        match *exp {
            Exp::Apply(exp1, exp2) => {
                let cont_id = self.builder.fresh_id();
                let param_id = self.builder.fresh_id();
                let app = self.convert_exp(
                    exp2,
                    Box::new(|comp, exp2| {
                        comp.convert_exp(
                            exp1,
                            Box::new(|_, exp1| Ok(CExp::Apply(exp1, exp2, cont_id))),
                        )
                    }),
                )?;
                let cont = cont(self, Val::Id(param_id))?;
                Ok(CExp::LetCont(
                    vec![in self.cps_bump;
                         (cont_id,
                          vec![in self.cps_bump; param_id],
                          &*self.cps_bump.alloc(cont))],
                    self.cps_bump.alloc(app),
                ))
            }
            Exp::Case(ref typ, scrut, clauses) => {
                let cont_id = self.builder.fresh_id();
                let var_id = self.builder.fresh_id();
                let match_cexp = self.convert_exp(
                    scrut,
                    Box::new(|comp, scrut| {
                        comp.convert_clauses(
                            &[(scrut, typ.clone())],
                            clauses,
                            &(|exp| CExp::Continue(cont_id, vec![in comp.cps_bump; exp])),
                        )
                    }),
                )?;
                Ok(CExp::LetCont(
                    vec![in self.cps_bump;
                         (cont_id, vec![in self.cps_bump; var_id], &*self.cps_bump.alloc(cont(self, Val::Id(var_id))?))],
                    &*self.cps_bump.alloc(match_cexp),
                ))
            }
            Exp::Integer(n) => cont(self, Val::Integer(n)),
            Exp::Lambda(ref dom, ref doms, clauses) => {
                if clauses.len() == 0 {
                    return Err(Error::Internal("Empty clauses".to_string())); // Handle empty type later
                }
                // NOTE: The number of patterns in a clause is not necessarily
                // the same as the number of arrows in the type!
                let mut params = std::vec::Vec::new();
                let first_param = self.builder.fresh_id();
                for (typ, _) in doms.iter().zip(clauses[0].pats.iter()) {
                    params.push((self.builder.fresh_id(), typ.clone()))
                }

                let mut scruts = std::vec::Vec::new();
                scruts.push((Val::Id(first_param), dom.clone()));
                for (id, typ) in &params {
                    scruts.push((Val::Id(*id), typ.clone()))
                }

                let ret_addr = self.builder.fresh_id();
                // fun x y z -> e
                //
                // becomes
                //
                // let a = fun x k1 ->
                //   let b = fun y k2 ->
                //     let c = fun z k3 -> k3 e
                //     in k2 c
                //   in k1 b
                // in k a
                let body = self.convert_clauses(
                    &scruts,
                    clauses,
                    &(|exp| CExp::Continue(ret_addr, vec![in self.cps_bump; exp])),
                )?;
                let (body, ret_addr) =
                    params
                        .iter()
                        .rev()
                        .fold((body, ret_addr), |(body, ret_addr), (param, typ)| {
                            let id = self.builder.fresh_id();
                            let new_ret_addr = self.builder.fresh_id();
                            (
                                CExp::Let(
                                    self.cps_bump.alloc(ADef {
                                        id,
                                        exp: AExp::Lambda(Lambda {
                                            domain: typ.clone(),
                                            param: *param,
                                            ret_addr,
                                            body: self.cps_bump.alloc(body),
                                        }),
                                    }),
                                    self.cps_bump.alloc(CExp::Continue(
                                        new_ret_addr,
                                        vec![in self.cps_bump; Val::Id(id)],
                                    )),
                                ),
                                new_ret_addr,
                            )
                        });
                let id = self.builder.fresh_id();
                Ok(CExp::Let(
                    self.cps_bump.alloc(ADef {
                        id,
                        exp: AExp::Lambda(Lambda {
                            domain: dom.clone(),
                            param: first_param,
                            ret_addr,
                            body: self.cps_bump.alloc(body),
                        }),
                    }),
                    self.cps_bump.alloc(cont(self, Val::Id(id))?),
                ))
            }
            Exp::Let(dec, exp) => {
                self.convert_dec(dec, Box::new(|comp| comp.convert_exp(exp, cont)))
            }
            Exp::String(ref st) => cont(self, Val::String(*st)),
            Exp::Var(ref v) => cont(self, Val::Id(*self.vars.get(v).unwrap())),
        }
    }

    fn convert_clauses<'any>(
        &mut self,
        typs: &[(Val, typ::Type)],
        clauses: &'typed [Case<'typed>],
        cont: &dyn Fn(Val) -> CExp<'cps>,
    ) -> Result<CExp<'cps>, Error<'any>> {
        let mut matrix = std::vec::Vec::new();
        let mut rhss = Vec::new_in(self.cps_bump);
        for Case { pat, pats, rhs } in clauses {
            let mut params = std::vec::Vec::new();
            self.gather_bindings(pat, &mut params);
            for pat in pats {
                self.gather_bindings(pat, &mut params);
            }
            let id = self.builder.fresh_id();
            let body = self.convert_exp(rhs, Box::new(|_, exp| Ok(cont(exp))))?;
            let mut v: Vec<Id> = Vec::new_in(self.cps_bump);
            for (_, id) in &params {
                v.push(*id)
            }
            rhss.push((id, v, &*self.cps_bump.alloc(body)));
            let params = Rc::new(params);
            matrix.push(Clause {
                lhs: std::iter::once(pat).chain(pats.iter()).collect(),
                rhs: id,
                vars: HashMap::new(),
                params: params,
            });
        }
        Ok(CExp::LetCont(
            rhss,
            &*self.cps_bump.alloc(self.compile_pats(typs, matrix)?),
        ))
    }

    fn compile_pats<'any>(
        &mut self,
        scruts: &[(Val, typ::Type)],
        mut matrix: std::vec::Vec<Clause<'typed>>,
    ) -> Result<CExp<'cps>, Error<'any>> {
        if matrix.len() == 0 {
            Err(Error::Internal("Empty matrix".to_string()))
        } else {
            match find_irrefutable(&matrix[0]) {
                None => {
                    matrix.truncate(1);
                    let Clause {
                        lhs,
                        rhs,
                        mut vars,
                        params,
                    } = matrix.remove(0);
                    for (pat, (scrut, _)) in lhs.iter().zip(scruts) {
                        for var in &pat.vars {
                            vars.insert(var.clone(), scrut.clone());
                        }
                    }
                    let mut args = Vec::new_in(self.cps_bump);
                    for (param, _) in params.iter() {
                        args.push(vars.get(param).unwrap().clone());
                    }
                    Ok(CExp::Continue(rhs, args))
                }
                Some(idx) => match scruts[idx].1.find() {
                    typ::Repr::Unsolved(_) => Err(Error::Internal("Unsolved".to_string())),
                    typ::Repr::Solved(typ::Expr::Arrow(_, _)) => {
                        Err(Error::Internal("Can't match on arrow type".to_string()))
                    }
                    typ::Repr::Solved(typ::Expr::Integer) => {
                        let mut set = HashSet::new();
                        for clause in &matrix {
                            find_nums(&mut set, &mut self.typed_bump, &clause.lhs[idx]);
                        }
                        let worklist = VecDeque::from_iter(matrix.iter().cloned());
                        let mut conts = vec![in self.cps_bump];
                        let mut new_scruts = Vec::with_capacity_in(conts.len() - 1, self.cps_bump);
                        for (i, scrut) in scruts.iter().enumerate() {
                            if i != idx {
                                new_scruts.push(scrut.clone());
                            }
                        }
                        let mut bmap = BTreeMap::new();
                        for n in set {
                            let id = self.builder.fresh_id();
                            let matrix = self.specialize_int(
                                idx,
                                scruts[idx].0.clone(),
                                n,
                                worklist.clone(),
                            );
                            let body = self.compile_pats(&new_scruts, matrix)?;
                            conts.push((id, vec![in self.cps_bump], &*self.cps_bump.alloc(body)));
                            bmap.insert(n, id);
                        }
                        let default_id = self.builder.fresh_id();
                        let default = self.default(idx, scruts[idx].0.clone(), worklist);
                        let body = self.compile_pats(&new_scruts, default)?;
                        conts.push((
                            default_id,
                            vec![in self.cps_bump],
                            &*self.cps_bump.alloc(body),
                        ));
                        let vec: std::vec::Vec<_> = bmap.into_iter().collect();
                        fn build_binary_search<'a, 'typed, 'cps>(
                            this: &'a mut Compiler<'typed, 'cps>,
                            scrut: Val,
                            sorted: &'a [(i64, Id)],
                        ) -> CExp<'cps> {
                            match sorted {
                                [] => panic!(),
                                [(_, v)] => CExp::Continue(*v, vec![in this.cps_bump]),
                                sorted => {
                                    let middle = sorted.len() / 2;
                                    let (k, _) = sorted[middle];
                                    let l_id = this.builder.fresh_id();
                                    let r_id = this.builder.fresh_id();
                                    let left = &sorted[..middle];
                                    let right = &sorted[middle..];
                                    let l_body = build_binary_search(this, scrut, left);
                                    let r_body = build_binary_search(this, scrut, right);
                                    CExp::LetCont(
                                        vec![in this.cps_bump;
                                             (l_id, vec![in this.cps_bump], &*this.cps_bump.alloc(l_body)),
                                             (r_id, vec![in this.cps_bump], &*this.cps_bump.alloc(r_body)),
                                        ],
                                        &*this.cps_bump.alloc(CExp::Lt(
                                            scrut,
                                            Val::Integer(k),
                                            l_id,
                                            r_id,
                                        )),
                                    )
                                }
                            }
                        }
                        Ok(CExp::LetCont(
                            conts,
                            self.cps_bump.alloc(build_binary_search(
                                self,
                                scruts[idx].0.clone(),
                                &vec[..],
                            )),
                        ))
                    }
                    typ::Repr::Solved(typ::Expr::Product(_)) => {
                        Err(Error::Internal("Todo product".to_string()))
                    }
                    typ::Repr::Solved(typ::Expr::String) => {
                        Err(Error::Internal("Todo string".to_string()))
                    }
                },
            }
        }
    }

    fn gather_bindings(
        &mut self,
        mut pat: &'typed Pat<'typed>,
        vec: &mut std::vec::Vec<(Var<'typed>, Id)>,
    ) {
        loop {
            for var in &pat.vars {
                let id = self.builder.fresh_id();
                self.vars.insert(var.clone(), id);
                vec.push((var.clone(), id))
            }
            match pat.inner {
                PatInner::Or(pat1, _) => {
                    pat = pat1;
                }
                PatInner::Int(_) | PatInner::Wild => return,
            }
        }
    }

    fn default(
        &mut self,
        idx: usize,
        scrut: Val,
        mut worklist: VecDeque<Clause<'typed>>,
    ) -> std::vec::Vec<Clause<'typed>> {
        let mut out = std::vec::Vec::new();
        while let Some(clause) = worklist.pop_front() {
            match clause.lhs[idx].inner {
                PatInner::Int(_) => {}
                PatInner::Or(pat1, pat2) => {
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat1;
                    worklist.push_back(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars.clone(),
                        params: clause.params.clone(),
                    });
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat2;
                    worklist.push_back(Clause {
                        lhs,
                        vars: clause.vars,
                        ..clause
                    });
                }
                PatInner::Wild => out.push(remove_pat(clause, idx, scrut.clone())),
            }
        }
        out
    }

    fn specialize_int(
        &mut self,
        idx: usize,
        scrut: Val,
        n: i64,
        mut worklist: VecDeque<Clause<'typed>>,
    ) -> std::vec::Vec<Clause<'typed>> {
        let mut out = std::vec::Vec::new();
        while let Some(clause) = worklist.pop_front() {
            match clause.lhs[idx].inner {
                PatInner::Int(m) if m == n => out.push(remove_pat(clause, idx, scrut.clone())),
                PatInner::Int(_) => {}
                PatInner::Or(pat1, pat2) => {
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat1;
                    worklist.push_back(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars.clone(),
                        params: clause.params.clone(),
                    });
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat2;
                    worklist.push_back(Clause {
                        lhs,
                        params: clause.params.clone(),
                        ..clause
                    });
                }
                PatInner::Wild => {
                    let mut lhs = clause.lhs.clone();
                    lhs.remove(idx);
                    out.push(Clause {
                        lhs,
                        params: clause.params.clone(),
                        ..clause
                    });
                }
            }
        }
        out
    }
}
