use super::*;
use crate::elab::{
    typ,
    typed::{Case, Dec, Exp, Pat, PatInner, Var},
};
use bumpalo::{vec, Bump};
use std::collections::VecDeque;
use std::collections::{hash_map::Entry, BTreeMap, HashMap};
use std::rc::Rc;

pub struct Compiler<'typed, 'cps> {
    builder: Builder,
    typed_bump: &'typed Bump,
    cps_bump: &'cps Bump,
    map: HashMap<Var<'typed>, Id>,
}

#[derive(Clone)]
pub struct Clause<'typed> {
    lhs: std::vec::Vec<&'typed Pat<'typed>>,
    rhs: Id,
    vars: HashMap<Var<'typed>, Id>,
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

fn find_nums<'typed>(out: &mut HashMap<i64, ()>, bump: &'typed Bump, pat: &'typed Pat<'typed>) {
    match pat.inner {
        PatInner::Int(n) => {
            out.insert(n, ());
        }
        PatInner::Or(pat1, pat2) => {
            find_nums(out, bump, pat1);
            find_nums(out, bump, pat2);
        }
        PatInner::Wild => {}
    }
}

fn remove_pat<'typed>(clause: Clause<'typed>, idx: usize, scrut: Id) -> Clause<'typed> {
    let mut lhs = clause.lhs.clone();
    lhs.remove(idx);
    let mut vars = clause.vars;
    for var in &clause.lhs[idx].vars {
        vars.insert(var.clone(), scrut);
    }
    Clause {
        lhs,
        rhs: clause.rhs,
        vars,
        params: clause.params.clone(),
    }
}

impl<'typed, 'cps> Compiler<'typed, 'cps> {
    pub fn convert_dec(&mut self, dec: &'typed Dec<'typed>, cont_id: Id) -> Result<CExp<'cps>, ()> {
        match dec {
            Dec::And(dec1, dec2) | Dec::Loc(dec1, dec2) | Dec::Seq(dec1, dec2) => {
                let id = self.builder.fresh_id();
                let dec1 = self.convert_dec(dec1, id)?;
                let dec2 = self.convert_dec(dec2, cont_id)?;
                Ok(CExp::LetCont(
                    vec![in self.cps_bump; (id, vec![in self.cps_bump], &*self.cps_bump.alloc(dec2))],
                    self.cps_bump.alloc(dec1),
                ))
            }
            Dec::Val(typ, pat, exp) => {
                let mut params = std::vec::Vec::new();
                self.gather_bindings(pat, &mut params);
                self.convert_exp(
                    exp,
                    Box::new(move |comp, exp| {
                        let scruts = std::vec![(exp, typ.clone())];
                        let matrix = std::vec![Clause {
                            lhs: std::vec![pat],
                            rhs: cont_id,
                            vars: HashMap::new(),
                            params: Rc::new(params),
                        }];
                        comp.compile_pats(&scruts, matrix)
                    }),
                )
            }
        }
    }

    pub fn convert_exp(
        &mut self,
        exp: &'typed Exp<'typed>,
        cont: Box<dyn FnOnce(&mut Self, Id) -> Result<CExp<'cps>, ()> + '_>,
    ) -> Result<CExp<'cps>, ()> {
        match *exp {
            Exp::Apply(exp1, exp2) => self.convert_exp(
                exp1,
                Box::new(move |comp, exp1| {
                    comp.convert_exp(
                        exp2,
                        Box::new(move |comp, exp2| {
                            let cont_id = comp.builder.fresh_id();
                            let param_id = comp.builder.fresh_id();
                            let cont = cont(comp, param_id)?;
                            Ok(CExp::LetCont(
                                vec![in comp.cps_bump;
                                     (cont_id,
                                      vec![in comp.cps_bump; param_id],
                                      &*comp.cps_bump.alloc(cont))],
                                comp.cps_bump.alloc(CExp::Apply(exp1, exp2, cont_id)),
                            ))
                        }),
                    )
                }),
            ),
            Exp::Case(ref typ, ref scrut, clauses) => {
                let cont_id = self.builder.fresh_id();
                let var_id = self.builder.fresh_id();
                self.convert_exp(scrut, Box::new(move |comp, scrut| {
                    let body = comp.convert_clauses(
                        &[(scrut, typ.clone())],
                        clauses,
                        Box::new(|comp, exp| Ok(CExp::Continue(cont_id, vec![in comp.cps_bump; exp]))),
                    )?;
                    let cont = cont(comp, var_id)?;
                    Ok(CExp::LetCont(
                        vec![in comp.cps_bump; (cont_id, vec![in comp.cps_bump; var_id], &*comp.cps_bump.alloc(cont))],
                        comp.cps_bump.alloc(body),
                    ))
                }))
            }
            Exp::Integer(n) => {
                let id = self.builder.fresh_id();
                let def = self.cps_bump.alloc(ADef {
                    exp: AExp::Integer(n),
                    id,
                });
                let cont = cont(self, id)?;
                Ok(CExp::Let(def, self.cps_bump.alloc(cont)))
            }
            Exp::Lambda(ref dom, clauses) => {
                let mut scruts = std::vec::Vec::new();
                for typ in dom {
                    scruts.push((self.builder.fresh_id(), typ.clone()));
                }
                let id = self.builder.fresh_id();
                self.convert_clauses(
                    &scruts,
                    clauses,
                    Box::new(|comp: &mut Self, exp| {
                        Ok(CExp::Continue(id, vec![in comp.cps_bump; exp]))
                    }),
                )
            }
            Exp::Let(_, _) => unimplemented!(),
            Exp::String(_) => unimplemented!(),
            Exp::Var(_) => unimplemented!(),
        }
    }

    fn convert_clauses(
        &mut self,
        typs: &[(Id, typ::Type)],
        clauses: &'typed [Case<'typed>],
        cont: Box<dyn Fn(&mut Self, Id) -> Result<CExp<'cps>, ()> + '_>,
    ) -> Result<CExp<'cps>, ()> {
        let mut matrix = std::vec::Vec::new();
        let mut rhss = vec![in self.cps_bump];
        for Case { lhs, rhs } in clauses {
            let mut params = std::vec::Vec::new();
            for pat in lhs {
                self.gather_bindings(pat, &mut params);
            }
            let id = self.builder.fresh_id();
            let rc = Rc::new(params);
            matrix.push(Clause {
                lhs: lhs.iter().collect(),
                rhs: id,
                vars: HashMap::new(),
                params: rc.clone(),
            });
            let body = self.convert_exp(rhs, Box::new(|comp: &mut Self, exp| cont(comp, exp)));
            rhss.push((id, body, rc))
        }
        self.compile_pats(typs, matrix)
    }

    fn compile_pats(
        &mut self,
        scruts: &[(Id, typ::Type)],
        mut matrix: std::vec::Vec<Clause<'typed>>,
    ) -> Result<CExp<'cps>, ()> {
        if matrix.len() == 0 {
            Err(())
        } else {
            match find_irrefutable(&matrix[0]) {
                None => {
                    matrix.truncate(1);
                    let Clause {
                        lhs,
                        mut vars,
                        params,
                        ..
                    } = matrix.remove(0);
                    for (pat, (scrut, _)) in lhs.iter().zip(scruts) {
                        for var in &pat.vars {
                            vars.insert(var.clone(), *scrut);
                        }
                    }
                    let mut args = Vec::new_in(self.cps_bump);
                    for (param, _) in params.iter() {
                        args.push(vars.get(param).unwrap().clone());
                    }
                    Ok(CExp::Continue(matrix[0].rhs, args))
                }
                Some(idx) => match scruts[idx].1.find() {
                    typ::Repr::Unsolved(_) => Err(()),
                    typ::Repr::Solved(typ::Expr::Arrow(_, _)) => Err(()),
                    typ::Repr::Solved(typ::Expr::Integer) => {
                        let mut map = HashMap::new();
                        for clause in &matrix {
                            find_nums(&mut map, &mut self.typed_bump, &clause.lhs[idx]);
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
                        for (n, ()) in map.iter_mut() {
                            let id = self.builder.fresh_id();
                            let matrix =
                                self.specialize_int(idx, scruts[idx].0, *n, worklist.clone());
                            let body = self.compile_pats(&new_scruts, matrix)?;
                            conts.push((id, vec![in self.cps_bump], &*self.cps_bump.alloc(body)));
                            bmap.insert(*n, id);
                        }
                        let default_id = self.builder.fresh_id();
                        let default = self.default(idx, scruts[idx].0, worklist);
                        let body = self.compile_pats(&new_scruts, default)?;
                        conts.push((
                            default_id,
                            vec![in self.cps_bump],
                            &*self.cps_bump.alloc(body),
                        ));
                        Ok(CExp::LetCont(
                            conts,
                            self.cps_bump
                                .alloc(CExp::CaseInt(scruts[idx].0, bmap, default_id)),
                        ))
                    }
                    typ::Repr::Solved(typ::Expr::Product(_)) => Err(()),
                    typ::Repr::Solved(typ::Expr::String) => Err(()),
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
                vec.push((var.clone(), self.builder.fresh_id()));
            }
            match pat.inner {
                PatInner::Or(pat1, pat2) => {
                    self.gather_bindings(pat1, vec);
                    pat = pat2;
                }
                PatInner::Int(_) | PatInner::Wild => return,
            }
        }
    }

    fn default(
        &mut self,
        idx: usize,
        scrut: Id,
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
                PatInner::Wild => out.push(remove_pat(clause, idx, scrut)),
            }
        }
        out
    }

    fn specialize_int(
        &mut self,
        idx: usize,
        scrut: Id,
        n: i64,
        mut worklist: VecDeque<Clause<'typed>>,
    ) -> std::vec::Vec<Clause<'typed>> {
        let mut out = std::vec::Vec::new();
        while let Some(clause) = worklist.pop_front() {
            match clause.lhs[idx].inner {
                PatInner::Int(m) if m == n => out.push(remove_pat(clause, idx, scrut)),
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