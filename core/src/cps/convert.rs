use super::*;
use crate::elab::{
    typ,
    typed::{Case, Exp, Pat, PatInner, Var},
};
use bumpalo::{vec, Bump};
use std::collections::VecDeque;
use std::collections::{hash_map::Entry, HashMap};

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
    vars: HashMap<Var<'typed>, ()>,
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

fn find_nums<'typed>(
    out: &mut HashMap<i64, std::vec::Vec<Clause<'typed>>>,
    bump: &'typed Bump,
    pat: &'typed Pat<'typed>,
) {
    match pat.inner {
        PatInner::Int(n) => {
            out.insert(n, std::vec::Vec::new());
        }
        PatInner::Or(pat1, pat2) => {
            find_nums(out, bump, pat1);
            find_nums(out, bump, pat2);
        }
        PatInner::Wild => {}
    }
}

impl<'typed, 'cps> Compiler<'typed, 'cps> {
    pub fn default(
        &mut self,
        idx: usize,
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
                    });
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat2;
                    worklist.push_back(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars,
                    });
                }
                PatInner::Wild => {
                    let mut lhs = clause.lhs.clone();
                    lhs.remove(idx);
                    out.push(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars,
                    });
                }
            }
        }
        out
    }

    pub fn specialize_int(
        &mut self,
        idx: usize,
        n: i64,
        mut worklist: VecDeque<Clause<'typed>>,
    ) -> std::vec::Vec<Clause<'typed>> {
        let mut out = std::vec::Vec::new();
        while let Some(clause) = worklist.pop_front() {
            match clause.lhs[idx].inner {
                PatInner::Int(m) if m == n => {
                    let mut lhs = clause.lhs.clone();
                    lhs.remove(idx);
                    out.push(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars,
                    });
                }
                PatInner::Int(_) => {}
                PatInner::Or(pat1, pat2) => {
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat1;
                    worklist.push_back(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars.clone(),
                    });
                    let mut lhs = clause.lhs.clone();
                    lhs[idx] = pat2;
                    worklist.push_back(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars,
                    });
                }
                PatInner::Wild => {
                    let mut lhs = clause.lhs.clone();
                    lhs.remove(idx);
                    out.push(Clause {
                        lhs,
                        rhs: clause.rhs,
                        vars: clause.vars,
                    });
                }
            }
        }
        out
    }

    pub fn compile_pats(
        &mut self,
        scruts: &[(Id, typ::Type)],
        matrix: std::vec::Vec<Clause<'typed>>,
    ) -> Result<CExp<'cps>, ()> {
        if matrix.len() == 0 {
            Err(())
        } else {
            match find_irrefutable(&matrix[0]) {
                None => Ok(CExp::Continue(matrix[0].rhs, vec![in self.cps_bump])),
                Some(idx) => match scruts[idx].1.find() {
                    typ::Repr::Unsolved(_) => Err(()),
                    typ::Repr::Solved(typ::Expr::Arrow(_, _)) => Err(()),
                    typ::Repr::Solved(typ::Expr::Integer) => {
                        let mut map = HashMap::new();
                        for clause in &matrix {
                            find_nums(&mut map, &mut self.typed_bump, &clause.lhs[idx]);
                        }
                        let worklist = VecDeque::from_iter(matrix.iter().cloned());
                        for (k, v) in map.iter_mut() {
                            v.clear();
                            let matrix = self.specialize_int(idx, *k, worklist.clone());
                            *v = matrix;
                        }
                        let _default = self.default(idx, worklist);
                        Err(())
                    }
                    typ::Repr::Solved(typ::Expr::Product(_)) => Err(()),
                    typ::Repr::Solved(typ::Expr::String) => Err(()),
                },
            }
        }
    }

    pub fn gather_bindings(
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

    pub fn convert_clauses(
        &mut self,
        typs: &[(Id, typ::Type)],
        clauses: &'typed [Case<'typed>],
        cont: Box<dyn Fn(&mut Self, Id) -> Result<CExp<'cps>, ()> + '_>,
    ) -> Result<CExp<'cps>, ()> {
        let mut matrix = std::vec::Vec::new();
        let mut rhss = vec![in self.cps_bump];
        for Case { lhs, rhs } in clauses {
            let mut vars = std::vec::Vec::new();
            for pat in lhs {
                self.gather_bindings(pat, &mut vars);
            }
            let id = self.builder.fresh_id();
            matrix.push(Clause {
                lhs: lhs.iter().collect(),
                rhs: id,
                vars: HashMap::new(),
            });
            let body = self.convert_exp(rhs, Box::new(|comp: &mut Self, exp| cont(comp, exp)));
            rhss.push((id, body, vars))
        }
        self.compile_pats(typs, matrix)
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
}
