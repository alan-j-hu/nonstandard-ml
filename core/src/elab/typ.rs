use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Level {
    Nat(i32),
    Omega,
}

impl Ord for Level {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Level::Nat(lhs), Level::Nat(rhs)) => lhs.cmp(rhs),
            (Level::Nat(_), Level::Omega) => Ordering::Less,
            (Level::Omega, Level::Nat(_)) => Ordering::Greater,
            (Level::Omega, Level::Omega) => Ordering::Equal,
        }
    }
}

impl PartialOrd for Level {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Var {
    level: Level,
    id: i32,
    rank: i32,
}

impl Var {
    fn occurs(self, ty: &Type) -> Result<(), ()> {
        match ty.find() {
            Repr::Solved(Expr::Arrow(dom, codom)) => {
                self.occurs(&dom)?;
                self.occurs(&codom)
            }
            Repr::Solved(Expr::Integer) => Ok(()),
            Repr::Solved(Expr::Product(typs)) => {
                for typ in typs {
                    self.occurs(&typ)?;
                }
                Ok(())
            }
            Repr::Solved(Expr::String) => Ok(()),
            Repr::Unsolved(var) => {
                if self.id == var.id {
                    Err(())
                } else {
                    if self.level < var.level {
                        *ty.inner.borrow_mut() = State::Unsolved(Var {
                            level: self.level,
                            ..var
                        });
                    }
                    Ok(())
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum State {
    Parent(Type),
    Solved(Expr<Type>),
    Unsolved(Var),
}

/// The representative element
pub enum Repr {
    Solved(Expr<Type>),
    Unsolved(Var),
}

#[derive(Default)]
pub struct Builder {
    level: i32,
    id: i32,
}

impl Builder {
    pub fn unsolved(&mut self) -> Type {
        let id = self.id;
        self.id = id + 1;
        Type::unsolved(Var {
            id,
            level: Level::Nat(self.level),
            rank: 0,
        })
    }

    pub fn solved(&self, e: Expr<Type>) -> Type {
        Type::solved(e)
    }

    pub fn push_level(&mut self) {
        self.level += 1
    }

    pub fn pop_level(&mut self) {
        self.level -= 1
    }

    pub fn inst(&mut self, typ: &Type) -> Type {
        match typ.find() {
            Repr::Solved(Expr::Arrow(dom, codom)) => {
                let dom = self.inst(&dom);
                let codom = self.inst(&codom);
                match (Rc::strong_count(&dom.inner), Rc::strong_count(&codom.inner)) {
                    (1, _) | (_, 1) => self.solved(Expr::Arrow(dom, codom)),
                    (_, _) => typ.clone(),
                }
            }
            Repr::Solved(Expr::Integer) => typ.clone(),
            Repr::Solved(Expr::Product(typs)) => {
                let typs: Vec<_> = typs.iter().map(|typ| self.inst(typ)).collect();
                for typ in &typs {
                    if Rc::strong_count(&typ.inner) == 1 {
                        return self.solved(Expr::Product(typs));
                    }
                }
                typ.clone()
            }
            Repr::Solved(Expr::String) => typ.clone(),
            Repr::Unsolved(var) => {
                if var.level > Level::Nat(self.level) {
                    self.unsolved()
                } else {
                    typ.clone()
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    inner: Rc<RefCell<State>>,
}

impl Type {
    fn unsolved(var: Var) -> Type {
        Type {
            inner: Rc::new(RefCell::new(State::Unsolved(var))),
        }
    }

    fn solved(e: Expr<Type>) -> Type {
        Type {
            inner: Rc::new(RefCell::new(State::Solved(e))),
        }
    }

    pub fn find(&self) -> Repr {
        loop {
            let state = &self.inner.borrow().clone();
            match state {
                State::Parent(ref p) => *self.inner.borrow_mut() = p.inner.borrow().clone(),
                State::Solved(ref e) => return Repr::Solved(e.clone()),
                State::Unsolved(var) => return Repr::Unsolved(*var),
            }
        }
    }

    pub fn unify(self, other: Type) -> Result<(), Diff> {
        match (self.find(), other.find()) {
            (Repr::Solved(e1), Repr::Solved(e2)) => match (e1, e2) {
                (Expr::Arrow(ty1, ty2), Expr::Arrow(ty3, ty4)) => {
                    match unify4(ty1, ty2, ty3, ty4) {
                        None => Ok(()),
                        Some((diff1, diff2)) => {
                            Err(Diff::Later(Box::new(Expr::Arrow(diff1, diff2))))
                        }
                    }
                }
                (Expr::Integer, Expr::Integer) => Ok(()),
                (Expr::Product(tys1), Expr::Product(tys2)) => {
                    if tys1.len() != tys2.len() {
                        Err(Diff::Diff(self, other))
                    } else {
                        match unify_many(tys1.iter(), tys2.iter()) {
                            Ok(()) => Ok(()),
                            Err(diffs) => Err(Diff::Later(Box::new(Expr::Product(diffs)))),
                        }
                    }
                }
                (Expr::String, Expr::String) => Ok(()),
                (_, _) => Err(Diff::Diff(self, other)),
            },
            (Repr::Solved(e), Repr::Unsolved(var)) => match var.occurs(&self) {
                Ok(()) => {
                    *other.inner.borrow_mut() = State::Solved(e);
                    Ok(())
                }
                Err(()) => Err(Diff::OccursLeft(self, var)),
            },
            (Repr::Unsolved(var), Repr::Solved(e)) => match var.occurs(&other) {
                Ok(()) => {
                    *self.inner.borrow_mut() = State::Solved(e);
                    Ok(())
                }
                Err(()) => Err(Diff::OccursRight(var, other)),
            },
            (Repr::Unsolved(self_var), Repr::Unsolved(other_var)) => {
                if self_var.id == other_var.id {
                    Ok(())
                } else {
                    // Var of greater rank becomes parent
                    let (lhs, lvar, rhs, rvar) = match self_var.rank.cmp(&other_var.rank) {
                        Ordering::Less => (self, self_var, other, other_var),
                        Ordering::Greater => (other, other_var, self, self_var),
                        Ordering::Equal => {
                            *other.inner.borrow_mut() = State::Unsolved(Var {
                                rank: other_var.rank + 1,
                                ..other_var
                            });
                            (self, self_var, other, other_var)
                        }
                    };
                    if lvar.level < rvar.level {
                        *rhs.inner.borrow_mut() = State::Unsolved(Var {
                            level: lvar.level,
                            ..rvar
                        })
                    }
                    *lhs.inner.borrow_mut() = State::Parent(rhs);
                    Ok(())
                }
            }
        }
    }
}

pub fn unify4(ty1: Type, ty2: Type, ty3: Type, ty4: Type) -> Option<(Diff, Diff)> {
    match (ty1.clone().unify(ty3), ty2.clone().unify(ty4)) {
        (Ok(()), Ok(())) => None,
        (Ok(()), Err(diff)) => Some(((Diff::Same(ty1)), diff)),
        (Err(diff), Ok(())) => Some((diff, Diff::Same(ty2))),
        (Err(diff1), Err(diff2)) => Some((diff1, diff2)),
    }
}

pub fn unify_many<'a, I1, I2>(lhs: I1, rhs: I2) -> Result<(), Vec<Diff>>
where
    I1: Iterator<Item = &'a Type>,
    I2: Iterator<Item = &'a Type>,
{
    let diffs: Vec<Diff> = lhs
        .zip(rhs)
        .map(|(lhs, rhs)| match lhs.clone().unify(rhs.clone()) {
            Ok(()) => Diff::Same(lhs.clone()),
            Err(diff) => diff,
        })
        .collect();
    let all_same = diffs.iter().all(|x| matches!(x, Diff::Same(_)));
    if all_same {
        Ok(())
    } else {
        Err(diffs)
    }
}

#[derive(Clone, Debug)]
pub enum Expr<T> {
    Arrow(T, T),
    Integer,
    Product(Vec<T>),
    String,
}

#[derive(Debug)]
pub enum Diff {
    Same(Type),
    Diff(Type, Type),
    OccursLeft(Type, Var),
    OccursRight(Var, Type),
    Later(Box<Expr<Diff>>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unify_unsolved() {
        let mut builder = Builder::default();
        let us = builder.unsolved();
        let s_ty = builder.solved(Expr::String);
        assert!(matches!(us.clone().find(), Repr::Unsolved(_)));
        assert!(us.clone().unify(s_ty.clone()).is_ok());
        assert!(matches!(us.clone().find(), Repr::Solved(_)));
    }

    #[test]
    fn unify_product() {
        let builder = Builder::default();
        let int = builder.solved(Expr::Integer);
        let p1 = builder.solved(Expr::Product(vec![int.clone()]));
        let p2 = builder.solved(Expr::Product(vec![int.clone(), int]));
        assert!(p1.clone().unify(p1.clone()).is_ok());
        assert!(p1.unify(p2).is_err());
    }

    #[test]
    fn unify_arrow() {
        let mut builder = Builder::default();
        let int = builder.solved(Expr::Integer);
        let us = builder.unsolved();
        let a1 = builder.solved(Expr::Arrow(int.clone(), int.clone()));
        let a2 = builder.solved(Expr::Arrow(int.clone(), us.clone()));
        assert!(matches!(us.clone().find(), Repr::Unsolved(_)));
        assert!(a1.clone().unify(a2.clone()).is_ok());
        assert!(matches!(us.clone().find(), Repr::Solved(_)));
    }

    #[test]
    fn occurs_left() {
        let mut builder = Builder::default();
        let int = builder.solved(Expr::Integer);
        let us = builder.unsolved();
        let a = builder.solved(Expr::Arrow(int.clone(), us.clone()));
        assert!(matches!(us.clone().find(), Repr::Unsolved(_)));
        assert!(a.unify(us).is_err());
    }

    #[test]
    fn occurs_right() {
        let mut builder = Builder::default();
        let int = builder.solved(Expr::Integer);
        let us = builder.unsolved();
        let a = builder.solved(Expr::Arrow(int.clone(), us.clone()));
        assert!(matches!(us.clone().find(), Repr::Unsolved(_)));
        assert!(us.unify(a).is_err());
    }

    #[test]
    fn occurs_refl() {
        let mut builder = Builder::default();
        let us = builder.unsolved();
        assert!(matches!(us.clone().find(), Repr::Unsolved(_)));
        assert!(us.clone().unify(us.clone()).is_ok());
        assert!(matches!(us.find(), Repr::Unsolved(_)));
    }
}
