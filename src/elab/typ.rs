use std::cell::RefCell;
use std::rc::Rc;

#[derive(Copy, Clone)]
pub struct Var {
    level: i32,
    id: i32,
}

#[derive(Clone)]
enum State {
    Parent(Type),
    Solved(Expr<Type>),
    Unsolved(Var),
}

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
            level: self.level,
        })
    }

    pub fn solved(&self, e: Expr<Type>) -> Type {
        Type::solved(e)
    }
}

#[derive(Clone)]
pub struct Type {
    inner: Rc<RefCell<State>>,
}

impl Type {
    pub fn unsolved(var: Var) -> Type {
        Type {
            inner: Rc::new(RefCell::new(State::Unsolved(var))),
        }
    }

    pub fn solved(e: Expr<Type>) -> Type {
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

    pub fn unify(self, other: Type) -> Result<(), Difference> {
        match (self.find(), other.find()) {
            (Repr::Solved(e1), Repr::Solved(e2)) => match (e1, e2) {
                (Expr::Arrow(ty1, ty2), Expr::Arrow(ty3, ty4)) => {
                    match unify4(ty1, ty2, ty3, ty4) {
                        None => Ok(()),
                        Some((diff1, diff2)) => Err(Difference(DiffType::Later(Box::new(
                            Expr::Arrow(diff1, diff2),
                        )))),
                    }
                }
                (Expr::Integer, Expr::Integer) => Ok(()),
                (Expr::Product(tys1), Expr::Product(tys2)) => {
                    if tys1.len() != tys2.len() {
                        Err(Difference(DiffType::Diff(self, other)))
                    } else {
                        match unify_many(tys1.iter(), tys2.iter()) {
                            Ok(()) => Ok(()),
                            Err(diffs) => {
                                Err(Difference(DiffType::Later(Box::new(Expr::Product(diffs)))))
                            }
                        }
                    }
                }
                (Expr::String, Expr::String) => Ok(()),
                (_, _) => Err(Difference(DiffType::Diff(self, other))),
            },
            (Repr::Solved(e), Repr::Unsolved(_)) => {
                *other.inner.borrow_mut() = State::Solved(e);
                Ok(())
            }
            (Repr::Unsolved(_), Repr::Solved(e)) => {
                *self.inner.borrow_mut() = State::Solved(e);
                Ok(())
            }
            (Repr::Unsolved(_), Repr::Unsolved(_)) => {
                *self.inner.borrow_mut() = State::Parent(other);
                Ok(())
            }
        }
    }
}

pub fn unify4(ty1: Type, ty2: Type, ty3: Type, ty4: Type) -> Option<(Difference, Difference)> {
    match (ty1.clone().unify(ty3), ty2.clone().unify(ty4)) {
        (Ok(()), Ok(())) => None,
        (Ok(()), Err(diff)) => Some((Difference(DiffType::Same(ty1)), diff)),
        (Err(diff), Ok(())) => Some((diff, Difference(DiffType::Same(ty2)))),
        (Err(diff1), Err(diff2)) => Some((diff1, diff2)),
    }
}

pub fn unify_many<'a, I1, I2>(lhs: I1, rhs: I2) -> Result<(), Vec<Difference>>
where
    I1: Iterator<Item = &'a Type>,
    I2: Iterator<Item = &'a Type>,
{
    let diffs: Vec<Difference> = lhs
        .zip(rhs)
        .map(|(lhs, rhs)| match lhs.clone().unify(rhs.clone()) {
            Ok(()) => Difference(DiffType::Same(lhs.clone())),
            Err(diff) => diff,
        })
        .collect();
    let all_same = diffs
        .iter()
        .all(|x| matches!(x, Difference(DiffType::Same(_))));
    if all_same {
        Ok(())
    } else {
        Err(diffs)
    }
}

#[derive(Clone)]
pub enum Expr<T> {
    Arrow(T, T),
    Integer,
    Product(Vec<T>),
    String,
}

#[derive(Clone)]
pub struct Forall(pub Type);

pub struct Difference(DiffType);

enum DiffType {
    Same(Type),
    Diff(Type, Type),
    Later(Box<Expr<Difference>>),
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
        let mut builder = Builder::default();
        let int = builder.solved(Expr::Integer);
        let p1 = builder.solved(Expr::Product(vec![int.clone()]));
        let p2 = builder.solved(Expr::Product(vec![int.clone(), int.clone()]));
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
}
