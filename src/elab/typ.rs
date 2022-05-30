use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
enum State {
    Parent(Rc<RefCell<Type>>),
    Solved(Expr<Rc<RefCell<Type>>>),
    Unsolved,
}

#[derive(Clone)]
pub struct Type(State);

impl Type {
    pub fn unsolved() -> Type {
        Type(State::Unsolved)
    }

    pub fn solved(e: Expr<Rc<RefCell<Type>>>) -> Type {
        Type(State::Solved(e))
    }
}

pub fn find(ty: Rc<RefCell<Type>>) -> Option<Expr<Rc<RefCell<Type>>>> {
    loop {
        let Type(state) = &ty.borrow().clone();
        match state {
            State::Parent(ref p) => *ty.borrow_mut() = p.borrow().clone(),
            State::Solved(ref e) => return Some(e.clone()),
            State::Unsolved => return None,
        }
    }
}

pub fn unify(lhs: Rc<RefCell<Type>>, rhs: Rc<RefCell<Type>>) -> Result<(), Difference> {
    match (find(lhs.clone()), find(rhs.clone())) {
        (Some(e1), Some(e2)) => match (e1, e2) {
            (Expr::Arrow(ty1, ty2), Expr::Arrow(ty3, ty4)) => match unify4(ty1, ty2, ty3, ty4) {
                None => Ok(()),
                Some((diff1, diff2)) => Err(Difference(DiffType::Later(Box::new(Expr::Arrow(
                    diff1, diff2,
                ))))),
            },
            (Expr::Integer, Expr::Integer) => Ok(()),
            (Expr::Product(tys1), Expr::Product(tys2)) => {
                if tys1.len() != tys2.len() {
                    Err(Difference(DiffType::Diff(lhs, rhs)))
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
            (_, _) => Err(Difference(DiffType::Diff(lhs, rhs))),
        },
        (Some(e), None) => {
            *rhs.borrow_mut() = Type(State::Solved(e));
            Ok(())
        }
        (None, Some(e)) => {
            *lhs.borrow_mut() = Type(State::Solved(e));
            Ok(())
        }
        (None, None) => {
            *lhs.borrow_mut() = Type(State::Parent(rhs));
            Ok(())
        }
    }
}

pub fn unify4(
    ty1: Rc<RefCell<Type>>,
    ty2: Rc<RefCell<Type>>,
    ty3: Rc<RefCell<Type>>,
    ty4: Rc<RefCell<Type>>,
) -> Option<(Difference, Difference)> {
    match (unify(ty1.clone(), ty3), unify(ty2.clone(), ty4)) {
        (Ok(()), Ok(())) => None,
        (Ok(()), Err(diff)) => Some((Difference(DiffType::Same(ty1)), diff)),
        (Err(diff), Ok(())) => Some((diff, Difference(DiffType::Same(ty2)))),
        (Err(diff1), Err(diff2)) => Some((diff1, diff2)),
    }
}

pub fn unify_many<'a, I1, I2>(lhs: I1, rhs: I2) -> Result<(), Vec<Difference>>
where
    I1: Iterator<Item = &'a Rc<RefCell<Type>>>,
    I2: Iterator<Item = &'a Rc<RefCell<Type>>>,
{
    let diffs: Vec<Difference> = lhs
        .zip(rhs)
        .map(|(lhs, rhs)| match unify(lhs.clone(), rhs.clone()) {
            Ok(()) => Difference(DiffType::Same(lhs.clone())),
            Err(diff) => diff,
        })
        .collect();
    let all_same = diffs.iter().all(|x| match x {
        Difference(DiffType::Same(_)) => true,
        _ => false,
    });
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

pub struct Difference(DiffType);

enum DiffType {
    Same(Rc<RefCell<Type>>),
    Diff(Rc<RefCell<Type>>, Rc<RefCell<Type>>),
    Later(Box<Expr<Difference>>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unify_unsolved() {
        let us = Rc::new(RefCell::new(Type::unsolved()));
        let s_ty = Rc::new(RefCell::new(Type::solved(Expr::String)));
        assert!(find(us.clone()).is_none());
        assert!(unify(us.clone(), s_ty.clone()).is_ok());
        assert!(find(us.clone()).is_some());
    }

    #[test]
    fn unify_product() {
        let int = Rc::new(RefCell::new(Type::solved(Expr::Integer)));
        let p1 = Rc::new(RefCell::new(Type::solved(Expr::Product(vec![int.clone()]))));
        let p2 = Rc::new(RefCell::new(Type::solved(Expr::Product(vec![
            int.clone(),
            int.clone(),
        ]))));
        assert!(unify(p1.clone(), p1.clone()).is_ok());
        assert!(unify(p1, p2).is_err());
    }

    #[test]
    fn unify_arrow() {
        let int = Rc::new(RefCell::new(Type::solved(Expr::Integer)));
        let us = Rc::new(RefCell::new(Type::unsolved()));
        let a1 = Rc::new(RefCell::new(Type::solved(Expr::Arrow(
            int.clone(),
            int.clone(),
        ))));
        let a2 = Rc::new(RefCell::new(Type::solved(Expr::Arrow(
            int.clone(),
            us.clone(),
        ))));
        assert!(find(us.clone()).is_none());
        assert!(unify(a1.clone(), a2.clone()).is_ok());
        assert!(find(us.clone()).is_some());
    }
}
