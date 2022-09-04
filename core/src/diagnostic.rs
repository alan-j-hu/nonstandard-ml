use super::syntax;
use crate::elab::typ::Diff;

#[derive(Debug)]
pub enum Error<'syn> {
    Syntax(syntax::Error<'syn>),
    RedefinedVar(String),
    UnboundVar(String),
    Unify(Diff),
    Internal(String),
}
