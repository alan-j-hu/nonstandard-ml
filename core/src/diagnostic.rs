use super::syntax;

#[derive(Debug)]
pub enum Error<'a> {
    Syntax(syntax::Error<'a>),
    Internal(String),
}
