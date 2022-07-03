use super::syntax;

pub enum Error<'a> {
    Syntax(syntax::Error<'a>),
    Elab,
}
