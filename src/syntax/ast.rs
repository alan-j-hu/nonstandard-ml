pub struct Located<T> {
    pub span: (lexgen_util::Loc, lexgen_util::Loc),
    pub node: T,
}

pub enum Pat<'a> {
    Or(&'a Located<Pat<'a>>, &'a Located<Pat<'a>>),
    Var(&'a str),
    Wild,
}

pub enum Exp<'a> {
    Apply(&'a Located<Exp<'a>>, &'a Located<Exp<'a>>),
    Case(&'a Located<Exp<'a>>, &'a [Case<'a>]),
    Integer(&'a str),
    Lambda(&'a [Case<'a>]),
    Let(&'a Located<Dec<'a>>, &'a Located<Exp<'a>>),
    String(&'a str),
    Var(&'a str),
}

pub struct Case<'a> {
    pub lhs: Located<Pat<'a>>,
    pub rhs: Located<Exp<'a>>,
}

pub enum Dec<'a> {
    And(&'a Located<Dec<'a>>, &'a Located<Dec<'a>>),
    Seq(&'a Located<Dec<'a>>, &'a Located<Dec<'a>>),
    Val(&'a Located<Pat<'a>>, &'a Located<Exp<'a>>),
}