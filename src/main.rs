use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod lexer;
lalrpop_mod!(pub parser);

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use crate::{ast, lexer, parser};
    use bumpalo::Bump;
    use lalrpop_util::ParseError;
    use lexgen_util::{LexerError, Loc};

    type Error<'a> = ParseError<Loc, lexer::Token<'a>, LexerError<lexer::Error<'a>>>;

    fn parse<'a>(bump: &'a Bump, s: &'a str) -> Result<ast::Dec<'a>, Error<'a>> {
        let lexer = lexer::Lexer::new(s);
        parser::DecParser::new().parse(bump, lexer)
    }

    #[test]
    fn test() {
        assert!(parse(&Bump::new(), "val x = 2").is_ok());
        assert!(parse(&Bump::new(), "val x = (fn x => 2) 3").is_ok());
        assert!(parse(&Bump::new(), "val x = case x of x => 2 | _ => 3 end").is_ok());
    }
}
