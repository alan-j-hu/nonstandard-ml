use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
use lexgen_util::{LexerError, Loc};

pub mod ast;
pub mod lexer;
lalrpop_mod!(pub parser, "/syntax/parser.rs");

pub type Error<'a> = ParseError<Loc, lexer::Token<'a>, LexerError<lexer::Error<'a>>>;

#[cfg(test)]
mod tests {
    use super::{ast, lexer, parser, Error};
    use bumpalo::Bump;

    fn parse<'a>(bump: &'a Bump, s: &'a str) -> Result<ast::Dec<'a>, Error<'a>> {
        let lexer = lexer::Lexer::new(s);
        parser::DecParser::new().parse(bump, lexer)
    }

    #[test]
    fn test() {
        assert!(parse(&Bump::new(), "val x = 2").is_ok());
        assert!(parse(&Bump::new(), "val x = fn x => 2 end 3").is_ok());
        assert!(parse(&Bump::new(), "val x = case x of x => 2 | _ => 3 end").is_ok());
        assert!(parse(&Bump::new(), "val x = case x of end").is_ok());
    }
}
