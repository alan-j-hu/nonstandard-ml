use lexgen::lexer;
use std::string::String;

#[derive(Clone, Debug)]
pub enum Token<'a> {
    Bar,
    Comma,
    Equal,
    Hyphen,
    Semicolon,
    Underscore,
    ThickArrow,
    LParen,
    RParen,
    IntegerLit(&'a str),
    StringLit(String),
    Var(&'a str),
    CaseKw,
    EndKw,
    FnKw,
    InKw,
    LetKw,
    OfKw,
    ValKw,
}

pub struct State {
    buf: String,
}

impl Default for State {
    fn default() -> Self {
        Self { buf: String::new() }
    }
}

pub enum Error<'a> {
    UnknownEscape(&'a str),
}

lexer! {
    pub Lexer(State) -> Token<'input>;

    type Error = Error<'input>;

    rule Init {
        $$whitespace,
        ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* => |lexer| {
            match lexer.match_() {
                "case" => lexer.return_(Token::CaseKw),
                "end" => lexer.return_(Token::EndKw),
                "fn" => lexer.return_(Token::FnKw),
                "in" => lexer.return_(Token::InKw),
                "let" => lexer.return_(Token::LetKw),
                "of" => lexer.return_(Token::OfKw),
                "val" => lexer.return_(Token::ValKw),
                s => lexer.return_(Token::Var(s)),
            }
        },
        ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* => |lexer| {
            lexer.return_(Token::Var(lexer.match_()))
        },
        ['0'-'9']+ => |lexer| {
            lexer.return_(Token::IntegerLit(lexer.match_()))
        },
        '|' = Token::Bar,
        ',' = Token::Comma,
        '=' = Token::Equal,
        '-' = Token::Hyphen,
        ';' = Token::Semicolon,
        '_' = Token::Underscore,
        '(' = Token::LParen,
        ')' = Token::RParen,
        '"' => |lexer| {
            lexer.switch(LexerRule::StringLit)
        },
        "=>" = Token::ThickArrow,
    }

    rule StringLit {
        '"' => |lexer| {
            let finished = std::mem::take(&mut lexer.state().buf);
            lexer.switch_and_return(LexerRule::Init, Token::StringLit(finished))
        },
        '\\' _ =? |lexer| {
            match lexer.match_() {
                "\\n" => {
                    lexer.state().buf.push('\n');
                    lexer.continue_()
                },
                "\\t" => {
                    lexer.state().buf.push('\t');
                    lexer.continue_()
                },
                "\\\"" => {
                    lexer.state().buf.push('\"');
                    lexer.continue_()
                },
                "\\\'" => {
                    lexer.state().buf.push('\'');
                    lexer.continue_()
                },
                s => {
                    lexer.return_(Err(Error::UnknownEscape(s)))
                },
            }
        },
        _ => |lexer| {
            let s = lexer.match_();
            lexer.state().buf.push_str(s);
            lexer.continue_()
        },
    }
}
