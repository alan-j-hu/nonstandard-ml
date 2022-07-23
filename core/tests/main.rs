use bumpalo::Bump;
use nonstandard_ml::{
    cps,
    diagnostic::Error,
    elab,
    syntax::{lexer, parser},
};
use std::fs;

fn test_syntax_fail(y: &str) {
    let program = fs::read_to_string(y).unwrap();
    let lexer = lexer::Lexer::new(&program);
    let bump = Bump::new();
    assert!(parser::ProgramParser::new().parse(&bump, lexer).is_err())
}

fn test_typecheck_fail(y: &str) {
    let program = fs::read_to_string(y).unwrap();
    let lexer = lexer::Lexer::new(&program);
    let bump = Bump::new();
    let dec = parser::ProgramParser::new().parse(&bump, lexer).unwrap();
    let typed = Bump::new();
    assert!(elab::elab(&typed, &dec).is_err())
}

fn test_pass(y: &str) {
    let program = fs::read_to_string(y).unwrap();
    let lexer = lexer::Lexer::new(&program);
    let bump = Bump::new();
    let dec = parser::ProgramParser::new().parse(&bump, lexer).unwrap();
    let typed = Bump::new();
    let (_, dec) = elab::elab(&typed, &dec).unwrap();
    let cps = Bump::new();
    cps::convert::convert(&typed, &cps, &dec).unwrap();
}

macro_rules! syntax_fail {
    ($x:ident, $y:expr) => {
        #[test]
        fn $x() {
            test_syntax_fail($y)
        }
    };
}

macro_rules! typing_fail {
    ($x:ident, $y:expr) => {
        #[test]
        fn $x() {
            test_typecheck_fail($y)
        }
    };
}

macro_rules! run_pass {
    ($x:ident, $y:expr) => {
        #[test]
        fn $x() {
            test_pass($y)
        }
    };
}

syntax_fail!(fail1, "tests/syntax-fail/syn1.nml");
typing_fail!(typ1, "tests/typing-fail/typ1.nml");
run_pass!(pass1, "tests/pass/pass1.nml");
run_pass!(curry1, "tests/pass/curry1.nml");
run_pass!(curry2, "tests/pass/curry2.nml");
