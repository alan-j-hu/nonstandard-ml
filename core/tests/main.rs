use bumpalo::Bump;
use nonstandard_ml::{
    cps,
    diagnostic::Error,
    elab, ssa,
    stringpool::StringPool,
    syntax::{self, lexer},
};
use std::fs;

fn test_syntax_fail(y: &str) {
    let pool_bump = Bump::new();
    let mut pool = StringPool::new(&pool_bump);
    let program = fs::read_to_string(y).unwrap();
    let lexer = lexer::Lexer::new_with_state(&program, lexer::State::new(&mut pool));
    let bump = Bump::new();
    assert!(syntax::parse(&bump, lexer).is_err())
}

fn test_typecheck_fail(y: &str) {
    let pool_bump = Bump::new();
    let mut pool = StringPool::new(&pool_bump);
    let program = fs::read_to_string(y).unwrap();
    let lexer = lexer::Lexer::new_with_state(&program, lexer::State::new(&mut pool));
    let bump = Bump::new();
    let dec = syntax::parse(&bump, lexer).unwrap();
    let typed = Bump::new();
    assert!(elab::elab(&typed, &dec).is_err())
}

fn test_pass(y: &str) {
    let pool_bump = Bump::new();
    let mut pool = StringPool::new(&pool_bump);
    let program = fs::read_to_string(y).unwrap();
    let lexer = lexer::Lexer::new_with_state(&program, lexer::State::new(&mut pool));
    let bump = Bump::new();
    let dec = syntax::parse(&bump, lexer).unwrap();
    let typed = Bump::new();
    let (_, dec) = elab::elab(&typed, &dec).unwrap();
    drop(bump);
    let cps = Bump::new();
    let (ret_addr, cexp) = cps::convert(&typed, &cps, &dec).unwrap();
    let ssa = Bump::new();
    ssa::compile(&ssa, ret_addr, &cexp).unwrap();
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
run_pass!(twodecs, "tests/pass/twodecs.nml");
run_pass!(decswildcard, "tests/pass/decswildcard.nml");
