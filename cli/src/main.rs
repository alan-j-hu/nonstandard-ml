use bumpalo::Bump;
use clap::Parser;
use nonstandard_ml::{
    diagnostic::Error,
    elab,
    stringpool::StringPool,
    syntax::{self, lexer},
};
use std::fs;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(value_parser)]
    input: String,
}

fn compile<'a>(program: &'a str, bump: &'a Bump) -> Result<(), Error<'a>> {
    let pool_bump = Bump::new();
    let mut pool = StringPool::new(&pool_bump);
    let lexer = lexer::Lexer::new_with_state(&program, lexer::State::new(&mut pool));
    let dec = syntax::parse(bump, lexer).map_err(|e| Error::Syntax(e))?;
    let typed = Bump::new();
    let _ = elab::elab(&typed, &dec)?;
    drop(dec);
    Ok(())
}

fn main() {
    let args = Args::parse();
    match fs::read_to_string(&args.input) {
        Err(e) => {
            eprintln!("{}", e)
        }
        Ok(program) => {
            let bump = Bump::new();
            match compile(&program, &bump) {
                Ok(()) => {}
                Err(ref e) => {
                    eprintln!("{:?}", e)
                }
            }
        }
    }
}
