use bumpalo::Bump;
use clap::Parser;
use nonstandard_ml::{
    diagnostic::Error,
    elab,
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
    let lexer = lexer::Lexer::new(&program);
    let dec = syntax::parse(bump, lexer).map_err(|e| Error::Syntax(e))?;
    let typed = Bump::new();
    let _ = elab::elab(&typed, &dec).map_err(|_| Error::Elab)?;
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
                Err(Error::Syntax(_)) => {
                    eprintln!("Syntax error")
                }
                Err(Error::Elab) => {
                    eprintln!("Type error")
                }
            }
        }
    }
}
