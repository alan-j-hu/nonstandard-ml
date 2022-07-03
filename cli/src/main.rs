use bumpalo::{Bump, collections::Vec, vec};
use clap::Parser;
use nonstandard_ml::{
    diagnostic::Error,
    elab::{Elaborator, ctx},
    syntax::{lexer, parser},
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
    let dec = parser::ProgramParser::new()
        .parse(bump, lexer)
        .map_err(|e| Error::Syntax(e))?;
    let typed = Bump::new();
    let mut elab = Elaborator::new();
    let mut v = vec!{in &typed};
    let scope = ctx::Scope::new();
    let ctx = ctx::Ctx::new(&scope);
    let _ = elab.elab_dec(&typed, &mut v, &ctx, &dec)
        .map_err(|_| Error::Elab)?;
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
