use std::{fs, process::exit};

mod lexer;
mod queue;
mod parser;

use clap::Parser;

use crate::lexer::lex_string;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
    #[arg(long)]
    codegen: bool,

    filename: String,
}

fn main() {
    let args = Args::parse();

    
    let content = fs::read_to_string(&args.filename)
        .expect("Failed to read the input file");

    println!("Processing file: {}", args.filename);

    println!("Lexeme: {:?}", lex_string(content));
    if args.lex {
        return;
    }

    if args.parse {
        return;
    }

    if args.codegen {
        return;
    }

   
}
