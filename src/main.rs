use std::{
    fs,
    process::{Command, exit},
};

mod asm_gen;
mod code_emission;
mod lexer;
mod parser;
mod queue;
mod tac;

use clap::Parser;

use crate::{asm_gen::gen_asm, code_emission::*, lexer::lex_string, parser::parse, tac::gen_tac};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
    #[arg(long)]
    codegen: bool,
    #[arg(long)]
    tacky: bool,

    filename: String,
}

fn asm_output_name(input: &str) -> String {
    if let Some(pos) = input.rfind('.') {
        format!("{}.s", &input[..pos])
    } else {
        format!("{}.s", input)
    }
}

fn exe_output_name(input: &str) -> String {
    if let Some(pos) = input.rfind('.') {
        input[..pos].to_string()
    } else {
        input.to_string()
    }
}

fn main() {
    let args = Args::parse();

    let content = fs::read_to_string(&args.filename).expect("Failed to read the input file");

    println!("Processing file: {}", args.filename);

    let lexeme = lex_string(content);
    println!("Lexeme: {:?}", lexeme);
    if args.lex {
        return;
    }

    let ast = parse(lexeme);
    println!("AST:\n{:#?}", ast);
    if args.parse {
        return;
    }

    let tac_ast = gen_tac(ast);
    println!("TAC-AST:\n{:#?}", tac_ast);
    if args.tacky {
        return;
    }

    let asm_ast = gen_asm(tac_ast);
    println!("ASM-AST:\n{:#?}", asm_ast);
    if args.codegen {
        return;
    }

    let asm = emit_assembly(&asm_ast);
    println!("Assembly:\n{:#?}", asm);

    let output_file = asm_output_name(&args.filename);

    fs::write(&output_file, asm).unwrap_or_else(|_| {
        eprintln!("Failed to write assembly output");
        exit(1);
    });

    println!("Assembly written to {}", output_file);

    let exe_file = exe_output_name(&args.filename);

    assemble_and_link(&output_file, &exe_file);

    println!("Executable written to {}", exe_file);

    println!("Running executable...");

    let status = Command::new(exe_file)
        .status()
        .expect("Failed to execute program");

    match status.code() {
        Some(code) => {
            let signed = code as i8;
            println!("Program result: {}", signed);
        }
        None => println!("Program terminated by signal"),
    }
}
