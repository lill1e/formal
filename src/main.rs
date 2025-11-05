mod explicate_control;
mod instructions;
mod interp;
mod lexer;
mod parser;
mod rco;

use anyhow::{Result, anyhow, bail};
use clap::Parser;
use std::{fs::File, io::Write, path::Path, process::Command};

use instructions::Instructions;
use lexer::lex;
use parser::parse;

use crate::instructions::Block;

fn stringify_blocks(blocks: &Vec<Block>) -> String {
    return blocks
        .iter()
        .map(|block| block.to_string())
        .collect::<Vec<String>>()
        .join("\n");
}

#[derive(Parser)]
struct Args {
    /// Whether to compile to a Lexical Analysis of the given program (Debug)
    #[arg(long)]
    lex: bool,

    /// Whether to compile to an Abstract Syntax Tree of the given program (Debug)
    #[arg(long)]
    parse: bool,

    /// Wheether to run the program after compiling (requires --output)
    #[arg(long)]
    run: bool,

    /// The program to compile
    #[arg(short, long)]
    program: String,

    /// The file to output the compiled program to
    #[arg(short, long)]
    output: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if args.lex {
        println!(
            "Input Program: {}\nLexical Analysis: {:?}",
            args.program.clone(),
            lex(args.program)
        );
    } else if args.parse {
        let p = args.program.clone();
        let l = lex(args.program);
        let ast = parse(l.clone());
        println!(
            "Input Program: {}\nLexical Analysis: {:?}\nAbstract Syntax Tree: {:?}",
            p, l, ast
        );
    } else {
        let p = args.program;
        let ast = parse(lex(p.clone()));
        let memory_asm = ast
            .clone()
            .remove_complex_operands()
            .explicate_control()
            .select_instructions()
            .assign_homes();
        let patched = memory_asm.0.patch_instructions().generate_asm(memory_asm.1);
        let blocks_str = stringify_blocks(&patched);
        let output_file = args.output.unwrap_or(String::from("temp"));

        // let output_file = args.output.unwrap_or(String::from(p_output));
        File::create(Path::new(&format!("{}.s", &output_file)))
            .expect("There was an error writing to the given file")
            .write_all((blocks_str + "\n").as_bytes())
            .expect("There was an error writing to the given file");
        let gcc_output = Command::new("gcc")
            .arg(format!("{}.s", &output_file))
            .arg("-o")
            .arg(format!("{}.out", &output_file))
            .output()?;
        if !gcc_output.status.success() {
            bail!("{}", String::from_utf8(gcc_output.stderr)?);
        }

        println!(
            "Input Program: {}\nExpected Output: {}\nProgram Exit Code: {}",
            p,
            ast.interpret(),
            Command::new(format!("./{}.out", &output_file))
                .output()?
                .status
                .code()
                .ok_or(anyhow!("Error getting program exit code"))?
        );
    }
    Ok(())
}
