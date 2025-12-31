mod explicate_control;
mod instructions;
mod interp;
mod lexer;
mod parser;
mod passes;
mod rco;

use anyhow::{Result, anyhow, bail};
use clap::Parser;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::Path,
    process::Command,
};

use instructions::Instructions;
use lexer::lex;
use parser::parse;

use crate::{instructions::Block, passes::passify};

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

    /// The input file to compile
    input: String,

    /// The file to output the compiled program to
    #[arg(short, long)]
    output: Option<String>,

    // Whether to display the result of each compiler pass
    #[arg(short, long)]
    debug: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let program = fs::read_to_string(args.input)?;
    if args.lex {
        println!(
            "Input Program:\n{}\nLexical Analysis: {:?}",
            program.clone(),
            lex(program)?
        );
    } else if args.parse {
        let p = program.clone();
        let l = lex(program)?;
        let ast = parse(l.clone());
        println!(
            "Input Program:\n{}\nLexical Analysis: {:?}\nAbstract Syntax Tree: {:?}",
            p, l, ast
        );
    } else {
        let lex_result = lex(program.clone())?;
        if args.debug && !args.lex {
            println!("Lexical Analysis (Debug): {:?}", lex_result);
        }
        let parsed_ast = parse(lex_result);
        if args.debug && !args.parse {
            println!("Abstract Syntax Tree (Debug):\n{}", parsed_ast.to_string());
        }
        let ast = passify(parsed_ast, args.debug);
        if args.debug {
            println!("Pass Results (Debug):\n{}", ast.to_string());
        }
        let rco = ast.clone().remove_complex_operands();
        if args.debug {
            println!("Remove Complex Operands:\n{}", rco.to_string());
        }
        let explicate_control = rco.clone().explicate_control();
        if args.debug {
            println!("Explicate Control:\n{}", explicate_control.to_string());
        }
        let select_instructions = explicate_control.clone().select_instructions();
        if args.debug {
            println!("Select Instructions: {:?}", select_instructions);
        }
        let assign_homes = select_instructions.clone().assign_homes();
        if args.debug {
            println!("Assign Homes: {:?}", assign_homes.0);
        }
        let patch_instructions = assign_homes
            .0
            .patch_instructions()
            .generate_asm(assign_homes.1);
        if args.debug {
            println!("Patch Instructions:");
            for patched_block in &patch_instructions {
                println!("{}: {:?}", patched_block.label, patched_block.instructions);
            }
            println!("");
        }
        let blocks_str = stringify_blocks(&patch_instructions);
        if args.debug {
            println!("Compield Assembly:\n{}", blocks_str);
        }
        let output_file = args.output.unwrap_or(String::from("temp"));

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
            "Input Program:\n{}\nExpected Output: {}\nProgram Exit Code: {}",
            program,
            ast.interpret(&mut HashMap::new()),
            Command::new(format!("./{}.out", &output_file))
                .output()?
                .status
                .code()
                .ok_or(anyhow!("Error getting program exit code"))?
        );
    }
    Ok(())
}
