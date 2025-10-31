mod interp;
mod lexer;
mod parser;

use lexer::lex;
use parser::parse;

fn main() {
    let l = lex(String::from("21+10+5+3+2+1"));
    let p = parse(l);
    println!("expected result: {}", p.interpret());
    println!("parse: {}", &p.to_string());
}
