use crate::lexer::{SymbolToken, Token};
use std::{iter::Peekable, vec::IntoIter};

#[derive(Debug, Clone)]
pub enum Node {
    Number(u32),
    Addition(Box<Node>, Box<Node>),
}

impl Node {
    fn stringify(&self) -> String {
        match self {
            Node::Number(n) => n.to_string(),
            Node::Addition(b1, b2) => format!("(+ {} {})", (*b1).stringify(), (*b2).stringify()),
        }
    }
}

impl ToString for Node {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

fn parse_number(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = iter.next_if(|t| matches!(t, Token::Number(_))) {
        match token {
            Token::Number(n) => Node::Number(n),
            _ => Node::Number(0),
        }
    } else {
        panic!("Invalid Program");
    }
}

fn parse_binary(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_number(iter);
    while let Some(_) = iter.next_if(|t| matches!(t, Token::Symbol(SymbolToken::Plus))) {
        let rhs = parse_number(iter);
        lhs = Node::Addition(Box::new(lhs), Box::new(rhs));
    }
    return lhs;
}

pub fn parse(tokens: Vec<Token>) -> Node {
    let mut iter = tokens.into_iter().peekable();
    return parse_binary(&mut iter);
}
