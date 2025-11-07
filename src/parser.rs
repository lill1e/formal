use crate::lexer::{SymbolToken, Token};
use std::{iter::Peekable, vec::IntoIter};

#[derive(Debug, Clone)]
pub enum Node {
    Number(i32),
    Addition(Box<Node>, Box<Node>),
    Subtraction(Box<Node>, Box<Node>),
    Begin(Vec<Node>, Box<Node>),
}

impl Node {
    fn stringify(&self) -> String {
        match self {
            Node::Number(n) => n.to_string(),
            Node::Addition(b1, b2) => format!("(+ {} {})", (*b1).stringify(), (*b2).stringify()),
            Node::Subtraction(b1, b2) => format!("(- {} {})", (*b1).stringify(), (*b2).stringify()),
            Node::Begin(nodes, last) => format!(
                "{}\n{}",
                nodes
                    .iter()
                    .map(|n| n.stringify())
                    .collect::<Vec<String>>()
                    .join("\n"),
                last.stringify()
            ),
        }
    }
}

impl ToString for Node {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

fn consume_semicolon(iter: &mut Peekable<IntoIter<Token>>) -> Option<Token> {
    iter.next_if(|t| matches!(t, Token::Semicolon))
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

fn parse_unary(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = iter.next_if(|t| matches!(t, Token::Symbol(SymbolToken::Minus))) {
        match token {
            Token::Symbol(SymbolToken::Minus) => match parse_number(iter) {
                Node::Number(n) => Node::Number(n * -1),
                n => n,
            },
            _ => parse_number(iter),
        }
    } else {
        return parse_number(iter);
    }
}

fn parse_binary(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_unary(iter);
    while let Some(tok) = iter.next_if(|t| {
        matches!(
            t,
            Token::Symbol(SymbolToken::Plus) | Token::Symbol(SymbolToken::Minus)
        )
    }) {
        let rhs = parse_unary(iter);
        match tok {
            Token::Symbol(SymbolToken::Plus) => lhs = Node::Addition(Box::new(lhs), Box::new(rhs)),
            Token::Symbol(SymbolToken::Minus) => {
                lhs = Node::Subtraction(Box::new(lhs), Box::new(rhs))
            }
            _ => {}
        }
    }
    return lhs;
}

fn parse_expression(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_binary(iter);
}

fn parse_statement(iter: &mut Peekable<IntoIter<Token>>) -> Option<Node> {
    match iter.peek() {
        Some(_) => {
            let expr = parse_expression(iter);
            consume_semicolon(iter);
            Some(expr)
        }
        None => None,
    }
}

pub fn parse(tokens: Vec<Token>) -> Node {
    let mut statements: Vec<Node> = Vec::new();
    let mut iter = tokens.into_iter().peekable();
    while let Some(statement) = parse_statement(&mut iter) {
        statements.push(statement);
    }
    let last = statements.pop().unwrap_or(Node::Number(0));
    return Node::Begin(statements, Box::new(last));
}
