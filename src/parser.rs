use crate::lexer::{SymbolToken, Token};
use std::{iter::Peekable, vec::IntoIter};

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Negation,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
    Addition,
    Subtraction,
}

#[derive(Debug, Clone)]
pub enum Node {
    Void,
    Number(i32),
    Boolean(bool),
    Begin(Vec<Node>, Box<Node>),
    Let(String, Box<Node>, Box<Node>),
    Reference(String),
    Assignment(String, Box<Node>),
    Unary(UnaryOperation, Box<Node>),
    Binary(BinaryOperation, Box<Node>, Box<Node>),
}

impl Node {
    fn stringify(&self) -> String {
        match self {
            Node::Void => String::from("void"),
            Node::Number(n) => n.to_string(),
            Node::Boolean(b) => b.to_string(),
            Node::Unary(UnaryOperation::Negation, b) => format!("(- {})", b.stringify()),
            Node::Unary(UnaryOperation::Not, b) => format!("(not {})", b.stringify()),
            Node::Binary(BinaryOperation::Addition, b1, b2) => {
                format!("(+ {} {})", b1.stringify(), b2.stringify())
            }
            Node::Binary(BinaryOperation::Subtraction, b1, b2) => {
                format!("(- {} {})", b1.stringify(), b2.stringify())
            }
            Node::Begin(nodes, last) => format!(
                "{}\n{}",
                nodes
                    .iter()
                    .map(|n| n.stringify())
                    .collect::<Vec<String>>()
                    .join("\n"),
                last.stringify()
            ),
            Node::Let(sym, rhs, body) => {
                format!("(let [({} {})] {})", sym, rhs.stringify(), body.stringify())
            }
            Node::Reference(sym) => sym.clone(),
            Node::Assignment(sym, rhs) => format!("(set! {} {})", sym, rhs.stringify()),
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

fn consume_let(iter: &mut Peekable<IntoIter<Token>>) -> Option<Token> {
    iter.next_if(|t| matches!(t, Token::Keyword(Keyword::Let)))
}

fn consume_equals(iter: &mut Peekable<IntoIter<Token>>) -> Option<Token> {
    iter.next_if(|t| matches!(t, Token::Symbol(SymbolToken::Equals)))
}

fn parse_unit(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = iter.next_if(|t| {
        matches!(
            t,
            Token::Number(_)
                | Token::Identifier(_)
                | Token::Boolean(_)
                | Token::Symbol(SymbolToken::Void)
        )
    }) {
        match token {
            Token::Number(n) => Node::Number(n),
            Token::Boolean(b) => Node::Boolean(b),
            Token::Identifier(ident) => Node::Reference(ident),
            Token::Symbol(SymbolToken::Void) => Node::Void,
            _ => Node::Void,
        }
    } else {
        panic!("Invalid Program");
    }
}

fn parse_unary(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = iter.next_if(|t| matches!(t, Token::Symbol(SymbolToken::Minus))) {
        match token {
            Token::Symbol(SymbolToken::Minus) => match parse_unit(iter) {
                Node::Number(n) => Node::Unary(UnaryOperation::Negation, Box::new(Node::Number(n))),
                n => n,
            },
            _ => parse_unit(iter),
        }
    } else {
        return parse_unit(iter);
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
            Token::Symbol(SymbolToken::Plus) => {
                lhs = Node::Binary(BinaryOperation::Addition, Box::new(lhs), Box::new(rhs))
            }
            Token::Symbol(SymbolToken::Minus) => {
                lhs = Node::Binary(BinaryOperation::Subtraction, Box::new(lhs), Box::new(rhs))
            }
            _ => {}
        }
    }
    return lhs;
}

fn parse_cmp(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_binary(iter);
}

fn parse_eq(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_binary(iter);
}

fn parse_assigment(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_eq(iter);
}

fn parse_expression(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_assigment(iter);
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

fn parse_binding(iter: &mut Peekable<IntoIter<Token>>) -> Option<Node> {
    if let Some(_) = consume_let(iter) {
        return match iter.next_if(|t| matches!(t, Token::Identifier(_))) {
            Some(Token::Identifier(sym)) => {
                if let Some(_) = consume_equals(iter) {
                    if let Some(rhs) = parse_statement(iter) {
                        if let Some(body) = parse_top(iter) {
                            return Some(Node::Let(sym, Box::new(rhs), Box::new(body)));
                        }
                    }
                    return None;
                }
                return parse_statement(iter);
            }
            _ => parse_statement(iter),
        };
    }
    parse_statement(iter)
}

fn parse_top(iter: &mut Peekable<IntoIter<Token>>) -> Option<Node> {
    match iter.peek() {
        Some(Token::Keyword(Keyword::Let)) => parse_binding(iter),
        None => None,
        _ => parse_statement(iter),
    }
}

pub fn parse(tokens: Vec<Token>) -> Node {
    let mut statements: Vec<Node> = Vec::new();
    let mut iter = tokens.into_iter().peekable();
    while let Some(statement) = parse_top(&mut iter) {
        statements.push(statement);
    }
    let last = statements.pop().unwrap_or(Node::Void);
    return Node::Begin(statements, Box::new(last));
}
