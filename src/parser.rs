use crate::lexer::{Keyword, SymbolToken, Token};
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
    Equals,
    NotEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
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
    If(Box<Node>, Box<Node>, Box<Node>),
    While(Box<Node>, Box<Node>),
}

impl Node {
    fn stringify(&self) -> String {
        match self {
            Node::Void => String::from("(void)"),
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
                "(begin {}{})",
                nodes
                    .iter()
                    .map(|n| n.stringify())
                    .collect::<Vec<String>>()
                    .join(" ")
                    + (if nodes.is_empty() { "" } else { " " }),
                last.stringify()
            ),
            Node::Let(sym, rhs, body) => {
                format!("(let [({} {})] {})", sym, rhs.stringify(), body.stringify())
            }
            Node::Reference(sym) => sym.clone(),
            Node::Assignment(sym, rhs) => format!("(set! {} {})", sym, rhs.stringify()),
            Node::Binary(BinaryOperation::Equals, b1, b2) => {
                format!("(eq? {} {})", b1.stringify(), b2.stringify())
            }
            Node::Binary(BinaryOperation::NotEquals, b1, b2) => {
                format!("(not (eq? {} {}))", b1.stringify(), b2.stringify())
            }
            Node::Binary(BinaryOperation::Greater, b1, b2) => {
                format!("(> {} {})", b1.stringify(), b2.stringify())
            }
            Node::Binary(BinaryOperation::GreaterEqual, b1, b2) => {
                format!("(>= {} {})", b1.stringify(), b2.stringify())
            }
            Node::Binary(BinaryOperation::Less, b1, b2) => {
                format!("(< {} {})", b1.stringify(), b2.stringify())
            }
            Node::Binary(BinaryOperation::LessEqual, b1, b2) => {
                format!("(<= {} {})", b1.stringify(), b2.stringify())
            }
            Node::If(cond, conseq, alt) => format!(
                "(if {} {} {})",
                cond.stringify(),
                conseq.stringify(),
                alt.stringify()
            ),
            Node::While(cond, body) => format!("(while {} {})", cond.stringify(), body.stringify()),
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

fn consume_keyword(iter: &mut Peekable<IntoIter<Token>>, keyword: &Keyword) -> Option<Token> {
    iter.next_if(|t| matches!(t, Token::Keyword(kw) if keyword == kw))
}

fn parse_unit(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) = iter.next_if(|t| {
        matches!(
            t,
            Token::Number(_)
                | Token::Identifier(_)
                | Token::Boolean(_)
                | Token::Symbol(SymbolToken::Void)
                | Token::Symbol(SymbolToken::LeftParen)
        )
    }) {
        match token {
            Token::Number(n) => Node::Number(n),
            Token::Boolean(b) => Node::Boolean(b),
            Token::Identifier(ident) => Node::Reference(ident),
            Token::Symbol(SymbolToken::Void) => Node::Void,
            Token::Symbol(SymbolToken::LeftParen) => {
                let expr = parse_expression(iter);
                match iter.peek() {
                    Some(Token::Symbol(SymbolToken::RightParen)) => {
                        iter.next();
                    }
                    _ => panic!("Expected Closing Parenthesis"),
                }
                expr
            }
            _ => Node::Void,
        }
    } else {
        panic!("Invalid Program");
    }
}

fn parse_unary(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    if let Some(token) =
        iter.next_if(|t| matches!(t, Token::Symbol(SymbolToken::Minus | SymbolToken::Bang)))
    {
        match token {
            Token::Symbol(SymbolToken::Minus) => match parse_unary(iter) {
                Node::Number(n) => Node::Unary(UnaryOperation::Negation, Box::new(Node::Number(n))),
                Node::Unary(UnaryOperation::Negation, child) => *child,
                n => n,
            },
            Token::Symbol(SymbolToken::Bang) => match parse_unary(iter) {
                Node::Boolean(b) => Node::Unary(UnaryOperation::Not, Box::new(Node::Boolean(b))),
                Node::Unary(UnaryOperation::Not, child) => *child,
                b => b,
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
    let mut lhs = parse_binary(iter);
    while let Some(tok) = iter.next_if(|t| {
        matches!(
            t,
            Token::Symbol(
                SymbolToken::Greater
                    | SymbolToken::GreaterEqual
                    | SymbolToken::Less
                    | SymbolToken::LessEqual
            )
        )
    }) {
        let rhs = parse_binary(iter);
        match tok {
            Token::Symbol(SymbolToken::Greater) => {
                lhs = Node::Binary(BinaryOperation::Greater, Box::new(lhs), Box::new(rhs))
            }
            Token::Symbol(SymbolToken::GreaterEqual) => {
                lhs = Node::Binary(BinaryOperation::GreaterEqual, Box::new(lhs), Box::new(rhs))
            }
            Token::Symbol(SymbolToken::Less) => {
                lhs = Node::Binary(BinaryOperation::Less, Box::new(lhs), Box::new(rhs))
            }
            Token::Symbol(SymbolToken::LessEqual) => {
                lhs = Node::Binary(BinaryOperation::LessEqual, Box::new(lhs), Box::new(rhs))
            }
            _ => {}
        }
    }
    return lhs;
}

fn parse_eq(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_cmp(iter);
    while let Some(tok) = iter.next_if(|t| {
        matches!(
            t,
            Token::Symbol(SymbolToken::DoubleEquals | SymbolToken::NotEquals)
        )
    }) {
        let rhs = parse_cmp(iter);
        match tok {
            Token::Symbol(SymbolToken::DoubleEquals) => {
                lhs = Node::Binary(BinaryOperation::Equals, Box::new(lhs), Box::new(rhs))
            }
            Token::Symbol(SymbolToken::NotEquals) => {
                lhs = Node::Binary(BinaryOperation::NotEquals, Box::new(lhs), Box::new(rhs))
            }
            _ => {}
        }
    }
    return lhs;
}

fn parse_assigment(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut lhs = parse_eq(iter);
    if let Some(_) = consume_equals(iter) {
        let rhs = parse_eq(iter);
        lhs = Node::Assignment(
            match lhs {
                Node::Reference(var) => var,
                _ => String::from("_"),
            },
            Box::new(rhs),
        )
    }
    return lhs;
}

fn parse_expression(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    return parse_assigment(iter);
}

fn parse_statement(iter: &mut Peekable<IntoIter<Token>>) -> Option<Node> {
    match iter.peek() {
        Some(Token::Symbol(SymbolToken::LeftBrace)) => {
            iter.next();
            let mut statements: Vec<Node> = Vec::new();
            while let Some(tok) = iter.peek() {
                match tok {
                    Token::Symbol(SymbolToken::RightBrace) => {
                        iter.next();
                        break;
                    }
                    _ => {
                        if let Some(statement) = parse_top(iter) {
                            statements.push(statement);
                        }
                    }
                }
            }
            let last = statements.pop().unwrap_or(Node::Void);
            Some(Node::Begin(statements, Box::new(last)))
        }
        Some(Token::Keyword(Keyword::If)) => {
            iter.next();
            Some(Node::If(
                Box::new(parse_expression(iter)),
                Box::new(parse_statement(iter).unwrap_or(Node::Void)),
                Box::new(match iter.peek() {
                    Some(Token::Keyword(Keyword::Else)) => {
                        iter.next();
                        parse_statement(iter).unwrap_or(Node::Void)
                    }
                    _ => Node::Void,
                }),
            ))
        }
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
                        return Some(Node::Let(sym, Box::new(rhs), Box::new(parse_iter(iter))));
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
        Some(Token::Keyword(Keyword::While)) => {
            iter.next();
            Some(Node::While(
                Box::new(parse_expression(iter)),
                Box::new(parse_statement(iter).unwrap_or(Node::Void)),
            ))
        }
        Some(Token::Symbol(SymbolToken::RightBrace)) => None,
        None => None,
        _ => parse_statement(iter),
    }
}

fn parse_iter(iter: &mut Peekable<IntoIter<Token>>) -> Node {
    let mut statements: Vec<Node> = Vec::new();
    while let Some(statement) = parse_top(iter) {
        statements.push(statement);
    }
    let last = statements.pop().unwrap_or(Node::Void);
    return Node::Begin(statements, Box::new(last));
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
