use std::{iter::Peekable, str::Chars};

use anyhow::{Result, bail};

#[derive(Debug, Clone)]
pub enum SymbolToken {
    Plus,
    Minus,
    Bang,
    Equals,
    DoubleEquals,
    NotEquals,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Void,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Let,
    If,
    Else,
    While,
}

#[derive(Debug, Clone)]
pub enum Token {
    Number(i32),
    Symbol(SymbolToken),
    Semicolon,
    Keyword(Keyword),
    Identifier(String),
    Boolean(bool),
}

fn lex_number(iter: &mut Peekable<Chars<'_>>) -> Token {
    let mut n: u32 = 0;
    while iter.peek().is_some() {
        let curr = iter.peek();
        match curr {
            Some(c) => match c {
                c if c.is_numeric() => match c.to_digit(10) {
                    Some(digit) => {
                        n *= 10;
                        n += digit;
                        iter.next();
                    }
                    None => break,
                },
                _ => break,
            },
            None => break,
        }
    }
    return Token::Number(n as i32);
}

pub fn lex_symbol(c: &char) -> Option<SymbolToken> {
    match c {
        '+' => Some(SymbolToken::Plus),
        '-' => Some(SymbolToken::Minus),
        '=' => Some(SymbolToken::Equals),
        '{' => Some(SymbolToken::LeftBrace),
        '}' => Some(SymbolToken::RightBrace),
        '(' => Some(SymbolToken::LeftParen),
        ')' => Some(SymbolToken::RightParen),
        '!' => Some(SymbolToken::Bang),
        '>' => Some(SymbolToken::Greater),
        '<' => Some(SymbolToken::Less),
        _ => None,
    }
}

pub fn lex_word(iter: &mut Peekable<Chars<'_>>) -> Token {
    let mut acc = String::new();
    while let Some(c) = iter.next_if(|&c| c.is_alphanumeric()) {
        acc.push(c);
    }
    match acc.as_str() {
        "let" => Token::Keyword(Keyword::Let),
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "void" => Token::Symbol(SymbolToken::Void),
        "if" => Token::Keyword(Keyword::If),
        "else" => Token::Keyword(Keyword::Else),
        "while" => Token::Keyword(Keyword::While),
        _ => Token::Identifier(acc),
    }
}

pub fn lex(s: String) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut iter = s.chars().into_iter().peekable();
    while iter.peek().is_some() {
        if let Some(c) =
            iter.next_if(|v| matches!(v, '+' | '-' | '=' | '{' | '}' | '(' | ')' | '!' | '>' | '<'))
        {
            if let Some(c2) = iter.next_if(|v| matches!(v, '=')) {
                match (c, c2) {
                    ('!', '=') => tokens.push(Token::Symbol(SymbolToken::NotEquals)),
                    ('=', '=') => tokens.push(Token::Symbol(SymbolToken::DoubleEquals)),
                    ('>', '=') => tokens.push(Token::Symbol(SymbolToken::GreaterEqual)),
                    ('<', '=') => tokens.push(Token::Symbol(SymbolToken::LessEqual)),
                    _ => bail!("Invalid Symbol"),
                }
            } else {
                match lex_symbol(&c) {
                    Some(tok) => tokens.push(Token::Symbol(tok)),
                    None => bail!("Uncheck Clause (Symbol Lexing)"),
                }
            }
        } else {
            if let Some(c) = iter.peek() {
                match c {
                    _ if c.is_numeric() => tokens.push(lex_number(&mut iter)),
                    _ if c.is_alphanumeric() => tokens.push(lex_word(&mut iter)),
                    ';' => {
                        iter.next();
                        tokens.push(Token::Semicolon);
                    }
                    _ => {
                        iter.next();
                    }
                }
            }
        }
    }
    Ok(tokens)
}
