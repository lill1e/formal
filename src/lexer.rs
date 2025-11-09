use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub enum SymbolToken {
    Plus,
    Minus,
    Equals,
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Let,
}

#[derive(Debug, Clone)]
pub enum Token {
    Number(i32),
    Symbol(SymbolToken),
    Semicolon,
    Keyword(Keyword),
    Identifier(String),
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
        _ => Token::Identifier(acc),
    }
}

pub fn lex(s: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iter = s.chars().into_iter().peekable();
    while iter.peek().is_some() {
        let curr = iter.peek();
        match curr {
            Some(c) => match c {
                c if c.is_numeric() => {
                    tokens.push(lex_number(&mut iter));
                }
                '+' | '-' | '=' => match lex_symbol(c) {
                    Some(tok) => {
                        tokens.push(Token::Symbol(tok));
                        iter.next();
                    }
                    None => {
                        iter.next();
                    }
                },
                ';' => {
                    tokens.push(Token::Semicolon);
                    iter.next();
                }
                _ if c.is_alphabetic() => tokens.push(lex_word(&mut iter)),
                _ => {
                    iter.next();
                }
            },
            None => {}
        }
    }
    tokens
}
