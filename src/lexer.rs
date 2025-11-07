use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub enum SymbolToken {
    Plus,
    Minus,
}
#[derive(Debug, Clone)]
pub enum Token {
    Number(i32),
    Symbol(SymbolToken),
    Semicolon,
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

pub fn lex_char(c: &char) -> Option<SymbolToken> {
    match c {
        '+' => Some(SymbolToken::Plus),
        '-' => Some(SymbolToken::Minus),
        _ => None,
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
                '+' | '-' => match lex_char(c) {
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
                _ => {
                    iter.next();
                }
            },
            None => {}
        }
    }
    tokens
}
