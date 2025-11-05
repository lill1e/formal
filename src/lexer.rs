use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub enum SymbolToken {
    Plus,
}
#[derive(Debug, Clone)]
pub enum Token {
    Number(u32),
    Symbol(SymbolToken),
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
    return Token::Number(n);
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
                '+' => {
                    tokens.push(Token::Symbol(SymbolToken::Plus));
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
