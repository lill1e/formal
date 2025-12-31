use std::collections::HashMap;

use crate::parser::Node;
use anyhow::{Ok, Result, bail};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Returnable {
    Number(i32),
    Boolean(bool),
    Void,
}

impl ToString for Returnable {
    fn to_string(&self) -> String {
        match self {
            Returnable::Number(n) => String::from(n.to_string()),
            Returnable::Boolean(b) => String::from(b.to_string()),
            Returnable::Void => String::from("Void"),
        }
    }
}

impl Returnable {
    fn unwrap_int(&self) -> Result<i32> {
        match self {
            Returnable::Number(n) => Ok(*n),
            _ => bail!("Invalid Type"),
        }
    }
}

impl Node {
    pub fn interpret(&self, env: &mut HashMap<String, Returnable>) -> Returnable {
        match self {
            Node::Addition(n, m) => n.interpret(env) + m.interpret(env),
            Node::Subtraction(n, m) => n.interpret(env) - m.interpret(env),
            Node::Void => Returnable::Void,
            Node::Number(n) => Returnable::Number(*n),
            Node::Begin(nodes, last) => {
                for node in nodes {
                    node.interpret(env);
                }
                return last.interpret(env);
            }
            Node::Let(sym, rhs, body) => {
                let rhs_new = rhs.interpret(env);
                env.insert(sym.clone(), rhs_new);
                body.interpret(env)
            }
            Node::Reference(sym) => env[sym].clone(),
            Node::Assignment(sym, rhs) => {
                let v = rhs.interpret(env);
                env.insert(sym.clone(), v);
                return Returnable::Number(0);
            }
        }
    }
}
