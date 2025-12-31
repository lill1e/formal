use std::collections::HashMap;

use anyhow::{Ok, Result, bail};

use crate::parser::{BinaryOperation, Node, UnaryOperation};

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

    fn unwrap_bool(&self) -> Result<bool> {
        match self {
            Returnable::Boolean(b) => Ok(*b),
            _ => bail!("Invalid Type"),
        }
    }
}

impl Node {
    pub fn interpret(&self, env: &mut HashMap<String, Returnable>) -> Returnable {
        match self {
            Node::Void => Returnable::Void,
            Node::Number(n) => Returnable::Number(*n),
            Node::Boolean(b) => Returnable::Boolean(*b),
            Node::Binary(BinaryOperation::Addition, n, m) => Returnable::Number(
                n.interpret(env).unwrap_int().unwrap() + m.interpret(env).unwrap_int().unwrap(),
            ),
            Node::Binary(BinaryOperation::Subtraction, n, m) => Returnable::Number(
                n.interpret(env).unwrap_int().unwrap() + m.interpret(env).unwrap_int().unwrap(),
            ),
            Node::Begin(nodes, last) => {
                let mut snapshot = env.clone();
                for node in nodes {
                    node.interpret(&mut snapshot);
                }
                return last.interpret(env);
            }
            Node::Let(sym, rhs, body) => {
                env.insert(sym.clone(), rhs.interpret(&mut env.clone()));
                body.interpret(env)
            }
            Node::Reference(sym) => env[sym].clone(),
            Node::Assignment(sym, rhs) => {
                env.insert(sym.clone(), rhs.interpret(&mut env.clone()));
                return Returnable::Void;
            }
            Node::Binary(BinaryOperation::Equals, n, m) => {
                Returnable::Boolean(n.interpret(env) == m.interpret(env))
            }
            Node::Binary(BinaryOperation::NotEquals, n, m) => {
                Returnable::Boolean(n.interpret(env) != m.interpret(env))
            }
            Node::Unary(UnaryOperation::Negation, n) => {
                Returnable::Number(-n.interpret(env).unwrap_int().unwrap())
            }
            Node::Unary(UnaryOperation::Not, b) => {
                Returnable::Boolean(!b.interpret(env).unwrap_bool().unwrap())
            }
            Node::Binary(BinaryOperation::Greater, b1, b2) => Returnable::Boolean(
                b1.interpret(env).unwrap_int().unwrap() > b2.interpret(env).unwrap_int().unwrap(),
            ),
            Node::Binary(BinaryOperation::GreaterEqual, b1, b2) => Returnable::Boolean(
                b1.interpret(env).unwrap_int().unwrap() >= b2.interpret(env).unwrap_int().unwrap(),
            ),
            Node::Binary(BinaryOperation::Less, b1, b2) => Returnable::Boolean(
                b1.interpret(env).unwrap_int().unwrap() < b2.interpret(env).unwrap_int().unwrap(),
            ),
            Node::Binary(BinaryOperation::LessEqual, b1, b2) => Returnable::Boolean(
                b1.interpret(env).unwrap_int().unwrap() <= b2.interpret(env).unwrap_int().unwrap(),
            ),
        }
    }
}
