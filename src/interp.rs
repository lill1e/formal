use std::collections::HashMap;

use crate::parser::Node;

impl Node {
    pub fn interpret(&self, env: &mut HashMap<String, i32>) -> i32 {
        match self {
            Node::Number(n) => *n,
            Node::Addition(n, m) => n.interpret(env) + m.interpret(env),
            Node::Subtraction(n, m) => n.interpret(env) - m.interpret(env),
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
            Node::Reference(sym) => env[sym],
        }
    }
}
