use crate::parser::Node;

impl Node {
    pub fn interpret(&self) -> u32 {
        match self {
            Node::Number(n) => *n,
            Node::Addition(n, m) => n.interpret() + m.interpret(),
        }
    }
}
