use crate::parser::Node;

impl Node {
    pub fn interpret(&self) -> i32 {
        match self {
            Node::Number(n) => *n,
            Node::Addition(n, m) => n.interpret() + m.interpret(),
            Node::Subtraction(n, m) => n.interpret() - m.interpret(),
            Node::Begin(nodes, last) => {
                for node in nodes {
                    node.interpret();
                }
                return last.interpret();
            }
        }
    }
}
